     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H/TITLE F8196R
      //*************************************************************************
      //  Description:  835 Payment Advice Extract
      //
      //  Created: 02/13/18   Programmer C:180      Project: 114MJC
      //*************************************************************************
      //  Revisions:
      //
      //
      //
      //
      //
      //
      //
      */ PENDING 02/16/18 ***   INCREMENT C1RAID AT GROUP BREAK;
      //                        Group/Vendor break
      //                        Group/Vendor processing
      //
      //
      //
      //
      //

     FFYEA835L1 UF   E           K DISK
     FFYEA835L2 UF   E           K DISK    rename(PEA01:PEA02)
     FFMC1L     UF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDAL2    IF   E           K DISK
     FFTFEL     IF   E           K DISK    usropn
     FFTABL     IF   E           K DISK    usropn
     FFTDGL     IF   E           K DISK
     FFAEEL     IF   E           K DISK
     FFDDEL3    IF   E           K DISK
     FFACLML8   IF   E           K DISK
     FFDCAL19   IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFAICHL    IF   E           K DISK    prefix (Isa)
     FFALINL2   IF   E           K DISK
     FWMCML1    IF   E           K DISK    usropn
     FFA835FP   O    E             DISK    usropn
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *=========================================================================
      *  Entry
      *=========================================================================
     D F8196R          PR
     D                                1A
     D                               18A
     D                                1A
     D                                1A
     D F8196R          PI
     D  @@BillType                    1A
     D  @@Flr                        18A
     D  @@Mode                        1A
     D  @@RetCode                     1A
      *=========================================================================
      *  Prototypes
      *=========================================================================
     D Read835         PR
     D Update835       PR
     D ParseISA        PR
     D ParseIEA        PR
     D ParseGS         PR
     D ParseGE         PR
     D ParseST         PR
     D ParseSE         PR
     D ParseBPR        PR
     D                                3P 0 CONST
     D                                5P 0 CONST
     D                                3P 0 CONST
     D                                9P 0 CONST
     D                                3P 0 CONST

     D GetBPR02        PR            18A
     D                                1A   CONST
     D                                3P 0 CONST
     D                                5P 0 CONST
     D                                3P 0 CONST
     D                                9P 0 CONST
     D                                3P 0 CONST
     D ParseTRN        PR
     D                                1A   CONST
     D ParseREF        PR
     D                                3A   CONST
     D ParseN1         PR
     D                                3A   CONST
     D ParseN3         PR
     D ParseN4         PR
     D ParseLX         PR
     D                                6P 0 CONST
     D ParseCLP        PR
     D ParseCAS        PR
     D                                5A   CONST
     D GetCas02        PR             3A
     D                                3A   CONST
     D                                5A   CONST
     D ParsePER        PR
     D ParseNM1        PR
     D                                3A   CONST
     D ParseSVC        PR
     D GetRaid         PR             9S 0
     D                                3P 0 CONST
     D                                1A   CONST
     D ParseDTM        PR
     D                                3A   CONST
     D                                7P 0 CONST
     D                                3A   CONST
     D                                7P 0 CONST
     D WriteData       PR
     D                              500A
     D Write835File    PR                  EXTPGM('F8196C1')
     D                                1A   CONST
     D                               30A   CONST
     D                              100A   CONST
     D                                1A   CONST
     D                                1A
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D                               56A   CONST
     D                               15P 5 CONST
     D DltOvrF         PR
     D                               10A   CONST
      *
     D GetFileName     PR            30A
     D                               30A   CONST

      /Copy SPRcompany
      /Copy SPRmodule
      *================================================================
      *  Data Structures
      *================================================================
      *
      *  ISA Data Structure
      *
     D ISADS         E DS                  EXTNAME(FAICHP) INZ
     D

     D IEAds           DS
     D  IEA01                         5    INZ
     D  IEA02                         9    INZ

      *
      *  Functional Group Header Data Structure
      *
     D GSDS          E DS                  EXTNAME(FAFGHP) INZ

     D GEds            DS
     D  GE01                          6    INZ
     D  GE02                          9    INZ

      *
      *  Header Data Structure
      *
     D HeadrDS       E DS                  EXTNAME(FAHDRP) INZ
     D   RefIdQu            1431   1433
     D   RefId              1434   1483


     D SEds            DS
     D  SE01                         10    INZ
     D  SE02                          9    INZ
      *
      *  Claim Data Structure
      *
     D ClmDS         E DS                  EXTNAME(FACLMP) PREFIX(ds) INZ

     D BPRds           DS
     D  BPR01                         2    INZ('I')
     D  BPR02                        18    INZ
     D  BPR03                         1    INZ('C')
     D  BPR04                         3    INZ('NON')
     D  BPR16                         8
      *
     D TRNds           DS
     D  TRN01                         2    INZ('1')
     D  TRN02                        50    INZ
     D  TRN03                        10    INZ

     D N1PRDS          DS
     D  N1pr01                        3    INZ
     D  N1pr02                       60    INZ

     D N1PEDS          DS
     D  N1pe01                        3    INZ
     D  N1pe02                       60    INZ
     D  N1pe03                        2    INZ
     D  N1pe04                       80    INZ

     D CLPDS           DS
     D  CLP01                        38A   INZ
     D  CLP02                         2A   INZ
     D  CLP03                        18A   INZ('0.00')
     D  CLP04                        18A   INZ('0.00')
     D  CLP05                        18A   INZ
     D  CLP06                         2A   INZ('WC')
     D  CLP07                        50A   INZ
     D  CLP08                         2A   INZ
     D  CLP09                         1A   INZ
     D  CLP10                         2A   INZ
     D  CLP11                         4A   INZ
     D  CLP12                        15A   INZ

     D CASDS           DS
     D  CAS01                         2A   INZ
     D  CAS02                         5A   INZ
     D  CAS03                        18A   INZ
     D  CAS05                         2A   INZ
     D  CAS06                         5A   INZ
     D  CAS07                        18A   INZ

     D DTMDS           DS
     D  DTM01                         3A   INZ
     D  DTM02                         8A   INZ

     D PERds           DS
     D  PER01                         2    INZ
     D  PER03                         2    INZ
     D  PER04                       256    INZ

     D SVCds           DS
     D  SVC01                       186    INZ
     D    SVC01#1                     2    Overlay(SVC01:1)
     D    SVC01#2                    48    Overlay(SVC01:3)
     D    SVC01#3                     2    Overlay(SVC01:51)
     D    SVC01#4                     2    Overlay(SVC01:53)
     D    SVC01#5                     2    Overlay(SVC01:55)
     D    SVC01#6                     2    Overlay(SVC01:57)
     D    SVC01#7                    80    Overlay(SVC01:59)
     D    SVC01#8                    48    Overlay(SVC01:139)
     D  SVC02                        18    INZ
     D  SVC03                        18    INZ
     D  SVC04                        48    INZ
     D  SVC05                        15    INZ
     D  SVC06                       186    INZ
     D    SVC06#1                     2    Overlay(SVC06:1)
     D    SVC06#2                    48    Overlay(SVC06:3)
     D    SVC06#3                     2    Overlay(SVC06:51)
     D    SVC06#4                     2    Overlay(SVC06:53)
     D    SVC06#5                     2    Overlay(SVC06:55)
     D    SVC06#6                     2    Overlay(SVC06:57)
     D    SVC06#7                    80    Overlay(SVC06:59)
     D    SVC06#8                    48    Overlay(SVC06:139)
     D  SVC07                        15    INZ

     D OVRDBFds        DS            56
     D  OVR01                        19    INZ('OVRDBF FILE(FTABL) ')
     D                                     OVERLAY(OVRDBFds:1)
     D  OVR02                        21    INZ('TOFILE(*LIBL/FTABLxx)')
     D                                     OVERLAY(OVRDBFds:20)
     D    OVR02A                      2A   OVERLAY(OVR02:19)
     D  OVR03                        16    INZ OVERLAY(OVRDBFds:41)
      *================================================================
     D OVRDBF2ds       DS            56
     D  OVR201                       19    INZ('OVRDBF FILE(FTFEL) ')
     D                                     OVERLAY(OVRDBF2ds:1)
     D  OVR202                       21    INZ('TOFILE(*LIBL/FTFELxx)')
     D                                     OVERLAY(OVRDBF2ds:20)
     D    OVR202a                     2A   OVERLAY(OVR202:19)
     D  OVR203                       16    INZ OVERLAY(OVRDBF2ds:41)
      *  Keys
      *================================================================
     D  KeyEOB         DS
     D   EAco#
     D   EAsub#
     D   EAfnd
     D   EAfyr
     D   EAcase
     D   EAeob#

     D  KeyEobDtl2     DS
     D   CAco#
     D   CAsub#
     D   CAfnd
     D   CAfyr
     D   CAcase
     D   CAeob#
     D   CAdseq
     D   EArcn
      *================================================================
      *  Stand alone
      *================================================================
     D Delimiter       S              1A   inz('*')
     D Time6           S               T   INZ
     D Data500         S            500A
     D PrvFnd          S                   LIKE(eafnd) inz
     D PrvCo           S                   LIKE(EAco#) inz
     D PrvSub          S                   LIKE(EAsub#) inz
     D PrvFein         S                   LIKE(EAfein) inz
     D PrvMod          S                   LIKE(EAmod) inz
     D PrvPnam         S                   LIKE(EApnam) inz
     D Raid            S              9P 0 INZ
     D Loop            S              5A   INZ
     D Date70          S              7P 0 INZ
     D Zero            S              4A   INZ('0.00')

     D ##Type          S              1A   INZ
     D ##Id            S             30A
     D ##Path          S             18A
     D ##Mode          S              1A
     D ##RetCod        S              1A   INZ('0')

     D EOL             S              1A   inz('~')
     D Length          S             15P 5 INZ(56)

      *
      * Counters
      *
     D LXcnt           S              6P 0 INZ
     D FunctGrpCnt     S              5P 0 INZ
     D TsetCnt         S              6P 0 INZ
     D SegCnt          S              5P 0 INZ
     D RecCnt          S              5P 0 INZ
     D i               S              5P 0 INZ
     D ErrCnt          S              5P 0 INZ

      *
      *  Indicators
      *
     D isEOF835        S               N   inz(*off)

      *
      *  Arrays
      *
     D TypAry          S             11A   DIM(999) inz
     D EobAry          S              9  0 DIM(999) inz
     D FeinAry         S              9  0 DIM(999) inz
     D FndAry          S              3P 0 DIM(999) inz
     D LoopAry         S              6A   DIM(999) inz
     D SegAry          S             25A   DIM(999) inz
     D ErrAry          S             30A   DIM(999) inz

      *
      *  Constants
      *
     D MissingFein     C                   Const('Missing Fein.')
      /Free

       //===================================================================
       // Main
       //===================================================================
       LxCnt = *zeros;
       Date70 = %dec(%date():*cymd);
       Read835();
       If not isEOF835;
          PrvCo   = EAco#;
          PrvSub  = EAsub#;
          PrvFnd  = Eafnd;
          PrvFein = EAfein;
          PrvMod  = EAmod;
          PrvPnam = EApnam;
          FunctGrpCnt  = 1;
          ParseISA();
          ParseGS();
          ParseST();
          ParseBPR(PrvCo:PrvSub:PrvFnd:PrvFein:PrvMod);
          ParseTRN('R');

          Dow not isEOF835;
             Select;
             When @@BillType = 'G';
                If EAco# <> PrvCo or EAsub# <> PrvSub or EAfnd <> PrvFnd ;
                   Exsr FeinBrk;
                   Exsr FndBrk;
                   FunctGrpCnt = FunctGrpCnt + 1;
                   If ##Retcod = '1';
                      Leave;
                   Endif;
                Else;
                   If EAfein <> PrvFein or EAmod <> PrvMod;
                      Exsr FeinBrk;
                   Endif;
                Endif;

             When @@BillType = 'V'; //Process vendor
                   If EAfein <> PrvFein or EAmod <> PrvMod;
                      Exsr VendorBrk;
                      FunctGrpCnt = FunctGrpCnt + 1;
                   Endif;
                   If ##Retcod = '1';
                      Leave;
                   Endif;
             Endsl;

             Exsr ProcEOB;     //Process EOB
             Update835();
             Read835();
          Enddo;

          If ##Retcod = '0';
             Select;
             When @@BillType = 'G'; //Process Group
                Exsr FeinBrk;
                Exsr FndBrk;
             When @@BillType = 'V'; //Process Vendor
                Exsr VendorBrk;
             Endsl;
          Endif;
       Endif;

       For i = 1 to ErrCnt;
          If *inOF = *On;
             Except HDG;
          Endif;
          If i = 1;
             PrvFnd  = FndAry(1);
             PrvFein = FeinAry(1);
             Except HDG;
          Endif;

          If PrvFnd <> FndAry(i) or PrvFein <> FeinAry(i);
             PrvFnd  = FndAry(i);
             PrvFein = FeinAry(i);
             Except HDG;
          Endif;

          Except Det;
       Endfor;

       If %Open(WMCML1);
          Close WMCML1;
       Endif;

       //Delete prior override
       DltOvrF('FTABL');
       DltOvrF('FTFEL');

       *Inlr = *on;
       //===================================================================
       // Process EOB data
       //===================================================================
       Begsr ProcEOB;
               // Removed ParseREF('F2'); /Situational - Pending criteria
       //Header
       ParseN1('PR');
       ParseN3();
       ParseN4();
       ParseN1('PE');
       ParseN3();
       ParseN4();

       //Detail
       LxCnt = LxCnt + 1;
       ParseLx(LxCnt);

       //Claim Level
       ParseCLP();
       ParseCAS(Loop);
       ParseNM1('QC');
       ParseNM1('IL');
       ParseNM1('82');
       ParseDTM('232':EOfrdt:'233':EOtodt);
       ParsePER();

       //Service Level
       ParseSVC();
       Endsr;
       //===================================================================
       // Fein Control Break
       //===================================================================
       Begsr FeinBrk;
          ParseSE();
          PrvFein = EAfein;
          PrvMod  = EAmod;
          PrvPnam = EApnam;
          LxCnt   = *zeros;
          If isEOF835 = *off and
             PrvCo = EAco# and PrvSub = EAsub# and PrvFnd = EAfnd;
             ParseST();
             ParseBPR(PrvCo:PrvSub:PrvFnd:PrvFein:PrvMod);
             ParseTRN('N');
          Endif;
       Endsr;
       //===================================================================
       // Fund Control Break
       //===================================================================
       Begsr FndBrk;
       ParseGE();                            //Write trailer segments
       ParseIEA();
       Chain (PrvCo:PrvSub:PrvFnd) FMFNL;    //Build File Name
       If %found(FMFNL);
          If %Open(FA835FP);
             Close FA835FP;
          Endif;
          ##Id = GetFileName(FNshnm);
          ##Type = @@BillType;
          ##Path = @@Flr;
          ##Mode = @@Mode;
          ##RetCod = '1';
          Write835file(##Type:##Id:##Path:##Mode:##RetCod); //Copy File
          If ##RetCod = '0' and isEOF835 = *off;
             If not %Open(FA835FP);
                Open FA835FP;
                PrvCo =  EACo#;
                PrvSub = EASub#;
                PrvFnd = EAfnd;
                TsetCnt = *zeros;
                ParseISA();                //Write Header segment for new group
                ParseGS();
                ParseST();
                ParseBPR(PrvCo:PrvSub:PrvFnd:PrvFein:PrvMod);
                ParseTRN('R');
             Endif;
          Endif;
       Endif;
       Endsr;
       //===================================================================
       // Vendor Control Break
       //===================================================================
       Begsr VendorBrk;
       ParseSE();
       ParseGE();                            //Write trailer segments
       ParseIEA();
       If %Open(FA835FP);
          Close FA835FP;
       Endif;
       ##Id = GetFileName(%Subst(PrvPnam:1:30));
       ##Type = @@BillType;
       ##Path = @@Flr;
       ##Mode = @@Mode;
       ##RetCod = '1';
       Write835file(##Type:##Id:##Path:##Mode:##RetCod); //Copy File
       If ##RetCod = '0' and isEOF835 = *off;
          If not %Open(FA835FP);
             Open FA835FP;
             PrvCo   = EACo#;
             PrvSub  = EASub#;
             PrvFnd  = EAfnd;
             PrvFein = EAfein;
             PrvMod  = EAmod;
             PrvPnam = EAPnam;
             LxCnt   = *zeros;
             TsetCnt = *zeros;
             ParseISA();                //Write Header segment for new group
             ParseGS();
             ParseST();
             ParseBPR(PrvCo:PrvSub:PrvFnd:PrvFein:PrvMod);
             ParseTRN('R');
          Endif;
       Endif;
       Endsr;

       //===================================================================
       // Initialization
       //===================================================================
       Begsr *INZSR;
       If not %Open(FA835FP);
          Open FA835FP;
       Endif;
       Select;
       When @@BillType= 'G';
           *in64 = *Off;
       When @@BillType= 'V';
           *in64 = *On;
       Endsl;

       If IsModEnabled('WorkComp');
          If not %Open(WMCML1);
             Open WMCML1;
          Endif;
       Endif;
       Endsr;
      /End-Free
     OQSYSPRT   E            HDG              02
     O                       UDATE         Y
     O                       Time6             +001
     O                                           68 '835 Payment Advice'
     O                                         +001 'Error Report'
     O                                          124 'Page:'
     O                       PAGE          2   +001
      *
     O          E            HDG         1
     O              N64                          66 'Group'
     O              N64      FndAry(i)         +001
     O               64                          64 'Vendor'
     O               64      FeinAry(i)        +001
      *
     O          E            HDG         3
     O                                            4 'EOB#'
     O                                           16 'Type'
     O                                           33 'Loop Id'
     O                                           46 'Segment Id'
     O                                           70 'Error'
      *
     O          E            DET         1
     O                       EobAry(i)            9
     O                       TypAry(i)           23
     O                       LoopAry(i)          32
     O                       SegAry(i)           61
     O                       ErrAry(i)           95
      *=====================================================================
      * Read FYEA835
      *=====================================================================
     P Read835         B
     D Read835         PI
      /Free
       Select;
       When @@BillType= 'G';
          Read PEA01;
          If %eof(FYEA835L1);
             isEOF835 = *On;
          Else;
             RecCnt = RecCnt + 1;
          Endif;
       When @@BillType= 'V';
          Read PEA02;
          If %eof(FYEA835L2);
             isEOF835 = *On;
          Else;
             RecCnt = RecCnt + 1;
          Endif;
       Endsl;
      /End-Free
     P Read835         E
      *=====================================================================
      * Update FYEA835
      *=====================================================================
     P Update835       B
     D Update835       PI
      /Free
       Select;
       When @@BillType= 'G';
          If EA835Q = 'B' and EA835E = *blanks;
             EA835E = 'G';
          Else;
             EA835E = 'E';
          Endif;
          Update PEA01;
       When @@BillType= 'V';
          If EA835Q = 'B' and EA835E = *blanks;
             EA835E = 'V';
          Else;
             EA835E = 'E';
          Endif;
          Update PEA02;
       Endsl;
      /End-Free
     P Update835       E
      *=====================================================================
      * Parse ISA segment
      *=====================================================================
     P ParseISA        B
     D ParseISA        PI
      /Free
       Chain %kds(KeyEob) FACLML8;
       Chain CLMicntl FAICHL;

       Reset Data500;
       Reset ISADS;
       ICHauqu = '00';
       ICHauin = *Blanks;
       ICHsiqu = '00';
       ICHsi = *Blanks;
       ICHinqu = 'ZZ';
       ICHinsid = %editc(GetCompFein(EAco#):'X');
       ICHinidqu = 'ZZ';
       If %found(FAICHL);
          ICHinrid = IsaICHinsid;
       Else;
          ICHinrid = *Blanks;
       Endif;
       ICHindt = %Subst(%Char(%date():*ISO0):3);
       ICHintm =  %Subst((%char(%time():*ISO0)):1:4);
       ICHrepsep = '^';
       ICHinctver = '00501';
       ICHinctl# = %Editc(GetRaid(EAco#:'N'):'X');
       ICHackreq = '0';
       ICHuseind = @@Mode;
       ICHcmelsep = ':';
       DATA500 = 'ISA*' + ICHauqu + Delimiter + ICHauin + Delimiter +
                  ICHsiqu + Delimiter + ICHsi + Delimiter + ICHinqu +
                  Delimiter + ICHinsid + Delimiter + ICHinidqu + Delimiter +
                  ICHinrid + Delimiter + ICHindt + Delimiter +ICHintm +
                  Delimiter + ICHrepsep + Delimiter + ICHinctver + Delimiter +
                  ICHinctl# + Delimiter + ICHackreq + Delimiter + ICHuseind +
                  Delimiter + ICHcmelsep + EOL;
       WriteData(Data500);

      /End-Free
     P ParseISA        E
      *=====================================================================
      * Parse IEA segment
      *=====================================================================
     P ParseIEA        B
     D ParseIEA        PI
      /Free
       Reset Data500;
       Reset IEADS;
       IEA01 = %Editc(FunctGrpCnt:'X');
       IEA02 = ICHinctl#;
       Data500 = 'IEA*' + %Trim(IEA01) + Delimiter + %trim(IEA02) + EOL;
       WriteData(Data500);
       FunctGrpCnt = *zeros;

      /End-Free
     P ParseIEA        E
      *=====================================================================
      * Parse GS segment                             Pending
      *=====================================================================
     P ParseGS         B
     D ParseGS         PI
      /Free
       Reset Data500;
       Reset GSDS;
       FGHidcd = 'HP';
       FGHapsncd = %editc(GetCompFein(EAco#):'X');
       If FGHapsncd = '000000000' or FGHapsncd = *blanks;
          @@RetCode = '1';
          ErrCnt = ErrCnt + 1;
          EobAry(ErrCnt) = *zeros;
          FeinAry(ErrCnt)= *Zeros;
          FndAry(ErrCnt) = EAfnd;
          LoopAry(ErrCnt) = *blanks;
          SegAry(ErrCnt) = 'GS';
          ErrAry(ErrCnt) = MissingFein;
       Endif;

       Select;
       When @@BillType= 'G';
       FGHaprccd = 'Receiver Code';
       When @@BillType= 'V';
       FGHaprccd = %Editc(EAfein:'X');
       Endsl;

       FGHdt = %Char(%date():*ISO0);
       FGHtm = %Char(%time():*ISO0);
       FGHcntl =  %Editc(GetRaid(EAco#:'R'):'X');  //Re-use previously assigned number.
       FGHragcd = 'X';
       FGHveridcd = '005010X221';
       DATA500 = 'GS*' + FGHidcd + Delimiter + %trim(FGHapsncd) + Delimiter +
                  %Trim(FGHaprccd) + Delimiter + FGHdt + Delimiter +
                  %Trim(FGHtm) + Delimiter + %Trim(FGHcntl) + Delimiter +
                  %trim(FGHragcd) + Delimiter + %Trim(FGHveridcd)+ EOL;
       WriteData(Data500);
      /End-Free
     P ParseGS         E
      *=====================================================================
      * Parse GE segment
      *=====================================================================
     P ParseGE         B
     D ParseGE         PI
      /Free
       Reset Data500;
       Reset GEDS;
       GE01 = %Editc(TsetCnt:'X');
       GE02 = FGHcntl;
       Data500 = 'GE*' + %Trim(GE01) + Delimiter + %trim(GE02) + EOL;
       WriteData(Data500);
      /End-Free
     P ParseGE         E
      *=====================================================================
      * Parse ST segment
      *=====================================================================
     P ParseST         B
     D ParseST         PI
      /Free
       TsetCnt = TsetCnt + 1;
       Reset Data500;
        HDtshIDCD = '835';
        HDtshCNTL = %Editc(TsetCnt:'X');
        HDtshICR  = *Blanks;
        DATA500 = 'ST*' + %Trim(HDtshIDCD) + Delimiter + %Trim(HDtshCNTL) + EOL;
        WriteData(Data500);
      /End-Free
     P ParseST         E
      *=====================================================================
      * Parse SE segment
      *=====================================================================
     P ParseSE         B
     D ParseSE         PI
      /Free
       Reset Data500;
        SegCnt = SegCnt + 1;
        SE01 = %Editc(SegCnt:'X');
        SE02 = %Editc(TsetCnt:'X');
        DATA500 = 'SE*' + %Trim(SE01) + Delimiter + %Trim(SE02) + EOL;
        WriteData(Data500);
        SegCnt = *zeros;
      /End-Free
     P ParseSE         E
      *=====================================================================
      * Parse BRT segment
      *=====================================================================
     P ParseBPR        B
     D ParseBPR        PI
     D   BprCo                        3P 0 CONST
     D   BprSub                       5P 0 CONST
     D   BprFnd                       3P 0 CONST
     D   BprFein                      9P 0 CONST
     D   BprMod                       3P 0 CONST
      /Free
        Reset Data500;
        Reset BPRds;
        BPR02 = GetBPR02(@@Billtype:BprCo:BprSub:BprFnd:BprFein:BprMod);
        BPR16 =  %Char(%date():*ISO0);
        DATA500 = 'BPR*' + %Trim(BPR01) + Delimiter + %Trim(BPR02) + Delimiter +
                  %Trim(BPR03) + Delimiter + %Trim(BPR04) + Delimiter +
                  Delimiter + Delimiter + Delimiter + Delimiter +
                  Delimiter + Delimiter + Delimiter + Delimiter +
                  Delimiter + Delimiter + Delimiter + %Trim(BPR16) + EOL;
        WriteData(Data500);
      /End-Free
     P ParseBPR        E
      *=====================================================================
      * Get BPR02 - Monetary amount BRP segment
      *=====================================================================
     P GetBPR02        B
     D GetBPR02        PI            18A
     D  Type835                       1A   Const
     D   BprCo                        3P 0 CONST
     D   BprSub                       5P 0 CONST
     D   BprFnd                       3P 0 CONST
     D   BprFein                      9P 0 CONST
     D   BprMod                       3P 0 CONST

     D TotPaid         S             18P 2
      *
      /Free
        Exec SQL
         Declare C1 cursor for
             Select sum(EApaid) sum01 from FYEA835P
             where EA835Q = :Type835 and EAco# = :BprCo and
                   EAsub# = :BprSub  and EAfnd = :BprFnd and
                   EAFEin = :BprFein and EAmod = :BprMod;

        Exec SQL
           Open C1;

        Dow SQLCOD = *Zero;
           Exec SQL
              Fetch C1 into :TotPaid;
        Enddo;

        Exec SQL
           Close C1;

        If TotPaid > *ZERO;
           Return  %TRIM(%Editc(TotPaid:'L'));
        Else;
           Return '0' +  %TRIM(%Editc(TotPaid:'L'));
        Endif;
      /End-Free
     P GetBPR02        E
      *=====================================================================
      * Parse TRN segment
      *=====================================================================
     P ParseTRN        B
     D ParseTRN        PI
     D  TrnRaidFlg                    1A   Const
      /Free
        Reset Data500;
        Reset TRNds;
        TRN01 = '1';
        TRN02 =  %Editc(GetRaid(EAco#:TrnRaidFlg):'X');

        TRN03 = '1' + %editc(GetCompFein(EAco#):'X');
        If TRN03 = '1000000000';
           @@RetCode = '1';
           ErrCnt = ErrCnt + 1;
           EobAry(ErrCnt) = *zeros;
           FeinAry(ErrCnt)= *Zeros;
           FndAry(ErrCnt) = EAfnd;
           LoopAry(ErrCnt) = *blanks;
           SegAry(ErrCnt) = 'TRN';
           ErrAry(ErrCnt) = MissingFein;
        Endif;

        If @@RetCode <> '1';
           DATA500 = 'TRN*' + %Trim(TRN01) + Delimiter + %Trim(TRN02) +
           Delimiter + %Trim(TRN03)+ EOL;
           WriteData(Data500);
        Endif;
      /End-Free
     P ParseTRN        E
      *=====================================================================
      * Parse Reference
      *=====================================================================
     P ParseRef        B
     D ParseRef        PI
     D  RefQu                         3A   CONST
      /Free
        Reset Data500;
        Reset HeadrDs;
        Select;
        When RefQu = 'F2';   //Version Identification
           Exsr RefF2;
        When RefQu = '6R';   //Line Item Control Number
           Exsr Ref6R;
        When RefQu = '0B';   //Line Item Control Number
           Exsr Ref0B;
        Endsl;
       //===================================================================
       // REF - Version Identification - Situational
       //===================================================================
       Begsr  RefF2;
        Reset Data500;
        RefIdQu = RefQu;
        RefId   = *blanks;   //no longer PENDING - Specs not complete. REMOVED
        DATA500 = 'REF*' + %Trim(RefIdQu) + Delimiter + %Trim(RefId) + EOL;
        WriteData(Data500);
       Endsr;
       //===================================================================
       // REF - Line Item Control Number - Situational
       //===================================================================
       Begsr  Ref6R;
        Reset Data500;
        RefIdQu = RefQu;
        RefId = LINlictl#;
        DATA500 = 'REF*' + %Trim(RefIdQu) + Delimiter + %Trim(RefId) + EOL;
        WriteData(Data500);
       Endsr;
       //===================================================================
       // REF - Rendering Provider Information - Situational
       //===================================================================
       Begsr  Ref0B;
        Reset Data500;
        RefIdQu = RefQu;
        RefId = DHrnlc;
        DATA500 = 'REF*' + %Trim(RefIdQu) + Delimiter + %Trim(RefId) + EOL;
        WriteData(Data500);
       Endsr;
      /End-Free
     P ParseRef        E
      *=====================================================================
      * Parse N1  Payer/Payee Identification
      *=====================================================================
     P ParseN1         B
     D ParseN1         PI
     D  N1entity                      3A   Const

      /Free
       //Process N1 loops
       Select;
       When N1entity = 'PR';  //1000A Payer Identification
          Loop = '1000A';
          Reset N1PRDS;
          Exsr N1@1000A;
       When N1entity = 'PE';  //1000B Payee Identification
          Loop = '1000B';
          Reset N1PEDS;
          Exsr N1@1000B;
        Endsl;
       //===================================================================
       // N1-PR Payer Identification
       //===================================================================
       Begsr  N1@1000A;
        Reset Data500;
        N1PR01 = 'PR';
        N1PR02 = GetCompNam(EAco#);
        DATA500 = 'N1*' + %Trim(N1PR01) + Delimiter + %Trim(N1PR02) + EOL;
        WriteData(Data500);
       Endsr;
       //===================================================================
       // N1-PE Payee Identification
       //===================================================================
       Begsr  N1@1000B;
        Reset Data500;
        N1pe01 = 'PE';
        N1pe02 =  EApnam;
        N1pe03 = 'FI';
        N1pe04 = %Editc(EAfein:'X');
        DATA500 = 'N1*' + %Trim(N1pe01) + Delimiter + %Trim(N1pe02) +
                  Delimiter + %Trim(N1pe03) + Delimiter + %Trim(N1pe04) +
                  EOL;
        WriteData(Data500);
       Endsr;
      /End-Free
     P ParseN1         E
      *=====================================================================
      * Parse N3  Payer/Payee Address
      *=====================================================================
     P ParseN3         B
     D ParseN3         PI
     D Adr1            s             40    INZ
     D Adr2            s             40    INZ
     D City            s             25    INZ
     D State           s              2    INZ
     D Zip             s              9p 0 INZ
      /Free
       //Process N3 loops
       Select;
       When Loop = '1000A';
          Exsr N3@1000A;
       When Loop = '1000B';
          Exsr N3@1000B;
       Endsl;
       //===================================================================
       // N3 Payer Address
       //===================================================================
       Begsr  N3@1000A;
        Reset Data500;
        GetCompAdr(EAco#:'D':Adr1:Adr2:City:State:Zip);
        If Adr1 <> *blanks;
           If Adr2 = *blanks;
              DATA500 = 'N3*' + %Trim(Adr1) + EOL;
           Else;
              DATA500 = 'N3*' + %Trim(Adr1) + Delimiter + %Trim(Adr2) + EOL;
           Endif;
           WriteData(Data500);
        Endif;
       Endsr;
       //===================================================================
       // N3 Payee Address
       //===================================================================
       Begsr  N3@1000B;
        Reset Data500;
        If EAadr1 <> *blanks;
           If EAadr2 = *blanks;
              DATA500 = 'N3*' + %Trim(EAadr1) + EOL;
           Else;
              DATA500 = 'N3*' + %Trim(EAadr1) + Delimiter + %Trim(EAadr2) + EOL;
           Endif;
           WriteData(Data500);
        Endif;
       Endsr;
      /End-Free
     P ParseN3         E
      *=====================================================================
      * Parse N4  - Payer/Payee City, State, and Zip
      *=====================================================================
     P ParseN4         B
     D ParseN4         PI
     D Adr1            s             40    INZ
     D Adr2            s             40    INZ
     D City            s             25    INZ
     D State           s              2    INZ
     D Zip             s              9p 0 INZ
      /Free
       //Process N4 loops
       Select;
       When Loop = '1000A';
          Exsr N4@1000A;
       When Loop = '1000B';
          Exsr N4@1000B;
       Endsl;
       //===================================================================
       // N4 Payer City, State, and Zip
       //===================================================================
       Begsr  N4@1000A;
        Reset Data500;
        GetCompAdr(EAco#:'D':Adr1:Adr2:City:State:Zip);
        If City <> *blanks;
           Data500 = 'N4*' + %Trim(City);
           Select;
           When State <> *blanks and Zip > *zeros;
              Data500 = %Trim(Data500) + Delimiter + State + Delimiter +
                        %Editc(Zip:'X');
           When State <> *blanks and Zip = *zeros;
              Data500 = %Trim(Data500) + Delimiter + State;
           When State = *blanks and Zip > *zeros;
              Data500 = %Trim(Data500) + Delimiter + Delimiter +
                        %Editc(Zip:'X');
           Endsl;
        Endif;

        If Data500 <> *blanks;
           Data500 = %Trim(Data500) + EOL;
           WriteData(Data500);
        Endif;
       Endsr;
       //===================================================================
       // N4 Payee City, State, and Zip
       //===================================================================
       Begsr  N4@1000B;
        Reset Data500;

        If EActy <> *blanks;                      // US style address
           Data500 = 'N4*' + %Trim(EActy);
           Select;
           When EAst <> *blanks and EAzip > *zeros;
              Data500 = %Trim(Data500) + Delimiter + EAst + Delimiter +
                        %Editc(EAzip:'X');
           When EAst <> *blanks and EAzip = *zeros;
              Data500 = %Trim(Data500) + Delimiter + EAst;
           When EAst = *blanks and EAzip > *zeros;
              Data500 = %Trim(Data500) + Delimiter + Delimiter +
                        %Editc(EAzip:'X');
           Endsl;

        Else;                                   // Out of Country address

           Chain (EAfein:EAmod) FMPDL;
           If %found(FMPDL) and (PDctry <> *blanks or PDadr3 <> *blanks);
              Data500 = 'N4*';
              Select;
              When PDctry <> *blanks AND PDadr3 <> *blanks;
                 Data500 = %Trim(Data500) + %Trim(PDctry) + Delimiter +
                           %Trim(PDadr3);
              When PDctry <> *blanks AND PDadr3 = *blanks;
                 Data500 = %Trim(Data500) +  %Trim(PDctry);

              When PDctry = *blanks AND PDadr3 <> *blanks;
                 Data500 = %Trim(Data500) + Delimiter + Delimiter +
                           %Trim(PDadr3);
              Endsl;
           Endif;
        Endif;

        If Data500 <> *blanks;
           Data500 = %Trim(Data500) + EOL;
           WriteData(Data500);
        Endif;
       Endsr;
      /End-Free
     P ParseN4         E
      *=====================================================================
      * Parse LX - Header Number
      *=====================================================================
     P ParseLX         B
     D ParseLX         PI
     D  LinNbr                        6P 0 CONST
      /Free
        Loop = '2000 ';
        Reset Data500;
        Data500 = 'LX' + Delimiter + %Editc(LinNbr:'X') + EOL;
        WriteData(Data500);
      /End-Free
     P ParseLx         E
      *=====================================================================
      * Parse CLP - Claim Payment Information
      *=====================================================================
     P ParseCLP        B
     D ParseCLP        PI
      /Free
        Loop = '2100 ';
        Reset CLPDS;
        Reset Data500;
        Chain %kds(KeyEob) FAEOLG;
        Chain %kds(KeyEob) FAEHL;
        Chain %kds(KeyEob) FAEUL;
        Chain %kds(KeyEob) FDDHL;
        If %found(FAEOLG);
           Exsr OVRDBF;
           Select;
           When EOBtyp = 'N';
              If %found(FAEHL);
                 CLP01 = EHacct;
                 If %Open(WMCML1);
                    Chain (EOco#:EOfnd:EOfyr:EOcase) WMCML1;
                    If %found(WMCML1);
                       CLP01 = %Trim(EHacct) + '-' + %Trim(CMpds#);
                    Endif;
                 Endif;
              Endif;
           When EOBtyp = 'H';
              If %found(FAEUL);
                 CLP01 = EUacct;
                 If %Open(WMCML1);
                    Chain (EOco#:EOfnd:EOfyr:EOcase) WMCML1;
                    If %found(WMCML1);
                       CLP01 = %Trim(EUacct) + '-' + %Trim(CMpds#);
                    Endif;
                 Endif;
              Endif;
           Endsl;

           CLP02 = '1' ;  //Pending possible value 22 for reversals per specs

           If EAchrg <> *zeros;
              CLP03 = %Trim(%Editc(EAchrg:'L'));
           Endif;

           If EApaid <> *zeros;
              CLP04 = %Trim(%Editc(EApaid:'L'));
           Endif;

           CLP06 = 'WC';
           CLP07 =  %Trim(%Editc(EAeob#:'X')) + '-' + %Trim(%Editc(EArcn:'X'));
           Select;
           When EOBtyp = 'N';              //Pending RM to populate EABTYP in 837
              If %found(FDDHL);
                 CLP08 = %Subst(DHpos:2:2);
              Endif;
           When EOBtyp = 'H';
              If %found(FAEUL);
                 CLP08 = %Subst(EUblty:1:2);
                 CLP09 = %Subst(EUblty:3:1);
                 CLP11 = %Subst(%Editc(EUdrg#:'X'):2:4);
                 If EOast = 'PA';
                    Chain EUdrg# FTFEL;
                    If %found(FTFEL);
                       If FErlwt = *zero;
                          CLP12 = Zero;
                       Else;
                          CLP12 = %Trim(%Editc(FErlwt:'L'));
                       Endif;
                    Endif;

                 Else;
                    If EUaddt = *zero;
                       Setll EUdrg# FTDGL;
                       Reade EUdrg# FTDGL;
                    Else;
                       Setgt (EUdrg#:EUaddt) FTDGL;
                       If not %found(FTDGL);
                          Setgt *hival FTDGL;
                       Endif;
                       Readpe EUdrg# FTDGL;
                    Endif;
                    If not %Eof(FTDGL);
                       If DGrelw = *Zero;
                          CLP12 = Zero;
                       Else;
                          CLP12 = %Trim(%Editc(DGrelw:'L'));
                       Endif;
                    Endif;
                 Endif;
              Endif;
           Endsl;
        Endif;

           Data500 = 'CLP*'    + %Trim(CLP01) + Delimiter + %Trim(CLP02) +
                     Delimiter + %Trim(CLP03) + Delimiter + %Trim(CLP04) +
                     Delimiter + %Trim(CLP05) + Delimiter + %Trim(CLP06) +
                     Delimiter + %Trim(CLP07) + Delimiter + %Trim(CLP08) +
                     Delimiter + %Trim(CLP09) + Delimiter + %Trim(CLP10) +
                     Delimiter + %Trim(CLP11) + Delimiter + %Trim(CLP12) + EOL;
           WriteData(Data500);
       //===================================================================
       // Override Database file
       //===================================================================
       Begsr OvrDbf;
       //Delete prior override
       DltOvrf('FTABL');
       DltOvrf('FTFEL');

       //Override File
       OVR02a = EOast;
       QCMDEXC(OvrDbfds:Length);
       If not %Open(FTABL);
         Open FTABL;
       Endif;

       OVR202a = EOast;
       QCMDEXC(OvrDbf2ds:Length);
       If not %Open(FTFEL);
         Open FTFEL;
       Endif;
       Endsr;
      /End-Free
     P ParseCLP        E
      *=====================================================================
      * Parse CAS - Claim Adjustment
      *=====================================================================
     P ParseCAS        B
     D ParseCAS        PI
     D CasLoop                        5A   Const

      /Free
       Reset CASDS;

       Select;
       When CasLoop = '2100';

          If EAndis > *Zero;
             CAS01 = 'CO';
             CAS02 = GetCas02('CO':CasLoop);
             CAS03 = %Trim(%Editc(EAndis:'L'));
             Exsr WriteCasClm;
          Endif;

          //Future - Nice to have check if CAS01 <> *blanks and CAS02 = *blanks, set error message.

          If EAmra> *Zero;
             CAS01 = 'PI';
             CAS02 = GetCas02('PI':CasLoop);
             CAS03 = %Trim(%Editc(EAmra:'L'));
             Exsr WriteCasClm;
          Endif;

          If EAothr > *Zero;
             CAS01 = 'PI';
             CAS02 = GetCas02('PI':CasLoop);
             CAS03 = %Trim(%Editc(EAothr:'L'));
             Exsr WriteCasClm;
          Endif;

          If EAoth2 > *Zero;
             CAS01 = 'PI';
             CAS02 = GetCas02('PI':CasLoop);
             CAS03 = %Trim(%Editc(EAoth2:'L'));
             Exsr WriteCasClm;
          Endif;

       When CasLoop = '2110';

          If CAndis > *Zero;
             CAS01 = 'CO';
             CAS02 = GetCas02('CO':CasLoop);
             CAS03 = %Trim(%Editc(CAndis:'L'));
             Exsr WriteCasClm;
          Endif;

           If CAmra > *Zero;
             CAS01 = 'PI';
             CAS02 = GetCas02('PI1':CasLoop);
             CAS03 = %Trim(%Editc(CAmra:'L'));
             Exsr WriteCasClm;
           Endif;

           If CAothr > *Zero;
             CAS01 = 'PI';
             CAS02 = GetCas02('PI2':CasLoop);
             CAS03 = %Trim(%Editc(CAothr:'L'));
             Exsr WriteCasClm;
           Endif;

           If CAoth2 > *Zero;
             CAS01 = 'PI3';
             CAS02 = GetCas02('PI3':CasLoop);
             CAS03 = %Trim(%Editc(CAoth2:'L'));
             Exsr WriteCasClm;
           Endif;

       Endsl;
       //--------------------------------------------------------------
       // Write CAS segment - Claim Adjustment
       //--------------------------------------------------------------
       Begsr WriteCasClm;
       Reset Data500;
       Data500 = 'CAS*' + %Trim(CAS01) + Delimiter + %Trim(CAS02) +
                 Delimiter + %Trim(CAS03) + EOL;
       WriteData(Data500);
       Endsr;
      /End-Free
     P ParseCAS        E
      *=====================================================================
      *  CAS Segment - Get Cas02 element
      *=====================================================================
     P GetCAS02        B
     D GetCAS02        PI             3A
     D  AdjCode                       3A   Const
     D  Cas2Loop                      5A   Const
     D  xRef2          S              3A   Inz
      /Free
       Select;
       When Cas2Loop = '2100';
          Setll %kds(KeyEob) FAEEL;
          Reade %kds(KeyEob) FAEEL;
          Dow not %eof(FAEEL);
             If EEEOB <> *blanks and EEprnt <> 'N';
                chain (EEco#:EEeob) PEB01;
                If %found(FTABL);
                   If EBxref = AdjCode;
                      xRef2 = EBxrf2;
                      Leave;
                   Endif;
                Endif;
             Endif;
          Reade %kds(KeyEob) FAEEL;
          Enddo;

       When Cas2Loop = '2110';
          Setll (EAco#:EAsub#:EAfnd:EAfyr:EAcase:EAeob#:EArcn:CAdseq) FDDEL3;
          Reade (EAco#:EAsub#:EAfnd:EAfyr:EAcase:EAeob#:EArcn:CAdseq) FDDEL3;
          Dow not %eof(FDDEL3);
             If DEprnt <> 'N';
                chain (DEco#:DEeob) PEB01;
                If %found(FTABL);

                   If DEesrc = 'N' and AdjCode = 'CO';
                      If EBxref = AdjCode;
                         xRef2 = EBxrf2;
                         Leave;
                      Endif;
                   Endif;

                   If CAmra > *Zero and AdjCode = 'PI1';
                      If DEesrc = 'A' or DEesrc = 'U';
                         If EBxref = %subst(AdjCode:1:2);
                            xRef2 = EBxrf2;
                            If DEesrc = 'A';       //Give priority to A
                               Leave;
                            Endif;
                         Endif;
                      Endif;
                   Endif;

                   If CAothr > *Zero and AdjCode = 'PI2';
                      If DEesrc = 'U';
                         If EBxref = %subst(AdjCode:1:2);
                            xRef2 = EBxrf2;
                            Leave;
                         Endif;
                      Endif;
                   Endif;

                   If CAoth2 > *Zero and AdjCode = 'PI3';
                      If DEesrc = 'K';
                         If EBxref = %subst(AdjCode:1:2);
                            xRef2 = EBxrf2;
                            Leave;
                         Endif;
                      Endif;
                   Endif;
                Endif;
             Endif;

          Reade (EAco#:EAsub#:EAfnd:EAfyr:EAcase:EAeob#:EArcn:CAdseq) FDDEL3;
          Enddo;
          Endsl;
          Return xRef2;
      /End-Free
     P GetCAS02        E
      *=====================================================================
      * Parse NM1
      *=====================================================================
     P ParseNM1        B
     D ParseNM1        PI
     D  NM1entity                     3A   CONST

      /Free
       Chain %kds(KeyEob) FAEOLG;
       Chain %kds(KeyEob) FACLML8;

       Select;
       When NM1entity = 'QC';  // Patient Name
          If %found(FAEOLG);
             Exsr NM1$2100QC;
          Endif;

       When NM1entity = 'IL';  // Insured Name
          If %found(FACLML8);
             Exsr NM1$2100IL;
          Endif;

       When NM1entity = '82';  // Service/Rendering Provider Name
          If %found(FACLML8);
             Exsr NM1$210082;
          Endif;
       Endsl;
       //--------------------------------------------------------------
       // NM1 Loop 2100 QC - Patient Name
       //--------------------------------------------------------------
       Begsr NM1$2100QC;
       Reset CLMds;
       Reset Data500;
       dsCLMpatENCD = NM1entity;
       dsCLMpatENQU = '1';
       dsCLMpatLSN  = EOlsn;
       dsCLMpatFSN  = EOfsn;
       dsCLMpatMNM  = EOmid;
       Data500 = 'NM1*' + %Trim(dsCLMpatENCD) + Delimiter +
                 %Trim(dsCLMpatENQU) +  Delimiter + %Trim(dsCLMpatLSN) +
                 Delimiter + %Trim(dsCLMpatFSN) + Delimiter +
                 %Trim(dsCLMpatMNM) + EOL;
       WriteData(Data500);

       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2100 IL - Insured Name
       //--------------------------------------------------------------
       Begsr NM1$2100IL;
       Reset CLMds;
       Reset Data500;
       dsCLMpatENCD = NM1entity;
       dsCLMpatENQU = '2';
       dsCLMsubICQ  = 'MI';
       Data500 = 'NM1*' + %Trim(dsCLMpatENCD) + Delimiter +
                 %Trim(dsCLMpatENQU) + Delimiter + %Trim(CLMsubLSN) +
                 Delimiter + %Trim(CLMsubFSN) + Delimiter + %Trim(CLMsubMID) +
                 Delimiter + Delimiter + Delimiter + %Trim(dsCLMsubICQ) +
                 Delimiter + %Trim(CLMsubSPI) + EOL;

       WriteData(Data500);
       Endsr;
       //--------------------------------------------------------------
       // NM1 Loop 2100 82 - Service/Rendering Provider Name
       //--------------------------------------------------------------
       Begsr NM1$210082;
       Reset CLMds;
       Reset Data500;
       dsCLMrnpIDCD = NM1entity;
       Data500 = 'NM1*' + %Trim(dsCLMrnpIDCD) + Delimiter +
                 %Trim(CLMrnpENQU) + Delimiter + %Trim(CLMrnpNM) +
                 Delimiter + %Trim(CLMrnpFNM) + Delimiter + %Trim(CLMrnpMNM) +
                 Delimiter + Delimiter + Delimiter + %Trim(CLMrnpIDQU) +
                 Delimiter + %Trim(CLMrnpNMID) + EOL;
       WriteData(Data500);
       Endsr;
      /End-Free
     P ParseNM1        E
      *=====================================================================
      * Parse DTM - Statement From or To Date -
      *=====================================================================
     P ParseDTM        B
     D ParseDTM        PI
     D  DTMfrCD                       3A   CONST
     D  DTMfrDT                       7P 0 CONST
     D  DTMtoCD                       3A   CONST
     D  DTMtoDT                       7P 0 CONST
      /Free
       Reset DTMDS;
       DTM01 = DTMfrCD;
       DTM02 = %Char(%date(DTMfrDT:*CYMD):*ISO0);
       Exsr WriteDTM;

       Reset DTMDS;
       DTM01 = DTMtoCD ;
       DTM02 = %Char(%date(DTMtoDT:*CYMD):*ISO0);
       Exsr WriteDTM;

       //--------------------------------------------------------------
       // Write DTM segment - Statement From or To Date
       //--------------------------------------------------------------
       Begsr WriteDTM;
       Reset Data500;
       Data500 = 'DTM*' + %Trim(DTM01) + Delimiter + %Trim(DTM02) + EOL;
       WriteData(Data500);
       Endsr;
      /End-Free
     P ParseDTM        E
      *=====================================================================
      * Parse PER - Claim Contact Information - Situational
      *=====================================================================
     P ParsePER        B
     D ParsePER        PI
      /Free
       Reset PERds;
       PER01 = 'CX';
       PER03 = 'TE';
       PER04 = GetCompPhon(EAco#:'1');

       If PER04 <> *blanks;    // Situational
          Data500 = 'PER*' + %Trim(PER01) + Delimiter + Delimiter + PER03 +
                     Delimiter + %Trim(PER04) + EOL;
          WriteData(Data500);
       Endif;
      /End-Free
     P ParsePER        E
      *=====================================================================
      * Parse SVC - Service Payment Information - Situational 2110
      *=====================================================================
     P ParseSVC        B
     D ParseSVC        PI
      /Free
       Loop = '2110 ';

       If EOBtyp = 'N' or EObtyp = 'H';         //Situational
          Setll %kds(KeyEOB) FDCAL19;
          Reade %kds(KeyEOB) FDCAL19;


          Dow not %EOF(FDCAL19);
             Reset SVCds;
             Exsr GetEobDtl;
             Select;
             When EObtyp = 'N';
                SVC01#2 = DHprcd;
                SVC01#3 = DHmod1;
                SVC01#4 = DHmod2;
                SVC01#5 = DHmod3;
                SVC01#6 = DHmod4;
                SVC04   = *Blanks;
                If %found(FDDHL) and DHbprc <> DHprcd and DHbprc <> *blanks;
                   SVC06#1 = 'HC';
                   SVC06#2 = DHbprc;
                   SVC06#7 = DHprds;
                Endif;

             When EObtyp = 'H';
                SVC01#2 = DUrate;
                SVC01#3 = DUpmod;
                SVC01#4 = DUmod2;
                SVC01#5 = DUmod3;
                SVC01#6 = DUmod4;
                SVC04   = %Editc(DUrvcd:'X');
                If %found(FDDUL) and DUbrat <> DUrate and DUbrat <> *blanks;
                   SVC06#1 = 'HC';
                   SVC06#2 = DUbrat;
                   SVC06#7 = DUdesc;
                Endif;
             Endsl;

             SVC01#1 = 'HC';
             If DAchrg <> *zeros;                              //Required
                SVC02 = %Trim(%Editc(DAchrg:'L'));
             Else;
                SVC02 = '0' + %Trim(%Editc(DAchrg:'L'));
             Endif;

             If DApaid <> *zeros;                              //Required
                SVC03 = %Trim(%Editc(DApaid:'L'));
             Else;
                SVC03 = '0' + %Trim(%Editc(DApaid:'L'));
             Endif;

             If CAunts <> *zeros;                              //Situational
                SVC05 = %Trim(%Editc(CAunts:'L'));
             Endif;

             Exsr WriteSVC;
             ParseDTM('150':CAfdat:'151':CAtdat);
             ParseCas(Loop);

             //Situational - Line item control number.
             Chain %kds(KeyEobDtl2:7) FALINL2;
             If %found(FALINL2) and LINlictl# <> *blanks;
                ParseREF('6R');
             Endif;

             //Situational - Rendering Provider Information.
             Chain %kds(KeyEobDtl2:7) FDDHL;
             If %found(FDDHL) and DHrnlc <> *blanks and EObtyp = 'N';
             ParseREF('0B');
             Endif;

             Reade %kds(KeyEOB) FDCAL19;
          Enddo;
       Endif;

       //--------------------------------------------------------------
       // Get EOB detail (SVC segment)
       //--------------------------------------------------------------
       Begsr GetEobDtl;
       Select;
       When EObtyp = 'H';
        Chain %kds(KeyEobDtl2:7) FDDUL;
       When EObtyp = 'N';
        Chain %kds(KeyEobDtl2:7) FDDHL;
       Endsl;
       Chain %kds(KeyEobDtl2) FDDAL2;
       Endsr;

       //--------------------------------------------------------------
       // Write SVC segment - Service Payment Information
       //--------------------------------------------------------------
       Begsr WriteSVC;
       Reset Data500;
       SVC01 = SVC01#1 + ':' + %Trim(SVC01#2) + ':' + %Trim(SVC01#3) + ':' +
               %Trim(SVC01#4) + ':' + %Trim(SVC01#5) + ':' + %Trim(SVC01#6) +
               ':' + %Trim(SVC01#7) + ':' + %Trim(SVC01#8);
       If SVC06#1 = *blanks;
          Data500 = 'SVC*' + %Trim(SVC01) + Delimiter + %Trim(SVC02) +
                    Delimiter + %Trim(SVC03) + Delimiter + %Trim(SVC04) +
                    Delimiter + %Trim(SVC05) + EOL;
       Else;
          SVC06 = SVC06#1 + ':' + %Trim(SVC06#2) + ':' + %Trim(SVC06#3) + ':' +
                  %Trim(SVC06#4) + ':' + %Trim(SVC06#5) + ':' + %Trim(SVC06#6) +
                  ':' + %Trim(SVC06#7) + ':' + %Trim(SVC06#8);

          Data500 = 'SVC*' + %Trim(SVC01) + Delimiter + %Trim(SVC02) +
                    Delimiter + %Trim(SVC03) + Delimiter + %Trim(SVC04) +
                    Delimiter + %Trim(SVC05) + Delimiter + %Trim(SVC06) +
                    Delimiter + %Trim(SVC07) + EOL;
       Endif;

       WriteData(Data500);
       Endsr;
      /End-Free
     P ParseSVC        E
      *=====================================================================
      * Write Data
      *=====================================================================
     P WriteData       B
     D WriteData       PI
     D  segData                     500A
      /Free
        Clear FA835REC;
        FA835data = SegData;
        Write FA835REC;
        SegCnt = SegCnt + 1;
      /End-Free
     P WriteData       E
      *=====================================================================
      * Get Remittance Advice Id.
      *=====================================================================
     P GetRaid         B
     D GetRaid         PI             9S 0
     D  RaidCo#                       3P 0  CONST
     D  RaidFlag                      1A    CONST
      /Free
       chain(N) RaidCo# PC101;
       If %found;
          If RaidFlag = 'N';               //New RAID
             chain RaidCo# PC101;
             C1raid = C1raid + 1;
             Update PC101;
          Endif;
             Return C1raid;                       //Existing RAID
       Else;
          Return 0;
       Endif;
      /End-Free
     P GetRaid         E
      *=====================================================================
      * Delete Override File
      *=====================================================================
     P DltOvrF         B
     D DltOvrF         PI
     D  File1                        10A   Const
      /Free
       Select;
       When File1 = 'FTABL' and %Open(FTABL);
          Close FTABL;
       When File1 = 'FTFEL' and %Open(FTFEL);
          Close FTFEL;
       Other;
       Endsl;
      /End-Free
     P DltOvrF         E
      *=====================================================================
      * GetFileName - Format file name
      *=====================================================================
     P GetFileName     B
     D GetFileName     PI            30A
     D  InName                       30A   Const

     D Remark          S             30A
     D intPos          S              5  0
      /Free
         Remark = InName;
         intPos = %scan('/':Remark);
         If intPos > 0;
            Dou intPos = *Zeros;
               Remark  = %replace('-':Remark:intPos);
               intPos = %scan('/':Remark);
            Enddo;
         Endif;
         Return Remark;
      /End-Free
     P GetFileName     E
