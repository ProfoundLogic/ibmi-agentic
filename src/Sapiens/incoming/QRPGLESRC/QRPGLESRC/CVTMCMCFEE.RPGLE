     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
      *================================================================
     H* CVTMCMCFEE - Import Bill Review Fees From MCMC
     H*
     H* CREATED: 14/01/2014 C: 282 Project: 46MHL
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H* 02/20/14 C:079 Project: 46MHL                                 *
     H*                Populate missing data element                  *
     H*                                                               *
     H*****************************************************************
      * MCMC Bill Review Holding File
     fFAF1P     if   e             disk    rename(pf101:pf10p)
      * Claim File
     fWMCML15   if   e           k disk
      * Claim File
     fWMCML1    if   e           k disk    rename(pcm01:pcm02)
      * WCMC CHK ISSUANCE CONTROL
     fWMCIL     IF   E           K DISK    rename(pci01:pci0p)
      * Request Payment File
     fWARPP     o    e             disk    rename(prp01:prp0p)
       //*** Beg Add *** 02/20/14 ****************************************
     fWMPDL     IF   E           K DISK
     fWTABL     IF   E           K DISK
       //*** End Add *** 02/20/14 ****************************************
      * Request Payment File Report
     fqsysprt   o    f  132        printer oflind(*inof) usropn
      *
     d EOF             s               n   inz( *OFF )
     d intCo#          s              3  0
     d intFnd          s              3  0
     d intFyr          s              3  0
     d intCase         s              7  0
     d charConm        s             40
     d charFnam        s             40
     d charProcess     s              1
     d intProcessY     s              5  0
     d intProcessN     s              5  0
     d intTotal        s              6  0
     d intPos          s              1  0
     d charDt          s             10
     d charDtMM        s              2
     d charDtDD        s              2
     d charDtYY        s              4
     d cmdLine         s            512
     d cmdLen          s             15  5
     d charPTYP        s              1
     d charSYS         s              1
     d charConst       c                   Const('C289')
     d title           c                   'MCMC Bill Review Fee Listing'
       //*** Beg Add *** 02/20/14 ****************************************
     d kkFein          s              9  0
     d kkMod           s              3  0
     d kkKey           s                   like(PCkey)
     d kkCo#           s              3  0
     d ##rtcd          s              1
     d ##Data          s             75
     d ##eftc          s              1
     d ##Appl          s              2
     d ##Co#           s              3
     d ##Fnd           s              3
     d ##Fyr           s              3
     d ##Case          s              7
     d                 ds
     d data                    1     75
     d  qxTrdt                 1      7  0
     d  qxFrdt                 8     14  0
     d  qxTodt                15     21  0
     d  qxTrcd                22     23
     d  qxRecd                24     25
     d  qxPv1                 26     27
     d  qxPv2                 28     29
     d  qxSeq#                30     34
     d  qxEob#                35     43
     d  qxFein                44     52
     d  qxMod                 53     55
     d  qxDsq#                56     60
       //*** End Add *** 02/20/14 ****************************************
      *
     d                 ds
     d charPds#                1     40
     d  charPds#1              1      4
     d  charPds#2              5      5
     d  charPds#3              6      7
     d  charPds#4              8      8
     d  charPds#5              9     15
     d  charPds#6             16     16
     d  charPds#7             17     40
      *
      * Date & Time structure
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
      *
     d PSDS          ESDS                  EXTNAME(SZQ1P)
       //*** Beg Add *** 02/20/14 ****************************************
     d/copy WPRBANK
       //*** End Add *** 02/20/14 ****************************************
      *
      * ComApi
     d Comapi          pr                  ExtPgm('WA001R')
     d intCo#                         3  0
     d intFnd                         3  0
     d charConm                      40
     d charFnam                      40

      * Qcmdexc
     d Qcmdexc         pr                  ExtPgm('QCMDEXC')
     d cmdLine                      512
     d cmdLen                        15  5

      * Write in Detail record in report file
     d WrtDetailRcd    pr

      * Write in Request Paymet File
     d WriteInPayFile  pr

      * Write in Summary record in report file
     d WrtSummRcd      pr

      * General Prototypes
     d/copy sPrDate

      * Key List
     c     keyWMCML15    Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    charPds#
      *
     c     keyWMCML1     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    intFyr
     c                   Kfld                    intCase
      *
     c     keyWMCIL      Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    charPTYP
     c                   Kfld                    charSYS
       //*** Beg Add *** 02/20/14 ****************************************
     c     keyWMPDL      Klist
     c                   Kfld                    kkFein
     c                   Kfld                    kkMod
     c     keyTBKY       Klist
     c                   Kfld                    kkCo#
     c                   Kfld                    kkKey
       //*** End Add *** 02/20/14 ****************************************
      *
     c                   time                    dsdttm
      /Free
        cmdLine = 'OVRPRTF FILE(QSYSPRT) PAGESIZE(66 132) OVRFLW(66)';
        cmdLen  = %len(%trim(CmdLine));
        Qcmdexc(cmdLine:cmdLen);

        charSYS  = 'P';   // system type
        charPTYP = 'N';   // payee type

        Open Qsysprt;
       // Write the Header in the Report File
        Except TitleFmt;
        Except HeaderFmt;

       ComApi(intCo#:intFnd:charConm:charFnam);
       intFnd = 001;

       // Read each record from MCMC Bill Review Holding File.
       Setll 1 pf10p;
       DoW Not EOF;
          Read pf10p;
          If %EOF(faf1p);
             Leave;
          Endif;                   // %EOF(faf1p)
       //*** Beg Add *** 02/20/14 ***********************************
          If F1fee = 0;
             WrtDetailRcd();
             intProcessN = intProcessN+1;
             Iter;
          Endif;
       //*** End Add *** 02/20/14 ***********************************

       charProcess = 'N';
          If charConst = %subst(F1clm#:1:4);
             charPds#1 = charConst;
             charPds#2 = '-';
             charPds#3 = %subst(F1clm#:5:2);
             charPds#4 = '-';
             charPds#5 = %subst(F1clm#:7:7);
             charPds#6 = '-';
             charPds#7 = %subst(F1clm#:14);
             Chain keyWMCML15 pcm01;
             If %Found(wmcml15);
                WriteInPayFile();
                WrtDetailRcd();
             EndIf;

          Else;

             intFyr  = %Int(%subst(F1clm#:1:3));
             intCase = %Int(%subst(F1clm#:5:7));
             Chain keyWMCML1 pcm02;
             If %Found(wmcml1);
                WriteInPayFile();
                WrtDetailRcd();
             EndIf;
          EndIf;

       If charProcess = 'N';
          WrtDetailRcd();
          intProcessN = intProcessN+1;
       Endif;

       EndDo;
       WrtSummRcd();
       Close Qsysprt;

       *InLr = *On;
      /End-Free
      *********************************************************************
     oqsysprt   e            TitleFmt       1 03
     o                       dsDate             + 2 '  /  /    '
     o                       dsTime             + 3 '  :  :  '
     o                       title               75
     o          e            HeaderFmt   3  1
     o                                           06 'REVIEW'
     o                                           25 'CLAIM'
     o                                           70 'FROM'
     o                                           95 'TO'
     o                                          110 'CHECK'
     o                                          125 'PROCESSED'
     o          e            HeaderFmt      2
     o                                           06 'NUMBER'
     o                                           26 'NUMBER'
     o                                           70 'DATE OF SERVICE'
     o                                           95 'DATE OF SERVICE'
     o                                          110 'AMOUNT'
     o                                          125 'YES/NO'
      *
     o          e            DetailFmt      1
     o                       F1RVW#              15
     o                       F1CLM#              45
     o                       F1FDOS              70
     o                       F1TDOS              95
     o                       F1FEE         J    111
     o                       charProcess        125
      *
     o          e            TotalFmt1   2  1
     o                                           25 'Total records processed ='
     o                       intProcessY         31
     o          e            TotalFmt2      1
     o                                           21 'Total records not pro'
     o                                           45 'cessed (0 pay,clm err) ='
     o                       intProcessN         51
     o          e            TotalFmt3      1
     o                                           13 'Final Total ='
     o                       intTotal            20
      //===================================================================
      // WriteInPayFile - Write record in request payment file
      //===================================================================
     P WriteInPayFile  B
     D WriteInPayFile  PI
      /Free
       charProcess  = 'Y';
       intProcessY += 1;

       RPco#     = CMco#;
       RPfnd     = CMfnd;
       RPfyr     = CMfyr;
       RPcase    = CMcase;
       RPseq#    = 1;
       RPrmds    = F1rvw#;
       RPamt     = F1fee;
       RPtrdt    = DateTo7(systemDate());
       //*** Beg Add *** 02/20/14 ****************************************
       // Provider Information
       RPpgtp    = 'G';
       RPfein    = 141847542;
       RPmod     = 0;
       kkFein    = RPfein;
       kkMod     = RPmod;

       Chain keyWMPDL ppd01;
       If %Found(wmpdl);
         If PDDBA1 <> 'Y';
           RPpnam = PDname;
           RPadr1 = PDadr1;
           RPadr2 = PDadr2;
         Else;
           RPpnam = PDadr1;
           RPadr1 = PDadr2;
           RPadr2 = *blanks;
         EndIf;
         RPcty = PDcty;
         RPst  = PDst;
         RPzip = PDzip;
         RPadr3= *blanks;
         RPctry= *blanks;
         If PDadr3 = *blanks;
           RPctry = PDctry;
           RPadr3 = PDadr3;
         EndIf;
       EndIf;

       RPstat    = ' ';
       RPncci    = CMncci;
       //*** End Add *** 02/20/14 ****************************************

       charDt      = F1fdos;
       intPos      = %scan('/':F1fdos:1);
       Select;
       When intPos = 2;
          charDtMM    = '0' + %subst(F1fdos:1:intPos-1);
       When intPos = 3;
          charDtMM    = %subst(F1fdos:1:intPos-1);
       When intPos = 4;
          charDtMM    = %subst(F1fdos:2:intPos-1);
       EndSl;
       charDt      = *blanks;
       charDt      = %subst(F1fdos:intPos+1);
       intPos      = %scan('/':charDt:1);
       Select;
       When intPos = 2;
          charDtDD    = '0' + %subst(charDt:1:intPos-1);
       When intPos = 3;
          charDtDD    = %subst(charDt:1:intPos-1);
       Endsl;
       charDtYY    = %subst(charDt:intPos+1);
       RPfrdt      = dtCYMD(%Int(charDtMM + charDtDD + charDtYY));

       charDt      = F1tdos;
       intPos      = %scan('/':F1tdos:1);
       Select;
       When intPos = 2;
          charDtMM    = '0' + %subst(F1tdos:1:intPos-1);
       When intPos = 3;
          charDtMM    = %subst(F1tdos:1:intPos-1);
       When intPos = 4;
          charDtMM    = %subst(F1tdos:2:intPos-1);
       EndSl;
       charDt      = *blanks;
       charDt      = %subst(F1tdos:intPos+1);
       intPos      = %scan('/':charDt:1);
       Select;
       When intPos = 2;
          charDtDD    = '0' + %subst(charDt:1:intPos-1);
       When intPos = 3;
          charDtDD    = %subst(charDt:1:intPos-1);
       Endsl;
       charDtYY    = %subst(charDt:intPos+1);
       RPtodt      = dtCYMD(%Int(charDtMM + charDtDD + charDtYY));

       Chain keyWMCIL pci0p;
       If %Found(wmcil);

          If CMltfl = 'I';
             RPbr7  = CIbr7l;
          Else;
             RPbr7  = CIbr7n;
          EndIf;

          If CMcvfl = 'Y';
             RPtrcd = CItrcv;
       //*** Beg Add *** 02/20/14 ***************************************
             RPbr2  = CIp1cv;
       //*** End Add *** 02/20/14 ***************************************
          Else;
             RPtrcd = CItrcd;
       //*** Beg Add *** 02/20/14 ***************************************
             RPbr2  = CIbr2;
       //*** End Add *** 02/20/14 ***************************************
          EndIf;

          RPsts  = CIsts;
       //*** Beg Add *** 02/20/14 ****************************************
       kkCo# = IntCo#;
       kkKey = RPtrcd;
       Chain keyTBKY ppc01;
       If %Found(WTABL);
         RPrecd = PCrecd;
         RPtype = PCtrtp;
         RPdesc = PCdesc;
       Else;
         RPrecd = *blanks;
         RPtype = *blanks;
         RPdesc = *blanks;
       EndIf;

       KKkey = RPrecd;
       Chain keyTBKY prt01;
       If %Found(WTABL);
         RPretp = RTtype;
       Else;
         RPtype = *blanks;
       EndIf;
       RPrcvd    = DateTo7(systemDate());

       ##Co# = %char(intCo#);
       ##Fnd = %char(intFnd);
       ##Fyr = %char(intFyr);
       ##Case = %char(intCase);
       ##appl = 'WC';
       qxTrdt = RPTrdt;
       qxFrdt = RPFrdt;
       qxTrcd = RPtrcd;
       qxRecd = RPRecd;
       qxPv1  = RPbr2;
       qxPv2  = RPbr7;
       qxFein = %char(RPfein);
       qxMod  = %char(RPMod);
       qxDsq# = *blanks;
       ##data = DATA;
       RPcktp = GetBankCode(##Co#: ##fnd: ##Fyr: ##Case: ##appl:
                 ##data: ##rtcd: ##eftc);
       //*** End Add *** 02/20/14 ****************************************

       EndIf;

       RPuser    = Q1user;
       RPpgm     = Q1pgm;
       RPdt      = DateTo7(systemDate());
       RPtime    = TimeTo6(SystemTime());
       RPiusr    = Q1user;
       RPipgm    = Q1pgm;
       RPidt     = DateTo7(systemDate());
       RPitm     = TimeTo6(SystemTime());
       Write prp0p;

      /End-Free
     P WriteInPayFile  E
      //===================================================================
      // WrtDetailRcd - Write Detail Record
      //===================================================================
     P WrtDetailRcd    B
     D WrtDetailRcd    PI
      /Free

       // Write the HeaderFMT
       If *inof;
          Except HeaderFmt;
          *inof   = *off;
       Endif;

       // Write the DetailFMT
       Except DetailFmt;

      /End-Free
     P WrtDetailRcd    E
      //===================================================================
      // WrtSummaryRcd - Write Summary Record
      //===================================================================
     P WrtSummRcd      B
     D WrtSummRcd      PI
      /Free
       // Write the Summary Record
       intTotal = intProcessY + intProcessN;
       Except TotalFmt1;
       Except TotalFmt2;
       Except TotalFmt3;
      /End-Free
     P WrtSummRcd      E
