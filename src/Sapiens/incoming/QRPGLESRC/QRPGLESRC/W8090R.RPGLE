     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
      *================================================================
     H* W8090R - Auto-Load Fee Processing Program
     H*
     H* CREATED: 01/17/2014 C: 282 Project: 133MOH
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H* 02/24/22 C:079 Support: PHW101158                             *
     H*                Change reserve type to EX client flagged       *
     H*                Inactivate 133WSE                              *
     H*                                                               *
     H* 03/23/21 C:180 Project: 133WSE                                *
     H*                Process UR fees from Rising Medical            *
     H*                Added From/To Svc date and Inv #               *
     H*                                                               *
     H* 02/10/17 C:309 Project: 133MPZ                                *
     H*                Passing Fein and Mode as parameter             *
     H*                                                               *
     H* 07/17/14 C:280 Project: 133MOH                                *
     H*                change reserve code from ex to le              *
     H*                                                               *
     H* 04/16/14 C:062 Project: 133MOH Priss 27  (Support 85338)      *
     H*                Set request status to P (Pended)               *
     H*                                                               *
     H* 02/21/14 C:282 Project: 133MOH Priss 18                       *
     H*                Added the handling for 'Claim number must      *
     H*                be numeric.'                                    *
     H*                                                               *
     H* 01/30/14 C:282 Project: 133MOH Priss 18                       *
     H*                Populating the other values in WARPP.          *
     H*                                                               *
     H*****************************************************************
      * MCMC Bill Review Holding File
     fWABHP     if   e             disk    rename(pbh01:pbh0p)
      * Claim File
     fWMCML1    if   e           k disk
      * WCMC CHK ISSUANCE CONTROL
     fWAB1P     o    e           k DISK    rename(pb101:pb10p)
      * Request Payment File
     fWARPP     o    e             disk    rename(prp01:prp0p)
      *** Beg Add *** 01/30/14 *********************************
      * Payee Demographics Mstr File
     fWMPDL     if   e           k disk
      *
     fWTABL     IF   E           K DISK
      *** End Add *** 01/30/14 *********************************
      * Request Payment File Report
     fqsysprt   o    f  132        printer oflind(*inof)
      *
     d EOF             s               n   inz( *OFF )
     d isError         s               n   inz( *OFF )
     d intCo#          s              3  0
     d intFyr          s              3  0
     d intFnd          s              3  0
     d intCase         s              7  0
     d charConm        s             40
     d charFnam        s             40
     d charFFC         s             15
     d intChrg#        s             12  0
     d charChrg        s              9
     d charError       s             40
     d intCost         s              9  2
     d dateValueIn     s              8  0
     d title           c                   'BRS Fees Unprocessed Record'
     d kkCo#           s              3  0
     d kkKey           s                   like(PCkey)
      *
      * Date & Time structure
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
      *
      *** Beg Add *** 01/30/14 *********************************
     d charCo#         s              3
     d charFnd         s              3
     d charFyr         s              3
     d charCase        s              7
     d charAppl        s              2
     d charData        s             75
     d charCktp        s             10
     d charRtcd        s              1
     d charEftc        s              1
     d charDate        s             10
      **** Beg Add *** 02/21/14 ********************************
     d intClm#         s             13  0
     d intLen          s              1  0
     d intPos          s              1  0
     d charCost        s              9
     d errRcdInF       s              1
      **** End Add *** 02/21/14 ********************************
      *** Begin Add *** 03/23/21 *******************************
     d intIx           s              5  0 inz
     d digits          c                   '0123456789'
      *** End   Add *** 03/23/21 *******************************
      *** Begin Add *** 02/24/22 *******************************
     d is133WSE        s              1n   Inz(*off)
      *** End   Add *** 02/24/22 *******************************
      *
     d                 DS
     d  DATA                   1     75
     d  QXTRDT                 1      7  0
     d  QXFRDT                 8     14  0
     d  QXTODT                15     21  0
     d  QXTRCD                22     23
     d  QXRECD                24     25
     d  QXPV1                 26     27
     d  QXPV2                 28     29
     d  QXSEQ#                30     34  0
     d  QXEOB#                35     43
     d  QXFEIN                44     52  0
     d  QXMOD                 53     55  0
     d  QXDSQ#                56     60  0
      *** End Add *** 01/30/14 *********************************
     d PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      * ComApi
     d Comapi          pr                  ExtPgm('WA001R')
     d intCo#                         3  0
     d intFnd                         3  0
     d charConm                      40
     d charFnam                      40

      * Verify the recor is valid
     d ChkValRcd       pr

      * Write in Detail record in report file
     d WrtDetailRcd    pr

      * Write in Request Paymet File
     d WriteInPayFile  pr

      * Write in Request Paymet File
     d WriteBFPmt      pr

      * General Prototypes
     d/copy sPrDate
      // *** Beg Add *** 01/30/14 *********************************************
     d/copy wprbank
      // *** End Add *** 01/30/14 *********************************************
      // *** Beg Add *** 02/24/22 *********************************************
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltId                     3
     d   outCltId      s              3    Inz('000')
      // *** End Add *** 02/24/22 *********************************************

     c     keyWMCML1     Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    intFyr
     c                   Kfld                    intCase
      *
     c     keyTBKY       Klist
     c                   Kfld                    kkCo#
     c                   Kfld                    kkKey
      *
     c                   time                    dsdttm

     c***** Beg Add *** 02/10/17 *******************************************
     c     *Entry        Plist
     c                   Parm                    @@Fein           10
     c                   Parm                    @@Mod             3
     c***** End Add *** 02/10/17 *******************************************

      /Free
       // Write the Header in the Report File
        Except TitleFmt;
        Except HeaderFmt;

       ComApi(intCo#:intFnd:charConm:charFnam);
       //*** Beg Add *** 02/24/22 *********************************************
       getClientId(outCltId);
       //*** End Add *** 02/24/22 *********************************************

       errRcdInF = *Blank;
       // First Pass :
       // Read each record from BRS Fee Holding File.
       Setll 1 pbh0p;
       DoW Not EOF;
          Read pbh0p;
          If %EOF(wabhp);
             Leave;
          Endif;                   // %EOF(wabhp)

       // *** Beg Add *** 02/21/14 ******************************
       intLen  = %Len(BHcost);
       intPos  = %Scan(' ':BHcost:1);
       If intPos <> 0;
          charCost = %SubSt(BHcost:intPos:intLen - (intPos-1)) +
                     %SubSt(BHcost:1:intPos-1);
          Else;
          charCost = BHCost;
       EndIf;

       // Verfying Claim must not be blank.
       If BHClm# = *Blanks;
          charError = *Blanks;
          charError = 'Claim Number has blank value.';
          charFFC   = BHclm#;
          WrtDetailRcd();
          errRcdInF = 'Y';
          Iter;
       EndIf;

       // Verifying Claim must be valid.
       If %Check(' 0123456789':BHclm#) <> 0;
          charError = *Blanks;
          charError = 'Invalid claim number.';
          charFFC   = BHclm#;
          WrtDetailRcd();
          errRcdInF = 'Y';
          Iter;
       EndIf;
          intClm# = %Int(BHclm#);
          BHclm#  = %EditC(intClm#:'X');
       // *** End Add *** 02/21/14 ******************************

          intFnd  = %Int(%SubSt(BHclm#:1:3));
          intFyr  = %Int(%SubSt(BHclm#:4:3));
       // *** Beg Chg *** 02/21/14 ******************************
       // intCase = %Int(BHcase);
       // intCost = %Dec(BHcost:9:2);
          intCase = %Int(%SubSt(BHclm#:7:7));
       // *** End Chg *** 02/21/14 ******************************
          charFFC = %SubSt(BHclm#:1:3) + '-' + %SubSt(BHclm#:4:3) + '-' +
                    %SubSt(BHclm#:7:7);

       // Verify that the record is valid
          ChkValRcd();
       // *** Beg Add *** 02/21/14 ******************************
          If isError = *On;
             errRcdInF = 'Y';
          EndIf;
       EndDo;

       // Second Pass : If no errors then read through the file
       // WABHP - BRS Fee Holding File.
       If errRcdInF = *Blank;
          Setll 1 pbh0p;
          DoW Not EOF;
              Read pbh0p;
              If %EOF(wabhp);
                 Leave;
              Endif;                   // %EOF(wabhp)

              intFnd  = %Int(%SubSt(BHclm#:1:3));
              intFyr  = %Int(%SubSt(BHclm#:4:3));
              intCase = %Int(%SubSt(BHclm#:7:7));
              Chain keyWMCML1 pcm01;

              // Write the record in BRS Fee Payment File.
              WriteBFPmt();
              WriteInPayFile();
          EndDo;
       EndIf;
       // *** End Add *** 02/21/14 ******************************
       // *** Beg Del *** 02/21/14 ******************************
       // If isError = *Off;
       //    Write the record in BRS Fee Payment File.
       //    WriteBFPmt();
       //    WriteInPayFile();
       // EndIf;
       // EndDo;
       // *** End Del *** 02/21/14 ******************************
       *InLr = *On;
      /End-Free
      *********************************************************************
     oqsysprt   e            TitleFmt       1 03
     o                       dsDate             + 2 '  /  /    '
     o                       dsTime             + 3 '  :  :  '
     o                       title               75
     o          e            HeaderFmt   3  1
     o                                           05 'Claim'
     o                                           35 'First Name'
     o                                           54 'Last Name'
     o                                           75 'Cost'
     o                                           85 'Error'
      *
     o          e            DetailFmt      1
     o                       charFFC             15
     o                       Bhfsn               40
     o                       Bhlsn               60
      *** Beg Chg *** 02/21/14 **********************************
     o****                   intCost       J     75
     o                       charCost            75
      *** End Chg *** 02/21/14 **********************************
     o                       charError          120
      *
      //===================================================================
      // ChkValRcd  - Check Valid Record
      //===================================================================
     P ChkValRcd       B
     D ChkValRcd       PI
      /Free

       isError = *Off;
       // Verifying Case must be numeric
       If %Check(' 0123456789':BHcase) <> 0;
          charError = *Blanks;
          charError = 'Invalid data in Case.';
          WrtDetailRcd();
          isError = *On;
       EndIf;

       // Verifying Bills must be numeric
       If %Check(' 0123456789':BH#bil) <> 0;
          charError = *Blanks;
          charError = 'Invalid data in Number of Bills.';
          WrtDetailRcd();
          isError = *On;
       EndIf;

       // Verifying Cost must be numeric
       If %Check(' .0123456789':BHcost) <> 0;
          charError = *Blanks;
          charError = 'Invalid data in Cost of Nurse Amount.';
          WrtDetailRcd();
          isError = *On;
       EndIf;

       // Verifying Claim must exist in Claim File.
       Chain keyWMCML1 pcm01;
       If Not %Found(wmcml1);
          charError = *Blanks;
          charError = 'No match on Claim.';
          WrtDetailRcd();
          isError = *On;
       EndIf;

       // Verifying Date of injury must be a valid
       If %Check('0123456789':BHdoi) <> 0;
          charError = *Blanks;
          charError = 'Invalid date.';
          WrtDetailRcd();
          isError = *On;
       Else;
          If %Int(BHdoi) <> 0;
             dateValueIn=%Int(BHdoi);
             If dtCheck(dateValueIn)=0;
                charError = *Blanks;
                charError = 'Invalid Date.';
                WrtDetailRcd();
                isError=*On;
             EndIf;
          EndIf;
       EndIf;

       // Verifying Charge must be numeric
       If %Check(' 0123456789':BHchrg) <> 0;
          charError = *Blanks;
          charError = 'Invalid data in Charged Amount.';
          WrtDetailRcd();
          isError = *On;
       EndIf;

       //*** Begin Add *** 03/23/21 *************************************
       //*** Begin Add *** 02/24/22 *************************************
       If is133WSE = *on;
       //*** End   Add *** 02/24/22 *************************************
       If BHfrdt = *blanks;
          charError = *Blanks;
          charError = 'Invalid From Service Date.';
          WrtDetailRcd();
          isError=*On;
       Else;
          intIx = %check(Digits:BHfrdt);
          If intIx > *zero;
            charError = *Blanks;
            charError = 'Invalid From Service Date.';
            WrtDetailRcd();
            isError = *On;
          Else;
             If %Int(BHfrdt) <> 0;
                dateValueIn=%Int(BHfrdt);
                If dtCheck(dateValueIn)=0;
                   charError = *Blanks;
                   charError = 'Invalid From Service Date.';
                   WrtDetailRcd();
                   isError=*On;
                EndIf;
             EndIf;
          Endif;
       Endif;


       If BHtodt = *blanks;
          charError = *Blanks;
          charError = 'Invalid To Service Date.';
          WrtDetailRcd();
          isError=*On;
       Else;
          intIx = %check(Digits:BHtodt);
          If intIx > *zero;
            charError = *Blanks;
            charError = 'Invalid To Service Date.';
            WrtDetailRcd();
            isError = *On;
          Else;
            If %Int(BHtodt) <> 0;
               dateValueIn=%Int(BHtodt);
               If dtCheck(dateValueIn)=0;
                  charError = *Blanks;
                  charError = 'Invalid To Service Date.';
                  WrtDetailRcd();
                  isError=*On;
               EndIf;
            EndIf;
          Endif;
       Endif;
       //*** Begin Add *** 02/24/22 *************************************
       Endif;
       //*** End   Add *** 02/24/22 *************************************
       //*** End Add *** 03/23/21 ***************************************
      /End-Free
     P ChkValRcd       E
      //===================================================================
      // WriteBFPmt - Write record in BRS Fee Payment File
      //===================================================================
     P WriteBFPmt      B
     D WriteBFPmt      PI
      /Free
        B1case = %Int(BHcase);
        B1name = BHname;
        B1ntwk = BHntwk;
        B1#bil = %Int(BH#bil);
        B1cost = %Dec(BHcost:9:2);
        B1fyr  = %Int(%SubSt(BHclm#:4:3));
        B1clm# = BHclm#;
        B1ss#  = BHss#;
        B1doi  = dtCYMD(%Int(BHdoi));
        B1lsn  = BHlsn;
        B1fsn  = BHfsn;
        intChrg# = %Int(BHChrg);
      /End-Free
     c                   MOVE      intChrg#      charChrg
     c                   MOVE      charChrg      B1chrg
      /Free
        //*** Begin Add *** 03/23/21 ***********************************
        //*** Begin Add *** 02/24/22 *************************************
        If is133WSE = *on;
        //*** End   Add *** 02/24/22 *************************************
        B1frdt  = dtCYMD(%Int(BHfrdt));
        B1todt  = dtCYMD(%Int(BHtodt));
        B1inv# = BHinv#;
        //*** Begin Add *** 02/24/22 *************************************
        Endif;
        //*** End   Add *** 02/24/22 *************************************
        //*** End   Add *** 03/23/21 ***********************************
        B1prfl = 'Y';
        B1prdt = DateTo7(systemDate());
        B1user = Q1user;
        B1time = TimeTo6(SystemTime());
        B1pgm  = Q1pgm;
        Write pb10p;
      /End-Free
     P WriteBFPmt      E
      //===================================================================
      // WriteInPayFile - Write record in request payment file
      //===================================================================
     P WriteInPayFile  B
     D WriteInPayFile  PI
      /Free

       RPco#     = CMco#;
       RPfnd     = CMfnd;
       RPfyr     = CMfyr;
       RPcase    = CMcase;
       RPseq#    = 1;
       //**** Beg Chg *** 02/10/17 ****************************************
       //RPfein    = 721232583;
       //RPMod     = 0;

       RPfein    = %Dec(@@Fein:9:0);
       RPMod     = %Dec(@@Mod:3:0);
       //**** End Chg *** 02/10/17 ****************************************

       //*** Begin Chg *** 04/16/14 ********************************************
       //RPstat    = ' ';
       RPstat    = 'P';
       //*** End   Chg *** 04/16/14 ********************************************
       RPbr2     = 'QR';

       Select;
       When CMltfl = 'I';
          RPbr7     = 'LT';
       When CMltfl = 'M';
          RPbr7     = 'NL';
       EndSl;
       // *** Beg Add *** 01/30/14 *********************************************
       RPtrdt  = DateTo7(systemDate());
       // *** Beg Chg *** 02/21/14 ******************************
       // RPfrdt  = DateTo7(systemDate());
       // RPtodt  = DateTo7(systemDate());
       //*** Begin Change *** 03/23/21 *****************************
       //RPfrdt  = DateTo7(GetFirstDay(systemDate()));
       //RPtodt  = DateTo7(GetLastDay(systemDate()));
       //*** Begin Add *** 02/24/22 *************************************
       If is133WSE = *on;
       //*** End   Add *** 02/24/22 *************************************
       RPfrdt  = B1frdt;
       RPtodt  = B1todt;
       RPrmds  = B1inv#;
       //*** Begin Add *** 02/24/22 *************************************
       Endif;
       //*** End   Add *** 02/24/22 *************************************
       //*** End   Change *** 03/23/21 *****************************
       // *** End Chg *** 02/21/14 ******************************
       RPamt   = B1cost;
       RPncci  = CMncci;
       RPwks   = 0;
       RPdays  = 0;
       RPtrcd  = 'EP';
       RPtype  = 'P';

       // *** Beg Chg *** 07/17/14 *********************************************
       // *** RPrecd  = 'EX';
       //*** Beg Chg *** 02/24/22 **********************************************
       //***RPrecd  = 'LE';
       Select;
       When outCltId = '133';
          RPreCd = 'EX';
       Other;
          RPreCd = 'LE';
       Endsl;
       //*** End Chg *** 02/24/22 **********************************************
       // *** End Chg *** 07/17/14 *********************************************

       //*** Begin Delete 03/23/21 *****************************************
       //RPrmds  = *Blanks;
       //*** End   Delete 03/23/21 *****************************************
       RPrmk1  = *Blanks;
       RPrmk2  = *Blanks;
       RPrmk3  = *Blanks;
       Chain (RPfein:RPMod) ppd01;
       If %Found(wmpdl);
          If PDdba1 <> 'Y';
             RPpnam = PDname;
             RPadr1 = PDadr1;
             RPadr2 = PDadr2;
          Else;
             RPpnam = PDname;
             RPadr1 = PDadr1;
             RPadr2 = *Blanks;
          EndIf;
       EndIf;
          RPcty  =  PDcty;
          RPst   =  PDst;
          RPzip  =  PDzip;
          RPadr3 =  *Blanks;
          RPctry =  *Blanks;

       If PDadr3 <> *Blanks;
          RPadr3 =  PDadr3;
          RPctry =  PDctry;
       EndIf;

       kkCo# = intCo#;
       KKkey = RPrecd;
       Chain keyTBKY prt01;
       If %Found(WTABL);
         RPretp = RTtype;
       Else;
         RPretp = *blanks;
       EndIf;

       RPbul1  = 0;
       RPchk#  = 0;
       RPoff#  = *Blanks;
       RPpgtp  = 'G';

       RPdsq#   = *Zero;
       charCo#  = %EditC(RPco#:'X');
       charFnd  = %EditC(RPfnd:'X');
       charFyr  = %EditC(RPfyr:'X');
       charCase = %EditC(RPcase:'X');
       QXTRDT   = RPtrdt;
       QXFRDT   = RPfrdt;
       QXTODT   = RPtodt;
       QXTRCD   = RPtrcd;
       QXRECD   = RPrecd;
       QXPV1    = RPbr2;
       QXPV2    = RPbr7;
       QXFEIN   = RPfein;
       QXMOD    = RPmod;
       QXDSQ#   = RPdsq#;

       //Call the Procedure to retrieve Bank Code.
       charData = DATA;
       charAppl = 'WC';
       charEftc = *Blanks;
       charCktp = GetBankCode(charCo#:charFnd:charFyr:charCase:charAppl:
                              charData:charRtcd:charEftc);
       RPcktp = charCktp;

       RPeftc = charEftc;
       RPacct = *Blank;
       RPdue = 0;
       RPsts = 'PD';
       RPsent = 'N';
       RPdseq = *Zero;
       RPltcd = *Blanks;
       RPntwk = 'B';
       RPfnal = *Blanks;
       // *** End Add *** 01/30/14 *********************************************

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
