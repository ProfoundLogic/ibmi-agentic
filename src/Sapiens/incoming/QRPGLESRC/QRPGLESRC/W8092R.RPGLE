     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
      *================================================================
     H* W8092R - Non-Medical Review Fees Processing Program
     H*
     H* CREATED: 09/18/2017 C:180  Project: 146MGU
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 08/01/22 C: 079 Support: 146/103137                           *
     H*                 Monitor claim conversion to integer           *
     H*                                                               *
     H* 04/02/19 C: 079 Support: 146/77976                            *
     H*                 Relax date received edit for 146              *
     H*                                                               *
     H* 10/28/18 C: 079 Support: 146/74177                            *
     H*                 Add and edit for year < 1900                  *
     H*                                                               *
     H* 07/19/18 C: 180 Project 146WHY                                *
     H*                 Add From/To Service dates, Received Date, and *
     H*                 Invoice #. Note: Invoice number field         *
     H*                 WARPP/RPINV# will not be used. Instead,       *
     H*                 WARPP/RPRMDS will be used.                    *
     H*                                                               *
     H* 03/07/18 C: 180 Support 64804 - 146MGU                        *
     H*                 Fix fein and group                            *
     H*                                                               *
     H* 02/13/18 C: 079 Project: 146MGU                               *
     H*                 Divide amount by 100 so payment is correct    *
     H*                                                               *
     H*****************************************************************
      * MCMC Non-Medical Review Fees File
     fwanmp     if   e             Disk    Rename(pnm01:pnm0p)
     fwmcml1    if   e           k Disk
     fwan1p     o    e           k Disk    Rename(pn101:pn10p)
     fwarpp     o    e             Disk    Rename(prp01:prp0p)
     fwmpdl     if   e           k Disk    Rename(ppd01:ppd00)
     fwmnrl     if   e           k Disk    Rename(pnr01:pnr00)
     fwtabl     if   e           K Disk    Include(pb201)
     f                                     Include(pb701)
     f                                     Include(prt01)
     f                                     Include(ppc01)
     fqsysprt   o    f  132        printer OflInd(*inOF)
     d*=================================================================
     d* Procedure Prototypes
     d*=================================================================
     d/copy sPrDate
     d/copy wprbank

     d* Company Api
     d Comapi          pr                  ExtPgm('WA001R')
     d intCo#                         3  0
     d intFnd                         3  0
     d charConm                      40
     d charFnam                      40

     d* Validate Record
     d ChkValRcd       pr

     d* Print Error Report
     d ErrorReport     pr

     d* Write in Request Paymet File
     d WritePayRqFile  pr

     d* Write in Request Paymet File
     d WriteN1pmt      pr
     d* Get the client number
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d*=================================================================
     d* Work Fields
     d*=================================================================
     d intCo#          s              3  0
     d intFyr          s              3  0
     d intFnd          s              3  0
     d intCase         s              7  0
     D intClm#         S             13  0
     d intLen          s              1  0
     d intPos          s              1  0
     d intFein         s                   Like(PDfein)
     d intMod          s                   Like(PDmod)
     d intAmt          s                   Like(RPamt)
     d intChrg#        s             12  0
     d intCost         s              9  2

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
     d charAmt         s              9
     d charConm        s             40
     d charFnam        s             40
     d charCLM         s             15
     d charChrg        s              9
     d charError       s             52

     d dateValueIn     s              8  0
     d kkKey           s                   Like(RTkey)
     d ErrFound        s              1
     d Time6           s               T   Inz
     d feinOK          s               n   Inz(*Off)
     d modOK           s               n   Inz(*Off)
     d isError         s               n   inz( *OFF )
      *
     d isSvcDate       s               n   inz( *OFF )
     d isDoi           s               n   inz( *OFF )
     d isRcvd          s               n   inz( *OFF )
     d  ClientID       s              3    inz('000')
     d  isFutSrvDt     s              1n   inz('0')

     d*=================================================================
     d* Constants
     d*=================================================================
     d title           c                   'Non-Medical Review Fee Import'

     d*=================================================================
     d* Data Structures
     d*=================================================================
     d                 ds
     d  data                   1     75
     d  QXtrDt                 1      7  0
     d  QXfrDt                 8     14  0
     d  QXtoDt                15     21  0
     d  QXtrCd                22     23
     d  QXreCd                24     25
     d  QXpv1                 26     27
     d  QXpv2                 28     29
     d  QXseq#                30     34  0
     d  QXeob#                35     43
     d  QXfein                44     52  0
     d  QXmod                 53     55  0
     d  QXdsq#                56     60  0

     d psds          esds                  ExtName(szq1p)

      *
      /Free

       //*=================================================================
       // Main Processing
       //*=================================================================

       // Write Error Report Headings
       Time6 = %time();
       Except TitleFmt;
       Except HeaderFmt;

       ComApi(intCo#:intFnd:charConm:charFnam);

       GetClientID(clientID);
       If clientID = '146';
          isFutSrvDt = *on;
       Endif;

       ErrFound = *Blank;

       //---------------------------------------------------------------
       // First Pass - Read through Non-Medical Review Fees Holding File
       //              for validation.
       //---------------------------------------------------------------
       Setll 1 pnm0p;
       Read    pnm0p;
       Dow Not %Eof(wanmp);

       // Format data.
       //*** Beg Add *** 08/01/22 *********************************************
        Monitor;
       //*** End Add *** 08/01/22 *********************************************
        intClm# = %Int(NMclm#);
        NMclm#  = %EditC(intClm#:'X');
        intFnd  = %Int(%SubSt(NMclm#:1:3));
        intFyr  = %Int(%SubSt(NMclm#:4:3));
        intCase = %Int(%SubSt(NMclm#:7:7));
        CharCLM = %SubSt(NMclm#:1:3) + '-' + %SubSt(NMclm#:4:3) + '-' +
                  %SubSt(NMclm#:7:7);
        intClm# = %Int(NMclm#);
       //*** Beg Add *** 08/01/22 *********************************************
        On-Error;
          IntClm# = 0;
          intFnd  = 0;
          intFyr  = 0;
          intCase = 0;
          CharClm = NMclm#;
        Endmon;
       //*** End Add *** 08/01/22 *********************************************


       // Validate Record
          ChkValRcd();

          If isError = *On;
             ErrFound = 'Y';
          EndIf;

          Read pnm0p;
       EndDo;


       //--------------------------------------------------------------------
       // Second Pass : If no errors, read through
       //               Non-Medical Review Fees Holding File again and
       //               Write to payment files.
       //--------------------------------------------------------------------
       If ErrFound = *Blank;
          Setll 1 pnm0p;
          Read    pnm0p;
          Dow Not %Eof(wanmp);

              intFnd  = %Int(%SubSt(NMclm#:1:3));
              intFyr  = %Int(%SubSt(NMclm#:4:3));
              intCase = %Int(%SubSt(NMclm#:7:7));
              Chain (intCo#:intFnd:intFyr:intCase) pcm01;

              // Write Non-Medical Review Fees Payment record.
              WriteN1pmt();

              // Write Payment Request record.
              WritePayRqFile();
             Read pnm0p;
          EndDo;
       EndIf;
       *InLr = *On;
      /End-Free
      *********************************************************************
     oQsysprt   e            TitleFmt       1 03
     O                       Udate         Y    + 2
     o                       Time6              + 1
     o                       title               75
      *
     o          e            HeaderFmt   3  1
     o                                           05 'Claim'
     o                                           35 'First Name'
     o                                           54 'Last Name'
     o                                           75 'Chk Amount'
     o                                           85 'Error'
      *
     o          e            DetailFmt      1
     o                       charCLM             15
     o                       NMfsn               40
     o                       NMlsn               60
     o                       charAmt             75
     o                       charError          132
      *
      //===================================================================
      // ChkValRcd  - Check Valid Record
      //===================================================================
     P ChkValRcd       B
     D ChkValRcd       PI
      /Free

       isError = *Off;
       isSvcDate = *Off;
       isDOI     = *Off;
       isRcvd    = *Off;

       intLen  = %Len(NMamt);
       intPos  = %Scan(' ':NMamt:1);
       If intPos <> 0;
          charAmt = %SubSt(NMamt:intPos:intLen - (intPos-1)) +
                     %SubSt(NMamt:1:intPos-1);
          Else;
          charAmt = NMamt;
       EndIf;

       // Claim must not be blank.
       If NMClm# = *Blanks;
          charError = *Blanks;
          charError = 'Claim Number has blank value.';
          CharCLM   = NMclm#;
          ErrorReport();
          ErrFound = 'Y';
       EndIf;

       // Claim must be valid.
       If %Check(' 0123456789':NMclm#) <> 0;
          charError = *Blanks;
          charError = 'Invalid claim number.';
          CharCLM   = NMclm#;
          ErrorReport();
          ErrFound = 'Y';
       EndIf;

       // Claim must exist in claim File.
       Chain (intCo#:intFnd:intFyr:intCase) pcm01;
       If Not %Found(wmcml1);
          charError = *Blanks;
          charError = 'No match on Claim.';
          ErrorReport();
          isError = *On;
       EndIf;

       // Name cannot be blanks.
       If NMname = *blanks;
          charError = *Blanks;
          charError = 'Name cannot be blank.';
          ErrorReport();
          isError = *On;
       Endif;

       // Last name cannot be blanks.
       If NMlsn = *blanks;
          charError = *Blanks;
          charError = 'Last name cannot be blank.';
          ErrorReport();
          isError = *On;
       Endif;

       // First name cannot be blanks.
       If NMfsn = *blanks;
          charError = *Blanks;
          charError = 'First name cannot be blank.';
          ErrorReport();
          isError = *On;
       Endif;

       // SSN must be numeric
       If %Check(' 0123456789':NMSS#) <> 0;
          charError = *Blanks;
          charError = 'Invalid data in SSN.';
          ErrorReport();
          isError = *On;
       EndIf;

       // Date of injury must be a valid
       If %Check('0123456789':NMdoi) <> 0;
          charError = *Blanks;
          charError = 'Invalid date of injury.';
          ErrorReport();
          isError = *On;
          isDOI = *On;
       Else;
          If %Int(NMdoi) <> 0;
             dateValueIn=%Int(NMdoi);
             If dtCheck(dateValueIn)=0 or %Subst(NMdoI:1:4) < '1900';
                charError = *Blanks;
                charError = 'Invalid date of injury.';
                ErrorReport();
                isError=*On;
                isDOI = *On;
             EndIf;
          EndIf;
       EndIf;

       // Verifying Fein
       // There must be a Fein
       If NMfein = ' ' Or NMfein = '000000000';
          charError = 'No FEIN provided.';
          ErrorReport();
          isError = *On;
       Else;
          //The Fein must be valid numeric data
          If %Check('0123456789':NMfein) <> 0;
             charError = 'Invalid FEIN data.';
             ErrorReport();
             isError = *On;
          Else;
             intFein = %Int(NMfein);
             feinOK  = *On;
          EndIf;
       EndIf;

       // Verifying Modifier
       If NMmod <> ' ';
          //The Fein must be valid numeric data
          If %Check('0123456789':NMmod) <> 0;
             charError = 'Invalid Mod data.';
             ErrorReport();
             isError = *On;
          Else;
             intMod = %Int(NMmod);
             modOK  = *On;
          EndIf;
       Else;
          intMod = 0;
          modOK  = *On;
       EndIf;

       // Payee must exist
       If feinOK and modOK;
          Chain (intFein:intMod) ppd00;
          If Not %Found(wmpdl);
             charError = 'Payee not found.';
             ErrorReport();
             isError = *On;
          EndIf;
       EndIf;

       // Verifying PV1 code
       kkKey = NMpv1;
       Chain (intCo#:kkKey) pb201;
       If Not %Found(wtabl);
          charError = 'Invalid Payment Value 1 Code';
          ErrorReport();
          isError = *On;
       EndIf;

       // Verifying PV2 code
       kkKey = NMpv2;
       Chain (intCo#:kkKey) pb701;
       If Not %Found(wtabl);
          charError = 'Invalid Payment Value 2 Code';
          ErrorReport();
          isError = *On;
       EndIf;

       // Verifying Reserve code
       kkKey = NMrsvCd;
       Chain (intCo#:kkKey) prt01;
       If Not %Found(wtabl);
          charError = 'Invalid Reserve Type Code';
          ErrorReport();
          isError = *On;
       EndIf;

       // Check Amount must be numeric
       If %Check(' .0123456789':NMamt) <> 0;
          charError = *Blanks;
          charError = 'Invalid data in check Amount.';
          ErrorReport();
          isError = *On;
       EndIf;

       // Case must be numeric
       If %Check(' 0123456789':NMcase) <> 0;
          charError = *Blanks;
          charError = 'Invalid data in Case.';
          ErrorReport();
          isError = *On;
       EndIf;

       // From Service Date must be valid

       If %Check('0123456789':NMfrdt) <> 0;
          charError = *Blanks;
          charError = 'Invalid from service date.';
          ErrorReport();
          isError = *On;
          isSvcDate = *On;
       Else;
          If %Int(NMfrdt) <> 0;
             dateValueIn=%Int(NMfrdt);
             If dtCheck(dateValueIn)=0 or %Subst(NMfrDt:1:4) < '1900';
                charError = *Blanks;
                charError = 'Invalid from service date.';
                ErrorReport();
                isError=*On;
                isSvcDate = *On;
             EndIf;
          EndIf;
       EndIf;


       If isSvcDate = *off and isDOI = *off;
          If %dec(%char(%date(NMfrdt:*ISO0):*cymd0):7:0) <
             %dec(%char(%date(NMdoi:*ISO0):*cymd0):7:0);
             charError = *Blanks;
             charError = 'From service date cannot be prior to date of injury.';
             ErrorReport();
             isError=*On;
             isSvcDate = *On;
          Endif;
       Endif;

       // To Service Date must be valid
       If %Check('0123456789':NMtodt) <> 0;
          charError = *Blanks;
          charError = 'Invalid to service date.';
          ErrorReport();
          isError = *On;
          isSvcDate = *On;
       Else;
          If %Int(NMtodt) <> 0;
             dateValueIn=%Int(NMtodt);
             If dtCheck(dateValueIn)=0 or %Subst(NMtoDt:1:4) < '1900';
                charError = *Blanks;
                charError = 'Invalid to service date.';
                ErrorReport();
                isError=*On;
                isSvcDate = *On;
             EndIf;
          EndIf;
       EndIf;

       If isSvcDate = *off and isDOI = *off;
          if %dec(%char(%date(NMtodt:*ISO0):*cymd0):7:0) <
             %dec(%char(%date(NMdoi:*ISO0):*cymd0):7:0);
              charError = *Blanks;
              charError = 'To service date cannot be prior to date of injury.';
              ErrorReport();
              isError=*On;
              isSvcDate = *On;
          Endif;
       Endif;

       // Valid service dates must be in proper ascending order
       If isSvcDate = *Off ;
          If %dec(%char(%date(NMfrdt:*ISO0):*cymd0):7:0) >
             %dec(%char(%date(NMtodt:*ISO0):*cymd0):7:0);
             charError = *Blanks;
             charError = 'From/To service dates must be in ascending order.';
             ErrorReport();
             isError=*On;
          Endif;
       Endif;

       // Received date must be valid

       If %Check('0123456789':NMrcvd) <> 0;
          charError = *Blanks;
          charError = 'Invalid received date.';
          ErrorReport();
          isError = *On;
          isRcvd = *On;
       Else;
          If %Int(NMrcvd) <> 0;
             dateValueIn=%Int(NMrcvd);
             If dtCheck(dateValueIn)=0 or %Subst(NMrcvD:1:4) < '1900';
                charError = *Blanks;
                charError = 'Invalid received date.';
                ErrorReport();
                isError=*On;
                isRcvd = *On;
             EndIf;
          EndIf;
       EndIf;

       // Valid receive date must be prior to service date.
       If isSvcDate = *Off and isRcvd = *off and isFutSrvDt = *off;
          If %dec(%char(%date(NMrcvd:*ISO0):*cymd0):7:0) <
             %dec(%char(%date(NMfrdt:*ISO0):*cymd0):7:0);
             charError = *Blanks;
             charError = 'Received date cannot be prior to service date.';
             ErrorReport();
             isError=*On;
          Endif;
       Endif;


      /End-Free
     P ChkValRcd       E
      //===================================================================
      // WriteN1pmt - Non-Medical Review Fees Payment File
      //===================================================================
     P WriteN1pmt      B
     D WriteN1pmt      PI
      /Free

        Clear    pn10p;
        N1clm#  = %Int(NMclm#);
        N1fnd   = CMfnd;
        N1fyr   = CMfyr;
        N1case  = CMcase;
        N1name  = NMname;
        N1lsn   = NMlsn;
        N1fsn   = NMfsn;
        N1ss#   = NMss#;
        N1doi   = dtCYMD(%Int(NMdoi));
        N1fein  = %Int(NMfein);
        N1mod   = %Int(NMmod);
        N1pv1   = NMpv1;
        N1pv2   = NMpv2;
        N1rsvcd = NMrsvcd;
        N1amt   = %Dec(NMamt:9:2)/100;
        N1prfl  = 'E';
        N1prdt  = DateTo7(systemDate());
        N1user  = Q1user;
        N1time  = TimeTo6(SystemTime());
        N1pgm   = Q1pgm;
        N1frdt = dtCYMD(%Int(NMfrdt));
        N1todt = dtCYMD(%Int(NMtodt));
        N1rcvd = dtCYMD(%Int(NMrcvd));
        N1inv# = NMinv#;
        Write pn10p;

      /End-Free
     P WriteN1pmt      E
      //===================================================================
      // WritePayRqFile - Write record in payment request file
      //===================================================================
     P WritePayRqFile  B
     D WritePayRqFile  PI
      /Free

       clear    prp0p;

       RPco#  = CMco#;
       RPfnd  = CMfnd;
       RPfyr  = CMfyr;
       RPcase = CMcase;
       RPseq# = 1;
       RPfein = %Int(NMfein);
       RPMod  = %Int(NMmod);
       RPstat = 'P';
       RPbr2  = NMpv1;
       RPbr7  = NMpv2;

       RPtrdt = DateTo7(systemDate());
       RPfrdt = DateTo7(GetFirstDay(systemDate()));
       RPtodt = DateTo7(GetLastDay(systemDate()));
       RPamt  = N1amt;
       RPncci = CMncci;
       RPwks  = 0;
       RPdays = 0;
       RPreCd = NMrsvCd;

       kkKey = RPreCd;
       Chain (intCo#:kkKey) prt01;
       If %Found(wtabl);
         RPreTp = RTtype;
       EndIf;

       Select;
       When RPreTp = 'C';
          RPtrCd = 'CP';
       When RPreTp = 'E';
          RPtrCd = 'EP';
       When RPreTp = 'L';
          RPtrCd = 'LP';
       When RPreTp = 'M';
          RPtrCd = 'MP';
       EndSl;

       kkKey = RPtrCd;
       Chain (intCo#:kkKey) ppc01;
       If %Found(wtabl);
         RPdesc = PCdesc;
       EndIf;

       RPtype  = 'P';

       RPrmds  = *Blanks;
       RPrmk1  = *Blanks;
       RPrmk2  = *Blanks;
       RPrmk3  = *Blanks;

       Chain (RPfein:RPMod) ppd00;
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

       Chain (RPco#:RPfein:RPmod) pnr00;
       If %Found(wmnrl);
          RPpgtp = 'G';
       Else;
          RPpgtp = ' ';
       EndIf;

       QXtrDt   = RPtrdt;
       QXfrDt   = RPfrdt;
       QXtoDt   = RPtodt;
       QXtrCd   = RPtrcd;
       QXreCd   = RPrecd;
       QXpv1    = RPbr2;
       QXpv2    = RPbr7;
       QXfein   = RPfein;
       QXmod    = RPmod;
       QXdSq#   = RPdsq#;

       //Call the Procedure to retrieve Bank Code.
       charCo#  = %EditC(RPco#:'X');
       charFnd  = %EditC(RPfnd:'X');
       charFyr  = %EditC(RPfyr:'X');
       charCase = %EditC(RPcase:'X');
       charData = DATA;
       charAppl = 'WC';
       charEftc = *Blanks;

       charCktp = GetBankCode(charCo#:charFnd:charFyr:charCase:charAppl:
                              charData:charRtcd:charEftc);

       RPcktp = charCktp;
       RPeftc = charEftc;
       RPacct = *Blank;
       RPdue  = 0;
       RPsts  = 'PD';
       RPsent = 'N';
       RPdseq = *Zero;
       RPltcd = *Blanks;
       RPntwk = ' ';
       RPfnal = *Blanks;
       RPbul1 = 0;
       RPchk# = 0;
       RPoff# = *Blanks;
       RPdsq# = *Zero;

       RPuser = Q1user;
       RPpgm  = Q1pgm;
       RPdt   = DateTo7(systemDate());
       RPtime = TimeTo6(SystemTime());
       RPiusr = Q1user;
       RPipgm = Q1pgm;
       RPidt  = DateTo7(systemDate());
       RPitm  = TimeTo6(SystemTime());

       RPfrdt = dtCYMD(%Int(NMfrdt));
       RPtodt = dtCYMD(%Int(NMtodt));
       RPrcvd = dtCYMD(%Int(NMrcvd));
       RPrmds = NMinv#;

       Write prp0p;

      /End-Free
     P WritePayRqFile  E
      //===================================================================
      // ErrorReport - Print Error Report
      //===================================================================
     P ErrorReport     B
     D ErrorReport     PI
      /Free

       // Write Headings
       If *inof;
          Except HeaderFmt;
          *inof   = *off;
       Endif;

       // Write Detail
       Except DetailFmt;

      /End-Free
     P ErrorReport     E
