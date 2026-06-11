     hCopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h/title WTRNWNOTE - Renewal Reminder Notices
     h dftname(WTRNWNOTE)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/16/24                                         *
      //  Programmer: C:340                                            *
      //  Project   : 153WAD - Renewal Reminder Notices                *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  01/16/24 C:340 PROJECT: 153WAD                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

      * Company file
     fsmcol     if   e           k disk
      * Fund/Group Master
     fwmfnl     if   e           k disk
      * Employer Detail File
     fWDELL3    if   e           k disk    Infds(dbfds1)
      * Employer Master File
     fWMEML     if   e           k disk
      * Group Auxiliary File
     fWDF2L     if   e           k disk
      * Scriptura System Printer Table File
     FSTP1L     IF   E           K DISK    rename(pp101:pp100)
      * XML Preparation File
     FXMLPREL1  UF   E           K DISK
      * Documents Setup File
     FSTDLL     IF   E           K DISK
      * Admin/DARTS Control Definition File
     fWMEXL     IF A E           K disk
      * Cancellation Detail File
     fWDCXL     IF A E           K disk
      * Cancellation History File
     fWDXHL     IF A E           K disk
      * Report File
     FQSYSPRT   O    F  132        Printer OflInd(*INOF)
     F                                     infds(prtfds)

     DdsSTDLP        E DS                  ExtName(STDLP)
     DdsWMFIP        e ds                  ExtName(WMFIP)  Inz Qualified
     DdsWMFIPsave      ds                  LikeDS(dsWMFIP) inz

     D w@TodayMDY      S               D   DATFMT(*MDY)
     D decNoticeDt     S              7S 0
     D decCancelDt     S              7S 0
     D eftDat          S              7S 0
     D toDate          S              7S 0
     D arBalAmount     S             15  2 Inz(0)
     D chrIncBatch     S              1    Inz(' ')
     D chrExcludeDep   S              1    Inz(' ')
     D chrMode         S             25    Inz(' ')
     D rcdCount        S              7S 0
     D xxRRN           S              9  0
     D xxPre           S              2
     d $#Mode          S              1    inz(*blank)
     D string          S             10
     D documentSetup   S               N   Inz(*Off)
     D strLang         S             60
     D strLangCode     S            250
     D message         S            250
     D dstime          S              6  0
     D dsdate          S              6  0
     D intInv#         S             10  0
     D inXml           S             61
     D strRRNType      S            100    inz
     D xmlFile         S             60    Inz(*Blanks)
     D msgDetail       S            100    Inz(*Blanks)
     D count           S              6  0
     D policy#         S             20    Inz(*Blanks)
     D @@Agt#          S              5  0
     D @@Anam          S             60
     D @@Prod          S              5  0
     D w@Req#          S                   Like(CxREQ#)
     D xxSubmit        S               N
     D first           S               N   Inz(*On)
     D flgNotice       S               N   Inz(*On)
     D flgCancel       S               N   Inz(*On)
     D clientID        S              3    inz('000')
     D prCO#           S              3
     D prFND           S              3
     D prFYR           S              3
     D prEMP#          S              9
     D prDIV           S              5
     D prREQ#          S              5
     D prCCRT          S              1    Inz(*Blanks)
     D prREPR          S              1    Inz('N')
     D prOUTQ          S             10    Inz(*Blanks)
     D prBat#          S              9    Inz('000000000')
     D prType          S              1    Inz(*Blanks)
     D prCert          S              1    Inz('Y')
     D prSrpMY         S              6    Inz(*Blanks)
     D prSweDT         S              8    Inz(*Blanks)

     D psds          ESDS                  ExtName(szq1p)

     D dbfds1          DS
     D  dbrrn1               397    400b 0

     D prtFDs          DS
     D curPage               367    368I 0
     D curLine               369    372I 0

     D SelectedPolicy  DS                  Inz
     D  @Co#                               Like(ELCO# )
     D  @Fnd                               Like(ELFND )
     D  @Fyr                               Like(ELFYR )
     D  @Emp#                              Like(ELEMP#)
     D  @Div                               Like(ELDIV )
     D  @PfDt                              Like(ELPFDT)
     D  @PtDt                              Like(ELPTDT)
     D  @BsDt                              Like(ELBSDT)
     D  @RtCd                              Like(ELRTCD)
     D  @CxRsn                             Like(F2RCxRsn)
     D  @Rrn                          9  0

      // Proto-Types
     D
     D GenScriptOut    pr                  ExtPgm('S4401C')
     D   inXml                       61
     D
     D AgtAPI          pr                  extpgm('WA002R')
     D   ##Co#                        3  0
     D   ##Fnd                        3  0
     D   ##Emp#                       9  0
     D   ##Div                        5  0
     D   ##Fyr                        3  0
     D   @@Agt#                       5  0
     D   @@Anam                      60
     D   @@Prod                       5  0
     D
     D StLetter        pr                  ExtPgm('STLETTER')
     D   letMode_                    10    const
     D   xmlFile_                    60
     D   message_                   250    options(*nopass)
     D   mode_                        1    const options(*nopass)
     D   rrn_                         9    const options(*nopass)
     D   prefix_                      2    const options(*nopass)
     D   string_                     10    const options(*nopass)
     D   co#_                         3    const options(*nopass)
     D   fnd_                         3    const options(*nopass)
     D
      // Get client ID
     D GetClientID     pr                  extpgm('RTVCLTID')
     D   outCltID                     3
     D
      // Generate Cancellation Notice
     D GenCancelNote   pr                  ExtPgm('W7166C')
     D   inCO#                        3
     D   inFND                        3
     D   inFYR                        3
     D   inEMP#                       9
     D   inDIV                        5
     D   inREQ#                       5
     D   inCCRT                       1
     D   inREPR                       1
     D   inOUTQ                      10
     D   inBat#                       9
     D   inType                       1
     D   inCert                       1
     D   inSrpMY                      6
     D   inSweDT                      8
     D
     D/copy sPrLetter
     D/copy wPrAr
     D/copy sprUsrPrms
     D/copy sPrGenPop
     D/copy sPrDate
     D/copy sPrString
     D/copy sPrSql
     D/copy sPrMessage
     D/copy sPrModule
     D/copy sCopyCompr
     D/copy sprCompany
     D/copy sPrTable
     D/copy sPrEmail
     D
     D
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxMultiG                       1    overlay(svlda:127)
     D xxDefDart                      1    overlay(svlda:128)
     D xxHold                         1    overlay(svlda:189)
     D xxSave                         1    overlay(svlda:190)
     D xxDarts                        1    overlay(svlda:251)
     D xxRMCD                         5    overlay(svlda:252)
     D xxCopyOut                      1    overlay(svlda:257)
     D xxPrint                        1    overlay(svlda:258)
     D xxPrinter                     10    overlay(svlda:259)
     D xxTray                        50    overlay(svlda:269)
     D xx#CopiesA                     3    overlay(svlda:319)
     D xxEmail                        1    overlay(svlda:322)
     D xxToEmail                    200    overlay(svlda:323)
     D xxSubject                    150    overlay(svlda:523)
     D xxBody                       200    overlay(svlda:673)
     D xxPrintI                       1    overlay(svlda:873)
     D xxPrinterI                    10    overlay(svlda:874)
     D xxTrayI                       50    overlay(svlda:884)
     D xxPrintA                       1    overlay(svlda:934)
     D xxPrinterA                    10    overlay(svlda:935)
     D xxTrayA                       50    overlay(svlda:945)

      *===============================================================*
      *              M   A   I   N     L  I  N  E
      *===============================================================*

        ExSr Main;
        *InLr = *On;
        Return;

      *===============================================================*
      * Main : Subroutine to Process Reminder Notices & Cancellation
      *===============================================================*
        BegSr Main;

        // Process Reminder Notices
          toDate = %int(%Char(%Date() : *CYMD0));
          eftDat = decNoticeDt;
          flgNotice = *On;
          flgCancel = *Off;
          ExSr SelectPolicies;

        // Process Cancellations
          toDate = %int(%Char(%Date() - %Days(09) : *CYMD0));
          eftDat = decCancelDt;
          flgCancel = *On;
          flgNotice = *Off;
          ExSr SelectPolicies;

          count = rcdCount ;
          EXCEPT TOTL;

        EndSr;

      *===============================================================*
      * Main : Subroutine to Process Reminder Notices & Cancellation
      *===============================================================*
        BegSr SelectPolicies;

        Exec SQL
          Declare SelectPolicies Cursor for
          Select ELCO#, ELFND, ELFYR, ELEMP#, ELDIV, ELPFDT, ELPTDT, ELBSDT,
                 ELRTCD, F2RCxRsn, RRN(WDELP)
                 From WMFNP
                 Join WDF2P  On F2CO#  = FNCO# and F2FND = FNFND
                 Join WDELP  On ELCO#  = FNCO# and ELFND = FNFND
                 Join WMEMP  On ELCO#  = EMCO# and ELFND = EMFND and
                                ELEMP# = EMEMP#  and ELDIV = EMDIV
          Where  ELPFDT = :eftDat and F2RNOTE = 'Y' and
                 EMSTRP = 'Y' and EMCAD1 = 0  and ELNWRW = 'R'
          Order by ELCO#, ELFND, ELFYR, ELEMP#, ELDIV
          For Read Only;

          Exec SQL
            Open SelectPolicies;

          Exec SQL
            Fetch SelectPolicies into :SelectedPolicy;
          Dow SQLCOD = 0;
            ArBalAmount = ARBalance(@BsDt:ToDate:@Co#:@Fnd:@Fyr:@Emp#);
            If ArBalAmount > 0;
               Policy# = 'WC' + %EditC(@Fnd:'X')  + '-' +
                                %EditC(@Emp#:'X') + '-' +
                                %EditC(@Fyr:'X');

               Select;
               When flgNotice = *On;
                 ExSr GenerateNotice;
               When flgCancel = *On;
                 ExSr CancelPolicies;
               EndSL;
               rcdCount += 1;
            EndIf;
            Exec SQL
              Fetch SelectPolicies into :SelectedPolicy;
          Enddo;

          Exec SQL
            Close SelectPolicies;

        EndSr;
      *===============================================================*
      * GenerateNotice : Subroutine to Generate Reminder Notices
      *===============================================================*
       BegSr GenerateNotice;

         If *InOF = *On;
            *InOF = *Off;
            Except Head1;
            Except Head2;
         Endif;
         If First = *On;
            First = *Off;
            Except Head2;
         Endif;

         Clear xmlFile;
         Clear message;
         Clear string;

         //* Set the prefix for using system folder control for output location.
         In *Lock *dtaara;
           xxPre = 'DL';
           xxEmail = 'Y';
           $#Mode = 'E';
           xxRRN  = @Rrn;
           xxCopyOut = 'Y';
         Out *DtaAra;

         //* Call StLetter to request an XML document name and write record.

         StLetter('XMLREQUEST':xmlFile:message:$#Mode:%editc(xxRRN:'X'):xxPre:
          string:%editc(@Co#:'X'):%editc(@Fnd:'X'));

         Chain (xmlFile) xp100;

         XPCat  = DLCat;
         XPSubc = DLSubC;
         XPType = DLType;
         XPDcid = DLDcId;
         XPRMCD = DLRmCd;
         XPIntr = ' ';
         XPFormP = ' ';
         XPFormT = ' ';
         XPInid = ' ';

         strLang = DLLang;
         Clear strLangCode;

         StLetter('LANGUAGE':strLang:strLangCode);

         XPLang = %trim(strLangCode);

         XPPath = DLPath;
         XPScrp = 'Y';
         XPData = 'Y';
         XPDart = 'Y';
         XPHold = ' ';
         XPSave = ' ';
         XPForm = '*STD';

         If DLPrin <> ' ' ;
            Chain (DLPrin) pp100;
            If %found;
               XPPnam = %trim(P1Name);
               If P1Type = 'N';
                  XPPunc = %trim(P1UNCN);
               Else;
                  XPOutq = %trim(P1KEY);
                  XPOLib = %trim(P1OLIB);
               Endif;
               XPCopy = DLCopy;
               XPTray = DLTray;
            Endif;
         Endif;

         If DLSubJ <> ' ';
            strRRNType = 'Policy';
            Reset dsRunInfo;
            dsRunInfo.docId   = DLDcId;
            dsRunInfo.docCat  = DLCat;
            dsRunInfo.docSub  = DLSubC;
            dsRunInfo.docLang = DLLang;
            XPMail = %trim(GetDefaultEmailRecipients(dsRunInfo:xxRRN:
             strRRNType));

            If XPMail <> ' ';
               XPSubj = %Trim(DLEmlSub);
               XPBody = %Trim(DLEmlBody);
            EndIf;
         EndIf;

         XPCopyE = DLCopyE;
         XPPgm   = Q1PGM;
         XPUser  = Q1USER;
         XPTime  = Timeto6(SystemTime());
         XPDt    = Dateto7(SystemDate());

         Update xp100;

         inXml = xmlFile + '$';

         In *Lock *dtaara;
           xxPre = 'DL';
           xxEmail = 'Y';
           $#Mode = 'E';
           xxRRN  = @Rrn;
         Out *DtaAra;

         Monitor;
           GenScriptOut(inXml);
         On-error;
           MsgDetail = 'Scriptura or Java error encountered, no document ' +
                       'produced for policy.';
           Except Det;
           LeaveSr;
         EndMon;

         MsgDetail = 'Document Created ' + %Trim(XPDir) + %Trim(XPFile);
         Except Det;

       EndSr;
      *===============================================================*
      * CancelPolicies:  Subroutine to Cancel the Policy
      *===============================================================*
       BegSr CancelPolicies;

        // Cancellation Detail

         Exec SQL
              Update WMEMP
              Set   EMCAD1 =:@PfDt
              Where EMCO#  =:@Co#  and
                    EMFND  =:@Fnd  and
                    EMEMP# =:@Emp# and
                    EMDIV  =:@Div     ;

         w@Req# = 0;
         Exec SQL
              Select Max(CxREQ#) into :w@Req#
              From WDCXL
              Where CXCO#  =:@Co#    and
                    CXFND  =:@Fnd    and
                    CXFYR  =:@Fyr    and
                    CXEMP# =:@Emp#   and
                    CXDIV  =:@Div ;

         Clear PCX01;

         CXco#  = @Co#;
         CXfnd  = @Fnd;
         CXemp# = @Emp#;
         CXdiv  = @Div;
         CXfyr  = @Fyr;
         CXreq# = w@Req# + 1;
         CXcxdt = @PfDt;
         Cxcdat = DateTo7(SystemDate);

         @@Agt# = 0;
         @@Anam = ' ';
         @@Prod = 0;
         AgtAPI(CXCo#:CXfnd:CXemp#:CXdiv:CXfyr:@@Agt#:
                 @@Anam:@@Prod);
         CXagt# = @@Agt#;
         CXrsn  = @CxRsn;
         CXrcrt = 'N';
         CXrein = 'N';
         CXrst  = @RtCd;
         CXamtd = ArBalAmount;
         CXcom1 = ' ';
         CXCom2 = ' ';
         CXstat = ' ';

         CXipgm = Q1pgm;
         CXiusr = Q1user;
         CXitm  = Timeto6(SystemTime());
         CXidt  = Dateto7(SystemDate());
         CXpgm  = Q1PGM;
         CXuser = Q1USER;
         CXtime = Timeto6(SystemTime());
         CXdt   = Dateto7(SystemDate());

         Write Pcx01;

        // Cancellation History

         Clear PXH01;
         XHco#  = @Co#;
         XHfnd  = @Fnd;
         XHemp# = @Emp#;
         XHdiv  = @Div;
         XHfyr  = @Fyr;
         XHreq# = CxREQ#;
         XHagt# = CXagt#;
         XHrsn  = CXrsn;
         XHcxdt = CXcxdt;
         XHcdat = Cxcdat;
         XHtrdt = Dateto7(SystemDate());
         XHPgm  = Q1pgm;
         XHUser = Q1user;
         XHTime = Timeto6(SystemTime());
         XHDt   = Dateto7(SystemDate());
         Write PXH01;

        // Cancellation History

         In *Lock *dtaara;

         xxRRN   = @Rrn;
         Chain (@Co#:@Fnd) WMEXL;
         If %Found and EXCxCd <> ' ';
            xxRMCD    = EXCxCd;
            xxDarts   = 'Y';
         Else;
            xxRMCD    = ' ';
         EndIf;

         Out *DtaAra;

         xxSubmit = OutputWindow('Cancellations':' ':'Y':'M':xxRRN);

        // Submit Cancellation Print Program

         prCO#  = %EditC(@Co#  :'X');
         prFND  = %EditC(@Fnd  :'X');
         prFYR  = %EditC(@Fyr  :'X');
         prEMP# = %EditC(@Emp# :'X');
         prDIV  = %EditC(@Div  :'X');
         prREQ# = %EditC(CxREQ#:'X');

         GenCancelNote(prCO#:prFND:prFYR:prEMP#:prDIV:prREQ#:prCCRT:
                       prREPR:prOUTQ:prBat#:prTYPE:prCert:prSrpMY:prSweDT);

          MsgDetail = 'Policy has been cancelled as of Today.';
          Except Det;

       EndSr;
      *===============================================================*
      * InZSr  - Initialzation Subroutine
      *===============================================================*
       Begsr *inzsr;

         GetClientID(clientID);                                           // Get Client ID
         ToDate      = %int(%Char(%Date() : *CYMD0));                     // Current Date
         decNoticeDt = %int(%Char(%Date() - %Days(1) : *CYMD0));          // Policy Start Date
         decCancelDt = %int(%Char(%Date() - %Days(10) : *CYMD0));         // Cancellation Date
         dstime      = %Dec(%Time(): *ISO);
         dsdate      = %dec(%date():*mdy);

         Except Head1;                                                    // Print Head1

         Exec SQL
            Select * into :dsSTDLP
            From STDLL
            Where DLCat = 'PS' and DLStat = 'A' and DLLang = 'English' and
                  DLType = 'AIC Direct Bill Reminder Notice';

         Select;
         When SQLCOD = 100;
          Except ErrMsg;
         When SQLCOD < 0;
         Other;
         EndSL;

       Endsr;

     OQSYSPRT   E            HEAD1          1 02
     O                       DSTIME              08 '  :  :  '
     O                                           67 'Renewal Reminder/Canc'
     O                                           82 'ellation Notice'
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD1          2
     O                                           10 'WTRNWNOTE'
     O                                           69 'Client - '
     O                       ClientID            73
     O                                          127 'Page:'
     O                       PAGE          Z    132
     O          E            HEAD2          1
     O                                            4 'Co#'
     O                                           25 'Carrier Policy #'
     O                                           39 'Description'
     O          E            DET            1
     O                       @Co#          4      4
     O                       Policy#             26
     O                       MsgDetail          128
     O          E            TOTL        1  2
     O                                           35 'Total Number of Policies :'
     O                       Count         B     45
     O          E            ErrMsg         2
     O                                           45 'No Document Set up Found'

