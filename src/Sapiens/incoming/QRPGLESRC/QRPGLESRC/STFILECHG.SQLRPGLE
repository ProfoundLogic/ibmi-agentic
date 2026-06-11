     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H Option(*SrcStmt : *NoDebugIO)
     H BndDir('STBNDDIR':'QC2LE')
     H dftactgrp(*no) actgrp(*CALLER)
      //===================================================================
     h/Title STFILECHG - Create Messages/XML/HTML for File Changes
      //===================================================================
      //  Created: 01/31/17   C:33   Project: WC2405
      //===================================================================
      // Revisions:
      //
      // 09/24/25 C:300 Support: 119767
      //                Message on ASDP07 about SXTRPOUT0, SXTRPOUT1, SXTRPOUT2
      //                files in PDSFIL129 -- *FILE HAS CONSUMED MORE THAN 90%
      //                OF THE LIMIT: 16100-MAXIMUM NUMBER OF MEMBERS (29750 OF
      //                32767=90.79%).
      //                Fix -- Remove these temporary members before RETURN
      //
      // 05/21/25 C:291 Support: 119060
      //                Added RtvXMLFileName procedure to create xmlprep for
      //                file rule when STLETTER is in call stack.
      //                When STLETTER was in call stack and GetXMLFileName
      //                was called, it again called STLETTER for XMLREQUEST
      //                that resulted in recursive call to STLETTER.
      //
      // 11/08/24 C:344 Support: 150/115584
      //               -If inPgm blank, rev email subject and body lines.
      //
      // 02/06/24 C:291 Project: 102WJI
      //                Initialized parameters to ExitProgram.
      *
      *  02/01/22 C:33  Support: 97790
      *                 Encopass email addresses in ''
      //
      //  08/04/20 C:33  Support: 90576
      //                 Retry member addition.
      //
      //  10/23/19 C:33  Support: 84490
      //                 Character exception handling.
      //
      //  07/01/19 C:33  Support: 78471
      //                 Process notification (email/sms) via Scriptura
      //                  follow-on program call instead of using interactive
      //                  Scriptura calls.
      //
      //  02/26/18 C:62  Ticket: 64565
      //                 Do't drop leading zero i job #.
      //
      //  10/26/17 C:33  Project: WC2405
      //                -Break body into 5000 character sections
      //
      //  09/06/17 C:33  Project: WC2405
      //                -Add Exit Program as an action
      //
      // 07/31/17  C:33  Project: WC2405 (no comments)
      //                -Send link to SMS
      //
      // 06/15/17  C:33  Project: WC2405 (no comments)
      //                -Add Scriptura report
      //                -Add SQL Action
      //                -Add daily digest
      //
      // 05/10/17  C:33  Project: WC2405 (no comments)
      //                 -Include inactive fields/frequency
      //                 -Include field comparisons
      //
      // 05/09/17  C:33  Project: WC2405
      //                 Do not lock data area    (no comments)
      //
      // 05/09/17  C:62  Ticket: 47134(99)
      //                 Close Files. (no comments)
      //
      // 03/31/17  C:33  Project: WC2405
      //                 Ensure files are in LIBL (no comments)
      //===================================================================
     fsmtrl     if   e           k disk    rename(ptr01:ptr00)   usropn
     f                                     extfile(@smtr)
     fsmtrpfil  if   e           k disk    rename(ptr01:ptr0fil) usropn
     f                                     extfile(@smtr)
     fsmtrpfld  if   e           k disk    rename(ptr01:ptr0fld) usropn
     f                                     extfile(@smtr)
     fsmtrlfrq  if   e           k disk    rename(ptr01:ptr0frq) usropn
     f                                     extfile(@smtr)
     fsmtrpact  if   e           k disk    rename(ptr01:ptr0act) usropn
     f                                     extfile(@smtr)
     fsmtrpsql  if   e           k disk    rename(ptr01:ptr0sql) usropn
     f                                     extfile(@smtr)
     fsmtrlrec  if   e           k disk    rename(ptr01:ptr0rec) usropn
     f                                     extfile(@smtr)
     fsxtrpxml  uf a e           k disk    rename(ptr01:xtr0xml) usropn
     f                                     extfile(@sxtr)
     Fsxtrprec  if a e           k DISK    rename(ptr01:xtr0rec) usropn
     f                                     extfile(@sxtr) prefix(x)
     fsxtrpout0 uf a e           k disk    rename(ptr01:xtr0out0) usropn
     f                                     extfile(@sxtr)
     f                                     extmbr(#sxtr)
     fsxtrpout1 uf a e           k disk    rename(ptr01:xtr0out1) usropn
     f                                     extfile(@sxtr)
     f                                     extmbr(#sxtr)
     fsxtrpout2 uf a e           k disk    rename(ptr01:xtr0out2) usropn
     f                                     extfile(@sxtr)
     f                                     extmbr(#sxtr)
      *** Beg Add ***** 05/21/25 ***********************************************
     fstscp     if   e             disk    rename(psc01:psc0p)
     fstsp1l    if   e           k disk    rename(psp011:psp010)
     fstspl     if   e           k disk    rename(psp01:psp00)
     fxmlprel1  if a e           k disk
     fxmlprel2  if   e           k disk    rename(xp100:xp102)
      *** End Add ***** 05/21/25 ***********************************************

     D@smtr            s             21    inz
     D@sxtr            s             21    inz
     D#sxtr            s             10    inz
     d days            s              3  0 inz
     D dtSchJob        s               d   inz
     D dtNull          s               d   inz
     d intPos          s              5  0 inz
     d***  Begin Add  ***  07/01/19  ******************************
     d isError         s              1n   inz
     d***  End   Add  ***  07/01/19  ******************************
     d isUseEmail      s              1n   inz
     d isUseExitPgm    s              1n   inz
     d***  Begin Add  ***  07/01/19  ******************************
     d isSubmitXML     s              1n   inz(*on)
     d***  End   Add  ***  07/01/19  ******************************
     d isUseSMS        s              1n   inz
     d isUseXML        s              1n   inz
     d outChgOld       s          32765a
     d outChgNew       s          32765a
     d outLibFile      s             10
     d outLibObj       s             10
     d outLibEnv       s             10
     d outPgm          s             10
     d outRptOld       s          32765a
     d outRptNew       s          32765a
     d outTrgName      s            128
     d outUser         s             10
     d strAttach       s            300a   inz
     d strSms          s            200a   inz
     d strSubject      s            200a   inz
     d strBody         s          10000a   inz
     d strBodySQL      s          10000a   inz
     d strCmd          s          15000a
     d strDescFile     s             75a
     d strDescPgm      s             75a
     d strDescUser     s             75a
     d strDirectory    s             40a
      //***  Begin Add  ***  02/01/22  *************
     d strEmailFrom    s             80a   inz varying
     d strEmailTo      s             80a   inz varying
      //***  End   Add  ***  02/01/22  *************
     d strExitPgm      s             21
     d strJob          s             10a
     d strOutput       s            250a   inz
     d strSuffix       s              3
     d strXml          s                   like(TRxml)
     d timeStamp       s               z
     d charNewLine     s              1    inz(x'0D')
      *** Beg Add ***** 05/21/25 ***********************************************
     d clientID        s              3    inz('000')
     d effdate         s              7  0 inz(0)
     D xxRoot          s             10
     D xxRtcd          s              1
     D xxPath          s             80
     D xxPflr          s             80
     D xxDcid          s             12
     D xxPre           s              2
     D xxSub#          s              5
     d pgmName         s             10
     d pgmExists       s              1
      *** End Add ***** 05/21/25 ***********************************************

     D* parameters passed to CEEDOD
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0

     D i1              s              5  0 inz

     d dsData          ds
     d  type                          1
     d  data                      32764
     d dsChgOld        ds                  likeds(dsData)
     d dsChgNew        ds                  likeds(dsData)
     d dsRptOld        ds                  likeds(dsData)
     d dsRptNew        ds                  likeds(dsData)
     D dsFil           ds                  likeRec(ptr0fil:*input) inz
     D dsFld           ds                  likeRec(ptr0fld:*input) inz
     D dsFrq           ds                  likeRec(ptr0frq:*input) inz
     d dsMsgInfo       ds                  likeds(dsRtnMsgInfo)
     d***  Begin Add  ***  07/01/19  ******************************
     ddsSXTRPout3    e ds                  extname(SXTRPout3) qualified inz
     d***  End   Add  ***  07/01/19  ******************************

     d psds          esds                  extname(szq1p)

     DdsFileChgInfo    ds          1000    dtaara(STFILECHG) inz
     D strDirLocation               100
     D strExtPrefix                 100
     D strSmsPrefix                 100

      *entry
     dStFileChg        pr                  Extpgm('STFILECHG')
     d inTrgName                    128
     d inUser                        10
     d inPgm                         10
     d inLibFile                     10
     d inLibObj                      10
     d inLibEnv                      10
     d inChgOld                   32765a
     d inChgNew                   32765a
     d inRptOld                   32765a
     d inRptNew                   32765a
     dStFileChg        pi
     d inTrgName                    128
     d inUser                        10
     d inPgm                         10
     d inLibFile                     10
     d inLibObj                      10
     d inLibEnv                      10
     d inChgOld                   32765a
     d inChgNew                   32765a
     d inRptOld                   32765a
     d inRptNew                   32765a

     DBuildDirectory   pr          1000

     DCheckAlpha       pr             1n
     D inValue                    32764                     const
     D inOperator                          like(TRoperCur)  const
     D inCompValue                         like(TRcmpValC)  const
     D inCompType                          like(TRcmpTypC)  const
     D inFieldType                         like(TRfieldTyp) const

     DCheckDate        pr             1n
     D inValue                    32764                     const
     D inOperator                          like(TRoperCur)  const
     D inCompValue                         like(TRcmpValC)  const
     D inCompType                          like(TRcmpTypC)  const
     D inFieldType                         like(TRfieldTyp) const

     DCheckNumeric     pr             1n
     D inValue                    32764                     const
     D inNewValue                 32764                     const
     D inOperator                          like(TRoperCur)  const
     D inCompValue                         like(TRcmpValC)  const
     D inCompType                          like(TRcmpTypC)  const
     D inFieldType                         like(TRfieldTyp) const

     d***  Begin Add  ***  07/01/19  ******************************
     dChkObj           pr                  extpgm('SRCHKOBJ')
     d outObj                        10    const
     d outType                        7    const
     d inError                        1
     d***  End   Add  ***  07/01/19  ******************************

     DChkMember        pr                  opdesc
     D inFile                        21    const
     D inMember                      10    const

     DCompareValues    pr             1n
     D inOldValue                 32764
     D inNewValue                 32764

     DDltRuleXML       pr
     D inCo#                          3  0 const
     D inName                        10    const
     D inSeq#                         5  0 const
     D inDigestDt                      d   const
     D inDigest                       6    const

     DGetDescField     pr            75
     D inField                       10    const

     DGetDescObj       pr            75
     D inType                        10    const
     D inObj                         10    const
     D inLibrary                     10    const options(*nopass)

     DGetFieldValue    pr         32764
     D inData                     32764

     DGetRuleXML       pr           250
     D inCo#                          3  0 const
     D inName                        10    const
     D inSeq#                         5  0 const
     D inDigestDt                      d   const
     D inDigest                       6    const

     DProcessData      pr
     D inOldData                  32764
     D inNewData                  32764

     DRmvExtraChar     pr         32765    opdesc
     D inField                    32765    options(*varsize) const
     D inChar                         1    const
     D inLen                         15  0 options(*nopass) const

     DRmvMember        pr                  opdesc
     D inFile                        21    const
     D inMember                      10    const

     DRtvSuffix        PR                  extpgm('RTVSUFFIX')
     D suffix                         3

     DUpdateXML        pr                  opdesc
     D inXML                        250    const

     DValidDataCond    pr             1n
     D inOldData                  32764
     D inNewData                  32764

     DDelayJob         pr            10u 0 extproc('sleep')
     D interval                      10u 0 value

      *** Beg Add ***** 05/21/25 ***********************************************
     D RtvCltID        pr                  ExtPgm('RTVCLTID')
     D clientID                       3    const

     d SNextDocID      pr                  ExtPgm('SNXTDCID')
     d   $#Fold_                     80
     d   $#Dcid_                     12
     d   QQPre_                       2    const
     d   QQCo#_                       3    const
     d   QQFnd_                       3    const
     d   QQRtcd_                      1    const
     d   QQSub_                       5    const
     d   QQPFlr_                     80

     d DocRoot         pr                  ExtPgm('RTVDOCROOT')
     d   $#Root_                     10

     d ChkPgmStack     pr                  ExtPgm('SRCHKPGM')
     d   pgmName                     10
     d   pgmExists                    1
      *** End Add ***** 05/21/25 ***********************************************

      /copy sPrDate
       //***  Begin Add  ***  08/04/20  *****************************
      /copy sPrFiles
       //***  End   Add  ***  08/04/20  *****************************
      /copy sPrMessage
      /copy sPrJobInfo
      /copy sPrRunCmd
      /copy sPrString
      /copy sPrUsrPrms
      /copy sPrXml
      /Copy CGIDEV2/QRPGLESRC,PrototypeB
      /Copy CGIDEV2/QRPGLESRC,QUSEC
      /Free
       //***  Begin Add  ***  07/01/19  *****************************
       Exec Sql Set Option Commit = *None;
       ChkObj('SXTRPOUT3':'*FILE':isError);
       If isError;
          isSubmitXML = *off;
       Endif;
       ChkObj('STFILECHN':'*PGM':isError);
       If isError;
          isSubmitXML = *off;
       Endif;
       //***  End   Add  ***  07/01/19  *****************************

       Exsr CheckInFields;
       Exsr OpenFiles;

       RtvSuffix(strSuffix);

       dsChgOld = inChgOld;
       dsChgNew = inChgNew;
       If dsChgOld.data = dsChgNew.data;
          //***  Begin Add  ***  09/24/25  *****************************
          Exsr CloseFiles;
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT0';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT1';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT2';
          RmvMember(@sxtr:#sxtr);
          //***  End   Add  ***  09/24/25  *****************************
          Return;
       Endif;
       dsRptOld = inRptOld;
       dsRptNew = inRptNew;

       dsChgOld.data=RmvExceptXML(dsChgOld.data);
       dsChgNew.data=RmvExceptXML(dsChgNew.data);

       timeStamp = %timeStamp;
       In dsFileChgInfo;
       strDirectory = strDirLocation;
       isUseXML = UseScriptura('Rule Report')
        or Use3rdXml('Rule Report');

       Chain inTrgName smtrlfrq dsFrq;
       Select;
       When not %found;
          //***  Begin Add  ***  09/24/25  *****************************
          Exsr CloseFiles;
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT0';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT1';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT2';
          RmvMember(@sxtr:#sxtr);
          //***  End   Add  ***  09/24/25  *****************************
          Return;
       When dsFrq.TRstat = 'I';
          //***  Begin Add  ***  09/24/25  *****************************
          Exsr CloseFiles;
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT0';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT1';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT2';
          RmvMember(@sxtr:#sxtr);
          //***  End   Add  ***  09/24/25  *****************************
          Return;
       Endsl;
       Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) smtrl;
       strCmd = 'DSPFFD FILE(' +
        %trim(dsFrq.TRlibrary) + '/' + %trim(dsFrq.TRfile) + ') ' +
        'OUTPUT(*OUTFILE) OUTFILE(QTEMP/APPCFGFLD)';
       RunCmd(strCmd:%len(strCmd));
       //*** Beg Chg **** 11/08/24 *****************************************
       // Setup email Subject Line and include the called pgm/desc
       // 'inPgm' when found in the program call stack.
       If inPgm <> ' ';
          strSubject = %trim(dsFrq.TRlibrary) + '/' +
          %trim(dsFrq.TRfile) + ' was updated by ' + %trim(inUser) +
          ' using ' + %trim(inPgm) + '.';
       Else;
          strSubject = %trim(dsFrq.TRlibrary) + '/' +
          %trim(dsFrq.TRfile) + ' was updated by ' + %trim(inUser) + '.';
       Endif;
       //*** End Chg **** 11/08/24 *****************************************
       strDescFile = GetDescObj('*FILE':dsFrq.TRfile:dsFrq.TRlibrary);
       strDescUser = GetDescObj('*USRPRF':inUser);
       strDescPgm  = GetDescObj('*PGM':inPgm);
       //*** Beg Chg **** 11/08/24 *****************************************
       // Setup email Subject Line and include the called pgm/desc
       // 'inPgm' when found in the program call stack.
       If inPgm <> ' ';
          strSubject = %trim(strDescFile) + ' was updated by ' +
          %trim(strDescUser) + ' using ' + %trim(strDescPgm) + '.';
       Else;
          strSubject = %trim(strDescFile) + ' was updated by ' +
          %trim(strDescUser) + '.';
       Endif;
       //*** End Chg **** 11/08/24 *****************************************
       strBody = %trim(strBody) + 'A record was';
       Select;
       When dsFrq.TRtrgEvent = '*INSERT';
          strBody = %trim(strBody) + ' inserted';
       When dsFrq.TRtrgEvent = '*DELETE';
          strBody = %trim(strBody) + ' deleted';
       When dsFrq.TRtrgEvent = '*UPDATE';
          strBody = %trim(strBody) + ' updated';
       Endsl;
       //*** Beg Chg **** 11/08/24 *****************************************
       // Setup email Body Line and include the called pgm/desc
       // 'inPgm' when found in the program call stack.
       If inPgm <> ' ';
          strBody = %trim(strBody) + ' in ' + %trim(strDescFile) +
          ' by ' + %trim(strDescUser) +
          ' using ' + %trim(strDescPgm);
       Else;
          strBody = %trim(strBody) + ' in ' + %trim(strDescFile) +
          ' by ' + %trim(strDescUser);
       Endif;
       //*** End Chg **** 11/08/24 *****************************************
       If not ValidDataCond(dsChgOld.data:dsChgNew.data);
          //***  Begin Add  ***  09/24/25  *****************************
          Exsr CloseFiles;
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT0';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT1';
          RmvMember(@sxtr:#sxtr);
          @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT2';
          RmvMember(@sxtr:#sxtr);
          //***  End   Add  ***  09/24/25  *****************************
          Return;
       Endif;
       ProcessData(dsRptOld.data:dsRptNew.data);
       // Include rule/trigger information
       strBody = %trim(strBody) + '<br><br>' +
        'Rule: ' + %trim(TRdesc) + '<br>' +
        'Trigger: ' + %trim(inTrgName) + '<br><br>';
       //***  Begin Add  ***  10/23/19  ***********************
       // Except apostrophe
       strSubject = %scanrpl('''':'&apos':strSubject);
       strBody    = %scanrpl('''':'&apos':strBody);
       //***  End   Add  ***  10/23/19  ***********************

       // Send information
       Setll (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0act;
       Dou %eof(smtrpact);
          Reade (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0act;
          Select;
          When %eof;
             Iter;
          When TRaction = 'Send Email';
             isUseEmail = *on;
          When TRaction = 'Send Text';
             isUseSMS = *on;
          When TRaction = 'Exit Program';
             isUseExitPgm = *on;
          When TRaction = 'Execute SQL';
             Exsr ProcessSQL;
          EndSL;
       Enddo;
       Exsr WriteXML01;
       Setll (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#:'Email') ptr0rec;
       Dou %eof(smtrlrec);
          Reade (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#:'Email') ptr0rec;
          Select;
          When %eof;
             Iter;
          When not isUseEmail and not isUseSMS;
             Iter;
          //*** Begin Add  ***  07/01/19  ******************************
          When isSubmitXML;
             Exsr WriteOut3;
             Iter;
          //*** End   Add  ***  07/01/19  ******************************
          Endsl;
          Exsr ProcessXML;
          Select;
          When TRdigest <> *blanks;
             Exsr ProcessDigest;
             Iter;
          Endsl;
          If isUseEmail and TRemail <> *blanks;
           //***  Begin Chg  ***  02/01/22  ************* +
           //strCmd = 'SNDEMAIL FROM(' + %trim(TRemail) + ') ' +
           // 'TO(' + %trim(TRemail) + ') ' +
             strEmailFrom = '''' + %trim(TRemail) + '''';
             strEmailTo = '''' + %trim(TRemail) + '''';
             strCmd = 'SNDEMAIL FROM(' + %trim(TRemail) + ') ' +
              'TO(' + %trim(TRemail) + ') ' +
           //***  End   Chg  ***  02/01/22  ************* +
              'SUBJECT(''' + %trim(strSubject) + ''') ' +
              'BODY(''' +
               %trim(%subst(strBody:1:5000)) + ''' ''' +
               %trim(%subst(strBody:5001:5000)) + ''') ' +
              %trim(strAttach);
             If strSuffix <> *blank and strSuffix <> '000';
                strCmd = %trim(strCmd) + ' SUFFIX(' + strSuffix + ')';
             Endif;
             RunCmd(strCmd:%len(%trim(strCmd)));
          Endif;
          If isUseSMS and TRtext <> *zeros;
             If strSms = *blanks;
                strSms = strSubject;
             Endif;
             If strSmsPrefix <> *blanks;
                strSms = %trim(strSmsPrefix) + ' ' + strSms;
             Endif;
             strCmd = 'SNDSMS TO(' + %trim(%editc(TRtext:'X')) + ') ' +
              'MESSAGE(''' + %trim(strSms) + ''')';
             If strSuffix <> *blank and strSuffix <> '000';
                strCmd = %trim(strCmd) + ' SUFFIX(' + strSuffix + ')';
             Endif;
             RunCmd(strCmd:%len(%trim(strCmd)));
          Endif;
       Enddo;
       //*** Begin Add  ***  07/01/19  ******************************
       If isSubmitXML;
          Exsr ProcessXML;
       Endif;
       //*** End   Add  ***  07/01/19  ******************************

       If isUseExitPgm and dsFrq.TRexitPgm <> *blanks;
          Exsr CallExitPgm;
       Endif;

       Exsr CloseFiles;
       @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT0';
       RmvMember(@sxtr:#sxtr);
       @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT1';
       RmvMember(@sxtr:#sxtr);
       @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT2';
       RmvMember(@sxtr:#sxtr);

       *InLR = *On;
       //================================================================== ==
       // Check incoming fields to determine if fully loaded
       //================================================================== ==
       Begsr CheckInFields;
        RmvExtraChar(inTrgName:'¢');
        RmvExtraChar(inChgOld:'¢':5000);
        RmvExtraChar(inChgNew:'¢':5000);
        RmvExtraChar(inRptOld:'¢':5000);
        RmvExtraChar(inRptNew:'¢':5000);
       Endsr;

       //================================================================== ==
       // Close the files
       //================================================================== ==
       Begsr CloseFiles;
        If %Open(smtrl);
           Close smtrl;
        EndIf;
        If %Open(smtrpfil);
           Close smtrpfil;
        EndIf;
        If %Open(smtrpfld);
           Close smtrpfld;
        EndIf;
        If %Open(smtrpact);
           Close smtrpact;
        EndIf;
        If %Open(smtrpsql);
           Close smtrpsql;
        EndIf;
        If %Open(smtrlfrq);
           Close smtrlfrq;
        EndIf;
        If %Open(smtrlrec);
           Close smtrlrec;
        EndIf;
        If %Open(sxtrpxml);
           Close sxtrpxml;
        EndIf;
        If %Open(sxtrprec);
           Close sxtrprec;
        EndIf;
        If %Open(sxtrpout0);
           Close sxtrpout0;
        EndIf;
        If %Open(sxtrpout1);
           Close sxtrpout1;
        EndIf;
        If %Open(sxtrpout2);
           Close sxtrpout2;
        EndIf;
       Endsr;

       //================================================================== ==
       // Call exit program
       //================================================================== ==
       Begsr CallExitPgm;
        //RunCmd('Call '+%trim(dsFrq.TRexitPgm):35);
        strExitPgm = Upper(dsFrq.TRexitPgm);
        //*** Beg Add ***** 02/06/24 ********************************
        outTrgName = inTrgName;
        outUser    = inUser;
        outPgm     = inPgm;
        outLibFile = inLibFile;
        outLibObj  = inLibObj;
        outLibEnv  = inLibEnv;
        outChgOld  = inChgOld;
        outChgNew  = inChgNew;
        outRptOld  = inRptOld;
        outRptNew  = inRptNew;
        //*** End Add ***** 02/06/24 ********************************
      /End-free
     C                   call      strExitPgm
     C                   parm                    outTrgName
     C                   parm                    outUser
     C                   parm                    outPgm
     C                   parm                    outLibFile
     C                   parm                    outLibObj
     C                   parm                    outLibEnv
     C                   parm                    outChgOld
     C                   parm                    outChgNew
     C                   parm                    outRptOld
     C                   parm                    outRptNew
      /Free
       Endsr;

       //================================================================== ==
       // Open the files
       //================================================================== ==
       Begsr OpenFiles;

        // Close Files if Still Open
        Exsr CloseFiles;

        //*** Begin Chg *** 02/26/18 *********************
        //*** #sxtr = 'M' + %editc(q1jobn:'Z');
        #sxtr = 'M' + %editc(q1jobn:'X');
        //*** End   Chg *** 02/26/18 *********************
        @smtr = %trim(Upper(inLibFile)) + '/' + 'SMTRL';
        Open smtrl;
        @smtr = %trim(Upper(inLibFile)) + '/' + 'SMTRPFIL';
        Open smtrpfil;
        @smtr = %trim(Upper(inLibFile)) + '/' + 'SMTRPFLD';
        Open smtrpfld;
        @smtr = %trim(Upper(inLibFile)) + '/' + 'SMTRPACT';
        Open smtrpact;
        @smtr = %trim(Upper(inLibFile)) + '/' + 'SMTRPSQL';
        Open smtrpsql;
        @smtr = %trim(Upper(inLibFile)) + '/' + 'SMTRLFRQ';
        Open smtrlfrq;
        @smtr = %trim(Upper(inLibFile)) + '/' + 'SMTRLREC';
        Open smtrlrec;
        @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPXML';
        Open sxtrpxml;
        @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPREC';
        Open sxtrprec;
        @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT0';
        ChkMember(@sxtr:#sxtr);
        Open sxtrpout0;
        @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT1';
        ChkMember(@sxtr:#sxtr);
        Open sxtrpout1;
        @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT2';
        ChkMember(@sxtr:#sxtr);
        Open sxtrpout2;
       Endsr;

       //================================================================== ==
       // Process the email digest
       //================================================================== ==
       Begsr ProcessDigest;
        If not isUseEmail;
           Clear TRemail;
        Endif;
        If not isUseSMS;
           Clear TRtext;
        Endif;
        days = 0;
        strJob = 'EML' + TRdigest;
        // If active job exists for digest time, begin processing for next day
        If ChkJobActive(strJob);
           days = 1;
        Endif;
        // If after digest time, begin processing for next day
        If SystemTime() > %time(TRdigest:*hms0);
           days = 1;
        Endif;
        dtSchJob = SystemDate() + %days(days);
        // Check for record for current rule
        Setll (TRco#:TRname:TRseq#:dtSchJob:TRdigest) xtr0rec;
        Dou %eof(sxtrprec);
           Reade (TRco#:TRname:TRseq#:dtSchJob:TRdigest) xtr0rec;
           Select;
           When %eof;
              Iter;
           When xTRemail = TRemail or xTRtext = TRtext;
              Leave;
           Endsl;
        Enddo;
        If %eof;
           xTRco# = TRco#;
           xTRname = TRname;
           xTRseq# = TRseq#;
           xTRdgstDt = dtSchJob;
           xTRdigest = TRdigest;
           xTRemail = TRemail;
           xTRtext = TRtext;
           xTRdt = DateTo7(SystemDate());
           Write xtr0rec;
        Endif;
        //Chain (TRco#:TRname:TRseq#:dtSchJob:TRdigest) xtr0xml;
        //If not %found;
        UpdateXML(GetRuleXML(TRco#:TRname:TRseq#:dtSchJob:TRdigest));
        //Endif;
        // If no job exists for digest time, submit scheduled job
        If not ChkJobJobQ(strJob);
           strCmd = 'SBMJOB ' +
            'CMD(CALL PGM(STRULEDGST) ' +
             'PARM(' +
              '''' + %char(dtSchJob:*usa/) + ''' ' +
              '''' + TRdigest + '''' +
              ') ' +
             ') ' +
            'SCDDATE(' + %char(dtSchJob:*usa0) + ') ' +
            'SCDTIME(' + TRdigest + ') ' +
            'JOB(' + %trim(strJob) + ') ';
           RunCmd(strCmd:%len(%trim(strCmd)));
        Endif;
       Endsr;

       //================================================================== ==
       // Process the SQL statement
       //================================================================== ==
       Begsr ProcessSQL;
        Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0sql;
        Select;
        When not %found;
           Leavesr;
        When TRsqlAct = *blank;
           Leavesr;
        Endsl;
        Monitor;
          Exec sql execute immediate :TRsqlAct;
        On-error;
          dsMsgInfo = RtvMsg('*');
          strBodySQL =
           'SQL statement: ' + %trim(TRsqlAct) + '<br><br>' +
           'Error recieved: ' + %trim(dsMsgInfo.msgId) + ' - ' +
            %trim(dsMsgInfo.msgText) + '<br><br>' +
           'Rule: ' + %trim(TRdesc) + '<br>' +
           'Trigger: ' + %trim(inTrgName) + '<br><br>';
          strCmd = 'SNDEMAIL FROM(' + %trim(GetUserEmail(TRuser)) + ') ' +
           'TO(' + %trim(GetUserEmail(TRuser)) + ') ' +
           'SUBJECT(''Rules - SQL Action Failed'') ' +
           'BODY(''' + %trim(strBodySQL) + ''') ';
          RunCmd(strCmd:%len(%trim(strCmd)));
        Endmon;
       Endsr;

       //================================================================== ==
       // Process the change via XML
       //================================================================== ==
       Begsr ProcessXML;
        Select;
        When not isUseXML;
           Leavesr;
        When TRdigest <> *blanks;
           Leavesr;
        When strAttach <> *blank;
           Leavesr;
        Endsl;
        Clear strSms;
        If strXML = *blank;
           strXML = GetRuleXML(dsFld.TRco#:dsFld.TRname:dsFld.TRseq#
            :dtNull:*blanks);
        Endif;
        UpdateXML(strXML);
        Monitor;
         //***  Begin Chg  ***  07/01/19  *********************************
         //CreateXML(strXML:*off);
           CreateXML(strXML:isSubmitXML);
         //***  End   Chg  ***  07/01/19  *********************************
           In dsXmlMsg;
        On-error;
           strXmlMsg = '*Failure';
        Endmon;
        //***  Begin Add  ***  07/01/19  *********************************
        // If the XML is submitted, we will not process anything else
        If isSubmitXML;
           Leavesr;
        Endif;
        //***  End   Add  ***  07/01/19  *********************************
        If strXmlMsg = *blanks or %subst(strXmlMsg:1:7) = 'Success';
           strOutput = GetXMLOutputName(strXML:*on);
           Clear strAttach;
           If strOutput <> *blank;
              strAttach = 'ATTACH((''' + %subst(strDescFile:1:30) + ''' ''' +
               %trim(strOutput) + '''))';
           Endif;
           In dsFileChgInfo;
           Select;
           When strExtPrefix <> *blanks;
              strSms = GetXMLOutputName(strXML:*off:2);
              If strSms <> *blank;
                 strSms = %trim(strExtPrefix) + %trim(strSms);
              Endif;
           Other;
              strSms = GetXMLOutputName(strXML:*on:2);
           Endsl;
        Endif;
        DltRuleXML(dsFld.TRco#:dsFld.TRname:dsFld.TRseq#:dtNull:*blanks);
       Endsr;

       //***  Begin Add  ***  07/01/19  ************************************
       //================================================================== ==
       // Write to the file used for post Scriptura processing
       //================================================================== ==
       Begsr WriteOut3;
        Select;
        When TRdigest <> *blanks;
           Exsr ProcessDigest;
           Leavesr;
        Endsl;
        If strXML = *blank;
           strXML = GetRuleXML(dsFld.TRco#:dsFld.TRname:dsFld.TRseq#
            :dtNull:*blanks);
        Endif;
        Reset dsSXTRPout3;
        dsSXTRPout3.TRxml = strXML;
        dsSXTRPout3.TRco# = TRco#;
        dsSXTRPout3.TRsubject = strSubject;
        dsSXTRPout3.TRbody = strBody;
        dsSXTRPout3.TRdescFile = strDescFile;
        If isUseEmail and TRemail <> *blanks;
           dsSXTRPout3.TRnotify = 'EMAIL';
           dsSXTRPout3.TRrecip = TRemail;
           Exec sql
            insert into SXTRPout3
             values :dsSXTRPout3;
        Endif;
        If isUseSMS and TRtext <> *zeros;
           dsSXTRPout3.TRnotify = 'SMS';
           dsSXTRPout3.TRrecip = %editc(TRtext:'X');
           If strSmsPrefix <> *blanks;
              dsSXTRPout3.TRsubject =
               %trim(strSmsPrefix) + ' ' + dsSXTRPout3.TRsubject;
           Endif;
           Exec sql
            insert into SXTRPout3
             values :dsSXTRPout3;
        Endif;
       Endsr;
       //***  End   Add  ***  07/01/19  ************************************

       //================================================================== ==
       // Write to the files to be processed via XML
       //================================================================== ==
       Begsr WriteXML01;
        If not isUseXML;
           Leavesr;
        Endif;
        If strXML = *blank;
           strXML = GetRuleXML(dsFld.TRco#:dsFld.TRname:dsFld.TRseq#
            :dtNull:*blanks);
        Endif;
        TRxml = strXML;
        TRuser = inUser;
        TRtimeStmp = timeStamp;
        TRuserDesc = GetDescObj('*USRPRF':inUser);
        TRtrgName = inTrgName;
        //TRiDt = SystemDate();
        Write xtr0out0;
        TRfileChg = %trim(dsFrq.TRlibrary) + '/' + %trim(dsFrq.TRfile);
        TRfileDesc = GetDescObj('*FILE':dsFrq.TRfile:dsFrq.TRlibrary);
        TRchgPgm = %trim(inPgm);
        TRpgmDesc = GetDescObj('*PGM':inPgm);
        Write xtr0out1;
       Endsr;
      /End-free

      //======================================================================
      // Build the directory
      //======================================================================
     PBuildDirectory   b
     DBuildDirectory   pi          1000

     DstrDir           s             80    inz('FileChanges')
     DstrEnv           s              1
     DstrRoot          s             10    inz
     DstrValue         s                   inz like(BuildDirectory)

     DstrPgm           s             21    inz

     DAddPrefix        pr                  extpgm(strPgm)
     D ioDir                         80
     DRtvRoot          pr                  extpgm(strPgm)
     D outRoot                       10
     DRtvEnv           pr                  extpgm(strPgm)
     D outEnv                         1
      /Free
       If strDirLocation = *blanks;
          strDirLocation = '\\PDSDEV';
       Endif;
       strPgm = %trim(inLibObj) + '/SA039R';
       AddPrefix(strDir);
       strPgm = %trim(inLibObj) + '/RTVDOCENV';
       RtvEnv(strEnv);
       If strEnv <> 'Q';
          strPgm = %trim(inLibObj) + '/RTVDOCROOT';
          RtvRoot(strRoot);
       Endif;
       If strRoot <> *blanks;
          strValue = '/' + %trim(strRoot);
       Endif;
       strValue = %trim(strDirLocation)
        + %trim(strValue) + '/' + %trim(strDir);
       strValue = ScanReplace('/':'\':%trim(strValue));
       Return %trim(strValue);
      /End-free
     PBuildDirectory   e

      //======================================================================
      // Compare alpha values
      //======================================================================
     PCheckAlpha       b
     DCheckAlpha       pi             1n
     D inValue                    32764                     const
     D inOperator                          like(TRoperCur)  const
     D inCompValue                         like(TRcmpValC)  const
     D inCompType                          like(TRcmpTypC)  const
     D inFieldType                         like(TRfieldTyp) const
      /Free
       If inOperator = *blanks;
          Return *on;
       Endif;
       Select;
       When inOperator = 'Begins with'
        and %subst(inValue:1:%len(%trim(inCompValue))) = %trim(inCompValue);
          Return *on;
       When inOperator = 'Contains'
        and %scan(%trim(inCompValue):%trim(inValue)) > 0;
          Return *on;
       Endsl;
       Return *off;
      /End-free
     PCheckAlpha       e

      //======================================================================
      // Compare date values
      //======================================================================
     PCheckDate        b
     DCheckDate        pi             1n
     D inValue                    32764                     const
     D inOperator                          like(TRoperCur)  const
     D inCompValue                         like(TRcmpValC)  const
     D inCompType                          like(TRcmpTypC)  const
     D inFieldType                         like(TRfieldTyp) const

     DdtCompValue      s               d   datfmt(*USA)
     DdtFirst          s               d   datfmt(*USA)
     DdtLast           s               d   datfmt(*USA)
     DdtValue          s               d   datfmt(*USA)
     DintNumComp       s             63  0
     DintNewValue      s             63  0
     DstrCompValue     s                   like(TRcmpValC)
      /Free
       If inOperator = *blanks;
          Return *on;
       Endif;
       // Greater than
       // Equal to
       // Less than
       Monitor;
          strCompValue = %trim(%xlate('/':'':inCompValue));
          intNumComp = %int(%trim(strCompValue));
          Select;
          When inFieldType = 'Date';
             dtValue = CvtToDate(%int(%trim(%xlate('/':'':inValue))));
          Other;
             dtValue = %date(inValue);
          Endsl;
          Select;
          When inCompType = 'MMDDYYYY';
             If DtCheck(%int(%trim(strCompValue))) = 0;
                Return *off;
             Endif;
             dtCompValue = CvtToDate(%int(%trim(strCompValue)));
             Select;
             When inOperator = 'Greater than'
              and dtValue > dtCompValue;
                Return *on;
             When inOperator = 'Equal to'
              and dtValue = dtCompValue;
                Return *on;
             When inOperator = 'Less than'
              and dtValue < dtCompValue;
                Return *on;
             Endsl;
          When inCompType = 'MMYYYY';
             If Dt6Check(%int(%trim(strCompValue))) = 0;
                Return *off;
             Endif;
             strCompValue = %replace('01':strCompValue:3:0);
             dtFirst = CvtToDate(%int(%trim(strCompValue)));
             dtLast = GetLastDay(dtFirst);
             Select;
             When inOperator = 'Greater than'
              and dtValue > dtLast;
                Return *on;
             When inOperator = 'Equal to'
              and (dtValue >= dtFirst and dtValue <=dtLast);
                Return *on;
             When inOperator = 'Less than'
              and dtValue < dtLast;
                Return *on;
             Endsl;
          When inCompType = 'MM';
             intNumComp = %int(%trim(strCompValue));
             Select;
             When intNumComp < 1;
                Return *off;
             When intNumComp > 12;
                Return *off;
             Endsl;
             Select;
             When inOperator = 'Greater than'
              and %subdt(dtValue:*months) > intNumComp;
                Return *on;
             When inOperator = 'Equal to'
              and %subdt(dtValue:*months) = intNumComp;
                Return *on;
             When inOperator = 'Less than'
              and %subdt(dtValue:*months) < intNumComp;
                Return *on;
             Endsl;
          When inCompType = 'DD';
             intNumComp = %int(%trim(strCompValue));
             Select;
             When intNumComp < 1;
                Return *off;
             When intNumComp > 31;
                Return *off;
             Endsl;
             Select;
             When inOperator = 'Greater than'
              and %subdt(dtValue:*days) > intNumComp;
                Return *on;
             When inOperator = 'Equal to'
              and %subdt(dtValue:*days) = intNumComp;
                Return *on;
             When inOperator = 'Less than'
              and %subdt(dtValue:*days) < intNumComp;
                Return *on;
             Endsl;
          When inCompType = 'YYYY';
             intNumComp = %int(%trim(strCompValue));
             Select;
             When inOperator = 'Greater than'
              and %subdt(dtValue:*years) > intNumComp;
                Return *on;
             When inOperator = 'Equal to'
              and %subdt(dtValue:*years) = intNumComp;
                Return *on;
             When inOperator = 'Less than'
              and %subdt(dtValue:*years) < intNumComp;
                Return *on;
             Endsl;
          Endsl;
       On-error;
          Return *off;
       Endmon;
       Return *off;
      /End-free
     PCheckDate        e

      //======================================================================
      // Compare date values
      //======================================================================
     PCheckNumeric     b
     DCheckNumeric     pi             1n
     D inValue                    32764                     const
     D inNewValue                 32764                     const
     D inOperator                          like(TRoperCur)  const
     D inCompValue                         like(TRcmpValC)  const
     D inCompType                          like(TRcmpTypC)  const
     D inFieldType                         like(TRfieldTyp) const

     DintNumComp       s             63  0
     DintNewValue      s             63  0
     DintValue         s             63  0
      /Free
       If inOperator = *blanks;
          Return *on;
       Endif;
       Monitor;
          intNumComp = %int(%trim(%xlate(',':'':inCompValue)));
          intValue = %int(%trim(%xlate(',':'':inValue)));
          intNewValue = %int(%trim(%xlate(',':'':inNewValue)));
       On-error;
          Return *off;
       Endmon;
       Select;
       When inCompType = '%';
          intNumComp = intNumComp * intValue / 100;
       Endsl;
       Select;
       When inOperator = 'Equal to';
          Return intValue = intNumComp;
       When inOperator = 'Less than';
          Return intValue < intNumComp;
       When inOperator = 'Greater than';
          Return intValue > intNumComp;
       When inOperator = 'Increased';
          Return intNewValue >= intValue + intNumComp;
       When inOperator = 'Decreased';
          Return intNewValue <= intValue - intNumComp;
       Endsl;
       Return *off;
      /End-free
     PCheckNumeric     e

     P*====================================================================
     P* Check for member existence
     P*====================================================================
     PChkMember        b
     DChkMember        pi                  opdesc
     D inFile                        21    const
     D inMember                      10    const

       //***  Begin Add  ***  08/04/20  *****************************
     dmaxTries         c                   4
     ddecTries         s              3  0 inz(1)
       //***  End   Add  ***  08/04/20  *****************************

      /Free
       //***  Begin Add  ***  08/04/20  *****************************
       // If the member can not be created after maxTries, the older process will ab-end as before
       If ChkFileMbr(inFile:'*FILE':inMember) = *off;
          For decTries = 1 to maxTries;
             // AddFileMbr returns *off, if successful
             If AddFileMbr(inFile:inMember) = *off;
                Leave;
             Endif;
          Endfor;
       Endif;
       If decTries <= maxTries;
          Return;
       Endif;
       //***  End   Add  ***  08/04/20  *****************************
        Monitor;
          strCmd = 'CHKOBJ OBJ(' + %trim(inFile) + ' ) OBJTYPE(*FILE) ' +
           'MBR(' + %trim(inMember) + ')';
          RunCmd(strCmd:%len(%trim(strCmd)));
        On-Error;
          strCmd = 'ADDPFM FILE(' + %trim(inFile) + ' ) ' +
           'MBR(' + %trim(inMember) + ')';
          RunCmd(strCmd:%len(%trim(strCmd)));
        Endmon;
      /End-free

     PChkMember        e

      //======================================================================
      // Compare values
      //======================================================================
     PCompareValues    b
     DCompareValues    pi             1n
     D inOldValue                 32764
     D inNewValue                 32764

     DdtComp           s               d   datfmt(*USA)
     DdtFirst          s               d   datfmt(*USA)
     DdtLast           s               d   datfmt(*USA)
     DdtOld            s               d   datfmt(*USA)
     DdtNew            s               d   datfmt(*USA)
     DintNumComp       s             63  0
     DintNewValue      s             63  0
     DintOldValue      s             63  0
     DisMeetsComp      s              1n   inz
     DstrCompType      s                   like(TRcmpTypC)
     DstrCompValue     s                   like(TRcmpValC)
     DstrFieldType     s                   like(TRfieldTyp)
     DstrOperator      s                   like(TRoperCur)
      /Free
       strFieldType = Upper(dsFld.TRfieldTyp);
       Select;
       When  dsFld.TRoperCur = *blanks and  dsFld.TRoperPrv = *blanks;
          Return *on;
       When strFieldType = 'DATE'
        or  strFieldType = 'TIMESTAMP';
          If not CheckDate(inNewValue:dsFld.TRoperCur:dsFld.TRcmpValC:
           dsFld.TRcmpTypC:dsFld.TRfieldTyp);
             Return *off;
          Endif;
          If not CheckDate(inOldValue:dsFld.TRoperPrv:dsFld.TRcmpValP:
           dsFld.TRcmpTypP:dsFld.TRfieldTyp);
             Return *off;
          Endif;
          Return *on;
       When strFieldType = 'CHAR'
        or  strFieldType = 'VARCHAR';
          If not CheckAlpha(Upper(inNewValue):dsFld.TRoperCur:
           Upper(dsFld.TRcmpValC):dsFld.TRcmpTypC:strFieldType);
             Return *off;
          Endif;
          If not CheckAlpha(Upper(inOldValue):dsFld.TRoperPrv:
           Upper(dsFld.TRcmpValP):dsFld.TRcmpTypP:strFieldType);
             Return *off;
          Endif;
          Return *on;
       When strFieldType = 'DECIMAL'
        or  strFieldType = 'NUMERIC'
        or  strFieldType = 'INTEGER'
        or  strFieldType = '7.0';
          If not CheckNumeric(inOldValue:inNewValue:dsFld.TRoperCur:
           dsFld.TRcmpValC:dsFld.TRcmpTypC:strFieldType);
             Return *off;
          Endif;
          If not CheckNumeric(inNewValue:inNewValue:dsFld.TRoperPrv:
           dsFld.TRcmpValP:dsFld.TRcmpTypP:strFieldType);
             Return *off;
          Endif;
          Return *on;
       Endsl;
       Return *off;
      /End-free
     PCompareValues    e

      //======================================================================
      // Delete XML record name
      //======================================================================
     PDltRuleXML       b
     DDltRuleXML       pi
     D inCo#                          3  0 const
     D inName                        10    const
     D inSeq#                         5  0 const
     D inDigestDt                      d   const
     D inDigest                       6    const

     dstrXml           s            250
      /Free
       Delete(e) (inCo#:inName:inSeq#:inDigestDt:inDigest) xtr0xml;
       Return;
      /End-free
     PDltRuleXML       e

      //======================================================================
      // Get field descriptions
      //======================================================================
     PGetDescField     b
     DGetDescField     pi            75
     D inField                       10    const

     DisEOF            s              1n   inz
     DstrCmd           s           3000    inz
     DstrDescField     s             75    inz
     DstrSqlCursor     s           1000    inz
     D
     DdsFile           ds
     D WHfldI                        10
     D WHfldE                        10
     D WHfTxt                        50
      /Free
       Clear dsFile;
       strSqlCursor = 'select WHfldI, WHfldE, WHfTxt ' +
        'from QTEMP/APPCFGFLD ' +
        'where WHfldI = ''' + %trim(inField) + '''' +
        ' or WHfldE = ''' + %trim(inField) + '''';
       Exec SQL Prepare sqlFile From :strSqlCursor;
       Exec SQL Declare cursorFile Cursor For sqlFile;
       Exec SQL Open cursorFile;
       Exec SQL Fetch next From cursorFile into :dsFile;
       Exec SQL Close cursorFile;
       If WHfTxt <> *blanks;
          strDescField = %trim(WHfTxt) + ' (';
       Endif;
       strDescField = %trim(strDescField) + %trim(inField);
       If WHfTxt <> *blanks;
          strDescField = %trim(strDescField) + ')';
       Endif;
       Return strDescField;
      /End-free
     PGetDescField     e

      //======================================================================
      // Get object descriptions
      //======================================================================
     PGetDescObj       b
     DGetDescObj       pi            75
     D inType                        10    const
     D inObj                         10    const
     D inLibrary                     10    const options(*nopass)

     DisEOF            s              1n   inz
     DstrCmd           s           3000    inz
     DstrDescObj       s             75    inz
     DstrLib           s             10    inz('*LIBL')
     DstrSqlCursor     s           1000    inz
     DstrType          s             10
     D
     DdsObj            ds
     D ODlbNm                        10
     D ODobNm                        10
     D ODobTx                        50
      /Free
       strType = inType;
       If %parms >= 3 and inLibrary <> *blanks;
          strLib = inLibrary;
       Endif;
       Dou isEOF;
          Clear dsObj;
          Monitor;
             strCmd = 'DSPOBJD ' +
              'OBJ(' + %trim(strLib) + '/' + %trim(inObj) + ') ' +
              'OBJTYPE(' + %trim(strType) + ') OUTPUT(*OUTFILE) ' +
              'OUTFILE(QTEMP/APPCFGOBJ)';
             RunCmd(strCmd:%len(strCmd));
          On-error;
             Select;
             When strType = '*PGM';
                strType = '*SRVPGM';
             When strType = '*SRVPGM';
                strType = '*MODULE';
             When strType = '*MODULE';
                isEOF = *on;
             Other;
                isEOF = *on;
             Endsl;
             Iter;
          Endmon;
          strSqlCursor = 'select ODlbNm, ODobNm, ODobTx ' +
           'from QTEMP/APPCFGOBJ';
          Exec SQL Prepare sqlObj From :strSqlCursor;
          Exec SQL Declare cursorObj Cursor For sqlObj;
          Exec SQL Open cursorObj;
          Exec SQL Fetch next From cursorObj into :dsObj;
          Select;
          When ODobTx <> *blanks;
             isEOF = *on;
          When strType = '*PGM';
             strType = '*SRVPGM';
          When strType = '*SRVPGM';
             strType = '*MODULE';
          When strType = '*MODULE';
             isEOF = *on;
          Other;
             isEOF = *on;
          Endsl;
          Exec SQL Close cursorObj;
       Enddo;
       If ODobTx <> *blanks;
          strDescObj = %trim(ODobTx) + ' (';
       Endif;
       If strLib <> '*LIBL';
          strDescObj = %trim(strDescObj) + %trim(strLib) + '/';
       Endif;
       strDescObj = %trim(strDescObj) + %trim(inObj);
       If ODobTx <> *blanks;
          strDescObj = %trim(strDescObj) + ')';
       Endif;
       Return strDescObj;
      /End-free
     PGetDescObj       e

      //======================================================================
      // Process data into individual fields
      //======================================================================
     PGetFieldValue    b
     DGetFieldValue    pi         32764
     D inData                     32764

     DstrValue         s          32764    inz
     DdecLength        s             15  0
      /Free
       decLength = dsFld.TRfldB;
       If dsFld.TRfldD > decLength;
          decLength = dsFld.TRfldD;
       Endif;
       strValue = %subst(inData:dsFld.TRposition:decLength);
       Select;
       When dsFld.TRfldT = 'Z';
       When dsFld.TRfldP <> 0;
          strValue = %replace('.':strValue:dsFld.TRfldD-dsFld.TRfldP+1:0);
       Endsl;
       Return %trim(strValue);
      /End-free
     PGetFieldValue    e

      //======================================================================
      // Get XML name
      //======================================================================
     PGetRuleXML       b
     DGetRuleXML       pi           250
     D inCo#                          3  0 const
     D inName                        10    const
     D inSeq#                         5  0 const
     D inDigestDt                      d   const
     D inDigest                       6    const

     dstrXml           s            250
      /Free
       //*** Beg Add ***** 05/21/25 ****************************************
       pgmName   = 'STLETTER';
       pgmExists = *Off;
       ChkPgmStack(pgmName:pgmExists);
       //*** End Add ***** 05/21/25 ****************************************
       If inDigestDt = dtNull;
          //*** Beg Chg ***** 05/21/25 ****************************************
          //***strXml = GetXMLFileName(' ':*zeros:inCo#:*zeros:'RU':'Rule':
          //*** 'Rule Report');
          If pgmExists = *On;
             strXml = RtvXMLFileName(' ':*zeros:inCo#:*zeros:'RU':'Rule':
              'Rule Report');
          Else;
             strXml = GetXMLFileName(' ':*zeros:inCo#:*zeros:'RU':'Rule':
              'Rule Report');
          Endif;
          //*** End Chg ***** 05/21/25 ****************************************
          Return %trim(strXml);
       Endif;
       Chain (inCo#:inName:inSeq#:inDigestDt:inDigest) xtr0xml;
       If not %found;
          TRco# = inCo#;
          TRname = inName;
          TRseq# = inSeq#;
          TRdgstDt = inDigestDt;
          TRdigest = inDigest;
          //*** Beg Chg ***** 05/21/25 ****************************************
          //***TRxml = GetXMLFileName(' ':*zeros:inCo#:*zeros:'RU':'Rule':
          //*** 'Rule Report');
          If pgmExists = *On;
             TRxml = RtvXMLFileName(' ':*zeros:inCo#:*zeros:'RU':'Rule':
              'Rule Report');
          Else;
             TRxml = GetXMLFileName(' ':*zeros:inCo#:*zeros:'RU':'Rule':
              'Rule Report');
          Endif;
          //*** End Chg ***** 05/21/25 ****************************************
          TRdt = DateTo7(SystemDate());
          Write xtr0xml;
       Endif;
       Return %trim(TRxml);
      /End-free
     PGetRuleXML       e

      //======================================================================
      // Process data into individual fields
      //======================================================================
     PProcessData      b
     DProcessData      pi
     D inOldData                  32764
     D inNewData                  32764

     DstrBodyTable     s          32764
     DstrValue         s          32764
      /Free
       strBodyTable = '<style>' +
        'th {text-align: left;}' +
        '</style>' +
        '<table style="width:100%">' +
        '<tr>' +
        '<th><strong><u>Field</u></strong></th>';
       If inOldData <> *blank;
          strBodyTable = %trim(strBodyTable) +
           '<th><strong><u>Current Value</u></strong></th>';
       Endif;
       If inNewData <> *blank;
          strBodyTable = %trim(strBodyTable) +
           '<th><strong><u>Prior Value</u></strong></th>';
       Endif;
       strBodyTable = %trim(strBodyTable) + '</tr>';
       Setll (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld;
       Dou %eof(smtrpfld);
          Reade (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld dsFld;
          Select;
          When %eof;
             Iter;
          Endsl;
          Exsr WriteXML;
          Select;
          When dsFld.TRtype <> 'Report';
             Iter;
          Endsl;
          strBodyTable = %trim(strBodyTable) + '<tr>' +
           '<td>' + %trim(GetDescField(dsFld.TRfield)) + '</td>';
          If inNewData <> *blank;
             strBodyTable = %trim(strBodyTable) +
              '<td>' + %trim(GetFieldValue(inNewData)) + '</td>';
          Endif;
          If inOldData <> *blank;
             strBodyTable = %trim(strBodyTable) +
              '<td>' + %trim(GetFieldValue(inOldData)) + '</td>';
          Endif;
          strBodyTable = %trim(strBodyTable) + '</tr>';
       Enddo;
       strBodyTable = %trim(strBodyTable) + '</table>';
       strBody = %trim(strBody) + '<br><br>' +
           %trim(strBodyTable);
       //------------
       Begsr WriteXML;
        If not isUseXML;
           Leavesr;
        Endif;
        If strXML = *blank;
           strXML = GetRuleXML(dsFld.TRco#:dsFld.TRname:dsFld.TRseq#
            :dtNull:*blanks);
        Endif;
        TRxml = strXML;
        TRco# = dsFld.TRco#;
        TRname = dsFld.TRname;
        TRseq# = dsFld.TRseq#;
        TRtimeStmp = timeStamp;
        TRtype = dsFld.TRtype;
        TRfield = dsFld.TRfield;
        TRfldDesc =  %trim(GetDescField(dsFld.TRfield));
        TRvalueCur = %trim(GetFieldValue(inNewData));
        TRvaluePrv = %trim(GetFieldValue(inOldData));
        Write xtr0out2;
       Endsr;
      /End-free
     PProcessData      e

     P*====================================================================
     P* Remove Last Character
     P*====================================================================
     PRmvExtraChar     b
     DRmvExtraChar     pi         32765    opdesc
     D inField                    32765    options(*varsize) const
     D inChar                         1    const
     D inFieldLen                    15  0 options(*nopass) const

     D f1              s              3  0
     D pos             s              5  0
     D returnFld       s                   like(inField) inz

      /Free
       returnFld = inField;
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If %parms >= 3 and inFieldLen <> *zero;
          inLen = inFieldLen;
       Endif;
       If %subst(inField:inLen:1) = inChar;
          returnFld = %subst(inField:1:inLen-1);
       Endif;
       Return returnFld;
      /End-free

     PRmvExtraChar     e

     P*====================================================================
     P* Remove member
     P*====================================================================
     PRmvMember        b
     DRmvMember        pi                  opdesc
     D inFile                        21    const
     D inMember                      10    const

      /Free
        Monitor;
          strCmd = 'RMVM FILE(' + %trim(inFile) + ' ) ' +
           'MBR(' + %trim(inMember) + ')';
          RunCmd(strCmd:%len(%trim(strCmd)));
        On-Error;
        Endmon;
      /End-free

     PRmvMember        e

     P*====================================================================
     P* Update XML
     P*====================================================================
     PUpdateXML        b
     DUpdateXML        pi                  opdesc
     D inXML                        250    const

     DdecDate          s                   like(TRdt)
      /Free
       decDate = DateTo7(SystemDate());
       //Chain (inXml) xtr0out0;
       //If %found;
       //   Return;
       //Endif;
       //Update TRxml in all files
       Setll *loval xtr0out0;
       Dou %eof(sxtrpout0);
          Read xtr0out0;
          Select;
          When %eof;
             Iter;
          Endsl;
          TRxml = inXML;
          TRdt = decDate;
          Update xtr0out0;
       Enddo;
       Setll *loval xtr0out1;
       Dou %eof(sxtrpout1);
          Read xtr0out1;
          Select;
          When %eof;
             Iter;
          Endsl;
          TRxml = inXML;
          TRdt = decDate;
          Update xtr0out1;
       Enddo;
       Setll *loval xtr0out2;
       Dou %eof(sxtrpout2);
          Read xtr0out2;
          Select;
          When %eof;
             Iter;
          Endsl;
          TRxml = inXML;
          TRdt = decDate;
          Update xtr0out2;
       Enddo;
       //Copy OUT* to main files
       @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT0';
       Exsr CopyFile;
       @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT1';
       Exsr CopyFile;
       @sxtr = %trim(Upper(inLibFile)) + '/' + 'SXTRPOUT2';
       Exsr CopyFile;
       //---------
       Begsr CopyFile;
        strCmd = 'CPYF FROMFILE(' + %trim(@sxtr) + ' ) ' +
         'TOFILE(' + %trim(@sxtr) + ' ) ' +
         'FROMMBR(' + %trim(#sxtr) + ') ' +
         'TOMBR(*FIRST) MBROPT(*ADD)';
        RunCmd(strCmd:%len(%trim(strCmd)));
       Endsr;
      /End-free

     PUpdateXML        e

      //======================================================================
      // Validate data against comparisons
      //======================================================================
     PValidDataCond    b
     DValidDataCond    pi             1n
     D inOldData                  32764
     D inNewData                  32764

     DstrNewValue      s          32764
     DstrOldValue      s          32764
      /Free
       Setll (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#:'Change') ptr0fld;
       Dou %eof(smtrpfld);
          Reade (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#:'Change') ptr0fld dsFld;
          Select;
          When %eof;
             Iter;
          When dsFld.TRstat = 'I';
             Iter;
          Endsl;
          strNewValue = GetFieldValue(inNewData);
          strOldValue = GetFieldValue(inOldData);
          If not CompareValues(strOldValue:strNewValue);
            Return *off;
          Endif;
       Enddo;
       Return *on;
      /End-free
     PValidDataCond    e

      //*** Beg Add ***** 05/21/25 ********************************************
      //======================================================================
      // Retrieve XML File name
      //======================================================================
     PRtvXMLFileName   b
     DRtvXMLFileName   pi            60
     d inMode                         1    const
     d inRRN                          9  0 const options(*nopass)
     d inCo#                          3  0 const options(*nopass)
     d inFnd                          3  0 const options(*nopass)
     d inPrefix                       2    const options(*nopass)
     d inString                      10    const options(*nopass)
     d inProcType                   100    const options(*nopass)

     d strCo#          s              3    inz(*all'0')
     d strFnd          s              3    inz(*all'0')
     d strPrefix       s              2    inz
     d processType     s            100    inz
     d strRRN          s              9    inz(*all'0')
     d strString       s             10    inz
     d strXMLName      s             60    inz
     d outfile         s             60    inz
     d myStamp         s             17
     d myStampFull     s             20
     d intCo#          s              3  0
     d xmlFileTemp     s                   like(XPXML)
     d xmlCheck        s              1    inz(*off)
     d myStampInt      s             17  0
      /Free

       If %parms >= 2;
          strRRN = %editc(inRRN:'X');
       Endif;
       If %parms >= 3;
          strCo# = %editc(inCo#:'X');
          intCo# = inCo#;
       Endif;
       If %parms >= 4;
          strFnd = %editc(inFnd:'X');
       Endif;
       If %parms >= 5;
          strPrefix = inPrefix;
       Endif;
       If %parms >= 6;
          strString = inString;
       Endif;
       If %parms >= 7;
          processType = inProcType;
       Endif;

       Read psc0p;
       myStamp = %subst(%char(%timestamp:*iso0):1:17);
       myStampFull = %subst(%char(%timestamp():*iso0):1:20);
       strXMLName = 'Flow_XML' + '_' + myStampFull + '_' + %trim(Q1User) +
        '.XML';
       outFile = %trim(strString) + '_' + %trim(Q1User) +
        '_' + myStamp + '.PDF';

       xmlCheck = *off;
       myStampInt = %int(myStamp);
       myStampInt = myStampInt + 1;
       myStamp = %editc(myStampInt:'X');

       //Get XML Template name
       DoU xmlCheck = *on;
          xmlFileTemp = %trim(strString) + '_' + %trim(Q1User) + '_' +
           myStamp + '.XML';
          Chain(n) (xmlFileTemp) xp102;
          //Check if xml file name already exists and increment time stamp.
          If %found;
             myStampInt = %int(myStamp);
             myStampInt = myStampInt + 1;
             myStamp = %editc(myStampInt:'X');
          Else;
             xmlCheck = *on;
          Endif;
       Enddo;

       Clear xp100;
       XPCo#  = intCo#;
       XPOrder = 1;
       XPFile = outFile;
       XPXMLT = xmlFileTemp;
       XPXML  = strXMLName;
       XPXMLF = %trim(SCXMLF);
       XPType = %trim(processType);
       RtvCltID(clientID);
       XPCltID = clientID;

       xxPre = strPrefix;
       Clear xxPath;
       Clear xxPflr;
       Clear xxDcid;
       Clear xxRtcd;
       xxSub# = '00000';
       Clear xxRoot;
       If xxPre <> *blank;
          SNextDocID(xxPath:xxDcid:xxPre:strCo#:strFnd:xxRtcd:xxSub#:xxPflr);
       Endif;
       DocRoot(xxRoot);
       XPDir  = '/' + %trim(xxRoot) + '/' + %trim(xxPath) + '/';


       //Get Doc Id and Path
       effdate = %Dec(%Char(%Date():*cymd0):7:0);
       Setgt  (processType:effdate) psp010;
       Readpe (processType) psp010;
       If not %eof;
          If S1SCltT <> ' ';
             XPDcid = %trim(S1SCltT);
             XPPath = %trim(S1PathC);
          Else;
             If S1SDefT <> ' ';
                XPDcid = %trim(S1SDefT);
                XPPath = %trim(S1PathD);
             Endif;
          Endif;
       Endif;

       Chain(n) (processType) psp00;
       If %found;
         XPScrp = SPScrO;
         XPdir2 = SPpathExt;
           If XPdir2 <> *blanks;
              XPfile2 = XPfile;
           Endif;
        Endif;

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = Timeto6(SystemTime());
       XPDt   = Dateto7(SystemDate());

       Write xp100;
       Feod xmlprel1;

       Return strXMLName;
      /End-free
     PRtvXMLFileName   e
      //*** End Add ***** 05/21/25 ********************************************

