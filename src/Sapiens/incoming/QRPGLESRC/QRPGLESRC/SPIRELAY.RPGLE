     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title SpiRelay - Relay Messenger API
     h option(*nodebugio:*srcstmt)
     h nomain
      *=============================================================*
      *  Created   : 07/27/16                                       *
      *  Programmer: C:136                                          *
      *  Project   : 114WYQ                                         *
      *=============================================================*
      * Revisions:                                                  *
      *                                                             *
      *=============================================================*
     frelayLog  o    e             disk    rename(prl01:prl00) usropn
     fsmcol     if   e           k disk    rename(pco01:pco00) usropn
     fstrml     if   e           k disk    rename(prm01:prm00) usropn
     fwdf2l     if   e           k disk    rename(pf201:pf200) usropn
     fwmcdl     if   e           k disk    rename(pcd01:pcd00) usropn
     fwmcmp     if   e             disk    rename(pcm01:pcm0p) usropn
      *=============================================================*
      * Data Structures
      *=============================================================*
     d psds          esds                  extname(szq1p)
     d #OfClmElements  c                   const(24)
     d clmIDValues     ds
     d                              100a   inz('{')                             1
     d                              100a   inz('"network_rid":')                2
     d                              100a   inz('"client_user_id":')             3
     d                              100a   inz('"template_code":')              4
     d                              100a   inz('"mobile_number":')              5
     d                              100a   inz('"consent_type":')               6
     d                              100a   inz('"engagement_type":')            7
     d                              100a   inz('"primary_account1":')           8
     d                              100a   inz('"account_record":')             9
     d                              100a   inz('{')                             10
     d                              100a   inz('"first_name":')                 11
     d                              100a   inz('"last_name":')                  12
     d                              100a   inz('"telephone1_number":')          13
     d                              100a   inz('"email":')                      14
     d                              100a   inz('"address1":')                   15
     d                              100a   inz('"address2":')                   16
     d                              100a   inz('"city":')                       17
     d                              100a   inz('"state_province":')             18
     d                              100a   inz('"postal_code":')                19
     d                              100a   inz('"country":')                    20
     d                              100a   inz('"date_of_birth":')              21
     d                              100a   inz('"misc_field1":')                22
     d                              100a   inz('"misc_field2":')                23
     d                              100a   inz('}}')                            24
     d  clmIDArry                   100a   overlay(clmIdValues)
     d                                     dim(#OfClmElements)

     d clmFldValues    ds
     d                              200a   inz(*blank)                           1
     d  dsNetWorkId                 200a   overlay(clmFldValues:*next)           2
     d  dsCltUsrId                  200a   overlay(clmFldValues:*next)           3
     d  dsMsgId                     200a   overlay(clmFldValues:*next)           4
     d  dsMobile#                   200a   overlay(clmFldValues:*next)           5
     d                              200a   inz('"express",')                     6
     d                              200a   inz('"validated",')                   7
     d  dsPrimAcct1                 200a   overlay(clmFldValues:*next)           8
     d                              200a   inz(*blank)                           9
     d                              200a   inz(*blank)                          10
     d  dsFirstName                 200a   overlay(clmFldValues:*next)          11
     d  dsLastName                  200a   overlay(clmFldValues:*next)          12
     d  dsTelephone#                200a   overlay(clmFldValues:*next)          13
     d  dsEmail                     200a   overlay(clmFldValues:*next)          14
     d  dsAddress1                  200a   overlay(clmFldValues:*next)          15
     d  dsAddress2                  200a   overlay(clmFldValues:*next)          16
     d  dsCity                      200a   overlay(clmFldValues:*next)          17
     d  dsState                     200a   overlay(clmFldValues:*next)          18
     d  dsPostalCode                200a   overlay(clmFldValues:*next)          19
     d  dsCountry                   200a   overlay(clmFldValues:*next)          20
     d  dsDOB                       200a   overlay(clmFldValues:*next)          21
     d  dsMiscField1                200a   overlay(clmFldValues:*next)          22
     d  dsMiscField2                200a   overlay(clmFldValues:*next)          23
     d                              200a   inz(*blank)                          24
     d  clmValArry                  200a   overlay(clmFldValues)
     d                                     dim(#OfClmElements)
      *=============================================================*
      * Prototypes
      *=============================================================*
      /copy sprRelay
      /copy sprDate
      /copy sprUsrPrms
     dBuildRelayJson   pr
     d inType                         2a   const
     d relayJson                  64512a
     dCloseFiles       pr
     d inTypeOfMsg                    2a   const
     dGetAcDt          pr            10a
     dGetDob           pr             8a
     dHasMsgId         pr              n
     d inTypeOfMsg                    2a   const
     d inKey                         10a   const
     dLoadDataQ        pr                  extpgm('QSNDDTAQ')
     d outDtaQ                       10    const
     d outLib                        10    const
     d outDtaQLen                     5  0 const
     d outData                    64512    const
     dOpenFiles        pr              n
     d inTypeOfMsg                    2a   const
     d rrnOfFile                      9  0 const
     dSmsActive        pr              n
     d typeOfFile                     2    const
     dWriteClmValues   pr
     dWriteLogFile     pr
     d inTypeOfMsg                    2a   const
     dWriteToDtaQ      pr
     d relayJson                  64512
      *=============================================================*
      * Procedures
      *=============================================================*
     pBuildRelayJson   b
     dBuildRelayJson   pi
     d inTypeMsg                      2a   const
     d relayJson                  64512a
     d i               s              3  0
      /free
       Select;
       When inTypeMsg = 'CS';
          WriteClmValues();
          For i = 1 to #OfClmElements;
             If clmValArry(i) = 'Omit';
                Iter;
             Endif;
             relayJson = %trim(relayJson) + %trim(clmIDArry(i))
              + %trim(clmValArry(i));
          EndFor;
       When inTypeMsg = 'PS';
       When inTypeMsg = 'MC';
       When inTypeMsg = 'PC';
       Endsl;
      /end-free
     pBuildRelayJson   e
      *=============================================================*
     pCloseFiles       b
     dCloseFiles       pi
     d inTypeOfMsg                    2a   const
      /free
       Select;
       When inTypeOfMsg  = 'CS';
          Close SMCOL;
          Close WDF2L;
          Close WMCDL;
          Close WMCMP;
       When inTypeOfMsg  = 'PS';
       When inTypeOfMsg  = 'MC';
       When inTypeOfMsg  = 'PC';
       Endsl;
      /end-free
     pCloseFiles       e
      *=============================================================*
     pGetAcDt          b
     dGetAcDt          pi            10a
      /free
       If CMacdt = *zeros;
          Return *zeros;
       Endif;
       Monitor;
          Return %char(%date(CMacdt:*cymd):*usa/);
       On-Error;
          Return *zeros;
       Endmon;
      /end-free
     pGetAcDt          e
      *=============================================================*
     pGetDob           b
     dGetDob           pi             8a
     d testdate        s               d
      /free
       If CDdtob = *zeros;
          Return *zeros;
       Endif;
       Monitor;
          Return %char(%date(CDdtob:*cymd):*iso0);
       On-Error;
          Return *zeros;
       Endmon;
      /end-free
     pGetDob           e
      *=============================================================*
     pHasMsgId         b
     dHasMsgId         pi              n
     d inTypeOfMsg                    2a   const
     d inKey                         10a   const
      /Free
       Open STRML;
       Select;
       When inTypeOfMsg = 'CS';
          Chain (CMco#:CMfnd:inKey) prm00;
          If %found;
             Close STRML;
             Return *on;
          Endif;
          Chain (CMco#:0:inKey) prm00;
          If %found;
             Close STRML;
             Return *on;
          Else;
             Close STRML;
             Return *off;
          Endif;
       When inTypeOfMsg = 'PS';
       When inTypeOfMsg = 'MC';
       When inTypeOfMsg = 'PC';
       Endsl;
       Close STRML;
      /End-Free
     pHasMsgId         e
      *=============================================================*
     pIsSmsDisabled    b                   export
     dIsSmsDisabled    pi              n
     d inTypeOfMsg                    2a   const
     d inRrnOfFile                    9  0 const
     d inKey                         10a   const
     d inProgram                     10a   const
     d inMsgId                      100a   const options(*nopass)
      /Free
       Select;
       When inTypeOfMsg  = 'CS';
          If not(OpenFiles(inTypeOfMsg:inRrnOfFile));
             Return *on;
          Endif;
          If CMOptOut = 'Y' or COnetId = *blanks or
           (%found(WDF2L) and F2DisSms = 'Y');
             CloseFiles(inTypeOfMsg);
             Return *on;
          Endif;
          Select;
          When %parms > 4;
          Other;
            If not(HasMsgID(inTypeOfMsg:inKey));
                CloseFiles(inTypeOfMsg);
                Return *on;
            Endif;
          Endsl;
          // Don't disable SMS checkbox if called from Claim Detail
          Select;
          When inProgram = 'WTCLMDTL';
             CloseFiles(inTypeOfMsg);
             Return *off;
          When %found(WMCDL) and CDDisSms = 'Y';
             CloseFiles(inTypeOfMsg);
             Return *on;
          Endsl;
          CloseFiles(inTypeOfMsg);
          Return *off;
       When inTypeOfMsg  = 'PS';
       When inTypeOfMsg  = 'MC';
       When inTypeOfMsg  = 'PC';
       Endsl;
       Return *off;
      /End-free
     pIsSmsDisabled    e
      *=============================================================*
     pOpenFiles        b
     dOpenFiles        pi              n
     d inTypeOfMsg                    2a   const
     d inRrnOfFile                    9  0 const
      /free
       Select;
       When inTypeOfMsg  = 'CS';
          Open SMCOL;
          Open WDF2L;
          Open WMCDL;
          Open WMCMP;
          Chain inRrnOfFile pcm0p;
          If not(%found);
             CloseFiles(inTypeOfMsg);
             Return *off;
          Endif;
          Chain CMco# pco00;
          If not(%found);
             CloseFiles(inTypeOfMsg);
             Return *off;
          Endif;
          // Group Aux may not be populated ...
          Chain (CMco#:CMfnd) pf200;
          // Claims Demographics will not not
          //  populated in Add mode yet
          Chain (CMss#) pcd00;
          CloseFiles(inTypeOfMsg);
          Return *on;
       When inTypeOfMsg  = 'PS';
       When inTypeOfMsg  = 'MC';
       When inTypeOfMsg  = 'PC';
       Endsl;
      /end-free
     pOpenFiles        e
      *=============================================================*
     pProcRelayReq     b                   export
     dProcRelayReq     pi              n
     d inTypeOfMsg                    2a   const
     d inRrnOfFile                    9  0 const
     d inKey                         10a   const
     d inMsgId                      100a   const options(*nopass)
     d relayJson       s          64512    inz(*blanks)
     d outMsgId        s            100a   inz(*blanks)
      /free
       If not(OpenFiles(inTypeOfMsg:inRrnOfFile));
          Return *off;
       Endif;
       If not(SmsActive(inTypeOfMsg));
          CloseFiles(inTypeOfMsg);
          Return *off;
       Endif;
       Select;
       When %parms > 3;
          RMMsgId = inMsgId;
       When not(HasMsgID(inTypeOfMsg:inKey));
          CloseFiles(inTypeOfMsg);
          Return *off;
       Endsl;
       BuildRelayJSON(inTypeOfMsg:relayJson);
       WriteToDtaQ(relayJson);
       WriteLogFile(inTypeOfMsg);
       CloseFiles(inTypeOfMsg);
       Return *on;
      /end-free
     pprocRelayReq     e
      *=============================================================*
     pSmsActive        b
     dSmsActive        pi              n
     d inTypeOfMsg                    2a   const
      /free
       If CONetId = *blank;
          Return *off;
       Endif;
       Select;
       When inTypeOfMsg  = 'CS';
          If CMOptOut = 'Y';
             Return *off;
          Endif;
          If F2DisSms = 'Y';
             Return *off;
          Endif;
          If CDDisSms = 'Y';
             Return *off;
          Endif;
          Return *on;
       When inTypeOfMsg  = 'PS';
       When inTypeOfMsg  = 'MC';
       When inTypeOfMsg  = 'PC';
       Endsl;
      /end-free
     pSmsActive        e
      *=============================================================*
     pWriteClmValues   b
     dWriteClmValues   pi
      /free
       dsNetWorkId  = '"' + %trim(CONetId) + '",';
       dsCltUsrId   = '"' + %trim(GetUserId(CMclex)) + '",';
       dsMsgId      = '"' + %trim(RMMsgId) + '",';
       dsMobile#    = '"' + %editc(CDcAra:'X')
        + %editc(CDcPhn:'X') + '",';
       dsPrimAcct1  = '"' + %editc(CMco#:'X') + %editc(CMfnd:'X') +
        %editc(CMfyr:'X') + %editc(CMcase:'X') + %editc(CMacdt:'X') +
        '#",';
       dsFirstName  = '"' + %trim(CDfsn)   + '",';
       dsLastName   = '"' + %trim(CDlsn)   + '",';
       dsTelephone# = '"' + %editc(CDcAra:'X')
        + %editc(CDcPhn:'X') + '",';
       If CDemad = *blank;
          dsEmail = 'Omit';
       Else;
          dsEmail = '"' + %trim(CDemad)  + '",';
       Endif;
       dsAddress1 = '"' + %trim(CDadr1)  + '",';
       If CDadr2 = *blank;
          dsAddress2 = 'Omit';
       Else;
          dsAddress2   = '"' + %trim(CDadr2)  + '",';
       Endif;
       If CDctry = *blanks or CDctry = 'USA';
          dsCity       = '"' + %trim(CDcty)   + '",';
          dsState      = '"' + %trim(CDst)    + '",';
          dsPostalCode = '"' + %editc(CDzip:'X')  + '",';
          dsCountry    = '"' + 'US'           + '",';
       Else;
          dsCity       = '"' + %trim(CDadr3)  + '",';
          dsState      = '"' + %trim(CDadr3)  + '",';
          dsPostalCode = '"' + %trim(CDadr3)  + '",';
          dsCountry    = '"' + %trim(CDctry)  + '",';
       Endif;
       dsDOB        = '"' + %trim(GetDob()) + '",';
       dsMiscField1 = '"' + %editc(CMfnd:'X')
        + %editc(CMcase:'X') + %editc(CMfyr:'X') + '",';
       dsMiscField2 = '"' + %trim(GetAcDt()) + '"';
      /end-free
     pWriteClmValues   e
      *=============================================================*
     pWriteLogFile     b
     dWriteLogFile     pi
     d inTypeOfMsg                    2a   const
      /Free
       Open RelayLog;
       Select;
       When inTypeOfMsg  = 'CS';
          RLclm#  = %editc(CMco#:'X') + %editc(CMfnd:'X') +
           %editc(CMcase:'X') + %editc(CMfyr:'X');
          RLname  = %trim(CDfsn) + ' ' + %trim(CDlsn);
          RLinjDt = CMacdt;
          RLmsgDt = %int(%char(%date():*CYMD0));
          RLdt    = RLmsgDt;
          RLtime  = %int(%char(%time():*HMS0));
          RLpgm   = Q1pgm;
          Write prl00;
       When inTypeOfMsg  = 'PS';
       When inTypeOfMsg  = 'MC';
       When inTypeOfMsg  = 'PC';
       Endsl;
       Close RelayLog;
      /End-free
     pWriteLogFile     e
      *=============================================================*
     pWriteToDtaQ      b
     dWriteToDtaQ      pi
     d relayJson                  64512
      /free
       LoadDataQ('DTQRELMSG':'*LIBL':64512:relayJson);
      /end-free
     pWriteToDtaQ      e
