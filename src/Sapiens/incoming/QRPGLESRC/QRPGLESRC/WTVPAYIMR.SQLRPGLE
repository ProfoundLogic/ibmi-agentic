     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h/TITLE Vpay 1099 Payment Update
     hoption(*nodebugio:*srcstmt)
     hDftactgrp(*no) Actgrp(*CALLER)
     h bnddir('WTBNDDIR')
      //================================================================
      //  DESCRIPTION:  This program reads WAVPP, edits the records, and
      //                update WDPAP/PAvTvp with "CLVCP" so that when
      //                when 1099's run in GO, those payments will not
      //                be included in the 1099 reporting.
      //
      //  Called from:  WTVPAYIMC1 - Override
      //  Calls      :  None
      //  Created on 12/16/2021 by C:291 for Project WC2825
      //================================================================
      // Revisions:
      //
      //
      //================================================================
      /Free
       //Dcl-F wdpap Usage(*Update) Rename(pel01:pel0p);
       Dcl-F wavpp Usage(*Input) Rename(pvp01:pvp0p) Infds(vpfds);
       Dcl-F wtvpayimo Printer OFlind(InOf) UsrOpn;
       // *Entry
       Dcl-Pr VPayImport ExtPgm('WTVPAYIMR');
          inFolder Char(256);
          inReturn Char(10);
          inFileNm Char(100);
          outReturn Char(10);
       End-Pr;
       Dcl-Pi VPayImport;
          inFolder Char(256);
          inReturn Char(10);
          inFileNm Char(100);
          outReturn Char(10);
       End-Pi;
       Dcl-Pr RTVCLTID  EXTPGM;
          strCltId char(3);
       End-Pr;
       //Stand alone fields
       Dcl-s adjStatus Char(1) Inz(' ');
       Dcl-s dtTranDate7 Packed(7:0);
       Dcl-S decChkAmt Zoned(15:2);
       Dcl-S hasFileError Ind Inz(*Off);
       Dcl-S intCount Zoned(9:0);
       Dcl-S intCo# Zoned(3:0);
       Dcl-S intGroup packed(3:0);
       Dcl-S intFein packed(9:0);
       Dcl-S intCheck# packed(7:0);
       Dcl-S intCase packed(7:0);
       Dcl-S intYear packed(3:0);
       Dcl-S intEob# packed(9:0);
       Dcl-S intSeq# packed(5:0);
       Dcl-S intMod  packed(3:0);
       Dcl-S isFirstError Ind Inz(*On);
       Dcl-s strRowCnt char(10) Inz(' ');
       Dcl-S strCltId char(3);
       Dcl-s strErrMsg char(150);
       Dcl-s strRowNbr Char(10);
       Dcl-S sysDate date;
       Dcl-S sysDate7 packed(7:0);
       Dcl-S sysTime6 packed(6:0);
       Dcl-S sysTimeStamp Timestamp Inz(*Loval);
       Dcl-s decNumber zoned(15:2);

      // Constants
       Dcl-c digits     '0123456789';
       Dcl-c validChars 'ABCDEFGHIJKLMNOPQRSTUVWXYZ+
                        abcdefghijklmnopqrstuvwxyz+
                        0123456789 ()<>!;%$#*?@+&^=-":/''.,~';

      // Data structures
       //File Information Data Structure
       Dcl-Ds vpfds;
        rrnVPP int(10) pos(397);
       End-Ds;
       Dcl-Ds VPPDtaDs ExtName('WAVPP') Inz;
       End-Ds;

       Dcl-Ds SZQ1P ExtName('SZQ1P') Psds;
       End-Ds;

       Dcl-Ds dsClaimId Qualified Inz;
        group  Char(3);
        year   Char(3);
        case   Char(7);
       End-Ds;

       Dcl-Ds dsPmtId146 Qualified Inz;
        tranDate Char(8);
        tranChk# Char(7);
       End-Ds;

       Dcl-Ds dsPmtIdNEob Qualified Inz;
        group  Char(3);
        year   Char(3);
        case   Char(7);
        chk#   Char(7);
        seq#   Char(5);
       End-Ds;

       Dcl-Ds dsPmtIdEob Qualified Inz;
        eob#   Char(9);
        recon# Char(3);
        chk#   Char(7);
        seq#   Char(5);
       End-Ds;

       Dcl-Ds dsPmtIdOth Qualified Inz;
        group  Char(3);
        year   Char(3);
        case   Char(7);
        chk#   Char(7);
       End-Ds;

      // Prototypes
      /Copy SprSql
      /copy sprdate
      /copy sprcompany
      /copy sprstring
      /copy wprusrprms
      /copy sprgenerr
      /copy wprtable
      //===============================================================
      // Main Processing
      //===============================================================
       Exec Sql Set Option Commit = *None;
       ProgramStart();
       If inReturn = 'FILE LOCK';
          outReturn = 'ERROR';
          PrintFileError();
       Else;
          BadDataChk();
          If hasFileError = *On;
             outReturn = 'ERROR';
          Else;
             ProcessWrkFile();
             outReturn = 'SUCCESS';
          Endif;
       Endif;

       If %Open(WTVPAYIMO);
          Close WTVPAYIMO;     //Close printer file
       Endif;

       *Inlr = *On;
       //----------------------------------------------------------------
       // BadDataChk - Invalid data check in all the incoming fields
       //----------------------------------------------------------------
       Dcl-Proc BadDataChk;
        Dcl-Pi *N Ind;
        End-Pi;
        // Check for empty file WAVPP
        hasFileError = *Off;
        Exec SQL
        Select Count(*)
        Into :intCount
        From wavpp;
        If intCount = 0;
           PrintError(' ':strRowCnt:'File is Empty');
           hasFileError = *On;
        Endif;
        //Check for bad data
        If hasFileError = *Off;
           Setll 1 pvp0p;
           Read pvp0p;
           Dow Not %Eof(WAVPP);
              If VPPDtaDs <> *Blank;
                 If VPPAYFEIN = ' ';
                    VPPAYFEIN = '000000000';
                 EndIf;
                 //Check numeric fields in WAVPP
                 If VPCLMID <> 'MULTIPLE CLAIMS';
                    InvalidNumber('Claim Id':VPCLMID:rrnVPP);
                 Endif;
                 InvalidNumber('Payment Id':VPPMTID:rrnVPP);
                 If strCltId = '146';
                    InvalidNumber('Group Number':VPGNUM:rrnVPP);
                 EndIf;
                 InvalidNumber('Tax Id':VPPAYFEIN:rrnVPP);
                 //InvalidNumber('Client Reference':VPCLTREFID:rrnVPP);
              Endif;
              Read pvp0p;
           Enddo;
        Endif;
       Return hasFileError;
       End-Proc;

      //----------------------------------------------------------------
      // InvalidNumber - look for invalid data in numeric field
      //----------------------------------------------------------------
       Dcl-Proc InvalidNumber;
       Dcl-Pi *N;
        inFldName char(25) const;
        inValueToChk char(30) const;
        inRowNbr int(10) const;
       End-Pi;
       Dcl-s intNumber zoned(40:0);

       strErrMsg = *Blank;
       Monitor;
          intNumber = %Dec(%Trim(inValueToChk):40:0);
       On-Error;
          strErrMsg = 'Numeric field has invalid data';
       EndMon;
       If strErrMsg <> *Blank;
          strRowNbr = %Char(inRowNbr);
          PrintError(inFldName:strRowNbr:strErrMsg);
       Endif;

       End-Proc;
       //----------------------------------------------------------------
       // PrintError - Print Erorr Report
       //----------------------------------------------------------------
       Dcl-Proc PrintError;
        Dcl-Pi *N;
         inFldNm Char(25) const;
         inRowNbr Char(10) const;
         inMessage Char(150) const;
        End-Pi;
        If isFirstError = *On;
           isFirstError = *Off;
           Open WTVPAYIMO;
           prTitle = 'VPay 1099 Payment Update Error Report';
           prInPath = %Trim(inFolder);
           Write RptHeader;
           Write ErrorHdr;
        Endif;
        If InOf = *On;
           Write RptHeader;
           Write ErrorHdr;
           InOf = *Off;
        Endif;
        prFldNm  = inFldNm;
        prRcdNum = inRowNbr;
        prErrMsg = inMessage;
        Write ErrorDtl;
        hasFileError = *On;
       End-Proc;
       //----------------------------------------------------------------
       // PrintFileError - Print File Error on the report
       //----------------------------------------------------------------
       Dcl-Proc PrintFileError;
        Open WTVPAYIMO;
        prTitle = 'VPay 1099 Payment Update Error Report';
        prInPath = %Trim(inFolder);
        Write RptHeader;
        Write ErrorHdr1;
           prErrMsg1 = 'File ' + %trim(inFileNm) +
            ' is locked or unavailable.' ;
           Write ErrorDtl1;
        Close WTVPAYIMO;     //Close printer file
       End-Proc;
      //---------------------------------------------------------------------
      // ProcessWrkFile - Process work file
      //---------------------------------------------------------------------
       Dcl-Proc ProcessWrkFile;
       Setll 1 pvp0p;
       Read pvp0p;
       Dow Not %Eof(WAVPP);
          //Client 146 processing
          Select;
          When strCltId = '146';
             Exsr Client146;
          Other;
             Exsr OtherClients;
          Endsl;
          Read pvp0p;
       Enddo;

       //===================================================================
       //Client 146 - Process for client 146
       //===================================================================
       Begsr Client146;
        Select;
        When VPCLMID = 'MULTIPLE CLAIMS';
        //Group Checks
           dsPmtId146 = VPPMTID;
           dtTranDate7 = DtCYMD(%int(dsPmtId146.tranDate));
           intGroup = %Int(VPGNUM);

           If VPPAYFEIN = ' ';
              VPPAYFEIN = '000000000';
           EndIf;
           intFein = %Int(VPPAYFEIN);
           intCheck# = %Int(dsPmtId146.tranChk#);
           Exec sql
           Update WDPAP
            Set PAvTyp = 'CLVCP',
             PApgm=:Q1pgm, PAuser=:Q1user, PAtime=:sysTime6, PAdt=:sysDate7
            Where PAco# = :intCo# and PAfnd = :intGroup and
             PAfein = :intFein and PAchk# = :intCheck# and
             PApgtp = 'G';
        Other;
        //Daily Checks
           dsPmtId146 = VPPMTID;
           dtTranDate7 = DtCYMD(%Int(dsPmtId146.tranDate));

           Select;
           When %Len(%trim(VPCLMID)) = 11;
              VPCLMID = '00' + %Trim(VPCLMID);
           When %Len(%trim(VPCLMID)) = 12;
              VPCLMID = '0' + %Trim(VPCLMID);
           EndSl;

           dsClaimId = VPCLMID;
           intGroup  = %Int(dsClaimid.group);
           intYear   = %Int(dsClaimid.year);
           intCase   = %int(dsClaimid.case);
           intCheck# = %Int(dsPmtId146.tranChk#);
           decChkAmt = %Dec(VPORGAMT:15:2);
           Exec sql
           Update WDPAP
            Set PAvTyp = 'CLVCP',
             PApgm=:Q1pgm, PAuser=:Q1user, PAtime=:sysTime6, PAdt=:sysDate7
            Where PAco# = :intCo# and PAfnd = :intGroup
             and PAfyr = :intYear and PAcase = :intCase
             and PAchk# = :intCheck# and PApgtp = ' ';
        Endsl;

       Endsr;

       //===================================================================
       //All other clients - Process for other clients
       //===================================================================
       Begsr OtherClients;

        Select;
        When %Len(%trim(VPpmtID)) = 23 Or %Len(%trim(VPpmtID)) = 18;
           VPpmtID = '00' + %Trim(VPpmtID);
        When %Len(%trim(VPCLMID)) = 24 Or %Len(%trim(VPpmtID)) = 19;
           VPpmtID = '0' + %Trim(VPpmtID);
        EndSl;

        Select;
        //Group Checks
        When %Len(%Trim(VPpmtID)) = 20;

           Select;
           When %Len(%trim(VPpmtID)) = 18;
              VPpmtID = '00' + %Trim(VPpmtID);
           When %Len(%trim(VPCLMID)) = 19;
              VPpmtID = '0' + %Trim(VPpmtID);
           EndSl;

           dsPmtIdOth = VPpmtID;
           intGroup  = %Int(dsPmtIdOth.group);
           intYear   = %Int(dsPmtIdOth.year);
           intCase   = %int(dsPmtIdOth.case);
           intCheck# = %Int(dsPmtIdOth.chk#);
           intFein   = 0;
           intMod    = 0;
           Exec sql
           Select PAfein, PAmod
            Into :intFein, :intMod
            From WDPAP
            Where PAco# = :intCo# and PAfnd = :intGroup
             and PAfyr = :intYear and PAcase = :intCase
             and PAchk# = :intCheck#
             and PApgtp = 'G' and PAeftc = 'V';
           Exec sql
           Update WDPAP
            Set PAvTyp = 'CLVCP',
             PApgm=:Q1pgm, PAuser=:Q1user, PAtime=:sysTime6, PAdt=:sysDate7
            Where PAco# = :intCo# and PAfnd = :intGroup
             and PAfein = :intFein and PAMod = :intMod
             and PAchk# = :intCheck#
             and PApgtp = 'G';

        Other;
        //Daily Checks
           Select;
           //Non-Eob
           When %len(%trim(VPpmtid)) = 25;  //Non-Eob

              dsPmtIdNEob = VPPMTID;
              intGroup  = %Int(dsPmtIdNEob.group);
              intYear   = %Int(dsPmtIdNEob.year);
              intCase   = %int(dsPmtIdNEob.case);
              intCheck# = %Int(dsPmtIdNEob.chk#);
              intSeq#   = %Int(dsPmtIdNEob.seq#);

              Exec sql
              Update WDPAP
               Set PAvTyp = 'CLVCP',
                PApgm=:Q1pgm, PAuser=:Q1user, PAtime=:sysTime6, PAdt=:sysDate7
               Where PAco# = :intCo# and PAfnd = :intGroup
                and PAfyr = :intYear and PAcase = :intCase
                and PAchk# = :intCheck#
                and PApgtp = ' ' and PAeob# = 0;
           //Eob
           When %Len(%trim(VPpmtid)) = 24;  //Eob
              dsPmtIdEob = VPPMTID;
              intEob#   = %Int(dsPmtIdEob.eob#);
              intCheck# = %Int(dsPmtIdEob.chk#);
              intSeq#   = %Int(dsPmtIdEob.seq#);
              //Get Claim number via Eob#
              Exec sql
              Select EOcase, EOfnd, EOfyr
              Into   :intCase, :intGroup, :intYear
               From FAEOP
               Where EOco# = :intCo# and EOeob# = :intEob#;

              Exec sql
              Update WDPAP
               Set PAvTyp = 'CLVCP',
                PApgm=:Q1pgm, PAuser=:Q1user, PAtime=:sysTime6, PAdt=:sysDate7
               Where PAco# = :intCo# and PAfnd = :intGroup
               and PAfyr  = :intYear and PAcase = :intCase
               and PAchk# = :intCheck#
               and PApgtp = ' ' and PAeob# <> 0;
           Endsl;
        Endsl;
       Endsr;
       End-Proc;
      //---------------------------------------------------------------
      // ProgramStart - Initialization
      //---------------------------------------------------------------
       Dcl-Proc ProgramStart;
        intCo# = userCo#(q1user);
        //Retrieve Client-ID
        RtvCltID(strCltId);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
        sysTimeStamp = %timestamp();
       End-Proc;
      /End-Free
