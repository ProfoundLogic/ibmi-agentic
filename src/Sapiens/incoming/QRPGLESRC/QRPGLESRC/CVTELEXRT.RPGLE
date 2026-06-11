     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H**********************************************************************
     H* Created: 03/04/24 C:079 Project: WC2951
     H**********************************************************************
     fWdell1    uf   e             Disk    UsrOpn
     fWdellb    if   e           k Disk    Rename(pel01:pel02) Prefix(E2:2)
     f                                     UsrOpn
     fWmeml     if   e           k Disk    UsrOpn
     fWmrsl     if   e           k Disk
     fWtgtl     if   e           k Disk
       //==============================================================
       // Stand-alone fields top
       //==============================================================
       Dcl-s svExRt Like(ELexRt);
       Dcl-s svNcci Like(ELncci);
       Dcl-s intStRptDiv Like(EMdiv);
       Dcl-s prvCo# Like(ELco#) Inz;
       Dcl-s prvFnd Like(ELfnd) Inz;
       Dcl-s prvFyr Like(ELfyr) Inz;
       Dcl-s prvEmp# Like(ELemp#) Inz;
       Dcl-s strQteLib Char(10);
       Dcl-s strEnv Char(4) Inz('*QTE');
       Dcl-s strCmd Char(255);
       //==============================================================
       // Stand-alone fields bottom
       //==============================================================
       // Data Structures top
       //==============================================================
     D PSDS          ESDS                  extname(SZQ1P)
       //==============================================================
       // Data Structures bottom
       //==============================================================
       // /Copy
       //==============================================================
       /copy sprDate
       /copy wprEmplr
       /copy sPrRunCmd
       //==============================================================
       // Mainline program
       //==============================================================
       // Live
       Exsr OpenFiles;
       Exsr UpdateExpRate;
       Exsr CloseFiles;

       // Quote
       Exsr OvrFiles;
       Exsr OpenFiles;
       Exsr UpdateExpRate;
       Exsr CloseFiles;

       Exsr ProgramEnd;
       //===============================================================
       // Subroutine UpdateExpRate
       //===============================================================
       BegSr UpdateExpRate;

       Read pel01;
       Dow Not %Eof(WDELL1);

          Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
          If %Found(WMEML);
             svExRt = EMexRt;
          Else;
             svExRt = ' ';
          Endif;

          // Only check the NCCI flag when co/fnd/fyr/emp# changes
          If prvCo# <> ELco# or prvFnd <> ELfnd or prvFyr <> ELfyr
           or prvEmp# <> ELemp#;

             svNcci = 'Y';
             Setll (ELco#:ELfnd:ELfyr:ELemp#) pel02;
             Reade (ELco#:ELfnd:ELfyr:ELemp#) pel02;
             Dow Not %Eof(WDELLB);
                Chain (E2co#:E2rtCd) prs01;
                If %Found(WMRSL);
                   Chain (RSco#:'NCCI':RSjrSt) pgt01;
                   If %Found(WTGTL);
                      svNcci = ' ';
                      Leave;
                   Endif;
                Endif;
                Reade (ELco#:ELfnd:ELfyr:ELemp#) pel02;
             Enddo;

             // If there are no NCCI states on the policy year check the
             // State reporting division from WMEMP
             If svNcci = 'Y';
                intStRptDiv = GetStRptDv(ELco#:Elfnd:Elemp#);
                Chain (ELco#:ELfnd:ELemp#:intStRptDiv) pem01;
                If EMexRt = '1' or EMexRt = '2';
                   svNcci = 'Y';
                Else;
                   svNcci = ' ';
                Endif;
             Endif;
          Endif;

          ELexRt = svExRt;
          ELncci = svNcci;
          ELuser = Q1user;
          ELpgm  = Q1pgm;
          ELdt   = DateTo7(SystemDate);
          ELtime = Timeto6(SystemTime);

          Update pel01;

          prvCo#  = ELco#;
          prvFnd  = ELfnd;
          prvFyr  = ELfyr;
          prvEmp# = ELemp#;

          Read pel01;

       Enddo;

       Endsr;
       //===================================================================
       // Subroutine OpenFiles
       //===================================================================
       Begsr OpenFiles;

       If Not %Open(WMEML);
          Open WMEML;
       Endif;
       If Not %Open(WDELL1);
          Open WDELL1;
       Endif;
       If Not %Open(WDELLB);
          Open WDELLB;
       Endif;

       Endsr;
       //===================================================================
       // Subroutine CloseFiles;
       //===================================================================
       Begsr CloseFiles;

       If %Open(WMEML);
          Close WMEML;
       Endif;
       If %Open(WDELL1);
          Close WDELL1;
       Endif;
       If %Open(WDELLB);
          Close WDELLB;
       Endif;

       Endsr;
       //===================================================================
       // Subroutine OvrFiles - Override file to Quote Library
       //===================================================================
       Begsr OvrFiles;

       strCmd = 'OVRDBF FILE(WMEML) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WMEML)';
       Monitor;
       RunCmd(strCmd:%len(strCmd));
       On-Error;
          Exsr ProgramEnd;
       Endmon;

       strCmd = 'OVRDBF FILE(WDELL1) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WDELL1)';
       Monitor;
       RunCmd(strCmd:%len(strCmd));
       On-Error;
          Exsr ProgramEnd;
       Endmon;

       strCmd = 'OVRDBF FILE(WDELLB) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WDELLB)';
       Monitor;
       RunCmd(strCmd:%len(strCmd));
       On-Error;
          Exsr ProgramEnd;
       Endmon;

       Endsr;
       //===================================================================
       // Subroutine ProgramEnd
       //===================================================================
       Begsr ProgramEnd;

       *inlr = *on;
       Return;

       Endsr;
