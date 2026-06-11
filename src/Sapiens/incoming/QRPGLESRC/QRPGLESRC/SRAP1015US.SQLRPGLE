       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SRAP1015US - Cancellation/Non-Renewal Notice to Third Party
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // Created: 05/18/21
       // Project: 140WBW
       // Programmer: Consultant 136
       //=============================================================
       // Revisions:
       //
       // 08/21/25 C:333 Support: 148/117703  (Recompile)
       //                For single print endorsements provide specific
       //                group and employer so that correct sig is found
       //
       // 08/29/24 C: 342 Support: 117201
       //                 Write Wxwxfmp file.
       //                 skipping call for GetXMLFileNm as it was writing
       //                 an AP1015US record in XMLTEML1 before validating
       //                 3rd Party in OthInf.
       //
       // 10/15/22 C: 333 Project: WC2834
       //                 Endorsement Recognition of Cancellation
       //
       // 09/30/21 C: 079 Project: 140WBW
       //                 Use multiple record subroutines
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
       Dcl-F WMCEL Keyed Rename(pce01:pce00);
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SRAP1015US;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SRAP1015US;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
       Dcl-S xxState Char(2);
     d/Include sCopyEn01d
      //***** Beg Del *** 10/15/22 ******************************
     d*****/Include wPrEmplr
      //***** End Del *** 10/15/22 ******************************
     d/Include wPrEndorse
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('AP1015US');
       // Additional entry program
       Dcl-C addPgm Const('SRxxxxxxI');
       //=============================================================
       // Main
       //=============================================================
     c/Include sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
        //*** Beg Chg ***** 08/29/24 ************************
        //*** Exsr OpenWorkFile;
        If processType = 'Mid-Term Endorsement';
           PrintingPolicy = *on;
        Endif;
        Open WXFMPMULTI;
        //*** End Chg ***** 08/29/24 ************************
        Setll (xxco#:xxfnd:xxfyr:xxemp#) pel02;
        Dou %Eof(WDELL1);
           Reade (xxco#:xxfnd:xxfyr:xxemp#) pel02;
           If %Eof;
              Leave;
           EndIf;

           //***** Beg Add *** 10/15/22 ******************************
           xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
           If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
              Iter;
           Endif;
           //***** End Add *** 10/15/22 ******************************

           xxState = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
           If Not(isIncludedState(ELco#:qqForm:xxState:ELpfdt));
              Iter;
           Endif;
           Setll (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pce00;
           Dou %Eof(WMCEL);
              Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pce00;
              If %Eof;
                 Leave;
              EndIf;
              If CEstat = 'I';
                 Iter;
              Endif;
              If CE3Pty = 'Y';
                 FMnm01 = CEname;
                 FMc401 = CEadr1;
                 FMc402 = CEadr2;
                 FMc403 = %Trim(CEcty) + ', ' + %Trim(CEst) + ' ' +
                  %Subst(%EditC(CEzip:'X'):1:5);
                 If %Subst(%EditC(CEzip:'X'):6:4) <> '0000';
                    FMc403 = %Trim(FMc403) + '-' +
                     %Subst(%Editc(CEZip:'X'):6:4);
                 Endif;
                 FMc101 = CEctry;
                 FMc405 = CEprov;
                 FMc601 = CEattn;
              Else;
                 Iter;
              Endif;
              //*** Beg Add ***** 08/29/24 ************************
              Exsr WriteWorkFile;
              //*** End Add ***** 08/29/24 ************************
              Exsr WriteMultiRcd;
           Enddo;
        Enddo;
        Exsr CloseWorkFile;
       Endsr;
       //============================================================
       //  Pgnstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
        writeInCopyBook = *Off;
       Endsr;
       //============================================================
       //  PgmEnd - End of program processing
       //============================================================
       Begsr PgmEnd;
       Endsr;
       //============================================================
       //  Valid - Subroutine to attach this form to the policy
       //============================================================
       Begsr Valid;
        Setll (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pce00;
        Dou %Eof(WMCEL);
           Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pce00;
           If %Eof;
              Leave;
           EndIf;
           If CEstat = 'I';
              Iter;
           Endif;
           If CE3Pty = 'Y';
              crtFrm = *On;
              Leave;
           Endif;
        Enddo;
       Endsr;
