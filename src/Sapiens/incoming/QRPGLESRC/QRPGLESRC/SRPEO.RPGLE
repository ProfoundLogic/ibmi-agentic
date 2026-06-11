       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        Text(*srcmbrtxt)
        BndDir('WTBNDDIR');
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // Created: 05/27/21
       // Project: wc2795
       //=============================================================
       // Revisions:
       //
       //  08/21/25 C:333 Support: 148/117703  (Recompile)
       //                 For single print endorsements provide specific
       //                 group and employer so that correct sig is found
       //                 and set the Scriptura Type = 'PEO Endorsement'
       //
       //  11/04/24 C:342 Support : 117820
       //                 Align ChkPEO with OthInf SR for WC120308
       //                 i.e. Adding FNpeoF condition in Valid SR
       //                 This need some java changes
       //
       //  10/31/22 C:333 Project: WC2834 (Recompile)
       //                 Endorsement Recognition of Cancellation
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
       //=============================================================
       // Main
       //=============================================================
       Dcl-Pi *N;
        qqCo#  Char(3);
        qqFnd  Char(3);
        qqEmp# Char(9);
        qqDiv  Char(5);
        qqFyr  Char(3);
        qqMode Char(1);
        qqForm char(15);
       End-Pi;
     f/Copy sCopyEn01f
       Dcl-C addPgm Const('SRxxxxxxI');                      // Additional entry program
     d/Copy wPrPeo
     d/Copy sCopyEn01d
       Dcl-s qqMod2 char(1) inz;
       Dcl-s qqRrn packed(9:0) inz;
     c/Copy sCopyEn01C                                                          // Copybook C specs
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
        If FNPEOF = *Blank;
           LeaveSr;
        Endif;
        attach = 'Y';
        Exsr ChkPEO;
        If attach = 'N';
           LeaveSr;
        Endif;
        dsPEOInfo = GetPEOInfo(xxCo#:xxFnd);
        If dsPeoInfo.Name  <> *blanks;
           FMnm01 = dsPeoInfo.Name;
        Else;
           FMnm01 = dsPeoInfo.FedName;
        Endif;
        FMc401 = dsPeoInfo.Address1;
        FMc402 = dsPeoInfo.Address2;
        FMc403 = dsPeoInfo.Address3;
       Endsr;
       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
       //*** Beg Add *** 08/21/25 ******************************************
       ScripturaType = 'PEO Endorsement';
       //*** End Add *** 08/21/25 ******************************************
       Endsr;
       //============================================================
       //  PgmEnd - End of program processing
       //============================================================
       Begsr PgmEnd;
        If printingPolicy;
           Chain inXML XMLTEML1;
           If %Found(XMLTEML1);
              XTtype = 'PEO Endorsement';
              Update XT100;
           Endif;
        Else;
           Chain inXML XMLPREL1;
           If %Found(XMLPREL1);
              XPtype = 'PEO Endorsement';
              Update XP100;
           Endif;
        Endif;
       Endsr;
       //============================================================
       //  Valid - Subroutine to attach this form to the policy
       //============================================================
       Begsr Valid;
        //*** Beg Chg ***** 11/04/24 *****************************
        //***crtFrm = *On;
        If FNpeoF <> *blanks;
          CrtFrm = *On;
        Endif;
        //*** End Chg ***** 11/04/24 *****************************
       Endsr;
