       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SRMT252 - MT 25-2 Processing/Print Program
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy.  Note that data has already been generated for
       //   file WMMUP2.
       //
       // Created: 07/13/22
       // Project: WC2872
       // Programmer: Consultant 079
       //=============================================================
       // Revisions:
       //
       // 08/21/25 C:333 Support: 148/117703  (Recompile)
       //                For single print endorsements provide specific
       //                group and employer so that correct sig is found
       //
       // 03/09/23 C:333 Project: WC2834 (Recompile)
       //                Endorsement Recognition of Cancellation
       //
       // 02/28/23 C:079 Support: 146-107390
       //                Correct Form Name
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fwmmup2L1  uf a e           k disk    rename(pmu01:pmup2) prefix(m2:2)
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SRMT252;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SRMT252;
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
     d*** Beg Del *** 02/28/23 ***************************************
     d*****/Include wPrEmplr
     d*** End Del *** 02/28/23 ***************************************
     d/Include wPrEndorse
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       //*** Beg Chg *** 02/28/23 ************************************
       //***Dcl-C qqForm Const('MT 25-2   ');
       Dcl-C qqForm Const('FORM 25-2 ');
       //*** End Chg *** 02/28/23 ************************************
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
       Endsr;
       //============================================================
       //  Pgnstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
       Endsr;
       //============================================================
       //  PgmEnd - End of program processing
       //============================================================
       Begsr PgmEnd;
        Setll (MUco#:MUfnd:MUfyr:MUemp#) pmup2;
        Reade (MUco#:MUfnd:MUfyr:MUemp#) pmup2;
        Dow Not %Eof(WMMUP2L1);
           M2xml = XPxmlT;
           M2user = Q1user;
           M2pgm  = Q1pgm;
           M2dt   = sysdat;
           M2time = dsTime;
           Update pmup2;
           Reade (MUco#:MUfnd:MUfyr:MUemp#) pmup2;
        Enddo;


       Endsr;
       //============================================================
       //  Valid - Subroutine to attach this form to the policy
       //============================================================
       Begsr Valid;
          crtFrm = *On;
       Endsr;
