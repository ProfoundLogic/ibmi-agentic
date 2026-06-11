       //============================================================
       Ctl-Opt Copyright('(c) 2025 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR31405A - NY Safe Patient Handling Act Pgm
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // Created: 07/28/21
       // Project: WC2801
       //=============================================================
       // Revisions:
       //
       //  08/21/25 C:333 Support: 148/117703  (Recompile)
       //                 For single print endorsements provide specific
       //                 group and employer so that correct sig is found
       //
       //  10/19/22 C:342 Project: WC2834 (Recompile)
       //                 Endorsement Recognition of Cancellation
       //
       //  99/99/99 C:999 Project: xxxxx
       //                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
       Dcl-F WDRRL Disk Usage(*input) Keyed;
       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
       Dcl-Pr SR31405A;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR31405A;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
       // Copybook D specs
     d/Copy sCopyEn01d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-S qqForm Char(15) Inz('WC310405A');
       // Additional entry program
       Dcl-C addPgm Const('SRxxxxxxI');
       //=============================================================
       // Main
       //=============================================================
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
       Endsr;
       //============================================================
       //  Pgnstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;

          runningSRText = *On;

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

          Setgt (ELco#:ELfnd:RSjrSt:ELpfDt) prr01;
          Readpe (ELco#:ELfnd:RSjrSt) prr01;
          If not %Eof(WDRRL);
             Select;
             When RRdv1C = 9651 and ELedc2 > 0; // Deviation 1
                crtFrm = *On;
             When RRdv2C = 9651 and ELedv2 > 0; // Deviation 2
                crtFrm = *On;
             Other;
                crtFrm = *Off;
             Endsl;
          Endif;

       Endsr;
