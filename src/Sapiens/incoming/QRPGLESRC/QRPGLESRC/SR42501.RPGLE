       //============================================================
       Ctl-Opt Copyright('(c) 2023 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR42501- LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC420501
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:342
       // CREATED:    11/11/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
       //
       // 09/12/23 C:333 Support: 113867
       //                Voluntary Comp form showing waiver endorsement
       //                screen
       //                - Similar issue for Amendment to the Retro
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================

     d #1pgm           s             10A   inz('W1054R')
     d #1flag          s              1A   inz
     d xkform          s                   like(enform)

       Dcl-Pr SR42501 ;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR42501 ;
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
       Dcl-S qqForm Char(15) inz('WC420501');
       // Additional entry program
       //*** Beg Chg ***** 09/12/23 **********************************
       //**Dcl-C addPgm Const('W0815R');
       Dcl-C addPgm Const('SRXXXXXI');
       //*** End Chg ***** 09/12/23 **********************************
       //=============================================================
       // Main
       //=============================================================
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;

        //Write the output record

        //If printing a policy page.  (Not called from W1054R)
        // SRCHKPGM(#1pgm:#1flag);
        ChkPgmInStack(#1pgm:#1flag);
        If #1Flag = '0';
           PrintingPolicy = *On;
        Else;
           PrintingPolicy = *Off   ;
        Endif;

        If isScriptUsed = 'Y';
           RunningSRtext = *ON;
        Endif;

       Endsr;
       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
          UseDfltAddparms = *Off;
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

        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        //Check each division for a matching jurisdiction state
        Dow not %eof(wdell1);
           Chain rsky prs01;
           If %found and rsJrst = enSt and elPfdt <> elPtdt;
              crtFrm = *On;
           EndIf;
           ReadE (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        EndDo;

       Endsr;
