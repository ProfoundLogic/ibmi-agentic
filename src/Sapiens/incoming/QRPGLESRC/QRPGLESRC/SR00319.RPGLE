       //============================================================
       Ctl-Opt Copyright('(c) 2022 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00319 - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000319
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    10/26/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
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
       Dcl-Pr SR00319;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00319;
        qqCo# Char(3);
        qqFnd Char(3);
        qqEmp# Char(9);
        qqDiv Char(5);
        qqFyr Char(3);
        qqMode Char(1);
        qqRrn Packed(9:0);
        qqMod2 Char(1);
       End-Pi;
     d #1PGM           s             10A   INZ('W1054R')
     d #1FLAG          s              1A   INZ
     D* Check Pgm
     D SRCHKPGM        pr                  ExtPgm('SRCHKPGM')
     D                               10
     D                                1
       // Copybook D specs
     d/Copy sCopyEn01d
       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000319  ');
       // Additional entry program
       Dcl-C addPgm Const('W0815R');
       //=============================================================
       // Main
       //=============================================================
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
      *
       //WRITE THE OUTPUT RECORD
      *
      *  If printing a policy page.  (Not called from W1054R)
        SRCHKPGM(#1PGM:#1FLAG);
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
        crtFrm = *On;
       Endsr;
