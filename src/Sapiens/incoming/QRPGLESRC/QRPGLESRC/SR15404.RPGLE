       //============================================================
       Ctl-Opt Copyright('(c) 2022 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR15404 -LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC150404
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    11/02/22            PROJECT: WC2834
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
     fWMNXL     IF   E           K DISK

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
     d #1PGM           s             10A   INZ('W1054R')
     d #1FLAG          s              1A   INZ
     D* Check Pgm
     D SRCHKPGM        pr                  ExtPgm('SRCHKPGM')
     D                               10
     D                                1

       Dcl-Pr SR15404;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR15404;
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
       Dcl-C qqForm Const('WC150404');
       // Additional entry program
       Dcl-C addPgm Const('W0815R');
       //=============================================================
       // Main
       //=============================================================
     C*
     C     NXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMST
     C                   KFLD                    ELPFDT
     C*
     C     NXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMST

       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;

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

        Chain RSKY prs01;
        If %found and rsJrst = enSt and elPfdt <> elPtdt;
           SetGT NXKY pnx01;
           ReadPE NXKY1 pnx01;
           Dow not %eof and nxStat = 'I';
              ReadPE NXKY1 pnx01;
           EndDo;

       //A VALID RECORD WAS FOUND, THERE IS A PENDING RATE CHG, THE
       //PENDING RATE CHG DATE IS NOT ZERO, AND IS LESS THAN/EQUAL TO
       //THE POLICY FROM DATE.

           If not %eof(wmnxl) and nxPrcf = 'Y' and nxPrcd <> *Zero
            and nxPrcd <= elPfdt;

       //A RATE CHG DATE HAS NOT BEEN ENTERED, OR IS GREATER THAN
       //THE POLICY FROM DATE, WRITE THE ENDORSEMENT FORM RECORD.
              crtFrm = *On;
           EndIf;
        EndIf;

       Endsr;
