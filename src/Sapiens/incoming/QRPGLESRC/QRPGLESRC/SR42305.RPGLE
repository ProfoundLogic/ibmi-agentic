       //============================================================
       Ctl-Opt Copyright('(c) 2023 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR42305 - TX - Experience Rating Modification Factor Endorsement
       //============================================================
       // Program Description:
       //
       //   Determines if this endorsement form should be attached to
       //   the policy and creates data in the Endorsement Form
       //   Workfile (WXWXFMP) prior to a form merge.
       //
       // PROGRAMMER: C:333
       // CREATED:    11/09/22            PROJECT: WC2834
       //=============================================================
       // Revisions:
       //
       // 09/12/23 C:333 Support: 113867
       //                Voluntary Comp form showing waiver endorsement
       //                screen
       //                - Similar issue for Exempt Employees Coverage
       //
       //=============================================================
       // NOTES:
       //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
       //  qqmode = 1 - Called to determine if form should be attached
       //  qqmode = 2 - Called to print endorsement.
       //  qqmode = 3 - Called for additional input.
       //  qqmode = 4 - Called for midterm processing.
       //=============================================================
     fWXFMP1    IF   E           K DISK    USROPN
     f                                     RENAME(PFM01:PFM02)

       // Copybook F specs
     f/Copy sCopyEn01f
       //=============================================================
       // Definition Specs
       //=============================================================
     d XXFRM1          s             15A

       Dcl-Pr SR42305;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR42305;
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
       Dcl-C qqForm Const('WC420305A ');
       // Additional entry program
       //*** Beg Chg ***** 09/12/23 **********************************
       //**Dcl-C addPgm Const('W0815R');
       Dcl-C addPgm Const('SR42305I');
       //*** End Chg ***** 09/12/23 **********************************
       //=============================================================
       // Main
       //=============================================================
     C*
     C     FMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
     C*
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;
     C*
     C*  Write the output record
     C*
     C                   OPEN      WXFMP1
     C     FMKY3         CHAIN     PFM02                              60
     C*
     C*  Move the custom fields to print
     C*
     C                   MOVEL     F3NM01        FMNM01                         NAME 1
     C                   MOVEL     F3NM02        FMNM02                         NAME 2
     C                   MOVEL     F3NM03        FMNM03                         NAME 3
     C                   MOVEL     F3NM04        FMNM04                         NAME 4
     C                   MOVEL     F3NM05        FMNM05                         NAME 5
     C                   MOVEL     F3NM06        FMNM06                         NAME 6
     C                   MOVEL     F3NM07        FMNM07                         NAME 7
     C                   MOVEL     F3NM08        FMNM08                         NAME 8
     C                   MOVEL     F3NM09        FMNM09                         NAME 9
     C                   MOVEL     F3NM10        FMNM10                         NAME 10
     C                   MOVEL     F3NM11        FMNM11                         NAME 11
     C                   MOVEL     F3NM12        FMNM12                         NAME 12
     C*
     C                   CLOSE     WXFMP1
     C*
       Endsr;
       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
       //*** Beg Chg ***** 09/12/23 *********************************
          //***UseDfltAddparms = *Off;
          UseDfltAddparms = *On;
       //*** End Chg ***** 09/12/23 *********************************
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
        Chain (xxco#:elrtcd) prs01;
        If %found and rsjrst = enst and elPFDT <> elPTDT;
           crtfrm = *on;
        Endif;
       Endsr;
