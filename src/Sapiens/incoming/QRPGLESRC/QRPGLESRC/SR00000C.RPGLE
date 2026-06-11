       //============================================================
       Ctl-Opt Copyright('(c) 2022 Sapiens. All Rights Reserved.')
        Option(*NoDebugIO:*SrcStmt)
        DftActGrp(*No)
        ActGrp(*Caller)
        BndDir('WTBNDDIR');
     h/Title SR00000B - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000000C
       //============================================================
       // Program Description:
       //
       //    THIS PROGRAM IS CALLED TO:
       //      1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE
       //         ATTACHED TO THE POLICY.
       //      2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE
       //         (WXFMP) PRIOR TO A FORM MERGE.
       //
       // PROGRAMMER: C:333
       // CREATED:    10/18/22            PROJECT: WC2834
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
       Dcl-Pr SR00000C;
        *N Char(3);
        *N Char(3);
        *N Char(9);
        *N Char(5);
        *N Char(3);
        *N Char(1);
        *N Packed(9:0);
        *N Char(1);
       End-Pr;
       Dcl-Pi SR00000C;
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
     d PrtPgm          pr                  extpgm('WC000000C')
     d  qqco#                         3
     d  qqfnd                         3
     d  qqemp#                        9
     d  qqdiv                         5
     d  qqfyr                         3
     d  qqmode                        1

       // Copybook D specs
     d/Copy sCopyEn01d

     d* Check Pgm
     d SrChkPgm        pr                  ExtPgm('SRCHKPGM')
     d                               10
     d                                1

       //=============================================================
       // Copybook required constants
       //=============================================================
       // Form ID
       Dcl-C qqForm Const('WC000000C');
       // Additional entry program
       Dcl-C addPgm Const('SRxxxxxxI');
       //=============================================================
       // Main
       //=============================================================
       // Copybook C specs (PLISTs, KLISTs and mainline)
     c     *DTAARA       DEFINE    W7413PDF      XXPDF             1
     c                   IN        XXPDF
     c/Copy sCopyEn01C
       //=============================================================
       // OthInf - Subroutine to load form specific information
       //=============================================================
       Begsr OthInf;

     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         xxPFDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         xxPTDT            8 0

      *  If printing a policy page.  (Not called from W1054R)
        SRCHKPGM(#1PGM:#1FLAG);
        If #1Flag = '0';
           PrintingPolicy = *On;
        Else;
           PrintingPolicy = *Off   ;
        Endif;
        RunningSRtext = *On;

       Endsr;
       //============================================================
       //  Pgmstr - Beginning of program processing
       //============================================================
       Begsr PgmStr;
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
