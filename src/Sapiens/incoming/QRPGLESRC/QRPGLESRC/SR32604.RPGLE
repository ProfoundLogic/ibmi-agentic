       CTL-OPT COPYRIGHT('(c) 2025 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no) ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //===============================================================*
       // WC320604                                                      *
       //                                                               *
       // Program Description:                                          *
       //                                                               *
       //   Determines if this endorsement form should be attached to   *
       //   the policy and creates data in the Endorsement Form         *
       //   file (WLEDP) prior to a form merge.                         *
       //                                                               *
       // Created   : 12/09/20                                          *
       // Programmer: C:180                                             *
       // Project   : 148WCE                                            *
       //===============================================================*
       // REVISIONS:                                                    *
       //                                                               *
       // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
       //                For single print endorsements provide specific *
       //                group and employer so that correct sig is found*
       //                                                               *
       //  10/19/22 C:342 Project: WC2834 (Recompile)                   *
       //                 Endorsement Recognition of Cancellation       *
       //                                                               *
       //===============================================================*
       // Notes:                                                        *
       //                                                               *
       //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
       //                                                               *
       //  QQMODE = 1 - Called to determine if form should be attached. *
       //  QQMODE = 2 - Called to print endorsement.                    *
       //  QQMODE = 3 - Called for additional input.                    *
       //  QQMODE = 4 - Called for mid-term processing.                 *
       //===============================================================*
       Dcl-Pi *N;
        qqCo#  char(3);
        qqFnd  char(3);
        qqEmp# char(9);
        qqDiv  char(5);
        qqFyr  char(3);
        qqMode char(1);
        qqRrn  packed(9:0);
        qqMod2 char(1);
       End-Pi;

       Dcl-F wledl disk usage(*input) keyed usropn prefix(ed);

      /Free
       /Copy sCopyEn01f
       /Copy sCopyEn01d
      /End-Free

       Dcl-c AddPgm const('SR32604I ');
       Dcl-s qqForm char(15) inz('WC320604    ');
       Dcl-s k1Form char(15) inz;

      /Free
       /Copy sCopyEn01C
      /End-Free

       //*==================================================================
       //*  OthInf - Subroutine to load form specific information & write
       //*==================================================================
       Begsr OthInf;
        Exsr OpenWorkFile;
        Open WLEDl;
        k1form = 'WC320604';
        Setll (xxco#:xxfnd:xxemp#:xxdiv:xxfyr:k1form) ped01;
        Reade (xxco#:xxfnd:xxemp#:xxdiv:xxfyr:k1form) ped01;

        Dow Not %eof(wledl);
           FMa701 = edFMa701;
           FM2001 = edFM2001;
           Exsr WriteMultiRcd;
           Reade (xxco#:xxfnd:xxemp#:xxdiv:xxfyr:k1form) ped01;
        Enddo;
        Close WLEDl;
        Exsr CloseWorkFile;
       Endsr;

       //*===================================================================
       //*  PGMSTR - Beginning of program processing
       //*===================================================================
       Begsr PgmStr;
        WriteInCopyBook = *Off;
       Endsr;

       //*===================================================================
       //*  PGMEND - End of program processing
       //*===================================================================
       Begsr PgmEnd;
       Endsr;

       //*===================================================================
       //*  VALID - Subroutine to attach this form to the policy
       //*===================================================================
       Begsr Valid;
       Endsr;
