     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BndDir('WTBNDDIR')
     H/TITLE SR17601H - LA Amendatory Endorsement
      //===============================================================*
      // WC170601H-                                                    *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 01/01/17                                          *
      // Programmer: C:306                                             *
      // Project   : WC2489                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
      //
      // 08/24/17 C:180 Project: WC2514/48762                         *
      //                Per GW and JH, non-prorata will always be      *
      //                checked for all clients. Prorata will never    *
      //                be checked. Also, remove ANC validation.       *
      //                                                               *
      //
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
     fwdrrl     if   e           k disk

      //* Copybook F specs
     f/copy scopyen01f

     d qqFORM          s             15a   inz('WC170601H ')
     d xxPreAmt        s             11  2 inz(0)
     d xxRatSt         s              2    inz
     d intANCstatC     s              5  0 inz
     d intMult         s              5  4 inz
     d intStRptDiv     s              5  0 inz
     d intStRptEffDt   s              7  0 inz

      //* Additional entry program
     d addpgm          c                   const('SRXXXXXXX ')

      //* Copybook D specs
     d/copy scopyen01d
     D*** Beg Del *** 09/20/24 *****************************************
     d***/copy wPrEmplr
     D*** End Del *** 09/20/24 *****************************************


     d SR17601H        PR
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1

     d SR17601H        PI
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1
    
     D StrDiv          s                   Like(ELdiv) Inz
     D StrFlg          s              1    Inz
     D EMrrn           s              9  0 Inz
      //=============================================================
      //  Main
      //=============================================================

      //* Copybook C specs (KLISTS and mainline)
     c/copy scopyen01c

      //=============================================================
      //  OTHINF - Subroutine to load form specific information
      //=============================================================
      /Free
       Begsr Othinf;
         // Get state reporting division's policy effective date
          StrDiv = GetStRptDv(xxCo#:xxfnd:xxemp#:emrrn:StrFlg);
          Chain (xxCo#:xxfnd:xxfyr:xxemp#:StRDiv) wdell1;
          intStRptEffdt = ELpfdt;
          Setll (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Dow not(%eof);
             xxRatSt = GetRatingState(xxco#:xxfnd:xxemp#:ELdiv);
             Setgt (ELco#:ELfnd:xxRatSt:intStRptEffdt) prr01;
             Readpe (ELco#:ELfnd:xxRatSt) prr01;
             If not %eof;
                intANCstatC = RRancs;
             Endif;
             If (xxRatSt = ENst or ENst = *Blanks)
              and (intANCstatC <> 0);
                xxPreAmt = xxPreAmt + ELanpr;
                intMult = RRancm;
             Endif;
             Reade (xxCo#:xxfnd:xxfyr:xxEmp#) wdell1;
          Enddo;

          // Populate schedule fields
          FMam01 = xxPreAmt;
          FMa541 = intMult;
          Eval(h)  FMam02 = FMam01 * FMa541;

       Endsr;
       //============================================================
       //  PGMSTR - Beginning of program processing
       //============================================================
       Begsr Pgmstr;
       Endsr;
       //============================================================
       //  PGMEND - End of program processing
       //============================================================
       Begsr Pgmend;
       Endsr;
       //============================================================
       //  VALID - Subroutine to attach this form to the policy
       //============================================================
       Begsr Valid;
       //** Beg Change *** 08/24/17 *******************************
        //Setgt  (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
        // Readpe (ELco#:ELfnd:RSjrst) prr01;
        // If Not(%Eof) and RRancs <> 0 and RRancm <> 0;
        //    crtFrm = *on;
        // Endif;
        crtFrm = *on;
       //** End Change *** 08/24/17 *******************************

       Endsr;
      /End-free
