     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BndDir('WTBNDDIR')
     H/TITLE SR17601J - LA Amendatory Endorsement
      //===============================================================*
      // WC170601J-                                                    *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 09/26/18                                          *
      // Programmer: C:291                                             *
      // Project   : WC2633                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/19/22 C: 342 Project: WC2834                               *
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
     fwdrrl     if   e           k disk

      //* Copybook F specs
     f/copy scopyen01f

     d qqFORM          s             15a   inz('WC170601J ')
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
      //***** Beg Del *** 10/19/22 ***********************************
     d*****/copy wPrEmplr
      //***** End Del *** 10/19/22 ***********************************


     d SR17601J        PR
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1

     d SR17601J        PI
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

         StrDiv = GetStRptDv(xxCo#:xxfnd:xxemp#:emrrn:StrFlg);
         Chain (xxCo#:xxfnd:xxfyr:xxemp#:StRDiv) wdell1;
         intStRptEffdt = ELpfdt;
         FMFL01 = *blanks;
         FMFL02 = 'X';

          Setll (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Dow not(%eof);
             //***** Beg Add *** 10/19/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
             //***** End Add *** 10/19/22 ******************************
             xxRatSt = GetRatingState(xxco#:xxfnd:xxemp#:ELdiv);
             Setgt (ELco#:ELfnd:xxRatSt:intStRptEffdt) prr01;
             Readpe (ELco#:ELfnd:xxRatSt) prr01;
             If not %eof;
                intANCstatC = RRancs;
             Endif;
             If (xxRatSt = ENst) and (intANCstatC <> 0);
                xxPreAmt = xxPreAmt + ELanpr;
                intMult = RRancm;
             Endif;
             //For LUBA only
             If CltId = '133';
                FMFL01 = '_';
                FMFL02 = 'X';
             Endif;

             //***** Beg Add *** 10/19/22 ******************************
             Endif;
             //***** End Add *** 10/19/22 ******************************
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
        crtFrm = *on;

       Endsr;
      /End-free
