     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BndDir('WTBNDDIR')
     H/TITLE SR2404B - MO Uncooperative Audit Endorsement
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 09/06/16                                          *
      // Programmer: C:298                                             *
      // Project   : WC2070                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //
      // 09/20/24 C:79  Project: WC2952
      //                Recompile
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

      //=========================================================================
      //  Arrays
      //=========================================================================
     d qqForm          s             15a   inz('WC240604B ')
     d xxPreAmt        s             11  2 inz(0)
     d xxRatSt         s              2    inz
     d intMult         s              5  4 inz
     d intANCstatC     s              5  0 inz
     d intStRptDiv     s              5  0 inz
     d intStRptEffDt   s              7  0 inz

      //* Additional entry program
     d addpgm          c                   const('SRXXXXXXX ')

     DSR24604B         pr
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     DSR24604B         pi
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     d/copy scopyen01d
     D*** Beg Del *** 09/20/24 *****************************************
     d***/copy wPrEmplr
     D*** End Del *** 09/20/24 *****************************************

      //=========================================================================
      //  KLIST definitions
      //=========================================================================
     c     rrky          KList
     c                   KFld                    ELco#
     c                   KFld                    ELfnd
     c                   KFld                    RSjrst
     c                   KFld                    intStRptEffDt

     c     rrky1         KList
     c                   KFld                    ELco#
     c                   KFld                    ELfnd
     c                   KFld                    RSjrst

      //=========================================================================
      //  Main
      //=========================================================================

      //* Copybook C specs (KLISTS and mainline)
     c/copy scopyen01c

      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr OthInf;

          intStRptDiv = GetStRptDv(xxCO#:xxFND:xxEMP#);
          Chain (xxCo#:xxFnd:xxEmp#:intStRptDiv:xxFyr) pel01;
          intStRptEffDt = ELpfdt;
          Setll (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Reade (xxCo#:xxfnd:xxfyr:xxemp#) wdell1;
          Dow not(%eof);
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
             Reade (xxCo#:xxfnd:xxfyr:xxEmp#) wdell1;
          Enddo;

          FMam01 = xxPreAmt;
          FMa541 = intMult;

       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PgmEnd;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;

          Setgt  (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
          Readpe (ELco#:ELfnd:RSjrst) prr01;
          If Not(%Eof) and RRancs <> 0 and RRancm <> 0;
             crtFrm = *on;
          Endif;

       Endsr;
      /End-Free
