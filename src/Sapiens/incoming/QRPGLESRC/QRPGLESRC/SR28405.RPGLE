     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BndDir('WTBNDDIR')
     H/TITLE SR28405 - NH Endorsement WC280405
      //===============================================================*
      // WC280405 - NH Endorsement                                     *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy.                                                 *
      //                                                               *
      // Created   : 09/22/20                                          *
      // Programmer: C:079                                             *
      // Project   : WC2763                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 11/03/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
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

     d qqForm          s             15a   inz('WC280405  ')

      //* Additional entry program
     d addPgm          c                   const('SRXXXXXXX ')

     d/copy scopyen01d

     D SR28405         pr
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR28405         pi
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
      //=========================================================================
      //  Main
      //=========================================================================
    
      //* Copybook C specs (KLISTS and mainline)
     c/copy scopyen01c
    
      //=========================================================================
      //  OTHINF - Subroutine to load form specific information
      //=========================================================================
      /Free
       Begsr OthInf;
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
       RunningSrText = *On;
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
