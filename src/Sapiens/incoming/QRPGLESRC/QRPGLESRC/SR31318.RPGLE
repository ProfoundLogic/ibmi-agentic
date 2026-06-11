     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR31318 - NY - Labor Contractor Exclusion Endorsement
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created: 10/25/17   C:33   Project: WC2570                    *
     *===============================================================*
     * Revisions:                                                    *
     *                                                               *
     *   10/19/22 C:342 Project: WC2834 (Recompile)                  *
     *                  Endorsement Recognition of Cancellation      *
     *                                                               *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*

     Fwdl4l     if   e           k disk    rename(pl401:pl400)

     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     D                 ds
     D state                          2
     D  fmfl01                        1    overlay(state)
     D  fmfl02                        1    overlay(state:2)

     D xxefdt          s                   like(fmeedt)

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqForm          s             15    inz('WC310318  ')

     ** Additional entry program
     D addpgm          c                   const('SR31318I  ')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     *entry
     DSR31318          pr                  extpgm('SR31318')
     D qqCo#                          3
     D qqFnd                          3
     D qqEmp#                         9
     D qqDiv                          5
     D qqFyr                          3
     D qqMode                         1
     D qqRRN                          9  0
     D qqMod2                         1
     DSR31318          pi
     D qqCo#                          3
     D qqFnd                          3
     D qqEmp#                         9
     D qqDiv                          5
     D qqFyr                          3
     D qqMode                         1
     D qqRRN                          9  0
     D qqMod2                         1
    
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     //=========================================================================
     //  OthInf - Subroutine to load form specific information
     //=========================================================================
      /Free
       Begsr OthInf;
        WriteInCopybook = *off;
       //* Write the output record
        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pl400;
        Dou %eof;
           Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pl400;
           Select;
           When %eof;
              Iter;
           When L4form <> qqForm;
              Iter;
           Endsl;
           FMnm01 = L4nam1;
           FMc401 = L4adr1;
           FMc402 = L4adr2;
           FMc403 = %trim(L4cty) + ', ' + L4st + '  ' +
            %subst(%editc(L4zip:'X'):1:5);
           If %subst(%editc(L4zip:'X'):6:4) <> '0000';
              FMc403 = %trim(FMc403) + '-' +
               %subst(%editc(L4zip:'X'):6:4);
           Endif;
          Exsr WriteWorkFile;
        Enddo;
       Endsr;
      //=========================================================================
      //  PgmStr - Beginning of program processing
      //=========================================================================
       Begsr PgmStr;
       Endsr;
      //=========================================================================
      //  PgmEnd - End of program processing
      //=========================================================================
       Begsr PgmEnd;
       Endsr;
      //=========================================================================
      //  Valid - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr Valid;
        //* This is a manual attach endorsement with additional input
        crtFrm = *on;
       Endsr;
      /END-FREE
