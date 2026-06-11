     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      /TITLE SRtext - "Text Only Endorsement" print/attach
      //===============================================================*
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 10/15/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 11/08/17 C:65  Project: 150WCC (Recompile)                    *
      //                Recompile for rating state and generate from   *
      //                PDF values being populated into work file.     *
      //                                                               *
      // 05/16/17 C:65  Project: SY371  (Recompile)                    *
      //                Add client path field from table file to       *
      //                copybook.                                      *
      //                                                               *
      // 03/01/17 C:136 Project: WC2481 (Recompile)                    *
      //                Add policy from to dates to XML workfile       *
      //                in copy code                                   *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
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

      //* Copybook F specs
      /COPY SCOPYEN01F

      //* Copybook D specs
      /COPY SCOPYEN01D

      //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     D  qqRrn          s              9  0
     D  qqMod2         s              1

     D SRTEXT          PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                               15

     D SRTEXT          PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqForm                       15

      //* Copybook C specs
     C/COPY SCOPYEN01C

      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================

       begsr othinf;
       endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
          RunningSRtext = *On;
       endsr;

       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       begsr valid;
          crtfrm = *On;
       endsr;
      /End-Free
