     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h bnddir('WTBNDDIR')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     //===============================================================*
     h/Title SR09408 - FL Experience Rating Modification Factor Revision
     //===============================================================*
     // WC090408 - FL Experience Rating Modification Factor Revision     *
     //                                                               *
     // Program Description:                                          *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXFMP) prior to a form merge.                     *
     //                                                               *
     // Created   : 04/25/17                                          *
     // Programmer: C:136                                             *
     // Project   : WC2486                                            *
     //===============================================================*
     // REVISIONS:                                                    *
     //                                                               *
     // 07/16/18 C:079 Support: 150 71292                             *
     //                Remove the conditions for attaching the form   *
     //                                                               *
     // ------------------------------------------------------------- *
     //  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
     // ------------------------------------------------------------- *
     //===============================================================*
     // Notes:                                                        *
     //                                                               *
     //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     //  This is a manual attach endorsement with additional input.   *
     //                                                               *
     //  QQMODE = 1 - Called to determine if form should be attached. *
     //  QQMODE = 2 - Called to print endorsement.                    *
     //  QQMODE = 3 - Called for additional input.                    *
     //  QQMODE = 4 - Called for mid-term processing.                 *
     //===============================================================*
     //* Copybook F specs
    f/copy sCopyEn01F
     //=========================================================================
     //  Arrays
     //=========================================================================
     d SR09408         pr
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1

     d SR09408         pi
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1
     //=========================================================================
     //  /COPY required constants
     //=========================================================================
     // Form ID
     d qqForm          s             15a   inz('WC090408  ')
     // Additional entry program
     d addPgm          c                   const('XXXXXXXXXX')
     //=========================================================================
     //  Data structures and constants
     //=========================================================================
    d/copy sCopyEn01D
     //=========================================================================
     //  Main
     //=========================================================================
    c/copy sCopyEn01C
     //=========================================================================
     //  OTHINF - Subroutine to load form specific information & write record
     //=========================================================================
      /Free
       BegSr OthInf;
       EndSr;
      //=========================================================================
      //  PGMSTR - Beginning of program processing
      //=========================================================================
       BegSr PgmStr;
       RunningSRtext = *On;
       EndSr;
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       BegSr PgmEnd;
       EndSr;
      //=========================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr Valid;
       //*** Beg Del *** 07/16/18 ******************************************
       //**** If ELemd1 > 0 or ELemd2 > 0  or ELemd3 > 0;
       //*** End Del *** 07/16/18 ******************************************
          crtFrm = *On;
       //*** Beg Del *** 07/16/18 ******************************************
       //**** Endif;
       //*** End Del *** 07/16/18 ******************************************
       EndSr;
      /End-Free
