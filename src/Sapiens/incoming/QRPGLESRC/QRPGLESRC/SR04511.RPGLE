     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
    H/TITLE SR04511  - CA - Retrospective Premium End - Short Form
     //===============================================================*
     // WC040511                                                      *
     //                                                               *
     // Program Description:                                          *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXFMP) prior to a form merge.                     *
     //                                                               *
     // Created   : 06/09/04                                          *
     // Programmer: C:61                                              *
     // Project   : CM027                                             *
     //===============================================================*
     // REVISIONS:                                                    *
     //                                                               *
     // 10/31/22 C:333 Project: WC2834 (Recompile)                    *
     //                Endorsement Recognition of Cancellation        *
     //                                                               *
     // 12/20/16 C:190 Project: SY371                                 *
     //                Add RunningSRtext indicator.                   *
     //                                                               *
     // 09/16/16 C:136 Project: SY371                                 *
      //                Implement Scriptura (Changes not commented)    *
     // ------------------------------------------------------------- *
     //  NOTE: Please add comments to the top of revisions.           *
     // ------------------------------------------------------------- *
     //===============================================================*
     // Notes:                                                        *
     //                                                               *
     //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     //                                                               *
     //  QQMODE = 1 - Called to determine if form should be attached. *
     //  QQMODE = 2 - Called to print endorsement.                    *
     //  QQMODE = 3 - Called for additional input.                    *
     //  QQMODE = 4 - Called for mid-term processing.                 *
     //===============================================================*
    
     //* Copybook F specs
     F/COPY SCOPYEN01F
     //=========================================================================
     //  Definition Specs
     //=========================================================================

     //* Copybook D specs
     D/COPY SCOPYEN01D
     //=========================================================================
     //  /COPY required constants
     //=========================================================================
    
     //* Form ID
     D qqForm          S             15a   Inz('WC040511  ')
    
     //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
      //Entry
     D SR04511         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR04511         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
     //=========================================================================
     //  *LIKE DEFN definitions
     //=========================================================================
    
    
     //=========================================================================
     //  PLIST definitions
     //=========================================================================

    
     //=========================================================================
     //  KLIST definitions
     //=========================================================================
    
    
     //=========================================================================
     //  Main
     //=========================================================================
    
     //* Copybook C specs
     C/COPY SCOPYEN01C
     //=========================================================================
     //  OTHINF - Subroutine to load form specific information & write record
     //=========================================================================
      /FREE
       Begsr othInf;
    
       If FMcpl# = *blanks;
          FMcpl# = %editc(xxfnd:'X') + %editc(xxemp#:'X') + %editc(xxfyr:'X');
       Endif;
    
       Endsr;
      //=========================================================================
      //  PGMSTR - Beginning of program processing
      //=========================================================================
       Begsr pgmstr;
        RunningSRText = *ON;
       Endsr;
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       Begsr pgmend;
       Endsr;
      //=========================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr valid;
         crtFrm = *on;
       Endsr;
      /END-FREE
