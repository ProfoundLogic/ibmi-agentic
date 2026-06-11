     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
     *
    H/TITLE SR29305A - NJ Employee Leasing Labor Contractor Exclusion End
     *===============================================================*
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 09/12/18  C:291 Project: WC2625                   *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 11/03/22 C:333 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
     *                                                               *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
    
     * Copybook F specs
     F/COPY SCOPYEN01F
    
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

      * Entry
     D SR29305A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR29305A        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     * Copybook D specs
     D/COPY SCOPYEN01D

     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          s             15A   inz('WC290305A ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')



     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
      /Free
      //=======================================================================
      // OTHINF - Subroutine to load form specific information & write record
      //=======================================================================
       begsr othinf;
       endsr;
    
      //=======================================================================
      // PGMSTR - Beginning of program processing
      //=======================================================================
       begsr pgmstr;
          RunningSRtext = *On;
       endsr;
    
      //=======================================================================
      // PGMEND - End of program processing
      //=======================================================================
       begsr pgmend;
       endsr;
    
      //=======================================================================
      // VALID - Subroutine to attach this form to the policy
      //=======================================================================
       Begsr valid;
          crtfrm = *off;
          If EMLSID = '3';
             crtfrm = *on;
          Endif;
       Endsr;

      /End-Free
    
