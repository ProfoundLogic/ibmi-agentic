     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     *
    H/TITLE SR22305 - MN Exclusion of Coverage for Leased Employees
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 02/22/17  C:180 Support 41323                                 *
      *                 Interface with Scriptura (SY371)              *
      *                                                               *
     * 02/27/06  C:61  PROJECT WC1422                                *
      *                 Created.
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
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
      * Entry
     D SR22305         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR22305         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
     D/COPY SCOPYEN01D

      *


     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D QQFORM          c                   const('WC220305  ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')



     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    
     c                   exsr      getfnd
      /Free
       RunningSRtext = *On;
      /End-Free
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     c     valid         begsr
    
     c                   if        ((fnpeof = 'S') and (eldiv = 0))
     c                             or ((fnpeof = 'M') and (elemp# = 1))
     c     rsky          chain     prs01                              60
    
     c                   if        (*in60 = *off)
     c                             and (rsjrst = 'MN')
     c                   move      *on           crtfrm
     c                   endif
    
     c                   endif
    
     c                   endsr
    
    
    
