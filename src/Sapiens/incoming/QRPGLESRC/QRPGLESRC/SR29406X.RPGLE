     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     *
    H/TITLE SR29406W - Update NJ Premium Discount Form
     *===============================================================*
     * WC290406W - Update NJ Premium Discount Form                   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/17/10                                          *
     * Programmer: C:190                                             *
     * Project   : WC1961                                            *
     *===============================================================*
      * Revised:
      *
     *                 It should attached to all NJ coverage.        *
     *                                                               *
      *
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
     *
     * Employer Policy Year
     Fwmepl     if   e           k disk
     *
     *
     Fwmprl1    if   e           k disk
     *
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *
     *
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR29406X        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR29406X        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     ** Copybook D specs
     D/COPY SCOPYEN01D


     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D QQFORM          c                   const('WC290406X ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')


     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     *
     *
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

    
     ** Additional input program parameters
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
     *
     *
     *
     *
     *
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
      /Free
       RunningSRtext = *on;
      /End-Free
     C                   endsr
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
     C     valid         begsr
     *
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      *
     C                   move      *on           crtfrm
     *
     C                   endif
     *
     C                   endsr
    
    
