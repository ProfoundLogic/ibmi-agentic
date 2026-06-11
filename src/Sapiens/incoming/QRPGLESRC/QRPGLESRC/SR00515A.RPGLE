     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
     *
    H/TITLE SR00515A -Retro Rating Plan Premium Flexibility Options
     *===============================================================*
     * WC000515A                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   "Print/Attach" program for single manual attach endorsement *
     *   with addtional input program that stores information        *
     *   in generic data file WXFMP2.                                *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 10/27/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 01/04/17 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
     *                                                               *
     * 10/30/09  C:190  Project 132WMQ                               *
     *                  Created.                                     *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     ** Additional Information File
     Fwxfmp2    if   e           k disk    usropn
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
      * Entry
     D SR00515A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR00515A        PI
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
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D QQFORM          S             15A   inz('WC000515A ')
    
     ** Additional entry program
     D addpgm          c                   const('SR00515IA ')
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
     ** Additional Input
    
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxForm
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     C                   open      wxfmp2
    
     C     fmky3         chain     pfm02                              60
    
     ** Move the custom fields to print
    

     C                   move      f4c701        fm2001                         Field 1
     c                   move      f4c702        fm2002                         Field 2
     c                   move      f4c703        fm2003                         Field 3
    

     ** Write the output record
     C                   exsr      WriteWorkFile
    
     C                   close     wxfmp2
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       WriteInCopyBook = *OFF;
      /End-Free
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
    
