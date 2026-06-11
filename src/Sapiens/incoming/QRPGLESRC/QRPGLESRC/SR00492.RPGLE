     * (C) Copyright FISERV Insurance Solutions, Inc., 2006
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00492 - 30 Day Notice of Cancellation Endorsement
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:
     *
      * 05/20/06 C:61  PROJECT 136WCA
     *                Created.
     *
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
      ** Statistical Reporting/Rating File
     Fwxfmp3    o    e             disk    usropn
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
     D/COPY SCOPYEN01D


     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          c                   const('WC000492  ')

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

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================

    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** open work file
     C                   open      wxfmp3
    
     *    initialize
     C                   clear                   pfm03
     c                   movel(p)  xxform        fmform
    
    
     ** Write the output record
     C                   write     pfm03
     C                   write     pfm01
     *
     ** Close the endorsement form workfile
     C                   close     wxfmp3
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
    
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C                   move      *on           crtfrm
    
     C                   endsr
    
    
    
