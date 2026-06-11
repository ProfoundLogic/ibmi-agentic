     * (C) Copyright Progressive Data Solutions, Inc., 2003
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR37110 - PA Terrorism Risk Insurance Extension Act End
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
     *
      * 06/22/06 C:61  SUPPORT wc114 51857
     *                Created.
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
      ** Print Data Work File
     Fwxfmp3    o    e             disk    usropn
    
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
     D qqform          c                   const('WC370110  ')

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

     ** Entry parameters
     c     *entry        plist
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    qqemp#            9
     c                   parm                    qqdiv             5
     c                   parm                    qqfyr             3
     c                   parm                    qqmode            1
     c                   parm                    qqrrn             9 0
     c                   parm                    qqmod2            1
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
     * Copybook c specs
     C/COPY SCOPYEN01c
    
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     c     othinf        begsr
    
     ** open work file
     c                   open      wxfmp3
    
     *    initialize
     c                   clear                   pfm03
     c                   movel(p)  xxform        fmform
    
     ** Write the output record
     c                   write     pfm03
     c                   write     pfm01
     *
     ** Close the endorsement form workfile
     c                   close     wxfmp3
    
     c                   endsr
    
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     c     pgmstr        begsr
    
     c                   endsr
    
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     c     pgmend        begsr
     *
     c                   endsr
    
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     c     valid         begsr
     *
     c     rsky          chain     prs01
     *
     c                   if        %found
     c                             and (rsjrst = 'PA')
     c                             and (elpfdt >= 1060101)
     c                   move      *on           crtfrm
     c                   endif
    
     c                   endsr
    

