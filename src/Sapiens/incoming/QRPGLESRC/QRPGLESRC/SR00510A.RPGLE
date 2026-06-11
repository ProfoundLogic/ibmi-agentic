     * (C) Copyright FISERV Insurance Solutions, Inc., 2007
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00510A -Retro Rating Wrap-Up Construction
     *===============================================================*
     * WC000510A                                                     *
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
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          c                   const('WC000510A ')
    
     ** Additional entry program
     D addpgm          c                   const('SR00510IA ')
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
     C                   kfld                    xxform
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
     c                   move      f4c704        fm2004                         Field 4
    

     ** Write the output record
     C                   write     pfm01
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
