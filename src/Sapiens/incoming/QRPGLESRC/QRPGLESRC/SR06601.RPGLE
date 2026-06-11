     *===============================================================*
     * (C) Copyright Progressive Data Solutions, Inc.,  2003
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR06601 - Connecticut NonRenewal Endorsement
     *===============================================================*
     * WC060601                                                      *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   12/02/03   C:61                                     *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     *                                                               *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for mid-term processing.                 *
     *===============================================================*

     ** Copybook F specs
     F/COPY SCOPYEN01F


     ** Copybook D specs
     D/COPY SCOPYEN01D

     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D qqform          c                   const('WC060601  ')

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')                  Dummy placeholder


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


     *=========================================================================
     *  KLIST definitions
     *=========================================================================


     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C


    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** Write the output record
     C                   write     pfm01
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
     C     rsky          chain     prs01
    
     C                   if        %found
     C                             and (rsjrst = enst)
    
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     C                   endif
    
     C                   endsr
    
    
