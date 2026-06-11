     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 2001
    H/TITLE SRNOTMO1 -MO WC NOTICE TO POLICYHOLDERS
     *===============================================================*
     * NOT-MO1 - Missouri WC Notice to Policyholders                 *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/29/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
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
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D xxform          c                   const('NOTXXMO1  ')
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

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     C                   parm                    qqmode
     C                   parm                    qqrrn
     C                   parm                    qqmod2
    
     ** Additional input program parameters
     C     addpl         pList
     C                   parm      *blanks       qqprds
     C                   parm      *blanks       qqcall
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
    C*
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     C*
     ** Write the output record
     C                   write     pfm01
     *
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C     rsky          chain     prs01                              60
     *
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     *
     *
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     *
     C                   endif
     *
     C                   endsr
