     *===============================================================*
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
    H/TITLE SR07408 - DE DELAWARE MERIT RATING PLAN ENDORSEMENT
     *===============================================================*
     * WC070408 - Delaware Merit Rating Plan Endorsement             *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 03/26/01                                          *
     * Programmer: C:90                                              *
     * Project   : WC1026                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/31/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 12/29/16 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
      *                                                               *
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
      **** Begin Add **** 12/29/16 *********************************************
      * Entry
     D SR07408         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR07408         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/29/16 *********************************************

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
      *** Begin Change *** 12/29/16 ********************************************
     D*** xxform          c                   const('WC070408  ')
     D QQFORM          S             15A   inz('WC070408  ')
      *** End   Change *** 12/29/16 ********************************************
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
      ** Begin Delete *******   12/29/16 ***************************************
     C***  *entry        plist
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qqrrn
     C***                parm                    qqmod2
    
     ** Additional input program parameters
     C***  addpl         pList
     C***                parm      *blanks       qqprds
     C***                parm      *blanks       qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     *** End   Delete *******   12/29/16 ***************************************
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
      *** Begin Delete *** 12/29/16 ********************************************
     C***                write     pfm01
      *** End   Delete *** 12/29/16 ********************************************
     *
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     /Free
       RunningSRtext = *ON;
     /End-Free
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
     C                             and elprom = 'M'
     *
     *
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     *
     C                   endif
     *
     C                   endsr
