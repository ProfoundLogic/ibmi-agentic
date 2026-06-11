     *===============================================================*
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H option(*nodebugio:*srcstmt)
    H/TITLE SR37405 - PA MERIT RATING PLAN ENDORSEMENT
     *===============================================================*
     * WC370405 - Pennsylvania Rating Plan Endorsement               *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 02/26/01                                          *
     * Programmer: C:90                                              *
     * Project   : CM1026                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
     *                                                               *
     * 01/03/17 C:136 Project: SY371                                 *
     *                Implement Phase 2 - changes not commented      *
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
      *Entry
     D SR37405         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR37405         PI
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
     D qqForm          s             15a   inz('WC370405  ')
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
     *
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
     *
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
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
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    C                   eval      runningSrText = *On
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     C                   if        ELprom = 'M'
     C                   move      *on           crtfrm
     C                   endif
     *
     C                   endsr
