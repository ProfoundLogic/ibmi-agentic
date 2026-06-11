     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
      /TITLE SR18402C - ME Maine Merit Rating Endorsement
      //===============================================================*
      // WC180402A - Maine Merit Rating Plan Endorsement               *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 07/07/23                                          *
      // Programmer: C:291                                             *
      // Project   : WC2914                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     D SR18402C        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR18402C        PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D QQFORM          S             15A   inz('WC180402C ')
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
     C     Othinf        Begsr
     *
     C*
     ** Write the output record
     *
     *
     C                   Endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     Pgmstr        Begsr
     *
      /Free
        RunningSRtext = *On;
      /End-Free
     *
     C                   Endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     Pgmend        Begsr
     *
     *
     C                   Endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     Valid         Begsr
     *
     C     rsky          Chain     prs01                              60
     *
     C                   If        *In60 = *Off
     C                             and rsjrst = enst
     C                             and elprom = 'M'
     *
     *
     ** Found a division with a matching jurisdiction state
     C                   Move      *On           crtfrm
     *
     C                   Endif
     *
     C                   Endsr
