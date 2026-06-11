     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR35402 - OK WC PREMIUM REDUCTION PROGRAM ENDORSEMENT
     *===============================================================*
     * WC350402A - Oklahoma WC Premium Reduction Program Endorsement *
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
      * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 01/03/17 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
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
     Fwdrrl     if   e           k disk
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
      * Entry
     D SR35402         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR35402         PI
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
     D QQFORM          S             15A   inz('WC350402A ')
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
     C     rrky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    rsjrst
     C                   kfld                    elpfdt
    C*
     C     rrky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    rsjrst
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
     *
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     *
      /Free
        RunningSRtext = *On;
      /End-Free
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
     ** Found a division with a matching jurisdiction state
     *
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  60
     C     *in60         ifeq      *off

     C     rrdv1c        ifeq      9880
     C     rrdv2c        oreq      9880
     C     rr1acd        oreq      9880
     C     rr1bcd        oreq      9880

     C                   move      *on           crtfrm
     *
     C                   endif
     *
     C                   endif
     *
     C                   endif
     *
     C                   endsr
