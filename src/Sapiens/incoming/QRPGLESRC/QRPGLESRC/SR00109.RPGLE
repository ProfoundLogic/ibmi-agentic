     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 2001
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00109 - OUTER CONTINENTAL SHELF LANDS ACT COVERAGE END
     *===============================================================*
     * WC000109A - Outer Continental Shelf Lands Act Coverage Endor. *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 05/17/01                                          *
     * Programmer: C: 90                                             *
     * Project   : CM027 SEQ#27458                                   *
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
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqform          c                   const('WC000109A ')
     ** Additional entry program
     D addpgm          c                   const('SR00109I  ')
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
     *
     ** Additional Input file
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform
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
     C                   open      wxfmp1
     *
     C     fmky3         chain     pfm02                              60
     *
     ** Move the custom fields to print
     C                   movel     f3c401        fmc401
     C                   movel     f3c402        fmc402
     C                   movel     f3c403        fmc403
     C                   movel     f3c404        fmc404
     C                   movel     f3c405        fmc405
     ** Write the output record
     C                   write     pfm01
     *
     C                   close     wxfmp1
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
     C                   move      *on           crtfrm
     *
     C                   endsr
