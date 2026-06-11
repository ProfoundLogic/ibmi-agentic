     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR00109B - Outer Continental Shelf Lands Act Coverage Endorsement
     *===============================================================*
     * WC000109B - Outer Continental Shelf Lands Act Coverage Endor. *
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
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
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
    
     ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)

     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     ** Form ID
     D qqForm          s             15a   inz('WC000109B ')

     ** Additional entry program
     D addpgm          c                   const('SR00109IB ')

      *entry parameters
     D SR00109B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR00109B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     ** Additional Input file
    
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxForm
    
    
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     C                   open      wxfmp1
     C     fmky3         chain     pfm02                              60
     *
     ** Move the custom fields to print
     C                   movel     f3c401        fmc401
     C                   movel     f3c402        fmc402
     C                   movel     f3c403        fmc403
     C                   movel     f3c404        fmc404
     C                   movel     f3c405        fmc405

     C                   close     wxfmp1
     *
     C                   endsr


     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr


     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr


     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C                   move      *on           crtfrm
    
     C                   endsr

