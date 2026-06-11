     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR08401B - DC - Safe Workplace Program Endorsement
     *===============================================================*
     * WC080401B - District of Columbia Safe Workplace Program       *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 04/23/02                                          *
     * Programmer: C:90                                              *
     *===============================================================*
     * Created on 03/26/18 by c:62 for Project WC2599                *
     *===============================================================*
     * Revisions:                                                    *
     *                                                               *
     * 10/31/22 C:333 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
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

     ** Copybook D specs
     D/COPY SCOPYEN01D
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC080401B ')
     ** Additional entry program
     D addpgm          c                   const('SR08401BI ')

      *entry parameters
     D SR08401B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR08401B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    C*
     *
     ** Additional Input file
     C     fmky3         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxForm
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
     C                   open      wxfmp1
     *
     C     fmky3         chain     pfm02                              60
     *
     ** Move the custom fields to print
     C                   movel     f3dt01        fmdt01
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
     *
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     *
     *
     C                   endsr
