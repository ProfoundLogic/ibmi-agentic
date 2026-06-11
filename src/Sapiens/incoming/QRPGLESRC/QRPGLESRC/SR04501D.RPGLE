     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

    H/TITLE SR04501D - CA - Retrospective Premium Endorsement
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     * WC040501.D                                                    *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 10/24/08                                          *
     * Programmer: C:190                                             *
     * Project   : 132WLL                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 10/31/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
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
     * Billing Schedule
     fwtabl     if   e           k disk    include(pbs01)
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
     D qqForm          s             15a   inz('WC040501D ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
     *entry parameters
     D SR04501D        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04501D        PI
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
      * Employer Master

     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
    
      * Billing Schedule

     c     bsky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey            10
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr

     * get info from state reporting division
    
     c     emky1         Chain     pem01                              60
    
     c                   Dow       (*in60 = *off) and (emstrp <> 'Y')
     c     emky1         reade     pem01                                  60
     c                   EndDo
    
     c                   If        (*in60 = *off)

     *   PAGE 3
      *   Tax multiplier
     c                   eval      fma546  = eltax
    
      *   Allocated Loss Adjustment Expenses
      *                  Excluded

      *   Loss Conversion Factor
     c                   eval      fma541  = ellcvf

      *   Per-Accident Loss Limit
     c                   eval      fmam01  = elretn

      *   Risk Excess Loss Factor
     c                   eval      fma545  = elelp

      *   Other Policies Subject to this Retrospective Premium Endorsement
      *   (Leave to blank).

      *   First computation (default to 'Yes').
     c*

      *   Subsequent computations (default to 'Yes')
     c*

      *   Estimated Standard Premium
     c                   eval      fmam02  = elstpr
     c*

      *   Minimum Retrospective Premium Ratio
     c                   eval      fma542  = elmnfc
     c*

      *   Maximum Retrospective Premium Ratio
     c                   eval      fma543  = elmxfc

      *   Basic Premium Factor
     c                   eval      fma544  = elbsfc

      *   PAGE 4

      *   Standard Premium, column (2)
     c*   FMAM02

      *   Basic Premium, column (2)
     c*   FMA544

     c                   endif                                                  *IN60 = '0'
    
     C                   endsr                                                  OTHINF
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
    
     C                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))
    
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = 'CA'
    
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     C                   endif
    
     C                   endif
    
     C                   endsr
    
    
