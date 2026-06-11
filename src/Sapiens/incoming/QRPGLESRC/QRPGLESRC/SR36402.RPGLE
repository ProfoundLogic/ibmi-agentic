     *===============================================================*
     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     *===============================================================*
     */TITLE SR36402 - OR CONTRACTING CLASSIFICATION PREMIUM ADJUSTMENT
     *                 ENDORSEMENT
     *===============================================================*
     * WC360402 - Oregon Contracting Classification Premium Adjustment
     *            Endorsement                                        *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/06/07                                          *
     * Programmer: C:184                                             *
     * Project   : WC1525                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 12/16/16 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
     *                                                               *
     *                                                               *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
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
      *
     * Employer Payroll
     Fwmepl     if   e           k disk
    
     * Payroll Class Description
     Fwmprl1    if   e           k disk
      *
     ** Copybook F specs
     F/COPY SCOPYEN01F

     *=========================================================================
     *  Definition Specs
     *=========================================================================
      * Entry
     D SR36402         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR36402         PI
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
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D QQFORM          S             15A   inz('WC360402  ')

     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')                  Dummy placeholder

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
     * Employer Payroll                                                    ====
     *                                                                     ====
     C     epky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     *
     C     prky          klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
     C                   kfld                    elpfdt
     *
     C     prky1         klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd
     C                   kfld                    epclas
    

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
    
    
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = 'OR'
     C                             and elasp1 = 0
     C     epky          setll     pep01
     C     epky          reade     pep01                                  60
      *
     C     *in60         doweq     *off
      *
     C     epstat        ifne      'I'
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  60
      *
     C     *in60         ifeq      *off
     C     prccfl        andeq     'Y'
     *
     *  Found division with Oregon Rating state and Contractor Class
     *
     C                   move      *on           crtfrm
     C                   endif
     *
     C                   endif
     *
     C     epky          reade     pep01                                  60
     C                   enddo
     *
     C                   endif
    
     C                   endsr
    
