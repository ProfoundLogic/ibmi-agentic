     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
    H/TITLE SR28602 - NH Per Claim Benefits Deductible Endorsement
     *===============================================================*
     *WC280602- NH Per Claim Benefits Deductible Endorsement       *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 03/15/13                                          *
     * Programmer: C:287                                             *
     * Project   : WC2173                                            *
     *===============================================================*
     *Revisions:                                                    *
      *                                                               *
      * 11/03/22 C:333 Project: WC2834                                *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 01/23/17 C:180 Project SY371 scriptura.                       *
      *                Add Scriptura interface.                       *
     *                                                               *
     *===============================================================*
     *Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
     **Copybook F specs
     F/COPY SCOPYEN01F
     Fwtddl2    if   e           k disk

     *------------------------------------------------------------------------
     * Definition Specs
     *------------------------------------------------------------------------
      * ENTRY
     D SR28602         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR28602         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
     **Copybook D specs
     D/COPY SCOPYEN01D
     *------------------------------------------------------------------------
     * /COPY required constants
     *------------------------------------------------------------------------

     **Form ID
     D qqform          s             15a   inz('WC280602   ')

     **Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     *------------------------------------------------------------------------
     * *LIKE DEFN definitions
     *------------------------------------------------------------------------
    
     *------------------------------------------------------------------------
     * PLIST definitions
     *------------------------------------------------------------------------
    
     *------------------------------------------------------------------------
     * KLIST definitions
     *------------------------------------------------------------------------
    
     C     ddky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    rsjrst
     C                   KFld                    eldcde
     C                   KFLD                    XXDDT             7 0
    
     C     ddky1         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    rsjrst
     C                   KFld                    eldcde
    
     *------------------------------------------------------------------------
     * Main
     *------------------------------------------------------------------------
    
     **Copybook C specs
     C/COPY SCOPYEN01C
    
     *------------------------------------------------------------------------
     * OTHINF- Subroutine to load form specific information & write record
     *------------------------------------------------------------------------
     C     othinf        begsr
    
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
       //***** Beg Add *** 11/03/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/03/22 ******************************
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = enst
    
      *Field Q1DATEc is used in calling SREFFD instead of Q1DATE to bypass the compile error of
      *filed previously define with different attibute.

     C                   MOVE      ELDCEF        Q1DATEc
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV

     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1DATEc           7

     C                   MOVE      Q1DATEc       XXDDT
     C     ddky          setgt     pdd01
     C     ddky1         readpe    pdd01                                  60
    
     C                   if        *in60 = *off
     c                             and (ddstat = 'A')
     C                             and ddampo <> 0
    
     **If a record is found and there is a per occurrence deductible
     **then print that deductible on the endorsement.
    
     C                   eval      fma701 = ddampo
     C                   leave
    
     C                   endif
    
     C                   endif
          //***** Beg Add *** 11/03/22 ******************************
          Endif;
          //***** End Add *** 11/03/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo
    
    
     C                   endsr

     *------------------------------------------------------------------------
     * PGMSTR- Beginning of program processing
     *------------------------------------------------------------------------
     C     pgmstr        begsr
    
    
     C                   endsr

     *------------------------------------------------------------------------
     * PGMEND- End of program processing
     *------------------------------------------------------------------------
     C     pgmend        begsr
    
    
     C                   endsr

     *------------------------------------------------------------------------
     * VALID- Subroutine to attach this form to the policy
     *------------------------------------------------------------------------
     C     valid         begsr
    
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = enst
    
      *Field Q1DATEc is used in calling SREFFD instead of Q1DATE to bypass the compile error of
      *filed previously define with different attibute.

     C                   MOVE      ELDCEF        Q1DATEc
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV

     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1DATEc           7

     C                   MOVE      Q1DATEc       XXDDT

     C     ddky          setgt     pdd01
     C     ddky1         readpe    pdd01                                  60
    
     C                   if        *in60 = *off
     C                             and (ddstat = 'A')
     C                             and ddampo <> 0
    
     **Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
    
     C                   endif
    
     C                   endif
    
     C                   endsr
