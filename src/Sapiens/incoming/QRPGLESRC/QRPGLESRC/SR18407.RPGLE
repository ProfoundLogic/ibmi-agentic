     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
     *
    H/TITLE SR18407 - Maine Anniversary Modification Rating Date Print/Attach program
     *===============================================================*
     * WC180407                                                      *
      *   Single automatically attached endorsement                   *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 11/03/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 12/27/16 C:180 Project SY371 scriptura                        *
      *                Add scriptura interface.                       *
     *                                                               *
     * 08/19/10  C:190 Project WC1935                                *
     *                 Created.                                      *
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
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

      **** Begin Add **** 12/27/16 *********************************************
      * Entry
     D SR18407         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR18407         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/27/16 *********************************************
     ** Copybook D specs
     D/COPY SCOPYEN01D
    

      * employer effective dates
     d edds            ds
     d  emefd1
     d  emefd2
     d  emefd3
     d  emefd4
     d  emefd5
    d ed                                  OVERLAY(edds)
     d                                     LIKE(emefd1) DIM(5)

      * employer cancel dates
     d cdds            ds
     d  emcad1
     d  emcad2
     d  emcad3
     d  emcad4
     d  emcad5
     d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)

    d Months          ds
    d                                9    inz('January  ')
    d                                9    inz('February ')
    d                                9    inz('March    ')
    d                                9    inz('April    ')
    d                                9    inz('May      ')
    d                                9    inz('June     ')
    d                                9    inz('July     ')
    d                                9    inz('August   ')
    d                                9    inz('September')
    d                                9    inz('October  ')
    d                                9    inz('November ')
    d                                9    inz('December ')
    
    d Mth                            9    Overlay(Months) Dim(12)
    
     d x1andt          s                   like(elandt)
     d x2andt          s                   like(elandt)
     d x#andt          s                   like(elandt)
     d x1co#           s                   like(elco#)
     d x1fnd           s                   like(elfnd)
     d x1emp#          s                   like(elemp#)
     d x1div           s                   like(eldiv)
     d x1fyr           s                   like(elfyr)
     d x#prt           s                   like(emmpfl)
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
      *** Begin Change *** 12/27/16 ********************************************
     D**** xxform          c                   const('WC180407  ')
     D QQFORM          S             15A   inz('WC180407  ')
      *** End   Change *** 12/27/16 ********************************************
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
      ** Begin Delete *******   12/27/16 ***************************************
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
      **** End   Delete **** 12/27/16 *****************************************
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
    
     * Employer Master
    
     C     emky1         klist
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elemp#
     C                   KFld                    eldiv
    
     C     emky2         klist
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
    
     C     emky3         klist
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#

     C     elky3         klist
     C                   KFld                    x1co#
     C                   KFld                    x1fnd
     C                   KFld                    x1emp#
     C                   KFld                    x1div
     C                   KFld                    xxfyr
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  CK_EMP_CX - Check if employer division is cancelled
     *=========================================================================
     c     ck_emp_cx     begsr

     c                   move      'Y'           emp_cx            1
     c     emky1         chain     pem01

     c                   if        %found

      *  Get Employer Effective/Cancellation date

     C                   z-add     5             x                 3 0

     c                   dow       (ed(x) = 0) and (x > 1)
     c                   sub       1             x
     c                   enddo

     c                   if        (cd(x) = 0) or (cd(x) > sysdat)
     c                   eval      emp_cx = 'N'
     c                   endif

     c                   endif

     c                   endsr

    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr

     c                   clear                   x#prt
     c                   if        x2andt <> *zeros
     c                   eval      x#andt = x2andt
     c                   eval      x#prt  = 'Y'
     c                   else
     c                   if        x1andt <> *zeros
     c                   eval      x#andt = x1andt
     c                   eval      x#prt  = 'Y'
     c                   endif                                                  x2andt <> *zeros
     c                   endif                                                  x2andt <> *zeros

     c                   if        x#prt  = 'Y'
     c                   move      x#andt        xxmmdd            4 0
     c                   movel     x#andt        xxcyy             3 0
     c                   movel     xxmmdd        xxmm              2 0
     c                   move      xxmmdd        xxdd              2 0

     c                   eval      fmc101 = mth(xxmm)
     c                   eval      fma701 = xxdd

     ** Write the output record
      *** Begin Delete *** 12/27/16 ********************************************
     c****               write     pfm01
      *** End   Delete *** 12/27/16 ********************************************

     c                   endif

     c                   endsr                                                  othinf
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      *
     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0
      *
     c     emky2         setll     pem01
     c     emky3         reade     pem01
     c                   dow       not %eof
     c                   eval      x1co#  = emco#
     c                   eval      x1fnd  = emfnd
     c                   eval      x1emp# = ememp#
     c                   eval      x1div  = emdiv
     c                   eval      x1fyr  = xxfyr
     c     elky3         chain     pel01                              60

     c  n60              if        (emstrp = 'Y')
     c                   eval      x1andt = elandt
     c                   endif

     c     rsky          chain     prs01                              60
     c  N60              if        rsjrst = 'ME'
     c                   eval      x2andt = elandt
     c                   endif

     c     emky3         reade     pem01
     c                   enddo

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
     c     rsky          chain     prs01                              60
    
     ** Found a division with a matching jurisdiction state
    
     c                   if        *in60 = *off
     c                             and rsjrst = enst

     * Check that employer division is not cancelled

     c                   exsr      ck_emp_cx
     c********           eval      x2andt = elandt

     c                   if        emp_cx = 'N' and x1andt <> *zeros
     c                   move      *on           crtfrm
     c                   endif
    
     C                   endif

     c                   endsr
    
