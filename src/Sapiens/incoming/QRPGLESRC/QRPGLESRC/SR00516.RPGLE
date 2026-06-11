     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H bnddir('WTBNDDIR')
    H/TITLE SR00516 - Retrospective Large Risk Alt Option Endors     *
     *===============================================================*
     * WC000516- Retrospective Large Risk Alt Option Endors          *
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
     *                                                               *
      * 10/27/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 12/20/16 C:180 Project SY371 scriptura                        *
      *                                                               *
     * 02/25/10  C:190 Seq# 71571                                    *
     *           Change the logic of this endorsement to not         *
     *           attach if the policy only has 'NJ' coverage.        *
     *                                                               *
     * 10/30/09  C:190 Project 132WMQ                                *
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
     * Billing Schedule
     fwtabl     if   e           k disk    include(pbs01)
    
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

      **** Begin Add **** 12/19/16 *********************************************
      * Entry
     D SR00516         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR00516         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/19/16 *********************************************
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
    d cd                                  OVERLAY(cdds)
     d                                     LIKE(emcad1) DIM(5)

    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
      **** Begin Change *** 12/20/16 ********************************
     D*** xxform          c                   const('WC000516  ')
     D qqform          c                   const('WC000516       ')
      **** End   Change *** 12/20/16 ********************************
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

      *** Begin Delete **** 12/20/16 ***************************************
     ** Entry parameters
     C***  *entry        plist
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qqrrn
     C***                parm                    qqmod2
     ***
     ** Additional input program parameters
     C***  addpl         pList
     C***                parm      *blanks       qqprds
     C***                parm      *blanks       qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
      *** End   Delete **** 12/20/16 ***************************************
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
      * Billing Schedule

     c     bsky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey            10
    
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
    
     ** Write the output record
      *** BEGIN DELETE ****** 12/19/16 *************************************
     C***                write     pfm01
      *** END   DELETE ****** 12/19/16 *************************************
     C                   endsr
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
      **** Begin Add *** 12/20/16 ****************************************
      /Free
       RunningSRText = *on;
      /End-Free
      **** End   Add *** 12/20/16 ****************************************
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
    ****** beg add ****** 02/25/10 ************************************
     c     rsky          chain     prs01                              60
     c                   if        (*in60 = *off)
     c                             and (rsjrst <> 'NJ')
    ****** end add ****** 02/25/10 ************************************
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))

     c                   exsr      ck_emp_cx

     c                   if        emp_cx = 'N'
     C                   move      *on           crtfrm
     C                   endif                                                  emp_cx = 'N'
    
     C                   endif                                                  *in60 = *off
    
    ****** beg add ****** 02/25/10 ************************************
     C                   endif                                                  rsjrst <> 'NJ'
    ****** end add ****** 02/25/10 ************************************
     C                   endsr                                                  Valid
    
