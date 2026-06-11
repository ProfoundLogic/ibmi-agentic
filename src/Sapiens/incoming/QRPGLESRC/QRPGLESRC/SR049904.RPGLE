     * (C) Copyright Progressive Data Solutions, Inc., 2004
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE PN049904 - CA - NOTICE REQUIRED BY LAW
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/04/04                                          *
     * Programmer: C:61                                              *
     * Project   : 132WAC                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 07/15/09  C:61  project 139WLR                                *
     *                 check division not cancelled                  *
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

     ** Copybook D specs
     D/COPY SCOPYEN01D
    
      ******* beg add ****** 07/15/09 *********************************

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

      ******* end add ****** 07/15/09 *********************************
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          c                   const('PN049904  ')
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
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      ******* beg add ****** 07/15/09 *********************************
    
     * Employer Master
    
     C     emky1         klist
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elemp#
     C                   KFld                    eldiv
    
      ******* end add ****** 07/15/09 *********************************
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
      ******* beg add ****** 07/15/09 *********************************
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

      ******* end add ****** 07/15/09 *********************************
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** Write the output record
     C                   write     pfm01
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      ******* beg add ****** 07/15/09 *********************************
      *
     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0
      *
      ******* end add ****** 07/15/09 *********************************
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
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = enst
      ******* beg add ****** 07/15/09 *********************************

     * Check that employer division is not cancelled

     c                   exsr      ck_emp_cx

     c                   if        emp_cx = 'N'
      ******* end add ****** 07/15/09 *********************************
    
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
      ******* beg add ****** 07/15/09 *********************************
     C                   endif
    
      ******* end add ****** 07/15/09 *********************************
     C                   endif
    
     C                   endsr
    
