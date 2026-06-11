     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR47303 - WV - PEO Extension Endorsement
     *===============================================================*
     * WC470303 - WEST VIRGINIA PROFESSIONAL EMPLOYER ORGANIZATION   *
      *            EXTENSION ENDORSEMENT                              *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 12/19/11                                          *
     * PROGRAMMER: C:282                                             *
     * PROJECT   : WC2094                                            *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     *  10/19/22 C:342 Project: WC2834 (Recompile)                   *
     *                 Endorsement Recognition of Cancellation       *
     *                                                               *
     *  10/06/17 C:136 Project: 150WBL                               *
     *                 PEO attachment criteria moved to copy code    *
     *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *                                                                        *
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
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
      *
     d PEO_nam1        s            100
     d PEO_nam2        s            100
     d PEO_ead1        s             50
     d PEO_ead2        s             50
     d PEO_cty         s             25
     d PEO_st          s              2
     d                 ds
     d PEO_zip                 1      9  0
     d PEO_zip1                1      5  0
     d PEO_zip2                6      9  0
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
      *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC470303  ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
      *entry parameters
     D SR47303         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR47303         PI
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
     * Employer Master
     *
     C     emky1         klist
     C                   KFld                    emco#
     C                   KFld                    emfnd
     C                   KFld                    ememp#
     C                   KFld                    emdiv
      *
     c     emky3         klist
     c                   kfld                    emco#
     c                   kfld                    emfnd
     c                   kfld                    xxemp#            9 0
     c                   kfld                    xxdiv             5 0
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
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
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     *
     c                   clear                   fm2001
     c                   clear                   fm2002
     c                   clear                   fmnm01
     c                   clear                   fmnm02
     c                   clear                   fmnm03
     *
     c                   eval      xxemp# = 1
     c                   eval      xxdiv  = 0
     *
     c     emky3         chain     pem01
     *
     c                   if        %found
      *
     c                   eval      PEO_nam1 = emname + emnam2
     c                   eval      PEO_nam2 = emfdnm + emfdn2
      *
     c                   if        (emmpfl = 'P')
      * Physical Address
     c                   eval      PEO_ead1 = empad1
     c                   eval      PEO_ead2 = empad2
     c                   eval      PEO_cty  = empcty
     c                   eval      PEO_st   = empst
     c                   eval      PEO_zip  = empzip

     c                   else
      * Mailing Address
     c                   eval      PEO_ead1 = emmad1
     c                   eval      PEO_ead2 = emmad2
     c                   eval      PEO_cty  = emmcty
     c                   eval      PEO_st   = emmst
     c                   eval      PEO_zip  = emmzip
     c                   endif
     *
     c                   endif
     *
     c                   eval      fm2001 = %trim(PEO_nam1)
     c                   eval      fm2002 = %trim(PEO_nam2)
     c                   eval      fmnm01 = %trim(PEO_ead1)
     c                   eval      fmnm02 = %trim(PEO_ead2)
     c                   eval      fmnm03 = %trim(PEO_cty) + ',' + ' ' +  PEO_st
     c                                       + ' '  + %char(PEO_zip1) + '-' +
     c                                      %char(PEO_zip2)
      *
     c                   if        fmnm02 = *blanks
     c                   eval      fmnm02 = fmnm03
     c                   eval      fmnm03 = *blanks
     c                   endif
      *
     c                   endsr


     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     c     pgmstr        begsr
      *
     c                   time                    dsdttm
     c                   Eval      srdti = dsdate
     c                   Call      'SRCVTDAT'    dtcymd
     c                   MoveL     srdto         sysdat            7 0
      *
     c                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     c     pgmend        begsr
     c                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     c     valid         begsr
     *
     * Check that employer division is not cancelled
     *
     c                   exsr      ck_emp_cx
     c                   if        emp_cx = 'N'
     c                   move      *on           crtfrm
     c                   endif                                                  emp_cx = 'N'

     c                   endsr
