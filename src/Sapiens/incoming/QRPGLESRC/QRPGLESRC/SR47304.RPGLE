     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR47304 - WV - PEO Client Company Exclusion Endorsement
     *===============================================================*
     * WC470304 - WEST VIRGINIA PROFESSIONAL EMPLOYER ORGANIZATION   *
      *            CLIENT COMPANY EXCLUSION ENDORSEMENT               *
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
     *  10/19/22 C:342 Project: WC2834                               *
     *                 Endorsement Recognition of Cancellation       *
     *                                                               *
     *  10/06/17 C:136 Project: 150WBL                               *
     *                 PEO attachment criteria moved to copy code    *
     *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
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
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
      *
     d xxflag          s              1
     d CLT_nam1        s            100
     d CLT_nam2        s            100
     d CLT_ead1        s             50
     d CLT_ead2        s             50
     d CLT_cty         s             25
     d CLT_st          s              2
     d                 ds
     d CLT_zip                 1      9  0
     d CLT_zip1                1      5  0
     d CLT_zip2                6      9  0
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
     D qqForm          s             15a   inz('WC470304  ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
      *entry parameters
     D SR47304         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR47304         PI
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
    
     * Employer Master
     *
     c     emky1         klist
     c                   KFld                    emco#
     c                   KFld                    emfnd
     c                   KFld                    ememp#
     c                   KFld                    emdiv
     *
     c     emky3         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elemp#
     c                   KFld                    eldiv
     *
     c     elky1         klist
     c                   KFld                    elco#
     c                   KFld                    elfnd
     c                   KFld                    elfyr
     c                   KFld                    elemp#
     c*                  KFld                    eldiv
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

     c                   z-add     5             x                 3 0

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
     c     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
     *
     c                   clear                   fm2001
     c                   clear                   fm2002
     c                   clear                   fmnm01
     c                   clear                   fmnm02
     c                   clear                   fmnm03
      *
     c                   open      wmfnl
     c     fnky          chain     pfn01                              60
     * Check that PEO policy group is not blank.
     c                   if        *in60 = *off and
     c                             fnpeof <> *blanks
      *
     c     elky1         setll     pel02
     c     elky1         reade     pel02
     c                   dow       not %eof(wdell1)
           //***** Beg Add *** 10/19/22 ******************************
           xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
           If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
           //***** End Add *** 10/19/22 ******************************
      *
     c                   eval      xxflag = ' '
      *
     c                   if        (elemp# <> 1) or
     c                             (eldiv  <> 0)
      * Employee leasing Id
     c     emky3         chain     pem01
     c                   if        (%found) and (emlsid = '2')
     ** Found a division with a matching jurisdiction state
     c     rsky          chain     prs01                              60
     c                   if        *in60 = *off
     c                             and rsjrst = 'WV'
     c                   eval      xxflag = 'Y'
      *
     c                   endif                                                  *in60 = *off
      *                                                                         and rsjrst = 'WV'
     c                   endif                                                  (%found) and (emlsid
      *                                                                         = '2')
     c                   endif                                                  (elemp# <> 1) or
     *
      *
     c                   if        xxflag = 'Y'
      *
     c     emky3         chain     pem01

     c                   if        %found
      *
     c                   eval      CLT_nam1 = emname + emnam2
     c                   eval      CLT_nam2 = emfdnm + emfdn2
      *
     c                   if        (emmpfl = 'P')
      * Physical Address
     c                   eval      CLT_ead1 = empad1
     c                   eval      CLT_ead2 = empad2
     c                   eval      CLT_cty  = empcty
     c                   eval      CLT_st   = empst
     c                   eval      CLT_zip  = empzip

     c                   else
      * Mailing Address
     c                   eval      CLT_ead1 = emmad1
     c                   eval      CLT_ead2 = emmad2
     c                   eval      CLT_cty  = emmcty
     c                   eval      CLT_st   = emmst
     c                   eval      CLT_zip  = emmzip
     c                   endif
      *
     c                   eval      fm2001 = %trim(CLT_nam1)
     c                   eval      fm2002 = %trim(CLT_nam2)
     c                   eval      fmnm01 = %trim(CLT_ead1)
     c                   eval      fmnm02 = %trim(CLT_ead2)
     c                   eval      fmnm03 = %trim(CLT_cty) + ',' + ' ' +  CLT_st
     c                                       + ' '  + %char(CLT_zip1) + '-' +
     c                                      %char(CLT_zip2)
      *
     c                   if        fmnm02 = *blanks
     c                   eval      fmnm02 = fmnm03
     c                   eval      fmnm03 = *blanks
     c                   endif
      *
     c                   endif
      *
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
      *
     c                   endif
      *
           //***** Beg Add *** 10/19/22 ******************************
           Endif;
           //***** End Add *** 10/19/22 ******************************
     c     elky1         reade     pel02
     c                   enddo
      *
     c                   endif                                                  *in60 = *off and
     *                                                                         fnpeof <> *blanks
     c                   close     wmfnl
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
     c     Valid         Begsr
      *
      * Employee leasing Id
      *
     c                   Exsr      ck_emp_cx
     c                   If        emp_cx = 'N' and EmLSID = '2'
     c                   Move      *On           crtFrm
     c                   Endif                                                  emp_cx = 'N'
      *
     c                   Endsr
