     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR47305 - WV - PEO Exclusion Endorsement
     *===============================================================*
     * WC470305 - WEST VIRGINIA PROFESSIONAL EMPLOYER ORGANIZATION   *
      *            EXCLUSION ENDORSEMENT                              *
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
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC470305  ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
      *entry parameters
     D SR47305         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR47305         PI
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
     *
     c     emky1         klist
     c                   kfld                    emco#
     c                   kfld                    emfnd
     c                   kfld                    xxemp#            9 0
     c                   kfld                    xxdiv             5 0
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
     c     emky1         chain     pem01
     *
     c                   if        %found
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
     c                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     c     pgmend        begsr
     c                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     c     valid         begsr
    
     c                   move      *on           crtfrm
    
     c                   endsr
    
