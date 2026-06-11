     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
    H/TITLE SR04401A - CA - Anniversary Rating Date Endorsement
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     * WC040401A                                                     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 04/04/18                                          *
     * Programmer: C:62                                              *
     * Project   : WC2586                                           *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 10/28/22 C:333 Project: WC2834                                *
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
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqForm          s             15a   inz('WC040401A ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
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
     *entry parameters
     D SR04401A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04401A        PI
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
    
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
       //***** Beg Add *** 10/28/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/28/22 ******************************
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C                             and elandt <> 0
     *
     c                   move      elandt        xxmmdd            4 0
     c                   movel     elandt        xxcyy             3 0
     c                   movel     xxmmdd        xxmm              2 0
     c                   move      xxmmdd        xxdd              2 0
     c     1900          add       xxcyy         xxccyy            4 0

     c                   eval      fmc101 = mth(xxmm)
     c                   eval      fma701 = xxdd
     c                   eval      fma702 = xxccyy
      /Free
       //* Write the output record
       WriteInCopybook = *Off;
       Exsr WriteWorkFile;
      /End-Free
     c                   leave
     c                   endif

          //***** Beg Add *** 10/28/22 ******************************
             Endif;
          //***** End Add *** 10/28/22 ******************************
     C     elky2         reade     pel02                                  60
     C                   enddo
    
     C                   endsr
    
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
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and rsjrst = enst
     C                             and elandt <> 0
    
     ** Found a division with a matching jurisdiction state
     C                   move      *on           crtfrm
     C                   endif
    
     C                   endsr
    
