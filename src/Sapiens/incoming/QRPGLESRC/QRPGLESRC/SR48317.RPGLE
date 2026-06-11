     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H option(*nodebugio:*srcstmt)
    H/TITLE SR48317 - WI Employee Leasing Company Client Exclusion p/a
     *===============================================================*
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *  Print/Attach for Multiple Manual with Additional Input       *
     *  Endorsement (data stored in file specifed for endorsement)   *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
     *                Endorsement Recognition of Cancellation        *
     *                                                               *
     * 01/03/17 C:136 Project: SY371                                 *
     *                Implement Phase 2 - changes not commented      *
     *                                                               *
     * 12/20/10 C:220 PROJECT WC1962                                 *
     *                Created.                                       *
     *                                                               *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*

     Fwledl     if   e           k disk    prefix(p_)

     ** Copybook F specs
     F/COPY SCOPYEN01F


     *=========================================================================
     *  Definition Specs
     *=========================================================================
      **Entry
     D SR48317         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR48317         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1

     D xxdat2          s              8  0
     D x1seq#          s              7  0
     D x1form          s                   like(fmform)
     D                                     inz('WC480317')

     ** Copybook D specs

     D/COPY SCOPYEN01D
     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D qqform          s             15a   inz('WC480317  ')

     ** Additional entry program
     D addpgm          c                   const('SR48317I  ')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     C     edky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    x1form
     C*
     C     edky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C                   kfld                    x1form
     C                   kfld                    x1seq#
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
    
     C     edky          setll     ped01
     C     edky          reade     ped01                                  60
     C                   dow       not %eof
      *
      *       move information to WXFMP fields to print on endorsement
      *
     C                   movel(p)  p_fm2011      fm2011
     C                   movel(p)  p_fm2012      fm2012
     C                   movel(p)  p_fm2013      fm2013
     C                   movel(p)  p_fm2014      fm2014
     C                   movel(p)  p_fm2015      fm2015
     C                   movel(p)  p_fm2016      fm2016
     C                   movel(p)  p_fm2017      fm2017
     C                   movel(p)  p_fm2018      fm2018
     C                   movel(p)  p_fm2019      fm2019
     C                   movel(p)  p_fm2020      fm2020
     C                   movel(p)  p_fm2021      fm2021
     C                   exsr      WriteWorkFile
      *
      * Clear WXFMP data fields used above.  Do NOT clear entire WXFMP
      *       record.  Example:
      *
     C                   clear                   fm2011
     C                   clear                   fm2012
     C                   clear                   fm2013
     C                   clear                   fm2014
     C                   clear                   fm2015
     C                   clear                   fm2016
     C                   clear                   fm2017
     C                   clear                   fm2018
     C                   clear                   fm2019
     C                   clear                   fm2019
     C                   clear                   fm2020
     C                   clear                   fm2021
    
     C     edky          reade     ped01                                  60
     C                   enddo
    
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
      /Free
       Begsr Pgmstr;
       useDfltAddParms = *Off;
       writeInCopyBook = *Off;
       Endsr;
      /End-free
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
