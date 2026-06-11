     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *
    H/TITLE SR31403A - NY - PPO Premium Endorsement
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
     * Created   : 02/22/12                                          *
     * Programmer: C:282                                             *
      * Project   : WC2127
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
      * 10/19/22 C:342 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
     *                                                               *
      * 01/27/17  C:180  SUPPORT 45431                                *
      *                  Fix error that prevented endorsement to      *
      *                  generate on policy page.                     *
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

      * Endorsement data
     Fwledl     if   e           k disk    prefix(p_)

     ** Copybook F specs
     F/COPY SCOPYEN01F

     *=========================================================================
     *  Definition Specs
     *=========================================================================

     d kkco#           s                   like(fmco#)
     d kkfnd           s                   like(fmfnd)
     d kkemp#          s                   like(fmemp#)
     d kkdiv           s                   like(fmdiv)
     d kkfyr           s                   like(fmfyr)

     ** Copybook D specs

     D/COPY SCOPYEN01D


     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqForm          s             15a   inz('WC310403A ')

     ** Additional entry program
     D addpgm          c                   const('SR31403AI ')

      *entry parameters
     D SR31403A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR31403A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     C     edky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    xxForm


     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C


     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
    
     C                   z-add     1             i
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqemp#        kkemp#
     C                   move      qqdiv         kkdiv
     C                   move      qqfyr         kkfyr
      *
     C     edky          setll     ped01
     C     edky          reade     ped01                                  60
    
     C                   dow       not %eof
      *
      *       move information to WXFMP fields to print on endorsement
      * Populate Premium Reduction %
     c                   movel(p)  p_fma521      fma521
      * Populate Insured Name, Address(es)
     c                   movel(p)  p_fm2001      fm2001
     c                   movel(p)  p_fm2002      fm2002
     c                   movel(p)  p_fm2003      fm2003
     c                   movel(p)  p_fm2004      fm2004
     c                   movel(p)  p_fm2005      fm2005
     c                   movel(p)  p_fm2006      fm2006
     c                   movel(p)  p_fm2007      fm2007
     c                   movel(p)  p_fm2008      fm2008
     c                   movel(p)  p_fm2009      fm2009
     *
      * Populate PPO Name(s)
     c                   movel(p)  p_fm2011      fm2011
     c                   movel(p)  p_fm2012      fm2012
     c                   movel(p)  p_fm2013      fm2013
     c                   movel(p)  p_fm2014      fm2014
     c                   movel(p)  p_fm2015      fm2015
     c                   movel(p)  p_fm2016      fm2016
      * Populate Effective Date of Employers participation
     c                   movel(p)  p_fmdt01      fmdt01
      * Populate Union Employees
     c                   if        p_fmfl01 = 'Y'
     c                   movel(p)  'X'           fmfl01
     c                   else
     c                   movel(p)  'X'           fmfl03
     c                   endif
      * Populate Union Employees in program
     c                   if        p_fmfl02 = 'Y'
     c                   movel(p)  'X'           fmfl02
     c                   else
     c                   movel(p)  'X'           fmfl04
     c                   endif
      * Total Estimated Number of Employees covered by PPO.
     c                   movel(p)  p_fma701      fma701
     *
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     *
      *       clear WXFMP data fields used above.  Do NOT clear entire WXFMP
      *       record.
      *
     C                   clear                   fma521
     C                   clear                   fm2001
     C                   clear                   fm2002
     C                   clear                   fm2003
     C                   clear                   fm2004
     C                   clear                   fm2005
     C                   clear                   fm2006
     C                   clear                   fm2007
     C                   clear                   fm2008
     C                   clear                   fm2009
     C                   clear                   fm2011
     C                   clear                   fm2012
     C                   clear                   fm2013
     C                   clear                   fm2014
     C                   clear                   fm2015
     C                   clear                   fm2016
     C                   clear                   fmdt01
     C                   clear                   fmfl01
     C                   clear                   fmfl02
     C                   clear                   fmfl03
     C                   clear                   fmfl04
     C                   clear                   fma701
      *
     C     edky          reade     ped01                                  60
     C                   enddo
    
     C                   endsr


     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      /Free
       UseDfltAddParms = *Off;
      /End-Free
     C                   endsr


     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr


     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
      ** This is a manual attach endorsement with additional input

     C                   move      *on           crtfrm
    
     C                   endsr


