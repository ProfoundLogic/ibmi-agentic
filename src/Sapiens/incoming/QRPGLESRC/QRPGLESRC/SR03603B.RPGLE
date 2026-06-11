     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR03603B - AR Contract Hauling Warranty Endorsement P/A
      *===============================================================*
      * Program Description:                                          *
      *                                                               *
      *  Print/Attach for Multiple Manual with Additional Input       *
      *  Endorsement (data stored in file specifed for endorsement)   *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * Revisions:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 10/27/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      *                                                               *
      *  qqmode = 1 - Called to determine if form should be attached. *
      *  qqmode = 2 - Called to print endorsement.                    *
      *  qqmode = 3 - Called for additional input.                    *
      *  qqmode = 4 - Called for midterm processing.                  *
      *===============================================================*
      * Endorsement data
     Fwledl     if   e           k disk    prefix(p_)

      /Copy SCOPYEN01F

     d kkco#           s                   like(fmco#)
     d kkfnd           s                   like(fmfnd)
     d kkemp#          s                   like(fmemp#)
     d kkdiv           s                   like(fmdiv)
     d kkfyr           s                   like(fmfyr)

      /Copy SCOPYEN01D
     D qqform          s             15A   Inz('WC030603B ')

     D addpgm          c                   const('SR03603BI ')

     D SR03603B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR03603B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1


     C     edky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    xxform

      /COPY SCOPYEN01C


      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr

     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqemp#        kkemp#
     C                   move      qqdiv         kkdiv
     C                   move      qqfyr         kkfyr

     C     edky          setll     ped01
     C     edky          reade     ped01                                  60

     C                   dow       not %eof
      *       move information to WXFMP fields to print on endorsement
     c                   movel(p)  p_fm2001      fm2001
     c                   movel(p)  p_fm2002      fm2002
     c                   movel(p)  p_fm2003      fm2003
     c                   movel(p)  p_fm2004      fm2004
     c                   movel(p)  p_fm2005      fm2005
     c                   movel(p)  p_fm2006      fm2006
     c                   movel(p)  p_fm2007      fm2007
     c                   movel(p)  p_fm2008      fm2008
     c                   movel(p)  p_fm2009      fm2009

     c                   movel(p)  p_fm2011      fm2011
     c                   movel(p)  p_fm2012      fm2012
     c                   movel(p)  p_fm2013      fm2013
     c                   movel(p)  p_fm2014      fm2014
     c                   movel(p)  p_fm2015      fm2015
     c                   movel(p)  p_fm2016      fm2016
     c                   movel(p)  p_fm2017      fm2017
     c                   movel(p)  p_fm2018      fm2018
     c                   movel(p)  p_fm2019      fm2019
      /Free
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free
      *       clear WXFMP data fields used above.  Do NOT clear entire WXFMP
      *       record.
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
     C                   clear                   fm2017
     C                   clear                   fm2018
     C                   clear                   fm2019

     C     edky          reade     ped01                                  60
     C                   enddo

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        Begsr
      /Free
          UseDfltAddparms = *Off;
      /End-Free
     C                   Endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr
     C                   move      *on           crtfrm
     C                   endsr
