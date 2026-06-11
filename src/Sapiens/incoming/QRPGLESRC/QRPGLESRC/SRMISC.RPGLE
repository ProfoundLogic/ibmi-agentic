     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)

    H/TITLE SRmisc - clt139 - Miscellaneous Change End Print/Attach
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
     * 10/13/09 C:61  PROJECT 139WLR                                 *
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

      * Miscellaneous Change
     Fwlmsl     if   e           k disk
     Fwlmsl1    if   e           k disk
     F                                     rename(pms01:pms02)

     ** Copybook F specs
     F/COPY SCOPYEN01F


     *=========================================================================
     *  Definition Specs
     *=========================================================================

     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##view          s              1  0

     D xxcrtf          s              1


     ** Copybook D specs

     D/COPY SCOPYEN01D


     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D xxform          c                   const('WC990301  ')

     ** Additional entry program
     D addpgm          c                   const('SRMISCI   ')


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr
     C                   parm                    qqmode
     C                   parm                    qqcrtf            1
    
     ** Additional input program parameters
    
     C     addpl         pList
     C                   parm      *off          ##kc
     C                   parm      *off          ##kl
     C                   parm      *blank        ##call
      *                                     additional input data file
     C                   parm      0             ##rrn
      *                                     wdelp
     C                   parm      elrrn         ##rrn2
     C                   parm      q1pgm         ##pgm            10            Program Name


     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     C     MSKY          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#

     C     MSKY1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    ldend#
      *


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
    
     C                   z-add     1             i                 1 0
      *

     C     MSKY1         setll     pMS02
     C     MSKY1         reade     pMS02
    
     C                   dow       not %eof
      *
      * move information to WXFMP fields to print on endorsement
      *
     C                   movel(p)  MSL001        fm2001
     C                   movel(p)  MSL002        fm2002
     C                   movel(p)  MSL003        fm2003
     C                   movel(p)  MSL004        fm2004
     C                   movel(p)  MSL005        fm2005
     C                   movel(p)  MSL006        fm2006
     C                   movel(p)  MSL007        fm2007
     C                   movel(p)  MSL008        fm2008
     C                   movel(p)  MSL009        fm2009
     C                   movel(p)  MSL010        fm2010
     C                   movel(p)  MSL011        fm2011
     C                   movel(p)  MSL012        fm2012
     C                   movel(p)  MSL013        fm2013
     C                   movel(p)  MSL014        fm2014
     C                   movel(p)  MSL015        fm2015
     C                   movel(p)  MSL016        fm2016
     C                   movel(p)  MSL017        fm2017
     C                   movel(p)  MSL018        fm2018
     C                   movel(p)  MSL019        fm2019
     C                   movel(p)  MSL020        fm2020
     C                   movel(p)  MSL021        fm2021
     C                   movel(p)  MSL022        fm2022
     C                   movel(p)  MSL023        fm2023
     C                   movel(p)  MSL024        fm2024
     C                   movel(p)  MSL025        fm2025
     C                   movel(p)  MSL026        fm2026
     C                   movel(p)  MSL027        fm2027
     C                   movel(p)  MSL028        fm2028
     C                   movel(p)  MSL029        fm2029
     C                   movel(p)  MSL030        fm2030
     C                   movel(p)  MSL031        fm2031
     C                   movel(p)  MSL032        fm2032
     C                   movel(p)  MSL033        fm2033
     C                   movel(p)  MSL034        fm2034
     C                   movel(p)  MSL035        fm2035
     C                   movel(p)  MSL036        fm2036
     C                   movel(p)  MSL037        fm2037
     C                   movel(p)  MSL038        fm2038
     C                   movel(p)  MSL039        fm2039
    
     C                   write     pfm01
      *
      * clear WXFMP data fields used above.
      *
     C                   clear                   fm2001
     C                   clear                   fm2002
     C                   clear                   fm2003
     C                   clear                   fm2004
     C                   clear                   fm2005
     C                   clear                   fm2006
     C                   clear                   fm2007
     C                   clear                   fm2008
     C                   clear                   fm2009
     C                   clear                   fm2010
     C                   clear                   fm2011
     C                   clear                   fm2012
     C                   clear                   fm2013
     C                   clear                   fm2014
     C                   clear                   fm2015
     C                   clear                   fm2016
     C                   clear                   fm2017
     C                   clear                   fm2018
     C                   clear                   fm2019
     C                   clear                   fm2020
     C                   clear                   fm2021
     C                   clear                   fm2022
     C                   clear                   fm2023
     C                   clear                   fm2024
     C                   clear                   fm2025
     C                   clear                   fm2026
     C                   clear                   fm2027
     C                   clear                   fm2028
     C                   clear                   fm2029
     C                   clear                   fm2030
     C                   clear                   fm2031
     C                   clear                   fm2032
     C                   clear                   fm2033
     C                   clear                   fm2034
     C                   clear                   fm2035
     C                   clear                   fm2036
     C                   clear                   fm2037
     C                   clear                   fm2038
     C                   clear                   fm2039
    
     C     MSKY1         reade     pMS02
     C                   enddo
    
     C                   endsr


     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     c     elky          chain     pel01
     c                   eval      xxcrtf = *off

     c                   if        ldenda = *blanks
     c                   eval      ldend# = 0
     c                   endif

     C                   endsr


     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr

     c                   if        (q1prms >= 7)
     c                   eval      qqcrtf = crtfrm
     c                   endif

     C                   endsr


     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
      ** This is a manual attach endorsement with additional input that can
      ** be called as a mid-term endorsement

     C                   select
     c                   When      qqmode = '4'
      *
     C     msky1         chain     pms02
      *
     C                   if        (%found) and (q1prms >= 7)
     c                   eval      crtfrm = *on
     c                   eval      xxcrtf = *on
     C                   endif
      *
     c                   other
     c                   move      *on           crtfrm
     c                   endsl
    
     C                   endsr

