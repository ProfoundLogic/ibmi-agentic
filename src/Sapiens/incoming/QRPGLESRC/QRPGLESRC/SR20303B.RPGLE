      *===============================================================*
     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      *===============================================================*
      * WC200303B - Massachusetts Notice to Policyholder Endorsement  *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 09/14/11                                          *
      * Programmer: C:190                                             *
      * Project   : WC2072                                            *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     f/copy scopyen01f
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     d qqform          c                   const('WC200303B ')

      ** Additional entry program
     d addpgm          c                   const('SRXXXXXXX ')

      ** Work Fields
     d                 ds
     d  xxpzip                 1      9  0
     d  xxpzp1                 1      5
     d  xxpzp2                 6      9
     d                 ds
     d  xxmzip                 1      9  0
     d  xxmzp1                 1      5
     d  xxmzp2                 6      9

     d grpAdr          s             60    dim(5)
     d xxgcsz          s             40    inz(*blanks)
     d adrLn           s              3  0 inz(*zeros)

     d/copy scopyen01d

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     c     *entry        plist
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    qqemp#            9
     c                   parm                    qqdiv             5
     c                   parm                    qqfyr             3
     c                   parm                    qqmode            1
     c                   parm                    qqrrn             9 0
     c                   parm                    qqmod2            1
    
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     c/copy scopyen01c
      *
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     c     othinf        begsr
      *
     c                   movea     *blanks       grpAdr
     c                   eval      fmc601 = *blanks
     c                   eval      fmc402 = *blanks
     c                   eval      fmc403 = *blanks
     c                   eval      fmc404 = *blanks
     c                   eval      fmc405 = *blanks
     c                   eval      adrLn = 1
     c                   movel     fnname        grpAdr(adrLn)
     c*
     c                   if        fnmad1 <> *blanks
     c                   eval      adrLn = adrLn + 1
     c                   movel     fnmad1        grpAdr(adrLn)
     c                   if        fnmad2 <> *blanks
     c                   eval      adrLn = adrLn + 1
     c                   movel     fnmad2        grpAdr(adrLn)
     c                   endif
     c                   eval      xxgcsz = *blanks
     c                   eval      xxmzip = fnmzip
     c                   movel     fnmcty        xxgcsz
     c     xxgcsz        cat       ',':0         xxgcsz
     c     xxgcsz        cat       fnmst:1       xxgcsz
     c     xxgcsz        cat       xxmzp1:2      xxgcsz
     c                   if        xxmzp2 <> '0000'
     c     xxgcsz        cat       '-':0         xxgcsz
     c     xxgcsz        cat       xxmzp2:0      xxgcsz
     c                   endif
     c                   eval      adrLn = adrLn + 1
     c                   movel     xxgcsz        grpAdr(adrLn)
     c*
     c                   else
     c                   eval      adrLn = adrLn + 1
     c                   movea     *blanks       grpAdr(adrLn)
     c                   movel     fnpad1        grpAdr(adrLn)
     c                   if        fnpad2 <> *blanks
     c                   eval      adrLn = adrLn + 1
     c                   movel     fnpad2        grpAdr(adrLn)
     c                   endif
     c                   eval      xxgcsz = *blanks
     c                   eval      xxpzip = fnpzip
     c                   movel     fnpcty        xxgcsz
     c     xxgcsz        cat       ',':0         xxgcsz
     c     xxgcsz        cat       fnpst:1       xxgcsz
     c     xxgcsz        cat       xxpzp1:2      xxgcsz
     c                   if        xxpzp2 <> '0000'
     c     xxgcsz        cat       '-':0         xxgcsz
     c     xxgcsz        cat       xxpzp2:0      xxgcsz
     c                   endif
     c                   eval      adrLn = adrLn + 1
     c                   movel     xxgcsz        grpAdr(adrLn)
     c*
     c                   endif
     c*
     c                   eval      fmc601 = grpAdr(1)
     c                   eval      fmc402 = grpAdr(2)
     c                   eval      fmc403 = grpAdr(3)
     c                   eval      fmc404 = grpAdr(4)
     c                   eval      fmc405 = grpAdr(5)
      *
      ** Write the output record
     c                   write     pfm01
      *
     c                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     c     pgmstr        begsr
      *
      *
     c                   endsr                                                  PGMSTR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     c     pgmend        begsr
      *
      *
     c                   endsr                                                  PGMEND
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     c     valid         begsr
      *
     c     rsky          chain     prs01                              60
      *
     c     *in60         ifeq      *off
     c     rsjrst        andeq     'MA'
      *
      ** Found a division with a matching jurisdiction state
     c                   move      *on           crtfrm
     c                   endif                                                  *IN60 = *OFF
      *
     c                   endsr                                                  VALID
