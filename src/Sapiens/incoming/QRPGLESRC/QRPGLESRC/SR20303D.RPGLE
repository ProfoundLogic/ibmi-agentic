     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      //===============================================================*
      // WC200303D - Massachusetts Notice to Policyholder Endorsement  *
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 11/03/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /copy scopyen01f

     d qqForm          s             15A   Inz('WC200303D ')

     d addpgm          c                   const('SRXXXXXXX ')

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


     D SR20303D        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR20303D        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1


      /copy scopyen01d
      /copy scopyen01c

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     c     othinf        begsr

     c                   movea     *blanks       grpAdr
     c                   eval      fmc601 = *blanks
     c                   eval      fmc402 = *blanks
     c                   eval      fmc403 = *blanks
     c                   eval      fmc404 = *blanks
     c                   eval      fmc405 = *blanks
     c                   eval      adrLn = 1
     c                   movel     fnname        grpAdr(adrLn)

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

     c                   endif

     c                   eval      fmc601 = grpAdr(1)
     c                   eval      fmc402 = grpAdr(2)
     c                   eval      fmc403 = grpAdr(3)
     c                   eval      fmc404 = grpAdr(4)
     c                   eval      fmc405 = grpAdr(5)

     c                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     c     pgmstr        begsr
     c                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     c     pgmend        begsr
     c                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     c     valid         begsr
     c                   move      *on           crtFrm
     c                   endsr
