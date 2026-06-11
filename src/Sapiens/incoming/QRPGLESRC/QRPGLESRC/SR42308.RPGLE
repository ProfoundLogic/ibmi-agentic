     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

    H/TITLE SR42308 - TX - Partners, Officers and Other Exclustion Endorsement
     *===============================================================*
     * WC420308 - Partners, Officers and Others Exclusion Endorsement*
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 11/10/97                                          *
     * PROGRAMMER: C:57                                              *
     * PROJECT   : CM027                                             *
     *                                                               *
     * REWRITE   : 12/02/99                                          *
     * PROGRAMMER: C:113                                             *
     * PROJECT   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 10/03/18 C:65  Project: Phw#71991 (Luba)                      *
      *                Group different types of business into the     *
     *                officer/other/partner categories.              *
     *                                                               *
     * 04/10/18 C:65  Project: Phw#49503 (Luba)                      *
      *                Write out a separate type of business for each *
     *                name - since names can belong to different     *
     *                types of business on a policy.                 *
     *                                                               *
     * 09/15/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
     *                                                               *
     * 10/04/10 C:061 Project: WC1913 PRISS 14                       *
     *              - Only Excluded Owner/Officers associated with
     *                TX division to print on WC040302A.
     *              - Only if 'Type of Business' <> '1'.
     *                                                               *
     * 06/30/10 C:220 Project: WC1913                                *
      *                In routine VALID: replace type of business     *
     *                condition                                      *
     *                                                               *
     * 02/05/09 C:61  SUPPORT WA133 67025                            *
     *                Print E/L Owner/Officers if E/L deletion date
     *                is non-zero or less that compare date.
     *                                                               *
     *  01/23/02 C:09  Project: CM027  (Seq #30152)                  *
     *                 Correct date format.                          *
     *                                                               *
     *  06/23/00 C:90  PROJECT: SUPPORT: #23310                      *
     *                 Compare entity deletion date against end.     *
     *                 effective date to determine if owner/officer  *
     *                 should be included.                           *
     *                                                               *
     *  01/12/00 C:113 PROJECT: SUPPORT #21390                       *
     *                 Corrected the conditioning on attaching       *
     *                 owner/officers.                               *
     *                                                               *
     *  12/02/99 C:113 PROJECT: CM027                                *
     *                 Rewrite to utilize RPGIV and endorsement      *
     *                 copybook.  Also, allow for inclusion of       *
     *                 Sole Proprietor types of business.            *
     *                                                               *
     *  10/06/99 C:113 PROJECT: SUPPORT #19795                       *
     *                 Changed to recognize Entity Owner/Officers.   *
     *                                                               *
     *  05/27/99 C:113 PROJECT: SUPPORT #17992                       *
     *                 Changed to allow for types of business > 4.   *
     *                                                               *
     *  04/29/99 C:113 PROJECT: SUPPORT #17874                       *
     *                 Look at endorsement date for cancel date if   *
     *                 called in midterm mode.                       *
     *                                                               *
     *  04/13/99 C:09 PROJECT SUPPORT (WA048/SEQ #17506)             *
     *                LOAD OWNER/OFFICER INFO FROM ALL DIVISIONS BUT *
     *                AVOID DUPLICATES.  ALSO, IGNORE CORPORATION    *
     *                FLAG, THIS IS NOT BEING USED ANYMORE.          *
     *                ONLY INCLUDE 'TX' OWNERS/OFFICERS              *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for mid-term processing.                 *
     *===============================================================*
      ** Owner/Officer Master File
     Fwmool     if   e           k disk
      ** Additional Entity/Location File
     Fwmall     if   e           k disk

     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================
     ** Copybook D specs
     D/COPY SCOPYEN01D
    
     D include         s              1
     D ix              s              2  0
     D qq308           s              1
     D x               s              2  0
     D xxnm46          s             46
     D xxtpob          s              1  0

     D xxCompDate      s                   like(ELPFDT)

     D                 ds
     D  nm                     1    460
     D                                     dim(10)
     D  FMNM01                 1     46
     D  FMNM02                47     92
     D  FMNM03                93    138
     D  FMNM04               139    184
     D  FMNM05               185    230
     D  FMNM06               231    276
     D  FMNM07               277    322
     D  FMNM08               323    368
     D  FMNM09               369    414
     D  FMNM10               415    460
     D*** begin add ***  04/10/18  ***********************************
     D                 ds
     D  tob                    1     10
     D                                     dim(10)
     D  FMFL01                 1      1
     D  FMFL02                 2      2
     D  FMFL03                 3      3
     D  FMFL04                 4      4
     D  FMFL05                 5      5
     D  FMFL06                 6      6
     D  FMFL07                 7      7
     D  FMFL08                 8      8
     D  FMFL09                 9      9
     D  FMFL10                10     10
     D*** end add *****  04/10/18  ***********************************
     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC420308  ')
    
     ** Additional entry program
     D addpgm          c                   const('SR00000I  ')
    
      *entry parameters
     D SR42308         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR42308         PI
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
     *  Employer Master file

     C     emky1         KList
     C                   KFld                    elco#
     C                   KFld                    elfnd
     C                   KFld                    elemp#
     C                   KFld                    eldiv
    
     ** Owner/Officer Master file
     C     ooKey         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     ** Additional Entity/Location file
     C     alKey         klist
     C                   kfld                    OOCO#
     C                   kfld                    OOFND
     C                   kfld                    OOEMP#
     C                   kfld                    OODIV
     C                   kfld                    OOELID
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C

     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
    
     C                   eval      ix = *zero
     C*** begin add ***  04/10/18  ***********************************
     C                   clear                   tob
     C*** end add *****  04/10/18  ***********************************
    
      ** Check all divisions for Texas exposure
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
     C     emky1         Chain     pem01

     c                   if        %found
     C     rsky          chain     prs01                              60
    
     C                   if        RSJRST = ENST
    
     ** Look for a qualifying "EXCLUDED" Owner/Officer record.
     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60
    
     C                   dow       *in60 = *off
    
     C                   if        OOEXFL = 'E'
     C                             and OOSTAT <> 'I'
    
     ** If called in midterm mode, use the endorsement date to check against, otherwise
     ** use the policy effective date.
     C                   move      *off          include
     C                   if        qqmode = '4'
     *
     C                   Eval      srdti = ldeedt
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         xxCompDate
     C                   else
     C                   move      ELPFDT        xxCompDate
     C                   endif

     C                   if        OOEFDT <= xxCompDate
     C                             and OOEXDT > xxCompDate
     C                             or OOEFDT <= xxCompDate
     C                             and OOEXDT = *zeros
     C                   move      *on           include
     C                   endif                                                  OOEFDT = xxCompDate
     C                   if        OOELID <> *blanks
     C                             and OOELID <> *zeros
     C     alKey         chain     pal01                              60
     C                   if        (ALDLDT <= xxCompDate) and
     C                             (ALDLDT <> 0)
     C                   move      *off          include
     C                   endif                                                  OOEFDT = xxCompDate
     C                   endif

     C                   if        include = *on

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm46
     C                   eval      x = 1
     C     xxnm46        lookup    nm(x)                                  65
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm46        nm(ix)
     C*** begin add ***  04/10/18  ***********************************
     C                   If        OOELID <> *blanks
     C                             and OOELID <> *zeros
     C                   Eval      tob(ix) = altpob
     C                   Else
     C                   Eval      tob(ix) = emtpob
     C                   Endif
     C* Group sole proprietor into "Other".
     C                   If        tob(ix) = '1'
     C                   Eval      tob(ix) = '4'
     C                   Endif                                                  EMTPOB = '1'
     C*** begin add ***  10/03/18  ***********************************
     C* Group LLC into "Other".
     C                   If        tob(ix) = 'A'
     C                   Eval      tob(ix) = '4'
     C                   Endif                                                  EMTPOB = '1'
     C* Group LLP into "Partnership".
     C                   If        tob(ix) = 'D'
     C                   Eval      tob(ix) = '2'
     C                   Endif                                                  EMTPOB = '1'
     C                   If        tob(ix) <> '2' and tob(ix) <> '3'
     C                             and tob(ix) <> '4'
     C                   Eval      tob(ix) = '4'
     C                   Endif
     C*** end add *****  10/03/18  ***********************************
     C*** end add *****  04/10/18  ***********************************

      ** This is so the name will print correctly on the form
     C                   if        EMTPOB = '1'
     C                   eval      FMTOBC = '4'
     C                   endif                                                  EMTPOB = '1'
    
     ** If the page is full, write and setup for more names
     C                   if        ix = 10
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm
     C*** begin add ***  04/10/18  ***********************************
     C                   clear                   tob
     C*** end add *****  04/10/18  ***********************************
     C                   eval      ix = *zero
     C                   endif                                                  ix = 10
     C                   endif                                                  *in65 = *off

     C                   endif                                                  include = *on

     C                   endif                                                  OOEXFL = 'I'
    
     C     ooKey         reade     poo01                                  60
     C                   enddo                                                  *in60 = *off
    
     C                   endif                                                  RSJRST = ENST

     C                   endif

     C     elky2         reade     pel02                                  60
     C                   enddo
    
     ** Write the output record.
     C                   if        ix <> *zero
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   endif
    
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
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

     C     emky1         Chain     pem01

     c                   if        %found
    
     ** Get Rate Set description
     C     rsky          chain     prs01                              60
     C                   if        RSJRST = ENST
    
     ** Look for a qualifying "EXCLUDED" Owner/Officer record.
     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60
    
     C                   dow       *in60 = *off
    
     C                   if        OOEXFL = 'E'
     C                             and OOSTAT <> 'I'
    
     ** If called in midterm mode, use the endorsement date to check against, otherwise
     ** use the policy effective date.
     C                   if        qqmode = '4'
     C                   move      *off          qq308
      *
     C                   Eval      srdti = ldeedt
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         xxCompDate
     C                   else
     C                   move      ELPFDT        xxCompDate
     C                   endif

     C                   if        OOEFDT <= xxCompDate
     C                             and OOEXDT > xxCompDate
     C                             or OOEFDT <= xxCompDate
     C                             and OOEXDT = *zeros
     C                   if        emtpob <> '1'
     C                   if        OOELID <> *blanks
     C                             and OOELID <> *zeros
     C     alKey         chain     pal01                              60
     C                   if        ALDLDT > xxCompDate
     C                             or ALDLDT = *zeros
     C                   move      *on           crtfrm
     C                   endif                                                  OOEFDT = xxCompDate
     C                   else
     C                   move      *on           crtfrm
     C                   endif
     C                   endif                                                   emtpob <> '1'

     C                   if        qqmode = '4'
     C                   move      *on           qq308
     C                   endif                                                  qqmode = '4'

     C                   leave
     C                   endif                                                  OOEFDT = xxCompDate

     C                   endif                                                  OOEXFL = 'E'

     C     ooKey         reade     poo01                                  60
     C                   enddo                                                  *in60 = *on
    
     C                   endif                                                  RSJRST = ENST

     C                   endif
    
     C                   endsr
