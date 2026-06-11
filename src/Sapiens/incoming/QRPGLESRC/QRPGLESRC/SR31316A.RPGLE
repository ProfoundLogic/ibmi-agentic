     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
    H/TITLE SR31316A - NY - Sole Prop & Partners Exclusion Endorsement
     *===============================================================*
     * WC310316A - NY Sole Prop and Partners Exclusion Endorsement   *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 05/26/11                                          *
     * PROGRAMMER: C:235                                             *
     * PROJECT   : WC2018                                            *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
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
     D iy              s              2  0
     D iz              s              2  0
     D x               s              2  0
     D y               s              2  0
     D z               s              2  0
     D xxnm200         s            200
     D xxtpob          s              1  0

     D xxCompDate      s                   like(ELPFDT)

     D                 ds
     D  nm                     1   1000
     D                                     dim(5)
     D  FM2001                 1    200
     D  FM2002               201    400
     D  FM2003               401    600
     D  FM2004               601    800
     D  FM2005               801   1000
     D                 ds
     D  nm1                    1   1000
     D                                     dim(5)
     D  FM2006                 1    200
     D  FM2007               201    400
     D  FM2008               401    600
     D  FM2009               601    800
     D  FM2010               801   1000
     D                 ds
     D  nm2                    1   1000
     D                                     dim(5)
     D  FM2011                 1    200
     D  FM2012               201    400
     D  FM2013               401    600
     D  FM2014               601    800
     D  FM2015               801   1000
     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D qqForm          s             15a   inz('WC310316A ')
    
     ** Additional entry program
     D addpgm          c                   const('SRxxxxxxxx')

      *entry parameters
     D SR31316A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1
     D SR31316A        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq316                         1
     D* Needed for copybook
     D  qqRrn          s              9  0
     D  qqMod2         s              1
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
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
    
     ** Employer Master file
     C     emKey         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C

     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
    
      /Free
          WriteInCopybook = *Off;
      /End-Free
      *
     C                   eval      ix = *zero
     C                   eval      iy = *zero
     C                   eval      iz = *zero
      *
     C     elky2         chain     pel02                              60
     C                   dow       *in60 = *off
     C     emkey         chain     pem01                              60
      * If type of business is Sole Prop or Partner, Then process
      *
     C                   if        EMTPOB = '1' or EMTPOB = '2' or
     C                             EMTPOB = 'A' or EMTPOB = 'D'
      *
    
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
      *
     C                   move      ldeedt        xxCompDate
     C                   else
     C                   move      ELPFDT        xxCompDate
     C                   endif


     C                   if        OOEFDT <= xxCompDate
     C                             and OOEXDT > xxCompDate
     C                             or OOEFDT <= xxCompDate
     C                             and OOEXDT = *zeros
     C                   move      *on           include
     C                   endif                                                  OOEFDT = xxCompDate
      *
     C                   if        OOELID <> *blanks
     C                             and OOELID <> *zeros
     C     alKey         chain     pal01                              60
     C                   if        ALDLDT <= xxCompDate
     C                   move      *off          include
     C                   endif                                                  OOEFDT = xxCompDate
     C                   endif

     C                   if        include = *on

     C                   if        EMTPOB = '1'

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm200
     C                   eval      x = 1
     C     xxnm200       lookup    nm(x)                                  65
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm200       nm(ix)

      ** This is so the name will print correctly on the form
     C                   if        EMTPOB = '1'
     C                   eval      FMTOBC = '1'
     C                   endif                                                  EMTPOB = '1'
    
     ** If the page is full, write and setup for more names
     C                   if        ix = 5
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm
     C                   eval      ix = *zero
     C                   endif                                                  ix = 05
     C                   endif                                                  *in65 = *off

     C                   endif                                                  include = *on

     C                   if        EMTPOB = '2'
     C     OOOFSN        cat(p)    OOOLSN:1      xxnm200
     C                   eval      y = 1
     C     xxnm200       lookup    nm1(y)                                 65
     C                   if        *in65 = *off
     C                   eval      iy = iy + 1
     C                   movel     xxnm200       nm1(iy)
    
     ** If the page is full, write and setup for more names
     C                   if        iy = 5
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm1
     C                   eval      iy = *zero
     C                   endif                                                  iy = 05
     C                   endif                                                  *in65 = *off
     C                   endif                                                  *in65 = *off

     C                   if        EMTPOB = 'A' or EMTPOB = 'D'

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm200
     C                   eval      z = 1
     C     xxnm200       lookup    nm2(z)                                 65
     C                   if        *in65 = *off
     C                   eval      iz = iz + 1
     C                   movel     xxnm200       nm2(iz)
    
     ** If the page is full, write and setup for more names
     C                   if        iz = 5
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   clear                   nm2
     C                   eval      iz = *zero
     C                   endif                                                  iz = 05
     C                   endif                                                  *in65 = *off
     C                   endif                                                  *in65 = *off

     C                   endif                                                  OOEXFL = 'I'
    
     C                   endif                                                  OOEXFL = 'I'
     C     ooKey         reade     poo01                                  60
     C                   enddo                                                  *in60 = *off
    
     C                   endif                                                  RSJRST = ENST
     C                   endif                                                  RSJRST = ENST
     C     elky2         reade     pel02                                  60
     C                   enddo
    
     ** Write the output record.
     C                   if        ix <> *zero or iy <> *zero or iz <> *zero
      /Free
          //* Write the output record
          Exsr WriteWorkFile;
      /End-Free
     C                   endif
    
      *
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
    
      * If type of business is Sole Prop or Partner, Then process
      *
     C                   if        EMTPOB = '1' or EMTPOB = '2' or
     C                             EMTPOB = 'A' or EMTPOB = 'D'
      *
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
      *
     C                   move      *off          qq316
     C                   move      ldeedt        xxCompDate
     C                   else
     C                   move      ELPFDT        xxCompDate
     C                   endif


     C                   if        OOEFDT <= xxCompDate
     C                             and OOEXDT > xxCompDate
     C                             or OOEFDT <= xxCompDate
     C                             and OOEXDT = *zeros
     C*
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

     C                   if        qqmode = '4'
     C                   move      *on           qq316
     C                   endif                                                  qqmode = '4'

     C                   leave
     C                   endif                                                  OOEFDT = xxCompDate

     C                   endif                                                  OOEXFL = 'E'

     C     ooKey         reade     poo01                                  60
     C                   enddo                                                  *in60 = *on
    
     C                   endif                                                  RSJRST = ENST
    
     C                   endif                                                  End EMTPOB <> 1,2
      *
     C                   endsr
