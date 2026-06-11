     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 2001
     *===============================================================*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR31313 - NY SOLE PROP AND PARTNERS COVERAGE ENDORSE
     *===============================================================*
     * WC310313A - New York Sole Prop and Partners Coverage Endorse  *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 05/10/01                                          *
     * PROGRAMMER: C:90                                              *
     * PROJECT   : CM027                                             *
     *                                                               *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 02/05/09 C:61  SUPPORT WA133 67025                            *
     *                Print E/L Owner/Officers if E/L deletion date
     *                is non-zero or less that compare date.
      *                                                               *
      *  10/04/02  C:132 Project: CM027  Seq#: WA109  33140           *
      *                  Change Office Vision forms to use correct    *
     *                  type of business codes. Add code in the      *
     *                  subroutines VALID & OTHINF to only process   *
     *                  EMTPOB type of business code 1 (Sole Prop)   *
     *                  or code 2 (Partnership)                      *
     *                                                               *
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
     D qq313           s              1
     D x               s              2  0
     D xxnm46          s             46
     D xxtpob          s              1  0
     D qqrrn           s              9  0
     D qqmod2          s              1

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
     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Form ID
     D qqform          c                   const('WC310313A ')
    
     ** Additional entry program
     D addpgm          c                   const('SRxxxxxxxx')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qq313             1
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
      **** BEG ADD **** 10/04/02 *******************************************
      * If type of business is Sole Prop or Partner, Then process
      *
     C                   if        EMTPOB = '1' or EMTPOB = '2'
      **** END ADD **** 10/04/02 *******************************************
     C                   eval      ix = *zero
    
      ** Check all divisions for Texas exposure
     C     elky2         chain     pel02                              60
    
     C                   dow       *in60 = *off
    
     C     rsky          chain     prs01                              60
    
     C                   if        RSJRST = ENST
    
     ** Look for a qualifying "INCLUDED" Owner/Officer record.
     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60
    
     C                   dow       *in60 = *off
    
     C                   if        OOEXFL = 'I'
     C                             and OOSTAT <> 'I'
    
     ** If called in midterm mode, use the endorsement date to check against, otherwise
     ** use the policy effective date.
     C                   move      *off          include
     C                   if        qqmode = '4'
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
      ****** beg chg **** 02/05/09 *****************************************
     C**********         if        ALDLDT <= xxCompDate
     C                   if        (ALDLDT <= xxCompDate) and
     C                             (ALDLDT <> 0)
      ****** end chg **** 02/05/09 *****************************************
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
    
     ** If the page is full, write and setup for more names
     C                   if        ix = 10
     C                   write     pfm01
     C                   clear                   nm
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
     C     elky2         reade     pel02                                  60
     C                   enddo
    
     ** Write the output record.
     C                   if        ix <> *zero
     C                   write     pfm01
     C                   endif
    
      **** BEG ADD **** 10/04/02 *******************************************
     C                   endif                                                  End EMTPOB <> 1,2
      **** END ADD **** 10/04/02 *******************************************
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
      **** BEG ADD **** 10/04/02 *******************************************
      * If type of business is Sole Prop or Partner, Then process
      *
     C                   if        EMTPOB = '1' or EMTPOB = '2'
      **** END ADD **** 10/04/02 *******************************************
      *
     ** Get Rate Set description
     C     rsky          chain     prs01                              60
     C                   if        RSJRST = ENST
    
     ** Look for a qualifying "INCLUDED" Owner/Officer record.
     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60
    
     C                   dow       *in60 = *off
    
     C                   if        OOEXFL = 'I'
     C                             and OOSTAT <> 'I'
    
     ** If called in midterm mode, use the endorsement date to check against, otherwise
     ** use the policy effective date.
     C                   if        qqmode = '4'
      *                                                                     ************************
     C                   move      *off          qq313
     C                   move      ldeedt        xxCompDate
     C                   else
     C                   move      ELPFDT        xxCompDate
     C                   endif


     C                   if        OOEFDT <= xxCompDate
     C                             and OOEXDT > xxCompDate
     C                             or OOEFDT <= xxCompDate
     C                             and OOEXDT = *zeros
      *
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
     C                   move      *on           qq313
     C                   endif                                                  qqmode = '4'

     C                   leave
     C                   endif                                                  OOEFDT = xxCompDate

     C                   endif                                                  OOEXFL = 'E'

     C     ooKey         reade     poo01                                  60
     C                   enddo                                                  *in60 = *on
    
     C                   endif                                                  RSJRST = ENST
    
      **** BEG ADD **** 10/04/02 *******************************************
     C                   endif                                                  End EMTPOB <> 1,2
      **** END ADD **** 10/04/02 *******************************************
      *
     C                   endsr
