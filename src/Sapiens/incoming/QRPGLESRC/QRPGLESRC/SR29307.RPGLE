      *===============================================================*
      *  (C) Copyright Progressive Data Solutions, Inc., 2001
      *===============================================================*
     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
    H/TITLE SR29307 - NJ SOLE PROP AND PARTNERS COVERAGE ENDORSE
     *===============================================================*
     * WC290307 - New Jersey Sole Prop and Partners Coverage Endorse *
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
      * 12/27/16 C:180 Project SY371 scriptura                        *
      *                Add scriptura interface.                       *
     *                                                               *
     * 08/10/09 C:61  SUPPORT WA114 69430                            *
     *                Change to attach/print for Type of business 'D'*
     *                                                               *
     * 02/05/09 C:61  SUPPORT WA133 67025                            *
     *                Print E/L Owner/Officers if E/L deletion date
     *                is non-zero or less that compare date.
     *                                                               *
      * 07/30/01 C:135 Project SUPPORT WA101/Seq 28383                *
      *          Added code to test for sole proprietorship or        *
      *          partnership                                          *
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
      **** Begin Add **** 12/16/16 *********************************************
      * Entry
     D SR29307         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1
     D                                9  0
     D                                1
     D SR29307         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qq307                         1
     D  qqrrn                         9  0
     D  qqmod2                        1
      **** End   Add **** 12/16/16 *********************************************
     D/COPY SCOPYEN01D
    
     D include         s              1
     D ix              s              2  0
      **** Begin Delete **** 12/27/16 **************************************
     D*** qq307           s              1
      **** End   Delete **** 12/27/16 **************************************
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
     *=========================================================================
     *  Copybook required constants
     *=========================================================================
     ** Form ID
      *** End   Change *** 12/27/16 ********************************************
     D**** xxform          c                   const('WC290307  ')
     D qqform          c                   const('WC290307  ')
      *** End   Change *** 12/27/16 ********************************************
    
     ** Additional entry program
     D addpgm          c                   const('SRxxxxxxxx')
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
    
     ** Entry parameters
      *** Begin Delete 12/27/16 ******************************************
     C***  *entry        plist
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
     C***                parm                    qqmode
     C***                parm                    qq307
    
     ** Additional input program parameters
     C***  addpl         pList
     C***                parm                    qqprds
     C***                parm      qqmod2        qqcall
     C***                parm                    qqco#
     C***                parm                    qqfnd
     C***                parm                    qqemp#
     C***                parm                    qqdiv
     C***                parm                    qqfyr
      *** End   Delete 12/27/16 ******************************************
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
     C*** BEG ADD *** 07/30/01 *********************************************
     C                   if        EMTPOB = '1'                                 '1' = sole prop
     C                             or EMTPOB = '2'                              '2' = partnership
     C*** beg add *** 08/10/09 *********************************************
     C                             or EMTPOB = 'A'                              '2' = partnership
     C                             or EMTPOB = 'D'                              '2' = partnership
     C*** end add *** 08/10/09 *********************************************
     C*** END ADD *** 07/30/01 *********************************************
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
      **** Begin Change **** 12/27/16 **************************************
     C*****              write     pfm01
      /Free
       Exsr WriteWorkFile;
      /End-Free
      **** End   Change **** 12/27/16 **************************************
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
     C*** BEG ADD *** 07/30/01 *********************************************
     C                   endif                                                  EMTPOB = 1, 2
     C*** END ADD *** 07/30/01 *********************************************
     C                   endif                                                  RSJRST = ENST
     C     elky2         reade     pel02                                  60
     C                   enddo
    
     ** Write the output record.
     C                   if        ix <> *zero
      **** Begin Change **** 12/27/16 **************************************
     C****               write     pfm01
      /Free
       Exsr WriteWorkFile;
      /End-Free
      **** End   Change **** 12/27/16 **************************************
     C                   endif
    
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
      *** Begin Add    *** 12/27/16 ********************************************
      /Free
       WriteInCopyBook = *Off;
       RunningSRText = *On;
      /End-Free
      *** End   Add    *** 12/27/16 ********************************************
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
     ** Get Rate Set description
     C     rsky          chain     prs01                              60
     C                   if        RSJRST = ENST
     C*** BEG ADD *** 07/30/01 *********************************************
     C                   if        EMTPOB = '1'                                 '1' = sole prop
     C                             or EMTPOB = '2'                              '2' = partnership
     C*** beg add *** 08/10/09 *********************************************
     C                             or EMTPOB = 'A'                              '2' = partnership
     C                             or EMTPOB = 'D'                              '2' = partnership
     C*** end add *** 08/10/09 *********************************************
     C*** END ADD *** 07/30/01 *********************************************
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
     C                   move      *off          qq307
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
     C                   move      *on           qq307
     C                   endif                                                  qqmode = '4'

     C                   leave
     C                   endif                                                  OOEFDT = xxCompDate

     C                   endif                                                  OOEXFL = 'E'

     C     ooKey         reade     poo01                                  60
     C                   enddo                                                  *in60 = *on
    
     C*** BEG ADD *** 07/30/01 *********************************************
     C                   endif                                                  EMTPOB = 1, 2
     C*** END ADD *** 07/30/01 *********************************************
     C                   endif                                                  RSJRST = ENST
    
     C                   endsr
