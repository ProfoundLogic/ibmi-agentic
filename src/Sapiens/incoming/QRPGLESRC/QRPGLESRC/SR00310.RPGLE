     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      /TITLE SR00310 - ENDORSEMENT ATTACH/PRINT PROGRAM
      *===============================================================*
      * WC000310 - Sole Proprietors, Owners, Officers and Others      *
      *            Inclusion Endorsement                              *
      *                                                               *
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * CREATED   : 11/12/97                                          *
      * PROGRAMMER: C:57                                              *
      * PROJECT   : CM027                                             *
      *===============================================================*
      * REVISED:
      *                                                               *
      * 01/21/19 C:65  Support: 76341 (Sunz)                          *
      *                Push 'ZZ' into each array position for ST      *
      *                array as the sorting is pushing blanks to the  *
      *                front and causing states to be re-added.       *
      *                                                               *
      * 06/22/17 C:136 Support: 47702                                 *
      *                Don't let excluded states print on the         *
      *                endorsement                                    *
      *                                                               *
      * 04/04/17 C:180 Support 46565                                  *
      *                Fix missing owner.                             *
      *                                                               *
      * 11/15/16 C:136 Project: SY371                                 *
      *                Corrected entry parms                          *
      *                                                               *
      * 05/23/16 C:136 Support: 41650                                 *
      *                Correct attachment and othinf logic by adding  *
      *                VerifyStTPOB subprocedure                      *
      *                                                               *
      * 07/08/14 C:242 Support:85985                                  *
      *                Do NOT attach Endorsment Form for Deleted Div  *
      *                                                               *
      * 02/16/10 C:190 SUPPORT 71067                                  *
      *                Corrected issue of duplicate printing.         *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * NOTES:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      *                                                               *
      *  qqmode = 1 - Called to determine if form should be attached. *
      *  qqmode = 2 - Called to print endorsement.                    *
      *  qqmode = 3 - Called for additional input.                    *
      *  qqmode = 4 - Called for mid-term processing.                 *
      *===============================================================*
     Fwmool     if   e           k disk
      ** Additional Entity/Location File
     Fwmall     if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fwdell8    if   e           k disk
     F                                     rename(pel01:pel03)
     Fststl3    if   e           k disk

      ** Copybook F specs
     F/COPY SCOPYEN01F

      ** Names to include on form
      **** Begin Change *** 04/04/17 ************************************
     D*nmx             s             46    dim(500)
     D nmx             s             46    dim(500) Ascend
      **** End   Change *** 04/04/17 ************************************
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

      ** Include this name flag
     D include         s              1

      ** Index
     D ix              s              2  0
     D ixx             s              3  0

      ** Index
     D x               s              2  0
     D k               s              3  0

      ** Owner/Officer 1st-last name concat
     D xxnm46          s             46

      ** Compare date
     D xxCompDate      s                   like(ELPFDT)
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     D y               s              2  0
     D iy              s              2  0
      **** Begin Chg ****** 04/04/17 ************************************
     D*st              s              2    dim(50)
     D st              s              2    dim(50) Ascend
      **** End   Chg ****** 04/04/17 ************************************

     D                 ds
     D xxname                       100
     D  fmemnm                             overlay(xxname)
     D  fmc215                             overlay(xxname:41)
     D  fmc441                             overlay(xxname:61)
     D                 ds
     D xxfnam                       100
     D  fmfenm                             overlay(xxfnam)
     D  fmc216                             overlay(xxfnam:41)
     D  fmc442                             overlay(xxfnam:61)

     D efdt            ds
     D  emefd1
     D  emefd2
     D  emefd3
     D  emefd4
     D  emefd5
     D ef                                  OVERLAY(efdt)
     D                                     LIKE(emefd1) DIM(5)
     D                                     inz

     D cxdt            ds
     D  emcad1
     D  emcad2
     D  emcad3
     D  emcad4
     D  emcad5
     D cx                                  OVERLAY(cxdt)
     D                                     LIKE(emcad1) DIM(5)
     D                                     inz

     D xxefdt          s                   like(emefd1)
     D xxcxdt          s                   like(emcad1)
     D xxsydt          s                   like(emcad1)
     d x1              s              3  0
      *** beg add ***** 06/22/17 *****************
     d isAprdOrExcd    s              1
     d j               s              2  0 inz
     d excStArr        s              2    dim(60)
      *** end add ***** 06/22/17 *****************

      ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
      *=========================================================================
      *  Copybook required constants
      *=========================================================================

      ** Form ID
     D qqForm          s             15A   Inz('WC000310  ')
     D qqMod2          s              1A

      ** Additional entry program
     D addpgm          c                   const('SR00000I  ')
      *** beg add ***** 11/15/16 ***************************
      // Define this field and delete qqRRN from entry parms
     d qqRRN           s              9  0
      *** end add ***** 11/15/16 ***************************

     D SR00310         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1

     D SR00310         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq310                         1

     D VerifyStTPOB    PR              N

      ** Owner/Officer file
     C     ooKey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    ELDIV
      ** Additional Entity/Location file
     C     alKey         klist
     C                   kfld                    OOCO#
     C                   kfld                    OOFND
     C                   kfld                    OOEMP#
     C                   kfld                    OODIV
     C                   kfld                    OOELID
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
     C     stky          KList
     C                   KFld                    xxkey            10
      ** Employer Detail file
     C     elky3         KList
     C                   KFld                    emco#
     C                   KFld                    emfnd
     C                   KFld                    ememp#
     C                   KFld                    emdiv
     C                   KFld                    xxfyr
     C     elkey4        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
      * SUBMISSION HISTORY FILE
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE

      ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
      /Free
       // *=========================================================================
       // *  GetCXDT - Get Employer Effective/Cancellation date
       // *=========================================================================
       Begsr GetCXDT;

          chain (elco#:elfnd:elemp#:eldiv) pem01;
          x1 = 5;

          Dow  ef(x1) = 0 and (x1 > 1);
             x1 = x1 - 1;
          enddo;
          xxefdt = ef(x1);
          xxcxdt = cx(x1);

       Endsr;
      /End-free
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr

      *** beg add ***** 06/22/17 ****************
     c                   Exsr      GetExcStates
      *** end add ***** 06/22/17 ****************

      * Get PEO Information if PEO Group
     c                   if        (fnpeof <> *blank)
     c                             and (ldenda <> *blanks)
     c                             and (ldend# > 0)
     c                   move      qqCO#         #5co#
     c                   move      qqfnd         #5fnd
     c                   move      qqemp#        #5emp#
     c                   move      qqdiv         #5div
     c                   move      qqfyr         #5fyr

     c                   CALL      'SRADD1'
     c                   PARM                    #5CO#             3 0
     c                   PARM                    #5FND             3 0
     c                   PARM                    #5EMP#            9 0
     c                   PARM                    #5DIV             5 0
     c                   PARM                    #5FYR             3 0
     c     xxname        PARM                    #5NAME          100
     c     xxfnam        PARM                    #5FNAM          100
     c                   PARM                    #5EAD1           40
     c                   PARM                    #5EAD2           40
     c                   PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C                   PARM                    #5FEIN            9 0
     C                   PARM                    #5EADR           90

     c                   endif

     C                   eval      ix = *zero
     C                   eval      ixx = *zero
     C                   z-add     0             i
      **** Begin Add **** 04/04/17 ***********************************
      /Free
       Clear st;
       Clear nmx;
      /End-Free
      **** End   Add **** 04/04/17 ***********************************
     C*** begin add ***  01/21/19  ***********************************
     C                   Move      'ZZ'          st
     C*** end add *****  01/21/19  ***********************************
     c                   clear                   E00310
     c                   z-add     0             x1st1
     c                   z-add     0             x1st2
     c                   z-add     0             x1st3
     c                   move      *blank        xxfrst            1
     C                   eval      iy = 1
      ** Only collect names for divisions with jurisdiction states
      ** other than Texas.  Texas has their own version.
     C     elky2         setll     pel02
     C     elky2         reade     pel02                                  60

     C                   dow       *in60 = *off
      /Free
          Exsr getcxdt;
          If (xxcxdt > elpfdt) or (xxcxdt = 0);
      /End-free
     C     rsky          chain     prs01                              60
      **Follow*the States rules.

      /Free
          If VerifyStTPOB();
      /End-free
      **** Begin Change *** 04/04/17 ************************************
     C****                   eval      y = 1
     C****     rsjrst        lookup    st(y)                                  65
      /Free
        sorta st;
        y = %lookup(rsjrst:st);
        If y = *zero;
          *in65 = *off;
        Else;
          *in65 = *on;
        Endif;
      /End-free
      **** End   Change *** 04/04/17 ************************************
     C                   if        *in65 = *off
     C                   clear                   nm
      /Free
       // Begin Add *** 04/04/17 ****************************************
        Clear nmx;
       // End   Add *** 04/04/17 ****************************************
      /End-Free
     C                   eval      ix = *zero
     C                   move      rsjrst        fmemst
     C                   move      rsjrst        svjrst            2
     C                   move      rsjrst        st(iy)
     C                   eval      iy = iy + 1
      *
     C     elkey4        setll     pel03
     C     elkey4        reade     pel03                                  60
     C                   dow       *in60 = *off

     C     rsky          chain     prs01                              60
     C                   if        rsjrst = svjrst
      ** Look for a qualifying "INCLUDED" Owner/Officer record.
     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60

     C                   dow       *in60 = *off

     C                   if        OOEXFL = 'I'
     C                             and OOSTAT <> 'I'
      ** If called in midterm mode, use the endorsement date to check against, otherwise
      ** use the policy effective date.
     C                   move      *off          include

     C                   if        qqmode = '4'

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
     C                   endif
     C                   if        OOELID <> *blanks
     C                             and OOELID <> *zeros
     C     alKey         chain     pal01                              60
     C                   if        (ALDLDT <= xxCompDate) and
     C                             (ALDLDT <> 0)
     C                   move      *off          include
     C                   endif
     C                   endif

     C                   if        include = *on  and i <=500

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm46
     C                   eval      x = 1
      **** Begin Change 04/04/17 ********************************************
     C***                   eval      k = 1
     C***     xxnm46        lookup    nmx(k)                                 65
      /Free
         Sorta nmx;
         k = %lookup(xxnm46:nmx);
         If k = *zero;
            *in65 = *off;
         Else;
            *in65 = *on;
         Endif;
      /End-Free
      **** End   Change 04/04/17 ********************************************
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   eval      ixx = ixx + 1
     C                   movel     xxnm46        nm(ix)
     C                   movel     xxnm46        nmx(ixx)
     c                   eval      i = i + 1
     c                   if        i = 1
     c                   movel     xxnm46        xxnm1
     c                   select
     c     emtpob        wheneq    '1'
     c                   move      'S'           xxds1
     c     emtpob        wheneq    '2'
     c                   move      'P'           xxds1
     c     emtpob        wheneq    '3'
     c                   move      'O'           xxds1
     c                   other
     c                   move      'X'           xxds1
     c                   endsl
     c                   movel     rsjrst        xxkey
     c     stky          chain     pst01                              60
     c                   if        *in60 = *off
     c                   move      stnst         x1st1
     c                   endif
     c                   endif
     c                   if        i = 2
     c                   movel     xxnm46        xxnm2
     c                   select
     c     emtpob        wheneq    '1'
     c                   move      'S'           xxds2
     c     emtpob        wheneq    '2'
     c                   move      'P'           xxds2
     c     emtpob        wheneq    '3'
     c                   move      'O'           xxds2
     c                   other
     c                   move      'X'           xxds2
     c                   endsl
     c                   movel     rsjrst        xxkey
     c     stky          chain     pst01                              60
     c                   if        *in60 = *off
     c                   move      stnst         x1st2
     c                   endif
     c                   endif
     c                   if        i = 3
     c                   movel     xxnm46        xxnm3
     c                   select
     c     emtpob        wheneq    '1'
     c                   move      'S'           xxds3
     c     emtpob        wheneq    '2'
     c                   move      'P'           xxds3
     c     emtpob        wheneq    '3'
     c                   move      'O'           xxds3
     c                   other
     c                   move      'X'           xxds3
     c                   endsl
     c                   movel     rsjrst        xxkey
     c     stky          chain     pst01                              60
     c                   if        *in60 = *off
     c                   move      stnst         x1st3
     c                   endif
     c                   endif
     c                   if        i = 3
      *  If printing a policy page.  (Not called from W1054R)
     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      * Update the endorsement reporting file with the proper data.
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      * Copy data into the record.
     C                   If        *in60 = *off

     c                   move      *blanks       pedata
     c                   movel     E00310        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt

     C                   if        xxfrst = *blank
     C                   update    ppe01
     C                   move      'y'           xxfrst
     c                   else
     c                   movel     q1user        peiusr
     c                   movel     q1pgm         peipgm
     c                   z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   write     ppe01
     C                   endif
     c                   clear                   E00310
     c                   z-add     0             x1st1
     c                   z-add     0             x1st2
     c                   z-add     0             x1st3
     C                   z-add     0             i

     C                   endif
     C                   endif

     C                   endif
      ** If the page is full, write and setup for more names
     C                   if        ix = 10

      /Free
          //* Write the output record
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free

     C                   clear                   nm
     C                   eval      ix = *zero
     C                   endif
     C                   endif

     C                   endif

     C                   endif

     C     ooKey         reade     poo01                                  60
     C                   enddo

     C                   endif
     C     elkey4        reade     pel03                                  60
     C                   enddo

     C                   if        ((ix <> *zero)
     C                             and (nm(1) <> *blanks))

      /Free
          //* Write the output record
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free

     C                   endif

     C                   endif
     C                   endif
      /free
          Endif;
      /end-free
     C     elky2         reade     pel02                                  60
     C                   enddo

     C     emky          chain     pem01                              60
     C                   dow       *in60 = *off
     C                             and EMSTRP <> 'Y'
     C     emky          reade     pem01                                  60
     C                   enddo

     C     elky3         chain     pel01                              60
     C     rsky          chain     prs01                              60
      ** Write the output record.
     c                   if        i <> 0
      *  If printing a policy page.  (Not called from W1054R)
     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      * Update the endorsement reporting file with the proper data.
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid

     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      * Copy data into the record.
     C                   If        *in60 = *off

     c                   move      *blanks       pedata
     c                   movel     E00310        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt

     C                   if        xxfrst = *blank
     C                   update    ppe01
     C                   move      'y'           xxfrst
     c                   else
     c                   movel     q1user        peiusr
     c                   movel     q1pgm         peipgm
     c                   z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   write     ppe01                                        i = 1
     C                   endif

     C                   endif                                                  *in60 ifeq off
     C                   endif

     C                   endif                                                  *in60 ifeq off

     C                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr

      *** beg del ***** 06/22/17 ***************************
     C***  rsky          chain     prs01                              60
      *** end del ***** 06/22/17 ***************************
      ***  Follow the States rules.
      /Free
       If VerifyStTPOB();
      /End-free

      ** Look for a qualifying "INCLUDED" Owner/Officer record.
     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60
    
     C                   dow       *in60 = *off
    
     C                   if        OOEXFL = 'I'
     C                             and OOSTAT <> 'I'

      ** If called in midterm mode, use the endorsement date to check against, otherwise
      ** use the policy effective date.
     C                   if        qqmode = '4'
     C                   move      *off          qq310

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
     C                   if        OOELID <> *blanks
     C                             and OOELID <> *zeros
     C     alKey         chain     pal01                              60
     C                   if        ALDLDT > xxCompDate
     C                             or ALDLDT = *zeros
     C                   move      *on           crtfrm
     C                   endif
     C                   else
     C                   move      *on           crtfrm
     C                   endif

     C                   if        qqmode = '4'
     C                   move      *on           qq310
     C                   endif

     C                   leave
     C                   endif

     C                   endif

     C     ooKey         reade     poo01                                  60
     C                   enddo

     C                   endif

     C                   endsr
      *** beg add ***** 06/22/17 *************
      *======================================================
      *  GetExcStates : Populate excluded state array
      *======================================================
      /Free
       Begsr GetExcStates;

       // Populate excluded state array
       // All records are either 'A'pproved or 'E'xcluded
       // Only needed for the OTHINF subroutine
       j = 1;
       Setll (xxco#:xxform) pes01;
       Reade (xxco#:xxform) pes01;
       Dow Not(%Eof);
          isAprdOrExcd = ESapex;
          excStArr(j) = ESst;
          j=j+1;
       Reade (xxco#:xxform) pes01;
       Enddo;

       Endsr;
      /End-Free
      *** end add ***** 06/22/17 *************

     D*--------------------------------------------------
     D*
     D*--------------------------------------------------
     P VerifyStTPOB    B
     D VerifyStTPOB    PI              N
      *** beg chg ***** 06/22/17 ***************
      ***/Free
      *** Select;
      *** When RSjrst = 'TX' and RSjrst = 'CA';
      ***    Return *off;
      *** When RSjrst = 'NJ' and (EMtpob = '1' or EMtpob = '2');
      ***    Return *off;
      *** When RSjrst = 'MA' and EMtpob <> '1' and EMtpob <> '2'
      ***  and EMtpob <> '6' and EMtpob <> 'A'
      ***  and EMtpob <> 'D' and EMtpob <> 'F';
      ***    Return *off;
      *** Other;
      ***    Return *on;
      *** EndSl;
      ***/End-free

      /Free
       Select;
       When %LookUp(RSjrst:excStArr) <> 0 and isAprdOrExcd = 'E';
          Return *off;
       When RSjrst = 'NJ' and (EMtpob = '1' or EMtpob = '2');
          Return *off;
       When RSjrst = 'MA' and EMtpob <> '1' and EMtpob <> '2'
        and EMtpob <> '6' and EMtpob <> 'A'
        and EMtpob <> 'D' and EMtpob <> 'F';
          Return *off;
       Other;
          Return *on;
       EndSl;
      /End-free
      *** end chg ***** 06/22/17 ***************
     P VerifyStTPOB    E
