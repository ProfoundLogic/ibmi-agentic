     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR00308 - ENDORSEMENT ATTACH/PRINT PROGRAM
      *===============================================================*
      * WC000308 - Partners, Officers and Others Exclusion Endorsement*
      *                                                               *
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * Revised:                                                      *
      *                                                               *
      * 11/15/16 C:136 Project: SY371                                 *
      *                Corrected entry parms                          *
      *                                                               *
      * 11/28/12 C:136 Support: WA133 80421                           *
      *                Allow endorsement to attach to sole            *
      *                proprietorships (removes WC1913)               *
      *                                                               *
      * 06/28/11 C:189 Support: WA139 SEQ77096                        *
      *                Do not auto attach for 'PA' coverage           *
      *                                                               *
      * 06/30/10 C:220 Project: WC1913                                *
      *                In routine VALID: replace type of business     *
      *                condition                                      *
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
      ** Owner/Officer Master File
     Fwmool     if   e           k disk
      ** Additional Entity/Location File
     Fwmall     if   e           k disk
      ** Employer payroll file
     Fwmepl     if   e           k disk
      ** Payroll master file
     Fwmprl1    if   e           k disk
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)

      ** Copybook F specs
     F/COPY SCOPYEN01F

     D include         s              1
     D ix              s              2  0
     D x               s              2  0
     D xxnm46          s             46

     D xxCompDate      s                   like(ELPFDT)
     D xxtpob          s                   like(EMTPOB)
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     D                 ds
     D  nm                     1    644
     D                                     dim(14)
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
     D  FMNM11               461    506
     D  FMNM12               507    552
     D  FMNM13               553    598
     D  FMNM14               599    644

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

      ** Copybook D specs
     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
      *=========================================================================
      *  Copybook required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15A   Inz('WC000308  ')
     D qqMod2          s              1A

      ** Additional entry program
     D addpgm          c                   const('SR00000I  ')
      *** beg add ***** 11/15/16 ***************************
      // Define this field and delete qqRRN from entry parms
     d qqRRN           s              9  0
      *** end add ***** 11/15/16 ***************************

     D SR00308         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                1

     D SR00308         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qq308                         1

      *=========================================================================
      *  KLIST definitions
      *=========================================================================
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
      ** Employer Payroll file
     C     epKey         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
      ** Payroll Master file
     c     prKey         klist
     C                   kfld                    xxco#
     C                   kfld                    ELRTCD
     C                   kfld                    EPCLAS
     C                   kfld                    ELPFDT
     c     prKey1        klist
     C                   kfld                    xxco#
     C                   kfld                    ELRTCD
     C                   kfld                    EPCLAS
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
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     othinf        begsr

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
     C                   z-add     0             i
     c                   clear                   E00308
     c                   move      *blank        xxfrst            1

      ** Only collect names for divisions with jurisdiction states
      ** other than Texas.  Texas has their own version.

     C     elky2         setll     pel02
     C     elky2         reade     pel02                                  60

     C                   dow       *in60 = *off
     C     rsky          chain     prs01                              60

     C                   if        RSJRST <> 'TX'
     c                             and RSJRST <> 'CA'
     c                             and RSJRST <> 'PA'

      ** Look for a qualifying "EXCLUDED" Owner/Officer record.
     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60

     C                   dow       *in60 = *off

     C                   if        OOEXFL = 'E'
     C                             and OOSTAT <> 'I'
      ** If called in midterm mode, use the endorsement date to check against, otherwise
      ** use the policy effective date.
     C                   if        qqmode = '4'
     C                   Eval      srdti = ldeedt
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         xxCompDate
     C                   else
     C                   move      ELPFDT        xxCompDate
     C                   endif

     C                   move      *off          include

     C                   if        OOEFDT <= xxCompDate
     C                             and OOEXDT > xxCompDate
     C                             or OOEFDT <= xxCompDate
     C                             and OOEXDT = *zeros
     C                   move      *on           include
     C                   endif
     C                   if        OOELID <> *blanks
     C                             and OOELID <> *zeros
     C     alKey         chain     pal01                              60
     C                   if        ALDLDT <= xxCompDate
     C                             and ALDLDT <> *zeros
     C                   move      *off          include
     C                   endif
     C                   endif
     C                   if        RSJRST = 'FL'
     C                             and ELASP1 = *zero

     C     epKey         setll     pep01
     C     epKey         reade     pep01                                  60

     C                   dow       *in60 = *off

     C                   if        EPSTAT <> 'I'

     C     prKey         setgt     ppr01
     C     prKey1        readpe    ppr01                                  60

     C                   if        *in60 = *off
     C                             and PRCCFL = 'Y'
     C                   move      *on           include
     C                   leave
     C                   endif

     C                   endif

     C     epKey         reade     pep01                                  60
     C                   enddo

     C                   endif

     C                   if        include = *on

     C     OOOFSN        cat(p)    OOOLSN:1      xxnm46
     C                   eval      x = 1
     C     xxnm46        lookup    nm(x)                                  65
     C                   if        *in65 = *off
     C                   eval      ix = ix + 1
     C                   movel     xxnm46        nm(ix)
     c                   eval      i = i + 1
     c                   if        i = 1
     c                   movel     xxnm46        x1nm1
     c                   select
     c     emtpob        wheneq    '2'
     c                   move      'P'           x1ds1
     c     emtpob        wheneq    '3'
     c                   move      'O'           x1ds1
     c                   other
     c                   move      'X'           x1ds1
     c                   endsl
     c
     c                   endif
     c                   if        i = 2
     c                   movel     xxnm46        x1nm2
     c                   select
     c     emtpob        wheneq    '2'
     c                   move      'P'           x1ds2
     c     emtpob        wheneq    '3'
     c                   move      'O'           x1ds2
     c                   other
     c                   move      'X'           x1ds2
     c                   endsl
     c                   endif
     c                   if        i = 3
     c                   movel     xxnm46        x1nm3
     c                   select
     c     emtpob        wheneq    '2'
     c                   move      'P'           x1ds3
     c     emtpob        wheneq    '3'
     c                   move      'O'           x1ds3
     c                   other
     c                   move      'X'           x1ds3
     c                   endsl
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
     c                   movel     E00308        pedata
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
     C                   z-add     0             i
     c                   clear                   E00308

     C                   endif

     C                   endif
     C                   endif

      ** This is so the name will print correctly on the form
     C                   if        RSJRST = 'AR'
     C                             and EMTPOB = '1'
     C                   eval      FMTOBC = '4'
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
     C     elky2         reade     pel02                                  60
     C                   enddo
      ** Write the output record.
     C                   if        ix <> *zero

      /Free
          //* Write the output record
          WriteInCopybook = *Off;
          Exsr WriteWorkFile;
      /End-Free

     C                   endif

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
     c                   movel     E00308        pedata
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

     C                   endif
     C                   endif

     C                   endif

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

     C     rsky          chain     prs01                              60
     C                   if        RSJRST <> 'TX'
     c                             and RSJRST <> 'CA'
     c                             and RSJRST <> 'PA'

     C                   move      EMTPOB        xxtpob

     C     ooKey         setll     poo01
     C     ooKey         reade     poo01                                  60

     C                   dow       *in60 = *off

     C                   if        OOEXFL = 'E'
     C                             and OOSTAT <> 'I'

     C                   if        qqmode = '4'
     C                   move      *off          qq308
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
     C                   if        RSJRST = 'FL'
     C                             and ELASP1 = *zero

     C     epKey         setll     pep01
     C     epKey         reade     pep01                                  60

     C                   dow       *in60 = *off

     C                   if        EPSTAT <> 'I'

     C     prKey         setgt     ppr01
     C     prKey1        readpe    ppr01                                  60

     C                   if        *in60 = *off
     C                             and PRCCFL = 'Y'
     C                   move      *on           crtfrm
     C                   leave
     C                   endif

     C                   endif

     C     epKey         reade     pep01                                  60
     C                   enddo

     C                   endif
     C                   if        RSJRST <> 'FL'
     C                             or xxtpob <> '1'
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
     C                   endif

     C                   if        qqmode = '4'
     C                   move      *on           qq308
     C                   endif

     C                   leave
     C                   endif

     C                   endif

     C     ooKey         reade     poo01                                  60
     C                   enddo

     C                   endif

     C                   endsr
