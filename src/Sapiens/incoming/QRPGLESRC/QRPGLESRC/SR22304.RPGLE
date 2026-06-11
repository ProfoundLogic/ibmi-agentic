     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR22304 - MN Employee Leasing Endorsement
       //===============================================================*
       //                                                               *
       //    PROGRAM DESCRIPTION:                                       *
       //                                                               *
       //       THIS PROGRAM IS CALLED TO:                              *
       //         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE       *
       //            ATTACHED TO THE POLICY.                            *
       //         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
       //            (WXFMP) PRIOR TO A FORM MERGE.                     *
       //                                                               *
       //                                                               *
       //****************************************************************
       //                                                               *
       // REVISIONS:                                                    *
       //                                                               *
       // 11/03/22 C:333 Project: WC2834                                *
       //                Endorsement Recognition of Cancellation        *
       //                                                               *
       // 07/28/21 C:65  Project: Phw#97037 (Sunz)                      *
       //                Reset the L2 array index after writing to file.*
       //                                                               *
       // 08/23/19 C:295 Support: 81204                                 *
       //                Correction in Type of address.                 *
       //                                                               *
       // 01/23/18 C:65  Project: 150WAD                                *
       //                Report dba name if no federal name.            *
       //                                                               *
       // 10/06/17 C:062 Project: 150WAP                                *
       //                Remove peo stuff from valid                    *
      //                                                               *
       // 07/25/17 C:180 Project: 150WAP                                *
       //                Handle groups with combined PEOs               *
       //                                                               *
       // 07/17/16 C:302 Project: SY371                                 *
       //                Scriptura phase 2                              *
       //                Clean up Program/Copybook                      *
       //****************************************************************

      /COPY SCOPYEN01F

     fwmall     if   e           k disk
     fwxpel     uf a e           k disk
     fwxpel1    if   e           k disk
     f                                     rename(ppe01:ppe02)

      /COPY SCOPYEN01D

      /COPY WWCPOLDAT4

     d fedds           ds
     d  fmc201
     d  fmc441
     d fed                           60    OVERLAY(fedds)

      * employer dba name extension
     d dbads           ds
     d  fmc202
     d  fmc442
     d dba                           60    OVERLAY(dbads)

      * zip code
     d ZIPds           ds
     d  ZIP5                          5  0
     d  ZIP4                          4  0
     d ZIP                            9    OVERLAY(ZIPds)

      * Location Address Lines
     d LOCds           ds
     d  FMC401
     d  FMC402
     d  FMC301
     d  FMC403
     d  FMC404
     d  FMC302
     d  FMC405
     d  FMC406
     d  FMC303
     d  FMC407
     d  FMC408
     d  FMC304
     d  FMC409
     d  FMC410
     d  FMC305
     d  FMC411
     d  FMC412
     d  FMC306
     d  FMC413
     d  FMC414
     d  FMC307
     d  FMC415
     d  FMC416
     d  FMC308
     d  FMC417
     d  FMC418
     d  FMC309
     d  FMC419
     d  FMC420
     d  FMC310
     d  FMC421
     d  FMC422
     d  FMC311
     d  FMC423
     d  FMC424
     d  FMC312
     d  FMC425
     d  FMC426
     d  FMC313
     d  FMC427
     d  FMC428
     d  FMC314
     d  FMC429
     d  FMC430
     d  FMC315
     d  FMC431
     d  FMC432
     d  FMC316
     d loc                          110    Overlay(locds) Dim(16)

     d adrds           ds
     d  FM2011
     d  FM2012
     d  FM2013
     d  FM2014
     d  FM2015
     d  FM2016
     d  FM2017
     d  FM2018
     d  FM2019
     d  FM2020
     d  FM2021
     d  FM2022
     d  FM2023
     d  FM2024
     d  FM2025
     d  FM2026
     d adr                          200    Overlay(adrds) Dim(16)

     d xxadr1          s             40
     d xxadr2          s             40
     d xxcty           s             25
     d state           s              2
     d xxsep           s              1
     d token           s            110
     d xxlast          s              1
     d L1              s              3  0
     d L2              s              3  0
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)

     D qqForm          s             15A   Inz('WC220304  ')

     D addpgm          c                   const('SRXXXXXXXX')

      * name returned from SRADDR
     d emnam           ds
     d  ##emn1                       50
     d  ##emn2                       50
     d  ##emn3                       50
     d  ##emn4                       50
     d  ##emn5                       50
     d  ##emn6                       50
     d  ##emn7                       50

     d nam                                 OVERLAY(emnam)
     d                                     LIKE(##emn1) DIM(7)
     D #1co#           s              3
     D #1fnd           s              3
     D #1fyr           s              3
     D #1emp#          s              9
     D #1div           s              5
     D ##odr           s              1
     D ##nodr          s              1
     D ##aodr          s              1
     D ##adrl          s              3  0
     D ##naml          s              3  0
     D ##fedl          s              3  0
     D ##fedl1         s              3  0
     D ##peo1          s             57
     D ##peo2          s             57
     D ##pgfl          s              1

     D SR22304         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR22304         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *  employer
     c     emky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
      *  entity / Location
     c     alky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

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

      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     othinf        begsr
      /Free
          WriteInCopybook = *Off;
      /End-Free
      * for each employer division with MN coverage
     c     elky2         setll     pel02
     c     elky2         reade     pel02

     c                   dow       not %eof
       //***** Beg Add *** 11/03/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/03/22 ******************************
      * if not PEO division and MN coverage
      *
      **** Begin Change *** 07/25/17 ***********************************************
     c***                   if        ((fnpeof = 'S') and (eldiv <> 0))
     c***                             or ((fnpeof = 'M') and (elemp# <> 1))

     c                   if        (fnpeof = 'S' and eldiv <> 0)  or
     c                             (fnpeof = 'M' and elemp# <> 1) or
     c                             (fnpeof = 'C' and elemp# = 1 and eldiv <> 0)
     c                             or (fnpeof = 'C' and ELemp# <> 1)
      **** End    Change *** 07/25/17 ***********************************************

     c     emky1         chain     pem01

     c                   if        %found
     c                   clear                   fed
     c                   clear                   dba
      *   write copy of endorsement
      *     dba name
     c                   eval      fmemnm  = emname
     c                   eval      dba     = emnam2
     c                   eval      FM2002  = EMname + EMnam2
      *     federal name
     c                   eval      fmfenm  = emfdnm
     c                   eval      fed     = emfdn2
     c                   eval      FM2001  = EMfdnm + emfdn2
      *     FEIN
     c                   eval      fmfein  = emfein
      *     UI#
     c                   eval      fmc203  = emue#
      *     address
      *       mailing
     c                   if        emmad1 <> *blank
     c                   eval      fmead1  = emmad1
     c                   eval      fmead2  = emmad2
     c                   eval      fmead3  = %trim(emmcty) + ', ' + emmst
     c                   move      emmzip        xxzip

     c                   else
      *       physical
     c                   eval      fmead1  = empad1
     c                   eval      fmead2  = empad2
     c                   eval      fmead3  = %trim(empcty) + ', ' + empst
     c                   move      empzip        xxzip
     c                   endif
      *       zip code
     c                   if        (xxzip1 <> *zero) and (xxzip1 <> *blanks)
     c                   eval      fmead3  = %trim(fmead3) + ' '
     c                                       + xxzip1

     c                   if        (xxzip2 <> *zero) and (xxzip2 <> *blanks)
     c                   eval      fmead3  = %trim(fmead3) + '-' + xxzip2
     c                   endif
     c                   endif
      *       blank 2 address line
     c                   if        fmead2 = *blanks
     c                   eval      fmead2 = fmead3
     c                   eval      fmead3 = *blanks
     c                   endif
      *       Get Location Addresses (if any)
     c                   exsr      getlocadr
      /Free
          Exsr WriteWorkFile;
      /End-Free

     c                   exsr      endrpt
     c                   endif

     c                   endif

          //***** Beg Add *** 11/03/22 ******************************
          Endif;
          //***** End Add *** 11/03/22 ******************************
     c     elky2         reade     pel02
     c                   enddo

     C                   endsr
      *=========================================================================
      *  GETLOCADR - Fill Additional Location output lines                  d
      *=========================================================================
     C     getlocadr     begsr
     c                   clear                   loc
     c                   eval      L1 = 1
     c                   eval      L2 = 1

     c     alky          setll     pal01
     c     alky          reade     pal01

     c                   dow       not %eof

     c                   if        aleltp = 'L'

     c                   if        alpad1 <> *blanks
     c                   eval      xxadr1  = alpad1
     c                   eval      xxadr2  = alpad2
     c                   eval      xxcty   = alpcty
     c                   eval      state   = alpst
     c                   move      alpzip        xxzip

     c                   else

     c                   eval      xxadr1  = almad1
     c                   eval      xxadr2  = almad2
     c                   eval      xxcty   = almcty
     c                   eval      state   = almst
     c                   move      almzip        xxzip
     c                   endif
      /Free
          adr(L2) = xxadr1 + '|' + xxadr2 + '|' + xxcty + '|' + state + '|'
                      + xxzip;
          L2 = L2 + 1;
      /End-Free
     c                   exsr      addadr
     c                   endif

     c     alky          reade     pal01
     c                   enddo

     c                   endsr
      *=========================================================================
      *  ADDadr - add token to "Work location addresses" output lines
      *=========================================================================
     C     addadr        begsr
     C                   eval      xxlast = 'N'

     c                   eval      token  = %trim(xxadr1)
     c                   eval      xxsep  = ';'
     c                   exsr      addtok

     c                   if        xxadr2 <> *blanks
     c                   eval      token  = %trim(xxadr2)
     c                   eval      xxsep  = ','
     c                   exsr      addtok
     c                   endif

     c                   eval      token  = %trim(xxcty)
     c                   eval      xxsep  = ','
     c                   exsr      addtok

     c                   eval      token  = %trim(state)
     c                   eval      xxsep  = ','
     c                   exsr      addtok

     c                   if        (xxzip1 <> *zero) and (xxzip1 <> *blanks)
     c                   eval      token  = %trim(xxzip1)

     c                   if        (xxzip2 <> *zero) and (xxzip2 <> *blanks)
     c                   cat       '-':0         token
     c                   cat       xxzip2:0      token
     c                   endif

     c                   eval      xxsep  = ' '
     c                   exsr      addtok
     c                   endif

     c                   if        xxlast = 'Y'
      /Free
          Exsr WriteWorkFile;
      /End-Free
     c                   clear                   loc
     c                   eval      L1 = 1
     c*** begin add ***  07/28/21  ************************************
     c                   eval      L2 = 1
     c*** end add *****  07/28/21  ************************************
     c                   endif

     c                   endsr
      *=========================================================================
      *  ADDtok - add token to "Work location addresses" output lines
      *=========================================================================
     C     addtok        begsr

     c                   z-add     0             LineLength        4 0
     c     ' '           checkr    loc(L1)       LineLength
     c                   z-add     0             TokenLength       4 0
     c     ' '           checkr    token         TokenLength

     c                   if        (110 - LineLength)
     c                              < (TokenLength + 2)
     c                   eval      loc(L1) = %trim(loc(L1)) + xxsep
     c                   eval      L1 = L1 + 1
     c                   eval      loc(L1) = token

     c                   else

     c                   if        loc(L1) = *blanks
     c                   eval      loc(L1) = token
     c                   else
     c                   cat       xxsep:0       loc(L1)
     c                   cat       token:1       loc(L1)
     c                   endif

     c                   endif

     c                   if        L1 > 14
     c*** begin add ***  07/28/21  ************************************
     c                             or L2 > 14
     c*** end add *****  07/28/21  ************************************
     c                   eval      xxlast = 'Y'
     c                   endif

     c                   endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     pgmstr        begsr
     c                   move      'Y'           xx1st             1
     c                   exsr      getfnd
     C                   endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     pgmend        begsr
     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     c     valid         begsr
       //*** Begin change *** 07/25/17 *************************************
     c***                   if        ((fnpeof = 'S') and (eldiv <> 0))
     c***                             or ((fnpeof = 'M') and (elemp# <> 1))
     c***                   move      *on           crtfrm
     c***                   endif
      /Free
       //*** Begin Chg *** 10/06/17 *********************************************
       //Select;
       //When fnpeof = 'S' and ELdiv <> 0;
       //   crtfrm = *on;
       //When fnpeof = 'M' and elemp# <> 1;
       //   crtfrm = *on;
       //When fnpeof = 'C' and elemp# = 1 and ELdiv <> 0;
       //   crtfrm = *on;
       //When fnpeof = 'C' and elemp# <> 1;
       //   crtfrm = *on;
       //Endsl;
         crtFrm = *On;
       //*** End   Chg *** 10/06/17 *********************************************
       //*** End change *** 07/25/17 *************************************
      /End-Free
     c                   endsr
      *=========================================================================
      * ENDRPT - Update Endorsement Reporting File
      *=========================================================================
     c     endrpt        begsr
      *  If printing a policy page.  (Not called from W1054R)
     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'
      * Update the endorsement reporting file with the proper data.
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
     c                   clear                   E22304
     c                   eval      xhnm1 = %trim(emfdnm)                        Name of Client
     C*** begin add ***  01/23/18  *********************************************
     c                   eval      xhnm1 = %trim(emfdnm + emfdn2)               Name of Client
     C                   If        emfdnm = ' '
     c                   eval      xhnm1 = %trim(emname + emnam2)               Name of Client
     C                   Endif
     C*** end add *****  01/23/18  *********************************************
     C*** Beg add *****  08/23/19  *********************************************
     C                   if        emstrp ='Y'                                  State reporting div
     C                   if        emmad1 <> ' '
     C                   eval      xhadt  = 1
     c                   eval      xhad1  = %trim(emmad1)                       Mail Address - Stre
     c                   eval      xhcty = %trim(emmcty)                        Mail Address - City
     c                   eval      xhst  = %trim(emmst)                         Mail Address - State
     c                   move      emmzip        xhzip                          Mail Address - Zip
     c                   elseif    empad1 <> ' '
     c                   eval      xhadt  = 1                                   Leas Addr Type code
     c                   eval      xhad1 = %trim(empad1)
     c                   eval      xhcty = %trim(empcty)                        Phy Address - City
     c                   eval      xhst  = %trim(empst)                         Phy Address - State
     c                   move      empzip        xhzip                          Phy Address - Zip
     c                   Endif
     c                   Else                                                   emstrp ='N'
     C*** end add *****  08/23/19  *********************************************
     c                   if        empad1 <> ' '
     c                   eval      xhadt  = 2                                   Leas Addr Type code
     c                   eval      xhad1 = %trim(empad1)
     c                   eval      xhcty = %trim(empcty)                        Phy Address - City
     c                   eval      xhst  = %trim(empst)                         Phy Address - State
     c                   move      empzip        xhzip                          Phy Address - Zip
     c                   elseif    emmad1 <> ' '
     c*** Beg chg *****  08/23/19  *********************************************
     c***                eval      xhadt  = 1                                   Leas Addr Type code
     c                   eval      xhadt  = 2                                   Leas Addr Type code
     C*** End chg *****  08/23/19  *********************************************
     c                   eval      xhad1  = %trim(emmad1)                       Mail Address - Stre
     c                   eval      xhcty = %trim(emmcty)                        Mail Address - City
     c                   eval      xhst  = %trim(emmst)                         Mail Address - State
     c                   move      emmzip        xhzip                          Mail Address - Zip
     c                   endif
     C*** Beg Add *****  08/23/19  *********************************************
     c                   endif                                                  State reporting div
     C*** End Add *****  08/23/19  *********************************************
     c                   eval      xhfei  = emfein                              Client FEIN
     c                   eval      xhuin  = emue#                               Clt Unemployment #

     c                   move      *blanks       pedata
     c                   movel     E22304        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      * PEO Name
     c                   move      elco#         #1CO#
     c                   move      elfnd         #1FND
     c                   move      elemp#        #1EMP#
     c                   move      eldiv         #1DIV
      * Get employer name
     C                   call      'SRADDR'
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1emp#
     C                   parm                    #1div
     C                   parm      ' '           ##odr
     C                   parm      ' '           ##nodr
     C                   parm      ' '           ##aodr
     C                   parm      *blanks       ##emn1
     C                   parm      *blanks       ##emn2
     C                   parm      *blanks       ##emn3
     C                   parm      *blanks       ##emn4
     C                   parm      *blanks       ##emn5
     C                   parm      *blanks       ##emn6
     C                   parm      *blanks       ##emn7
     C                   parm      *zero         ##adrl
     C                   parm      *zero         ##naml
     C                   parm      *zero         ##fedl
     C                   parm      *blanks       ##peo1
     C                   parm      *blanks       ##peo2
     C                   parm      '1'           ##pgfl

     c                   eval      pename = ##peo1

     c                   if        xx1st = 'Y'
     C                   update    ppe01
     c                   eval      xx1st = 'N'

     c                   else

     c                   movel     q1user        peiusr
     c                   movel     q1pgm         peipgm
     c                   z-add     dstime        peitim
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         peidt
     C                   write     ppe01
     c                   endif

     c                   endif                                                  *in60 ifeq off

     C                   endif

     c                   endsr
