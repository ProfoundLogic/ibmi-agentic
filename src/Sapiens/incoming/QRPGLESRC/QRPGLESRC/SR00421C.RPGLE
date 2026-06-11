     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00421C - CATASTROPHE PREMIUM ENDORSEMENT
      //===============================================================*
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
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      // 08/01/08  C:190 PROJECT WC1744                                *
      //           C:61  Created.                                      *
      //                                                               *
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
     F/COPY SCOPYEN01F
      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
      ** Statistical Reporting/Rating File
     Fwxfmp3    o    e             disk    usropn

     D/COPY SCOPYEN01D

     D                 DS
     D  ST                                 like(fmcd01)
     D                                     DIM(52) ASCEND

     D                 DS
     D  RT                                 like(fmrt01)
     D                                     DIM(52) ASCEND

     D                 DS
     D  PR                                 like(fmam01)
     D                                     DIM(52) ASCEND

     D  FM1ds          ds
     dfmc401
     dfmc402
     dfmc403
     dfmc404
     dfmc405
     dfmc406
     dfmc407
     dfmc408
     dfmc409
     dfmc410
     dfmc411
     dfmc412
     dfmc413
     dfmc414
     dfmc415
     dfmc416
     dfmc417
     dfmc418
     dfmc419
     dfmc420
     dfmc421
     dfmc422
     dfmc423
     dfmc424
     dfmc425
     dfmc426
     dfmc427
     dfmc428
     dfmc429
     dfmc430
     dfmc431
     dfmc432
     dfmc433
     dfmc434
     dfmc435
     dfmc436
     dfmc437
     dfmc438
     dfmc439
     dfmc440
     dfmc441
     dfmc442
     D fm1                                 OVERLAY(fm1ds)
     D                                     LIKE(fmc401) DIM(42)

     D  FM2ds          ds
     dfmnm01
     dfmnm02
     dfmnm03
     dfmnm04
     dfmnm05
     dfmnm06
     dfmnm07
     dfmnm08
     dfmnm09
     dfmnm10
     D fm2                                 OVERLAY(fm2DS)
     D                                     LIKE(fmnm01) DIM(10)

     d x1              s              3  0

     d stsav           s                   like(fmcd01)
     d rtsav           s                   like(fmrt01)
     d prsav           s                   like(fmam01)

     d sttmp           s                   like(fmcd01)
     d rttmp           s                   like(fmrt01)
     d prtmp           s                   like(fmam01)

     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4

     d kkefdt          s                   like(elpfdt)

     D qqForm          S             15a   Inz('WC000421C ')

     D addpgm          c                   const('SRXXXXXXXX')

     D SR00421C        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00421C        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *  Statistical Reporting/Rating
     C     rrky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
      *
     C     rrky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    kkefdt

      //=========================================================================
      //  Main
      //=========================================================================

     C/COPY SCOPYEN01C
      //=========================================================================
      //  OTHINF - Subroutine to load form specific information & write record
      //=========================================================================
     C     othinf        begsr
      ** open work file
     C                   open      wxfmp3
      *    initialize
     C                   clear                   pfm03
     c                   movel(p)  xxform        fmform
      ** Get state / rate information
     C     elky2         Chain     pel02                              60

     C                   Dow       *in60 = *off
     C     rsky          Chain     prs01                              60

     C                   if        (*in60 = *off)
     C                   eval      kkefdt = elpfdt
     C     rrky1         setgt     prr01
     C     rrky          readpe    prr01                                  60

     C     *in60         Ifeq      *off
     C     rrdtrc        andne     0
     c                   move(p)   RRCO#         #1co#
     c                   move(p)   RRFND         #1fnd
     c                   move(p)   ELRTCD        ##rtcd
     c                   move(p)   RRDTRC        ##clas
     c                   move(p)   ELPFDT        ##efdt

     c                   call      'SRLCM'
     c                   parm                    #1co#
     c                   parm                    #1fnd
     c                   parm                    ##rtcd
     c                   parm                    ##clas
     c                   parm                    ##efdt
     c                   parm                    ##lcm

     c                   move      ##lcm         xxlcm
     c                   eval(h)   rtsav  = RRDTRR * xxlcm
     c                   movel(p)  rsjrst        stsav
     c                   eval      prsav  = eledtr
     c                   exsr      srins
     C                   EndIf

     C                   EndIf

     C     elky2         ReadE     pel02                                  60
     C                   EndDo
      ** Write the output record
     c                   exsr      srout

     C                   write     pfm03
      ** Close the endorsement form workfile
     C                   close     wxfmp3

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
      *  srins - Insertion sort
      *=========================================================================
     C     srins         begsr

     c                   z-add     1             x1

     C     x1            Dowle     52

     c                   select

     C     st(x1)        wheneq    *blanks
     c                   movel(p)  stsav         st(x1)
     c                   z-add     rtsav         rt(x1)
     c                   z-add     prsav         pr(x1)
     c                   leave

     C     stsav         whengt    st(x1)
     c                   add       1             x1
     c                   iter

     C     stsav         wheneq    st(x1)
     c     rtsav         ifgt      rt(x1)
     c                   z-add     rtsav         rt(x1)
     c                   endif
     c                   add       prsav         pr(x1)
     c                   leave

     C                   other
     c                   movel(p)  st(x1)        sttmp
     c                   z-add     rt(x1)        rttmp
     c                   z-add     pr(x1)        prtmp
     c                   movel(p)  stsav         st(x1)
     c                   z-add     rtsav         rt(x1)
     c                   z-add     prsav         pr(x1)
     c                   movel(p)  sttmp         stsav
     c                   z-add     rttmp         rtsav
     c                   z-add     prtmp         prsav
     c                   add       1             x1
     c                   iter

     c                   endsl

     C                   EndDo

     C                   endsr
      *=========================================================================
      * SROUT - Move data from arrays to output fields
      *=========================================================================
     C     srout         begsr

     c                   movel(p)  emnam2        fmc313
     c                   move      emnam2        fmc314
     c                   movel(p)  emfdn2        fmc315
     c                   move      emfdn2        fmc316
     c                   z-add     1             x1

     c     x1            Dowle     26

     c                   if        st(x1) = *blank
     c                   leave
     c                   endif

     c                   eval      fm1(x1) = '    ' +
     c                             st(x1) +
     c                             '   ' +
     c                             %editc(rt(x1):'4') +
     c                             '   ' +
     c                             %editc(pr(x1):'K':'$')
     c                   add       1             x1
     c                   enddo

     c                   z-add     27            x1

     c     x1            Dowle     42

     c                   if        st(x1) = *blank
     c                   leave
     c                   endif

     c                   eval      fm1(x1) = '      ' +
     c                             st(x1) +
     c                             '   ' +
     c                             %editc(rt(x1):'4') +
     c                             '   ' +
     c                             %editc(pr(x1):'K':'$')
     c                   add       1             x1
     c                   enddo

     c                   z-add     43            x1

     c     x1            Dowle     52

     c                   if        st(x1) = *blank
     c                   leave
     c                   endif

     c                   eval      fm2(x1-42) = '      ' +
     c                             st(x1) +
     c                             '   ' +
     c                             %editc(rt(x1):'4') +
     c                             '   ' +
     c                             %editc(pr(x1):'K':'$')

     c                   add       1             x1
     c                   enddo

     C                   endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     valid         begsr

     C     rsky          chain     prs01                              60

     C                   if        (*in60 = *off)
     c                             and (elpfdt >= 1080901)
     c                   eval      kkefdt = elpfdt
     c     rrky1         setgt     prr01
     c     rrky          readpe    prr01                                  60
      *  Attach endorsement if non-zero domestic code for jurisdiction state
     C     *in60         ifeq      *off
     C     rrdtrc        andne     *zero
     C                   move      *on           crtfrm
     C                   endif

     C                   endif

     C                   endsr
