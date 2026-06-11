     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *
      * WA722R - ADJNAMES API
      *****************************************************************
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
      *****************************************************************
      * Revised:
      *
      * 01/11/10  C:61  Project: 135WNO
      *                 Add processing for new form "BI DED OK 05 09"
      *
      * 11/13/09  C:190 Project: 135WNJ
      *                 Add processing for new forms with
      *                 non-standard names:
      *
      * 09/15/09  C:190 Project: 135WNH
      *                 Add processing for new forms with
      *                 non-standard names:
      *
      * 09/15/09 C:136 Project: 135WMR
      *                Use API for a single endorsement. Leave loop when
      *                blank encountered in xf1.
      *
      * 09/08/09  C:190 Project: 135WNG
      *                 Add processing for new forms with
      *                 non-standard names:
      *
      * 08/24/09  C:190 Project: 135WNF
      *                 Created.
      *
      *****************************************************************

     d xf              s             18    dim(500)
     d xf1             s             35    dim(50)

     d cltid           s              3
     d endname         c                   const('WC990610A -
     d                                     PPC/ICI (PA)')
     d endnam1         c                   const('WC990710A PPC/ICI (IL)')
     d endnam2         c                   const('WC990710A PPC/ICI (TN)')
     d endnam3         c                   const('WC990610A PPC/ICI (GA)')
     d endnam4         c                   const('WC990710 PPC/ICI (FL)')
     d endnam5         c                   const('WC990710 PPC/ICI')
     d endnam6         c                   const('BI DED 05 09')
     D endnam7         c                   const('BI DED CA 05 09')
     D endnam8         c                   const('BI DED IL 05 09')
     D endnam9         c                   const('BI DED PA 05 09')
     D endnam10        c                   const('BI DED ME 05 09')
     D endnam11        c                   const('BI DED MO 05 09')
    *** BEG ADD ******* 09/08/09 ******************************************
     D endnam12        c                   const('BI DED ID 05 09')
     D endnam13        c                   const('BI DED KY 05 09')
     D endnam14        c                   const('BI DED WV 05 09')
    *** END ADD ******* 09/08/09 ******************************************
    *** BEG ADD ******* 09/15/09 ******************************************
     D endnam15        c                   const('BI DED AR 05 09')
     D endnam16        c                   const('BI DED CT 05 09')
     D endnam17        c                   const('BI DED DE 05 09')
    *** END ADD ******* 09/15/09 ******************************************
    *** BEG ADD ******* 11/13/09 ******************************************
     D endnam18        c                   const('BI DED OR 05 09')
     D endnam19        c                   const('BI DED GA 05 09')
     D endnam20        c                   const('BI DED UT 05 09')
    *** END ADD ******* 11/13/09 ******************************************
    *** BEG ADD ******* 01/11/10 ******************************************
     D endnam21        c                   const('BI DED OK 05 09')
    *** end ADD ******* 01/11/10 ******************************************

     c     *entry        plist
     c                   parm                    qqxf            900
     c                   parm                    qqxf1          1750

     C                   call      'RTVCLTID'
     C                   parm                    cltid
     C*
     c*====================================================================
     c* ADJUST THE NAMES
     c*====================================================================
     c                   movea     qqxf          xf
     c                   movel(P)  xf            xf1
     c*
     c                   if        cltid = '135'
     c                   move      1             j                 2 0
     c                   dow       j<=50
      *** beg add ***** 09/15/09 ******************************************
     c                             and xf1(j) <> *blanks
      *** end add ***** 09/15/09 ******************************************
     c*
     c                   if        %subst(xf1(j):1:11)= 'WC990610APA'
     c                   movel     *Blanks       @efdt             7
     c                   Eval      @efdt = %subst(xf1(j):12:7)
     c*
     c     endname       cat(p)    @efdt         xf1(j)
     c*
     C                   endif

     C                   if        %subst(xf1(j):1:11)= 'WC990710AIL'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):12:7)
     C*
     c     endnam1       cat(p)    @efdt         xf1(j)
     c*
     C                   endif

     C                   if        %subst(xf1(j):1:11)= 'WC990710ATN'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):12:7)
     C*
     C     endnam2       cat(p)    @efdt         xf1(j)
     C*
     C                   endif
     C*
     C                   if        %subst(xf1(j):1:11)= 'WC990610AGA'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):12:7)
     C*
     C     endnam3       cat(p)    @efdt         xf1(j)
     C*
     C                   endif

     C                   if        %subst(xf1(j):1:10)= 'WC990710FL'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     C*
     c     endnam4       cat(p)    @efdt         xf1(j)
     c*
     C                   endif

     C                   if        %subst(xf1(j):1:10)= 'WC990710PI'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     C*
     c     endnam5       cat(p)    @efdt         xf1(j)
     c*
     C                   endif

     C                   if        %subst(xf1(j):1:10)= 'BIDED509CA'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam7       cat(p)    @efdt         xf1(j)
     c     endnam7       cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509CA

     C                   if        %subst(xf1(j):1:10)= 'BIDED509IL'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam8       cat(p)    @efdt         xf1(j)
     c     endnam8       cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509IL

     C                   if        %subst(xf1(j):1:10)= 'BIDED509PA'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam9       cat(p)    @efdt         xf1(j)
     c     endnam9       cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509PA

     C                   if        %subst(xf1(j):1:10)= 'BIDED509MO'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam10      cat(p)    @efdt         xf1(j)
     c     endnam10      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509MO

     C                   if        %subst(xf1(j):1:10)= 'BIDED509ME'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam11      cat(p)    @efdt         xf1(j)
     c     endnam11      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509ME
    *** BEG ADD ******* 09/08/09 ******************************************
     C                   if        %subst(xf1(j):1:10)= 'BIDED509ID'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam12      cat(p)    @efdt         xf1(j)
     c     endnam12      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509ID

     C                   if        %subst(xf1(j):1:10)= 'BIDED509KY'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam13      cat(p)    @efdt         xf1(j)
     c     endnam13      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509KY

     C                   if        %subst(xf1(j):1:10)= 'BIDED509WV'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam14      cat(p)    @efdt         xf1(j)
     c     endnam14      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509WV
    *** END ADD ******* 09/08/09 ******************************************
    *** BEG ADD ******* 09/15/09 ******************************************
     C                   if        %subst(xf1(j):1:10)= 'BIDED509AR'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam15      cat(p)    @efdt         xf1(j)
     c     endnam15      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509AR

     C                   if        %subst(xf1(j):1:10)= 'BIDED509CT'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam16      cat(p)    @efdt         xf1(j)
     c     endnam16      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509CT

     C                   if        %subst(xf1(j):1:10)= 'BIDED509DE'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam17      cat(p)    @efdt         xf1(j)
     c     endnam17      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509DE
    *** END ADD ******* 09/15/09 ******************************************
    *** BEG ADD ******* 11/13/09 ******************************************
     C                   if        %subst(xf1(j):1:10)= 'BIDED509OR'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam18      cat(p)    @efdt         xf1(j)
     c     endnam18      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509OR

     C                   if        %subst(xf1(j):1:10)= 'BIDED509GA'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam19      cat(p)    @efdt         xf1(j)
     c     endnam19      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509GA

     C                   if        %subst(xf1(j):1:10)= 'BIDED509UT'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam20      cat(p)    @efdt         xf1(j)
     c     endnam20      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509UT
    *** END ADD ******* 11/13/09 ******************************************
    *** BEG ADD ******* 01/11/10 ******************************************

     C                   if        %subst(xf1(j):1:10)= 'BIDED509OK'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):11:7)
     c     endnam21      cat(p)    @efdt         xf1(j)
     c     endnam21      cat(p)    @efdt         xf(j)
     C                   endif                                                  BIDED509UT
    *** end ADD ******* 01/11/10 ******************************************

     C                   if        %subst(xf1(j):1:8)= 'BIDED509'
     C                   movel     *Blanks       @efdt             7
     C                   Eval      @efdt = %subst(xf1(j):9:7)
     c     endnam6       cat(p)    @efdt         xf1(j)
     c     endnam6       cat(p)    @efdt         xf(j)
     C                   endif

     C                   add       1             j
     C                   Enddo
     C                   endif

     c                   movea     xf1           qqxf1

     C                   return
