     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE WA066R - Lost Days Calculation.
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:
     H*
     H*    To calculate the number of Lost Days
     H*
     H*  CALLED FROM:
     H*
     H*
     H*  CALLS TO:
     H*
     H*    PROGRAMMER: C:190
     H*    CREATED:    08/29/11     PROJECT: WC2067
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     h*  08/13/14 C:279 Project: WC2067                               *
     h*                 Updated Lost Days calculation based on Date   *
     h*                 of Death and Date of Settlement.              *
     h*                                                               *
     H*****************************************************************
     fwmwhl     if   e           k disk
     fwzdbp     if   e             disk
      ******* beg add ********* 08/13/14 ******************************
     fwmlll     if   e           k disk    rename(pll01:pll00)
     fwmcml1    if   e           k disk
      ******* end add ********* 08/13/14 ******************************
      *
     d y               s              3  0
     d z               s              3  0
     d @@rtw           s              7  0
     d xxrtw           s              7  0
     d @@dbb           s              7  0
     d xxdbb           s              7  0
     d Totadl          s              5  0
     d @@Totadl        s              5  0
     d qqto            s              8  0
     d xxdt            s              8  0
     d @@WCEpgm        s              1
     d xxWCEpgm        s              1
     d @@co#           s                   like(whco#)
     d @@fnd           s                   like(whfnd)
     d @@fyr           s                   like(whfyr)
     d @@case          s                   like(whcase)
     d xxco#           s                   like(whco#)
     d xxfnd           s                   like(whfnd)
     d xxfyr           s                   like(whfyr)
     d xxcase          s                   like(whcase)
      ******* beg add ********* 08/13/14 ******************************
     d intStlDt        s                   like(LLstdt)
     d intDate         s                   like(LLstdt)
     d intDateArr      s              7  0 dim(3) ascend
     d intCount        s              2  0
      ******* end add ********* 08/13/14 ******************************
     d*
     d                 ds
     D dsdttm                        14  0
     d dsdate                         8  0 overlay(dsdttm:7)
     d dstime                         6  0 overlay(dsdttm)
      *
     d lr              s              7  0 dim(198)                             All WHDATE dates
     d l               s              7  0 dim(99)                              Loosing Time
     d r               s              7  0 dim(99)                              Returned to work


     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@fnd
     c                   parm                    @@fyr
     c                   parm                    @@case
     c                   parm                    @@rtw
     c                   parm                    @@dbb
     c                   parm                    @@Totadl
     c                   parm                    @@WCEpgm
      *
     c     dtmdyy        plist
     c                   parm                    srdti             8 0
     c                   parm      '*CYMD'       srffmt            8
     c                   parm      *blanks       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1

     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

      *** Work History file.
     c     whkey1        klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c                   kfld                    xxcase
      *
     c                   eval      y = 1
     c                   eval      z = 1

      ** Populate work date Arrays

     c                   move      *off          lost              1
     c                   move      *off          rtw               1
     c                   eval      xxco#  = @@co#
     c                   eval      xxfnd  = @@fnd
     c                   eval      xxfyr  = @@fyr
     c                   eval      xxcase = @@case
     c                   eval      xxrtw  = @@rtw
     c                   eval      xxdbb  = @@dbb
     c                   eval      xxWCEpgm = @@WCEpgm

      /free
       //******* beg add ********** 08/13/14 ***********************************
       intStlDt = 0;
       intDateArr(1) = 0;

       Chain (@@co#:@@fnd:@@fyr:@@case) pll00;
       If %found(wmlll);
          intStlDt = LLstdt;
       Endif;

       Chain (@@co#:@@fnd:@@fyr:@@case) pcm01;
       Exsr getErlstDt;

       //******* end add ********** 08/13/14 ***********************************
      /end-free
     c                   z-add     0             Y
     c     whkey1        chain(n)  pwh01                              99
     c                   dow       *in99 = *off
     c*
     c                   if        lost = *off
     c                   if        whwkst = 'L'
     c                   add       1             y
     c                   z-add     whdate        lr(y)
     c                   move      *on           lost
     c                   endif
     c                   else
     c                   if        whwkst = 'R'
     c                   if        rtw = *off
     c                   add       1             y
     c                   z-add     whdate        lr(y)
     c                   move      *on           rtw
     c                   else
     c                   z-add     whdate        lr(y)
     c                   endif
     c                   else
     c                   if        whwkst = 'L'
     c                   add       1             y
     c                   z-add     whdate        lr(y)
     c                   move      *off          rtw
     c                   endif
     c                   endif
     c                   endif
     c*
     c     whkey1        reade(n)  pwh01                                  99
     c                   enddo
     c*

     c                   z-add     1             y
     c                   z-add     1             z                 3 0
     c                   dou       lr(y) = *zero
     c                   z-add     lr(y)         l(z)
     c                   add       1             y
     c                   z-add     lr(y)         r(z)
     c                   add       1             y
     c                   add       1             z
     c                   enddo
      *
     c                   z-add     1             y                 3 0
     c                   z-add     *zero         Totadl
      *
     c                   dou       l(y) = *zero
      /free
       //******* beg add ********* 08/13/14 ************************************
       If intDate > 0  and l(y) > intDate;
          Leave;
       Endif;
       //******* end add ********* 08/13/14 ************************************
      /end-free
     c                   z-add     l(y)          srdti
     c                   call      'SRCVTDAT'    dtmdyy                         7,0 --> 8,8
     c                   movel     srdto         qqfrom            8 0

     c                   if        r(y) = *zero  and l(y) <> *zero
     c                   time                    dsdttm
     c                   call      'SRPAST8'
     c                   parm      dsdate        qqdt              8 0
      ****** beg chg ******** 08/13/14 *****************************************
     c******             parm      1             qqdays            5 0
     c                   parm      *zero         qqdays            5 0
      ****** end chg ******** 08/13/14 *****************************************
     c                   parm                    qqerr             1
     c                   parm                    qqterm            1
      *
     C                   z-add     qqdt          srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         r(y)
     c                   endif

      /free
       //******* beg add ********* 08/13/14 ************************************
       If intDate > 0  and r(y) > intDate;
          r(y) = intDate;
       Endif;
       //******* end add ********* 08/13/14 ************************************
      /end-free
     c                   z-add     r(y)          srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         qqto              8 0
      *
     c                   if        qqfrom <> *zero and qqto <> *zero
      ********** beg add ********* 08/13/14 ************************************
     c                             and r(y) > l(y)
      ********** end add ********* 08/13/14 ************************************
     c                   call      'SRDDIF8'
     c                   parm                    qqfrom
     c                   parm                    qqto
     c                   parm      *zero         qqdays            5 0
     c                   parm      *blank        qqerr             1

     c                   add       qqdays        Totadl
     c                   add       1             y
     c                   else
     c                   leave
     c                   endif
     c                   enddo

      *  Now add days between dates.

      /free
       //******* beg add ********** 08/13/14 ***********************************
       intDateArr(1) = xxRTW;
       Exsr getErlstDt;

       // Use earliest fo Actual RTW/Date of Death/Date of Settlement
       xxRTW = intDate;
       //******* end add ********** 08/13/14 ***********************************
      /end-free

     c                   select

     c* If  the value in @@rtw parm is not zeros; the calculation process is
     c*
     c                   when      xxrtw <> 0

     c                   eval      srdti = xxdbb
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         qqfrom            8 0
     c                   eval      srdti = xxrtw
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         qqto              8 0

     c* If the value in @@rtw parm is zeros; the calculation will be based on
      * the system date and then subtracting 1 day.
     c                   when      xxrtw = 0 and xxWCEpgm = ' '
     c                   eval      srdti = xxdbb
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         qqfrom            8 0
     c                   time                    dsdttm
     c                   call      'SRPAST8'
     c                   parm      dsdate        qqdt              8 0
      ****** beg chg ******** 08/13/14 *****************************************
     c******             parm      1             qqdays            5 0
     c                   parm      *zero         qqdays            5 0
      ****** end chg ******** 08/13/14 *****************************************
     C                   parm                    qqerr             1
     C                   parm                    qqterm            1
     c
     c                   eval      qqto = qqdt

     c* If called from the Warehouse Claim Extract program W8904R
     c                   when      xxrtw = 0 and xxWCEpgm = 'Y'
     c     1             chain     pdb01                              60
     c                   if        *in60 = *off
     c                   eval      srdti = xxdbb
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         qqfrom            8 0
     c                   movel     dbsdt         qqto              8 0
     c                   endif                                                  if *in60 = 0

     c                   endsl

     c                   call      'SRDDIF8'
     c                   parm                    qqfrom
     c                   parm                    qqto
     c                   parm      *zero         qadays            5 0
     c                   parm      *blank        qqerr
      *
     c                   add       qadays        Totadl
     c                   eval      @@Totadl  = Totadl
      *
     c                   eval      *inlr= *on
     c                   return
      /free
       //********* beg add ******** 08/13/14 ***********************************
       //-----------------------------------------------------------------------
       // Get Earliest Date
       //-----------------------------------------------------------------------
       Begsr getErlstDt;

       intDateArr(2) = intStlDt;
       intDateArr(3) = CMdod;
       SortA intDateArr;
       intDate  = 0;

       For intCount = 1 to 3;
          If intDateArr(intCount) > 0 and intDate = 0;
             intDate = intDateArr(intCount);
          Endif;
       Endfor;

       Endsr;
       //********* end add ******** 08/13/14 ***********************************
      /end-free
