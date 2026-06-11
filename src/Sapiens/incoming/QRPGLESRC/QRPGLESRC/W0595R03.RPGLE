     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*********************************************************************
     H/TITLE W0595R03 - Fringe Benefits Calculator                        *
     H*********************************************************************
     H*  CREATED: 06/01/11  PROGRAMMER: C:190 PROJECT: 25WPW              *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0595f03  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    f*
    f*  Physical file definition (for maintaining record)
     fwaf1p     uf a e             disk
     f                                     rename(pf101:dbrcdf)
     fwmcmp     if   e             disk
     fwaf1l     uf   e           k disk
     fwaf2l     uf a e           k disk
     fwmwcp     uf   e             disk
     fwatil     if   e           k disk
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d optnm           c                   'Fringe Benefits Calculator'
     d comapi          c                   'WA001R'
     d #ofscr          c                   1
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d ##co#           s              3  0
     d ##conm          s             40
     d ##fnam          s             40
     d ##fnd           s              3  0
     d @@kc            s              1
     d ##kc            s              1
     d @@kl            s              1
     d ##kl            s              1
     d @@mode          s              1
     d @@mode1         s              1
     d @@rrn           s              9  0
     d @@rrn1          s              9  0
     d @@rrn2          s              9  0
     d @@seq#          s              5  0
     d @@beg           s              1
     d kkco#           s                   like(f1co#)
     d kkfnd           s                   like(f1fnd)
     d kkfyr           s                   like(f1fyr)
     d kkcase          s                   like(f1case)
     d kkseq#          s                   like(f1seq#)
     d kksseq#         s                   like(f1seq#)
     d sqseq#          s                   like(f1seq#)
     d nseq#           s                   like(f1seq#)  inz(0)
     d xxerr           S              1a   inz
     d xxfyr           s                   like(f1fyr)
     d xxChkDelete     s                   like(*in03)

     d i               s              2  0
     d count           s              2  0
     d count1          s              2  0
     D of#             s              2  0
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
    d*---------------------------------------------------------------------

     d* Health Insure array.
     d hi2ds           ds
     d schlth1
     d schlth2
     d schlth3
     d schlth4
     d schlth5
     d schlth6
     d schlth7
     d schlth8
     d schlth9
     d schlth10
     d schlth11
     d schlth12
     d schlth13
     d schlth14
     d hi2                                 overlay(hi2ds)
     d                                     like(schlth1) dim(14) inz(0)

      *

     d* Laundry Cleaning array
     d lc2ds           ds
     d scldry1
     d scldry2
     d scldry3
     d scldry4
     d scldry5
     d scldry6
     d scldry7
     d scldry8
     d scldry9
     d scldry10
     d scldry11
     d scldry12
     d scldry13
     d scldry14
     d lc2                                 overlay(lc2ds)
     d                                     like(scldry1) dim(14) inz(0)

      *

     d* Clothing Uniforms
     d cu2ds           ds
     d scclot1
     d scclot2
     d scclot3
     d scclot4
     d scclot5
     d scclot6
     d scclot7
     d scclot8
     d scclot9
     d scclot10
     d scclot11
     d scclot12
     d scclot13
     d scclot14
     d cu2                                 overlay(cu2ds)
     d                                     like(scclot1) dim(14) inz(0)

      *

     d* Lodging Rents array
     d lr2ds           ds
     d scrent1
     d scrent2
     d scrent3
     d scrent4
     d scrent5
     d scrent6
     d scrent7
     d scrent8
     d scrent9
     d scrent10
     d scrent11
     d scrent12
     d scrent13
     d scrent14
     d lr2                                 overlay(lr2ds)
     d                                     like(scrent1) dim(14) inz(0)

      *

     d* Food Meals.
     d fm2ds           ds
     d scfood1
     d scfood2
     d scfood3
     d scfood4
     d scfood5
     d scfood6
     d scfood7
     d scfood8
     d scfood9
     d scfood10
     d scfood11
     d scfood12
     d scfood13
     d scfood14
     d fm2                                 overlay(fm2ds)
     d                                     like(scfood1) dim(14) inz(0)

      *

     d* Vehical Fuel.
     d vf2ds           ds
     d scfuel1
     d scfuel2
     d scfuel3
     d scfuel4
     d scfuel5
     d scfuel6
     d scfuel7
     d scfuel8
     d scfuel9
     d scfuel10
     d scfuel11
     d scfuel12
     d scfuel13
     d scfuel14
     d vf2                                 overlay(vf2ds)
     d                                     like(scfuel1) dim(14) inz(0)

      *

     d* Others.
     d ot2ds           ds
     d scothr1
     d scothr2
     d scothr3
     d scothr4
     d scothr5
     d scothr6
     d scothr7
     d scothr8
     d scothr9
     d scothr10
     d scothr11
     d scothr12
     d scothr13
     d scothr14
     d ot2                                 overlay(ot2ds)
     d                                     like(scothr1) dim(14) inz(0)
      *

     d* Subseq# array
     d sb2ds           ds
     d hfld1
     d hfld2
     d hfld3
     d hfld4
     d hfld5
     d hfld6
     d hfld7
     d hfld8
     d hfld9
     d hfld10
     d hfld11
     d hfld12
     d hfld13
     d hfld14
     d sb2                                 overlay(sb2ds)
     d                                     like(hfld1) dim(14) inz(0)
     d* Subseq# array for WAF2P
     d f2p                                 like(hfld1)
     d                                     dim(14)
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn                          wmwcp
     C                   parm                    @@rrn1                         wmcmp
     C                   parm                    @@beg
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *
     C     xxkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
      *
     C     xxkey1        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C                   kfld                    kksseq#
      *
     C     xxkey2        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

     C     xxkeyp        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     sqkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    sqseq#
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   exsr      callr
     C                   endsr
    c*================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
    c*================================================================
     c     errck1        begsr

     c     xxkey         setll     pti01
     c     xxkey         reade     pti01                                  60
     c                   eval      i = 1
     c                   eval      count = 0
     c                   dow       *in60 = *off
     c                   eval      count = count + 1
     c                   eval      i = i + 1
     c                   if        i > 14
     c                   leave
     c                   endif
     c     xxkey         reade     pti01                                  60
     c                   enddo

     c                   eval      i = 1
     c                   eval      count1 = 0
     c                   do        14
     c                   if        hi2(i) <> 0 or lc2(i) <> 0  or
     c                             cu2(i) <> 0 or lr2(i) <> 0  or
     c                             fm2(i) <> 0 or vf2(i) <> 0  or
     c                             ot2(i) <> 0
     c                   eval      count1 = count1 + 1
     c                   if        count1 > count
     c                   eval      of# = 40 + i
     c                   move      *on           *in(of#)
     c                   eval      *in90 = '1'
     c                   leavesr
     c                   endif                                                  count1 > count
     c                   eval      i = i + 1
     c                   endif                                                  hi2(i) <> 0

     c                   enddo

     c                   endsr
     c*
    c*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr
     c*
     c                   if        *inke = *on and @@mode = 'P'
     c                   eval      hi2 = 0
     c                   eval      lc2 = 0
     c                   eval      cu2 = 0
     c                   eval      lr2 = 0
     c                   eval      fm2 = 0
     c                   eval      vf2 = 0
     c                   eval      ot2 = 0
     c                   eval      sb2 = 0
     c                   eval      f2p = 0
     c                   endif
     c*
     c     xxkey         setll     pf101
     c     xxkey         reade     pf101                                  60
     c                   eval      i = 1
     c                   dow       (*in60 = *off) or
     c                             (*inke = *on and *in60 = *on)

     c                   if        *in60 = *off
     c                   eval      hi2(i) =f1hlth                               Health Insurance.
      *
     c                   eval      lc2(i) =f1ldry                               Laundry cleaning.
     c*
     c                   eval      cu2(i) =f1clots                              Clothing Uniform.
     c*
     c                   eval      lr2(i) =f1rent                               Lodging rent
     c*
     c                   eval      fm2(i) =f1food                               Food meals
     c*
     c                   eval      vf2(i) =f1fuel                               Vehical fuel
     c*
     c                   eval      ot2(i) =f1othr                               Others
     c*
     c                   eval      sb2(i) =f1sseq#                              Load Subseq#
     c                   else
     c                   eval      hi2(i) = 0
     c                   eval      lc2(i) = 0
     c                   eval      cu2(i) = 0
     c                   eval      lr2(i) = 0
     c                   eval      fm2(i) = 0
     c                   eval      vf2(i) = 0
     c                   eval      ot2(i) = 0
     c                   eval      sb2(i) = 0
     c                   eval      f2p(i) = 0
     c                   endif

     c                   eval      i = i + 1
     c                   if        i > 14
     c                   leave
     c                   endif
     c     xxkey         reade     pf101                                  60
     c                   enddo
     c*
     c                   endsr
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr
     c*
     c                   eval      f1co#  = cmco#
     c                   eval      f1fnd  = cmfnd
     c                   eval      f1fyr  = cmfyr
     c                   eval      f1case = cmcase

     C                   endsr                                                  mvscdb
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr

     C                   eval      *inlr = *on
     c                   return

     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C     @@rrn1        chain(n)  pcm01                              99
     C                   move      cmco#         kkco#
     C                   move      cmfnd         kkfnd
     C                   move      cmfyr         kkfyr
     C                   move      cmcase        kkcase
      *
     C                   call      'WA006R1'
     C                   parm                    kkco#
     C                   parm                    kkfnd
     C                   parm                    kkfyr
     C                   parm                    kkcase
     C                   parm                    sccs01
     C                   parm                    sccs02
     C                   parm                    sccs03
      *
     c                   eval      scprd1 = '1'
     c                   eval      scprd2 = '2'
     c                   eval      scprd3 = '3'
     c                   eval      scprd4 = '4'
     c                   eval      scprd5 = '5'
     c                   eval      scprd6 = '6'
     c                   eval      scprd7 = '7'
     c                   eval      scprd8 = '8'
     c                   eval      scprd9 = '9'
     c                   eval      scprd10= '10'
     c                   eval      scprd11= '11'
     c                   eval      scprd12= '12'
     c                   eval      scprd13= '13'
     c                   eval      scprd14= '14'
     c*
      *
     C* Get original master rcd info
     C     @@rrn         chain(n)  pwc01                              99
B01  C                   if        *in99 = *off
     C                   move      wcco#         kkco#
     C                   move      wcfnd         kkfnd
     C                   move      wcfyr         kkfyr
     C                   move      wccase        kkcase
     C                   move      wcseq#        kkseq#
     C                   endif

     C                   endsr                                                  pgmstr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     c                   exsr      wrtupd
     c                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     c                   exsr      wrtupd
     C                   endsr
    C*================================================================
    C* WRTUPD - Write & Update after processing
    C*================================================================
     c     wrtupd        begsr

     c                   eval      i = 1
     c                   do        14
     c                   eval      xxChkDelete = *off

     c***Check for non-blank screen fields
     c                   if        hi2(i) = 0 and lc2(i) = 0  and
     c                             cu2(i) = 0 and lr2(i) = 0  and
     c                             fm2(i) = 0 and vf2(i) = 0  and
     c                             ot2(i) = 0

     c****** Check for Deleted record
     c                   eval      xxChkDelete = *on
     c                   endif
     c*
     c                   eval      kkseq#  = wcseq#
     c                   eval      kksseq# = sb2(i)
     c     xxkey1        chain     pf101                              60

     c                   if        *in60 = *off
     c*
     c                   if        xxChkDelete
     c                   delete    pf101
     c                   eval      i = i + 1
     c                   iter
     c                   endif
     c*
     c** If record alreday exist, update the record.

     c                   eval      f1hlth  = hi2(i)                             Health Insurance.
     c*
     c                   eval      f1ldry  = lc2(i)                             Laundry cleaning.
     c*
     c                   eval      f1clots = cu2(i)                             Clothing Uniform.
     c*
     c                   eval      f1rent  = lr2(i)                             Lodging rent
     c*
     c                   eval      f1food  = fm2(i)                             Food meals
     c*
     c                   eval      f1fuel  = vf2(i)                             Vehical fuel
     c*
     c                   eval      f1othr  = ot2(i)                             Others

     C                   movel     q1user        f1user
     C                   movel     q1pgm         f1pgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f1dt
     C                   movel     dstime        f1tm

     c                   exsr      mvscdb
     c                   update    pf101

     c                   else

     c                   if        xxChkDelete
     c                   eval      i = i + 1
     c                   iter
     c                   endif
     c*
     c** Write new record
     C* Get next seq#
     c                   move      wcseq#        sqseq#
     c     sqkey         setgt     pf101
     c     xxkeyp        readpe    pf101                                9999
B01  c                   if        *in99 = *off
     c                   eval      nseq# = f1sseq# + 1
E01  c                   else
     c                   eval      nseq# = nseq# + 1
E01  c                   endif

     c                   eval      sb2(i)  = nseq#                              subseq#

     c                   clear                   dbrcdf
     c*
     c                   eval      f1hlth  = hi2(i)                             Health Insurance.
     c*
     c                   eval      f1ldry  = lc2(i)                             Laundry cleaning.
     c*
     c                   eval      f1clots = cu2(i)                             Clothing Uniform.
     c*
     c                   eval      f1rent  = lr2(i)                             Lodging rent
     c*
     c                   eval      f1food  = fm2(i)                             Food meals
     c*
     c                   eval      f1fuel  = vf2(i)                             Vehical fuel
     c*
     c                   eval      f1othr  = ot2(i)                             Others
     c*
     c                   eval      f1sseq# = nseq#                              Subseq
     c*
     c                   eval      f1seq# = wcseq#                              Seq
     c*
     C                   movel     q1user        f1iusr
     C                   movel     q1pgm         f1ipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f1idt
     C                   movel     dstime        f1itm

     c                   exsr      mvscdb

     c                   write     dbrcdf

     c                   endif

     c                   eval      i = i + 1

     c     xxkey         reade     pf101                                  60
     c                   enddo
     C****               feod      watip
     C****               eval      outflg = *on

     c***Write/update the WAF2P record
     c     xxkey         chain     pf201                              60
     c                   if        *in60 = *off
     c                   eval      f2hinf = %xfoot(hi2)                         Health Tot
     c                   eval      f2ldrf = %xfoot(lc2)                         Laundry Tot
     c                   eval      f2clof = %xfoot(cu2)                         Clothing Tot
     c                   eval      f2renf = %xfoot(lr2)                         Lodging Tot
     c                   eval      f2fodf = %xfoot(fm2)                         Food Tot
     c                   eval      f2fulf = %xfoot(vf2)                         Vehical Tot
     c                   eval      f2othf = %xfoot(ot2)                         Other Tot
     c                   eval      f2co#  = cmco#
     c                   eval      f2fnd  = cmfnd
     c                   eval      f2fyr  = cmfyr
     c                   eval      f2case = cmcase
     C                   movel     q1user        f2iusr
     C                   movel     q1pgm         f2ipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f2idt
     c                   movel     dstime        f2itm
     c                   if        *in95 = *on
     c                   eval      f2calc = ' '
     c                   endif
     c                   update    pf201
     c                   else
     c** Write new record

     c                   clear                   pf201

     c                   eval      f2hinf = %xfoot(hi2)                         Health Tot
     c                   eval      f2ldrf = %xfoot(lc2)                         Laundry Tot
     c                   eval      f2clof = %xfoot(cu2)                         Clothing Tot
     c                   eval      f2renf = %xfoot(lr2)                         Lodging Tot
     c                   eval      f2fodf = %xfoot(fm2)                         Food Tot
     c                   eval      f2fulf = %xfoot(vf2)                         Vehical Tot
     c                   eval      f2othf = %xfoot(ot2)                         Other Tot
     c                   eval      f2seq# = wcseq#
     c                   eval      f2co#  = cmco#
     c                   eval      f2fnd  = cmfnd
     c                   eval      f2fyr  = cmfyr
     c                   eval      f2case = cmcase
     C                   movel     q1user        f2iusr
     C                   movel     q1pgm         f2ipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f2idt
     C                   movel     dstime        f2itm
     c                   write     pf201
     c                   endif

     C                   exsr      callr

     c                   endsr                                                  wrtupd
    C*================================================================
    C* CALLR  - Call next program
    C*================================================================
     C     callr         begsr

     C                   call      'W0595R04'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    @@mode
     c                   parm                    @@rrn                          wmwcp
     c                   parm                    @@rrn1                         wmcmp
     c                   parm      ' '           @@beg

     c                   if        *inkl = '1' and  @@beg = ' '
     c                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        @@beg = 'Y'
     c                   eval      @@kc  = '1'
     c                   eval      *in90 = '0'
     c                   endif

     C                   endsr

    C*================================================================
