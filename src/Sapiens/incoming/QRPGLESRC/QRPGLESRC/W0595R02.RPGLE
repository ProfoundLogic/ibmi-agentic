     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*********************************************************************
     H/TITLE W0595R02 - Anticipated Indemnity Payment Reserve Worksheet
     H*********************************************************************
     H*  CREATED: 06/01/11  PROGRAMMER: C:190 PROJECT: 25WPW              *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 09/08/11 C:53  Project: 25WPW  priss 118
     H*                Only allow F4 if Fringe = Y
     H*
     H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0595f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwatip     uf a e             disk
     F                                     rename(pti01:dbrcdf)
     Fwmcmp     if   e             disk
     Fwatil     uf   e           k disk
     Fwmwcp     uf   e             disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D*optnm           c                   'Wage Calculation'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D ##kc            s              1
     D @@kl            s              1
     D ##kl            s              1
     D @@mode          s              1
     D @@mode1         s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D @@rrn2          s              9  0
     D @@beg           s              1
     D kkco#           s                   like(tico#)
     D kkfnd           s                   like(tifnd)
     D kkfyr           s                   like(tifyr)
     D kkcase          s                   like(ticase)
     D kkseq#          s                   like(tiseq#)
     D kksseq#         s                   like(tiseq#)
     D sqseq#          s                   like(tiseq#)
     D nseq#           s                   like(tiseq#)  inz(0)
     d xxerr           S              1a   inz
     D xxfyr           s                   like(tifyr)
     d xxChkDelete     s                   like(*in03)

     d xxfdop          s              8  0
     d xxldop          s              8  0
     d xxhrsw          s              7  2
     d xxwger          s              9  2
     d i               s              2  0
     d z               s              2  0
     d x               s              2  0
     D optnm           s             40
     D of#             s              2  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------

     d* First day of period array.
     d fd2ds           ds
     d scfdop1
     d scfdop2
     d scfdop3
     d scfdop4
     d scfdop5
     d scfdop6
     d scfdop7
     d scfdop8
     d scfdop9
     d scfdop10
     d scfdop11
     d scfdop12
     d scfdop13
     d scfdop14
     d fd2                                 overlay(fd2ds)
     d                                     like(scfdop1) dim(14) inz(0)

      *

     d* Last day period array
     d fl2ds           ds
     d scldop1
     d scldop2
     d scldop3
     d scldop4
     d scldop5
     d scldop6
     d scldop7
     d scldop8
     d scldop9
     d scldop10
     d scldop11
     d scldop12
     d scldop13
     d scldop14
     d fl2d                                overlay(fl2ds)
     d                                     like(scfdop1) dim(14) inz(0)

      *

     d* Hours array
     d hr2ds           ds
     d schrsw1
     d schrsw2
     d schrsw3
     d schrsw4
     d schrsw5
     d schrsw6
     d schrsw7
     d schrsw8
     d schrsw9
     d schrsw10
     d schrsw11
     d schrsw12
     d schrsw13
     d schrsw14
     d hr2                                 overlay(hr2ds)
     d                                     like(schrsw1) dim(14) inz(0)

      *

     d* Wages earned array
     d wg2ds           ds
     d scwger1
     d scwger2
     d scwger3
     d scwger4
     d scwger5
     d scwger6
     d scwger7
     d scwger8
     d scwger9
     d scwger10
     d scwger11
     d scwger12
     d scwger13
     d scwger14
     d wg2                                 overlay(wg2ds)
     d                                     like(scwger1) dim(14) inz(0)
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

     d* Calc flag array
     d ca2                                 like(ticalc)
     d                                     dim(14)

     d prErrchk        pr             1a
     d strdat                         8  0
     d lstdat                         8  0
     d hrswrk                         7  2
     d wgsern                         9  2
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
     C                   parm                    @@mode1                        IIBS/TIBS
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
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     c                   exsr      valcalc
     c                   if        *in90 = *off
     c                   eval      i = 1
     c                   do        14
     c                   if        (fd2(i) <> 0 and ca2(i) <> 'Y') or
     c                             *in95 = *on
     C                   eval      ##mgid = '2'
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif                                                  fd2(z)
     c                   eval      i = i + 1
     c                   enddo
     c                   if        @@mode1 = 'I'
     c                   exsr      wrtupd
     c                   endif
     c                   endif                                                  *in90 = 0
     C                   endsr
     C*
    C*================================================================
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
    C* VALCALC- Validate F7
    C*================================================================
     C     valcalc       begsr

     c                   move      ' '           error             1
     ***********************************************************************************************
     **** PAY PERIOD 1 *******
     ***********************************************************************************************
     C*  Required for the first line.
     c                   if        scfdop1 = 0
     C                   eval      *in28 = '1'
     C                   eval      *in90 = '1'
     c                   eval      error = 'Y'
     c                   leavesr
     c                   endif
     c*

     c                   eval      xxfdop  = scfdop1
     c                   eval      xxldop  = scldop1
     c                   eval      xxhrsw  = schrsw1
     c                   eval      xxwger  = scwger1

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in31 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in31 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in31 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'                                  strdt < wcstdt
     c                   eval      *in31 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in31 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in61 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in61 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop1 = xxfdop
     c                   eval      scldop1 = xxldop
     c                   eval      schrsw1 = xxhrsw
     c                   eval      scwger1 = xxwger
     c                   eval      error = ' '

     c                   endif
     ***********************************************************************************************
     **** PAY PERIOD 2 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop2
     c                   eval      xxldop  = scldop2
     c                   eval      xxhrsw  = schrsw2
     c                   eval      xxwger  = scwger2

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in32 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in32 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in32 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'                                  strdt < wcstdt
     c                   eval      *in32 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in32 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in62 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in62 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop2 = xxfdop
     c                   eval      scldop2 = xxldop
     c                   eval      schrsw2 = xxhrsw
     c                   eval      scwger2 = xxwger
     c                   eval      error = ' '

     c                   endif

     ***********************************************************************************************
     **** PAY PERIOD 3 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop3
     c                   eval      xxldop  = scldop3
     c                   eval      xxhrsw  = schrsw3
     c                   eval      xxwger  = scwger3

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in33 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in33 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in33 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in33 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in33 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in63 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in63 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop3 = xxfdop
     c                   eval      scldop3 = xxldop
     c                   eval      schrsw3 = xxhrsw
     c                   eval      scwger3 = xxwger
     c                   eval      error = ' '

     c                   endif

     ***********************************************************************************************
     **** PAY PERIOD 4 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop4
     c                   eval      xxldop  = scldop4
     c                   eval      xxhrsw  = schrsw4
     c                   eval      xxwger  = scwger4

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in34 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in34 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in34 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in34 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in34 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in64 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in64 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop4 = xxfdop
     c                   eval      scldop4 = xxldop
     c                   eval      schrsw4 = xxhrsw
     c                   eval      scwger4 = xxwger
     c                   eval      error = ' '

     c                   endif

     ***********************************************************************************************
     **** PAY PERIOD 5 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop5
     c                   eval      xxldop  = scldop5
     c                   eval      xxhrsw  = schrsw5
     c                   eval      xxwger  = scwger5

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in35 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in35 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in35 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in35 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in35 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in65 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in65 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop5 = xxfdop
     c                   eval      scldop5 = xxldop
     c                   eval      schrsw5 = xxhrsw
     c                   eval      scwger5 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 6 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop6
     c                   eval      xxldop  = scldop6
     c                   eval      xxhrsw  = schrsw6
     c                   eval      xxwger  = scwger6

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in36 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in36 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in36 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in36 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in36 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in66 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in66 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop6 = xxfdop
     c                   eval      scldop6 = xxldop
     c                   eval      schrsw6 = xxhrsw
     c                   eval      scwger6 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 7 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop7
     c                   eval      xxldop  = scldop7
     c                   eval      xxhrsw  = schrsw7
     c                   eval      xxwger  = scwger7

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in37 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in37 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in37 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in37 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in37 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in67 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in67 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop7 = xxfdop
     c                   eval      scldop7 = xxldop
     c                   eval      schrsw7 = xxhrsw
     c                   eval      scwger7 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 8 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop8
     c                   eval      xxldop  = scldop8
     c                   eval      xxhrsw  = schrsw8
     c                   eval      xxwger  = scwger8

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in38 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in38 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in38 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in38 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in38 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in68 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in68 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop8 = xxfdop
     c                   eval      scldop8 = xxldop
     c                   eval      schrsw8 = xxhrsw
     c                   eval      scwger8 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 9 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop9
     c                   eval      xxldop  = scldop9
     c                   eval      xxhrsw  = schrsw9
     c                   eval      xxwger  = scwger9

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in39 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in39 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in39 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in39 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in39 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in69 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in69 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop9 = xxfdop
     c                   eval      scldop9 = xxldop
     c                   eval      schrsw9 = xxhrsw
     c                   eval      scwger9 = xxwger
     c                   eval      error = ' '

     c                   endif

     c*
     ***********************************************************************************************
     **** PAY PERIOD 10 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop10
     c                   eval      xxldop  = scldop10
     c                   eval      xxhrsw  = schrsw10
     c                   eval      xxwger  = scwger10

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in41 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in41 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in41 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in41 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in41 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in70 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in70 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop10 = xxfdop
     c                   eval      scldop10 = xxldop
     c                   eval      schrsw10 = xxhrsw
     c                   eval      scwger10 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 11 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop11
     c                   eval      xxldop  = scldop11
     c                   eval      xxhrsw  = schrsw11
     c                   eval      xxwger  = scwger11

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in42 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in42 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in42 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in42 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in42 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in71 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in71 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop11 = xxfdop
     c                   eval      scldop11 = xxldop
     c                   eval      schrsw11 = xxhrsw
     c                   eval      scwger11 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 12 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop12
     c                   eval      xxldop  = scldop12
     c                   eval      xxhrsw  = schrsw12
     c                   eval      xxwger  = scwger12

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in43 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in43 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in43 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in43 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in43 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in72 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in72 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop12 = xxfdop
     c                   eval      scldop12 = xxldop
     c                   eval      schrsw12 = xxhrsw
     c                   eval      scwger12 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 13 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop13
     c                   eval      xxldop  = scldop13
     c                   eval      xxhrsw  = schrsw13
     c                   eval      xxwger  = scwger13

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in44 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in44 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in44 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in44 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in44 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in73 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in73 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop13 = xxfdop
     c                   eval      scldop13 = xxldop
     c                   eval      schrsw13 = xxhrsw
     c                   eval      scwger13 = xxwger
     c                   eval      error = ' '

     c                   endif
     c*
     ***********************************************************************************************
     **** PAY PERIOD 14 *******
     ***********************************************************************************************
     c                   eval      xxfdop  = scfdop14
     c                   eval      xxldop  = scldop14
     c                   eval      xxhrsw  = schrsw14
     c                   eval      xxwger  = scwger14

     c                   eval      xxerr = prErrchk(xxfdop:xxldop
     c                                     :xxhrsw:xxwger)
     c                   if        xxerr <> ' '
     c*
     c                   select
     c                   when      xxerr = '1'
     c                   eval      *in45 = *on                                  Invalid 1st Date

     c                   when      xxerr = '6'                                  strdt < wcstdt
     c                   eval      *in45 = *on
     c                   eval      *in96 = *on

     c                   when      xxerr = '2'
     c                   eval      *in45 = *on
     c                   eval      *in92 = *on                                  Fst date blank

     c                   when      xxerr = '7'
     c                   eval      *in45 = *on
     c                   eval      *in94 = *on

     c                   when      xxerr = '3'
     c                   eval      *in30 = *on
     c                   eval      *in45 = *on                                  Invalid lst date

     c                   when      xxerr = '4'
     c                   eval      *in74 = *on                                  Hours

     c                   when      xxerr = '5'
     c                   eval      *in74 = *on                                  Wage
     c                   eval      *in91 = *on                                  Wage
     c                   endsl
     c*
     c                   eval      error = 'Y'
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   eval      scfdop14 = xxfdop
     c                   eval      scldop14 = xxldop
     c                   eval      schrsw14 = xxhrsw
     c                   eval      scwger14 = xxwger
     c                   eval      error = ' '

     c                   endif

     c* Lst date is within the 'Start date' and 'Last date' range on the 'Wage Calc Main'
     c***********        eval      i = 14
     c***********        do        14
     c***********        if        fl2(i) <> 0
     c***********        z-add     fl2(i)        srdti
     c***********        call      'SRCVTDAT'    dtcymd
     c***********        movel     srdto         xxlstd            7 0
     c***********        if        (xxlstd > wclsdt) or
     c***********                  (xxlstd < wcstdt)
      ***
     c***********        if        i >=10
     c***********        eval      of# = 31 + i
     c***********        move      *on           *in(of#)
     c***********        else
     c***********        eval      of# = 30 + i
     c***********        move      *on           *in(of#)
     c***********        endif
      ***
     C***********        eval      *in94 = '1'
     C***********        eval      *in90 = '1'
     c***********        leavesr
     c***********        endif
     c***********        leave
     c***********        endif
     c***********        eval      i = i - 1
     c***********        enddo

     C                   endsr                                                  VALCALC
      *** BEG ADD *** 09/08/11 *********************************************
    C*================================================================
    C* MSGAPI - Call the message display API
    C*================================================================
     C     msgapi        begsr
     C*
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     C*
     C                   endsr
      *** END ADD *** 09/08/11 *********************************************
    C*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr
     c*
     c                   if        *inke = *on and @@mode  = 'A'
     c                   eval      fd2 = 0
     c                   eval      fl2d = 0
     c                   eval      hr2 = 0
     c                   eval      wg2 = 0
     c                   eval      sb2 = 0
     c                   eval      ca2 = ' '
     c                   endif
     c
     c     xxkey         setll     pti01
     c     xxkey         reade     pti01                                  60
     c                   eval      i = 1
     c                   dow       (*in60 = *off) or
     c                             (*inke = *on and *in60 = *on)

     c                   if        *in60 = *off
     c* Load start date
     C                   z-add     tistdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fd2(i)
     c* Load last date
     C                   z-add     tilsdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         fl2d(i)
     c* Load hours
     c                   if        @@mode1 = 'I'
     c                   eval      hr2(i) = 0
     c                   else
     c                   eval      hr2(i) =tihours
     c                   endif

     c* Load wages
     c                   eval      wg2(i) =tiwage
     c* Load Subseq#
     c                   eval      sb2(i) =tisseq#
     c* Load TICALC
     c                   eval      ca2(i) =ticalc

     c                   else
     c                   eval      fd2(i) = 0
     c                   eval      fl2d(i) = 0
     c                   eval      hr2(i) = 0
     c                   eval      wg2(i) = 0
     c                   eval      sb2(i) = 0
     c                   eval      ca2(i) = ' '
     c                   endif


     c                   eval      i = i + 1
     c                   if        i > 14
     c                   leave
     c                   endif
     c     xxkey         reade     pti01                                  60
     c                   enddo
     c*
     c*
     c                   eval      schrtot = %xfoot(hr2)
     c                   eval      scwgtot = %xfoot(wg2)

     c                   endsr
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr
     c*
     c                   eval      tico#  = cmco#
     c                   eval      tifnd  = cmfnd
     c                   eval      tifyr  = cmfyr
     c                   eval      ticase = cmcase

     c                   if        @@mode1 = 'I'
     c                   eval      tihours= hr2(i)
     c                   endif

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
     C     *inkd         caseq     *on           kddo                     90
     C     *inkf         caseq     *on           kfdo                     90
     C     *inkg         caseq     *on           kgdo                     90
     C                   endcs
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
     c                   if        @@mode1 = 'T'
     c                   eval      optnm = 'TIBS Wage Calculation Maintenance'
     c                   eval      *in93 = *on
     C                   endif
     c                   if        @@mode1 = 'I'
     c                   eval      optnm = 'IIBS Wage Calculation Maintenance'
     c                   endif
     C                   movel(p)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
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

B01  C                   if        @@mode = 'A'
     c                   eval      @@mode = 'P'
     c                   endif

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
     c                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   endsr
    C*================================================================
    C* WRTUPD - Write & Update after processing
    C*================================================================
     c     wrtupd        begsr

     c                   eval      i = 1

     c                   do        14
     c                   eval      xxChkDelete = *off

     c***Check for non-blank screen fields
     c                   if        fd2(i) = 0   or
     c                             @@mode1 = 'I' and i > 12

     c****** Check for Deleted record
     c                   eval      xxChkDelete = *on
     c                   endif
     c*
     c                   eval      kkseq#  = wcseq#
     c                   eval      kksseq# = sb2(i)
     c     xxkey1        chain     pti01                              60

     c                   if        *in60 = *off
     c*
     c                   if        xxChkDelete
     c                   delete    pti01
     c                   eval      i = i + 1
     c                   iter
     c                   endif
     c*
     c** If record alreday exist, update the record.

     c                   z-add     fd2(i)        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         tistdt                         start date.
     c*
     c                   z-add     fl2d(i)       srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         tilsdt                         last date.
     c*
     c                   eval      tihours = hr2(i)                             hours
     c*
     c                   eval      tiwage  = wg2(i)                             wages
     c*
     c                   eval      ticalc  = 'Y'                                calc

     c                   if        @@mode1 = 'I'
     c                   eval      tirtyp = 'I'
     c                   endif

     c                   if        @@mode1 = 'T'
     c                   eval      tirtyp = 'T'
     c                   endif

     C                   movel     q1user        tiuser
     C                   movel     q1pgm         tipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tidt
     C                   movel     dstime        titm

     c                   exsr      mvscdb
     c                   update    pti01

     c                   else

     c                   if        xxChkDelete
     c                   eval      i = i + 1
     c                   iter
     c                   endif
     c*
     c** Write new record
     C* Get next seq#
     c                   move      wcseq#        sqseq#
     c     sqkey         setgt     pti01
     c     xxkeyp        readpe    pti01                                9999
B01  c                   if        *in99 = *off
     c                   eval      nseq# = tisseq# + 1
E01  c                   else
     c                   eval      nseq# = nseq# + 1
E01  c                   endif

     c                   eval      sb2(i)  = nseq#                              subseq#

     c                   clear                   dbrcdf
     c*
     c                   z-add     fd2(i)        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         tistdt                         start date.
     c*
     c                   z-add     fl2d(i)       srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         tilsdt                         last date.
     c*
     c                   eval      tihours = hr2(i)                             hours.
     c*
     c                   eval      tiwage  = wg2(i)                             wages
     c*
     c                   eval      tisseq# = nseq#                              Subseq
     c                   eval      tiseq# = wcseq#                              Seq
     c*
     c                   eval      ca2(i)  = 'Y'                                Calc
     c                   eval      ticalc  = 'Y'                                Calc

     c                   if        @@mode1 = 'I'
     c                   eval      tirtyp = 'I'
     c                   endif

     c                   if        @@mode1 = 'T'
     c                   eval      tirtyp = 'T'
     c                   endif

     C                   movel     q1user        tiiusr
     C                   movel     q1pgm         tiipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tiidt
     C                   movel     dstime        tiitm

     c                   exsr      mvscdb

     c                   write     dbrcdf

     c                   endif

     c                   eval      i = i + 1

     c     xxkey         reade     pti01                                  60
     c                   enddo
                                                                                WRTUPD
     c                   endsr

    C*================================================================
    C*================================================================
    C* KDDO - F4=Fringe processing
    C*================================================================
     C     kddo          begsr
      *
      * Dont allow if WCSDEM = blank
      *** BEG DEL *** 09/08/11 *********************************************
     C*******            if        wcsdem = 'Y'
      *** END DEL *** 09/08/11 *********************************************
      *** BEG ADD *** 09/08/11 *********************************************
     C                   select
     C                   when      wcsdem = 'Y'
      *** END ADD *** 09/08/11 *********************************************
     C                   eval      ##mgid = '1'
     C                   exsr      msgapi
     c                   eval      *in90 = *on
     c                   leavesr
      *** BEG DEL *** 09/08/11 *********************************************
     c*******            endif
      *** END DEL *** 09/08/11 *********************************************
      *** BEG ADD *** 09/08/11 *********************************************
     C                   when      wcnpwf <> 'Y'
     C                   eval      ##mgid = '0'
     C                   exsr      msgapi
     c                   eval      *in90 = *on
     c                   leavesr
     C                   endsl
      *** END ADD *** 09/08/11 *********************************************
      *
      * Dont allow if TICALC = blank
     c                   eval      i = 1
     c                   do        14
     c                   if        fd2(i) <> 0 and ca2(i) <> 'Y'
     C                   eval      ##mgid = '2'
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   eval      i = i + 1
     c                   enddo
     c
     C                   call      'W0595R03'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    @@mode
     c                   parm                    @@rrn                          wmwcp
     c                   parm                    @@rrn1                         wmcmp
     c                   parm      ' '           @@beg

     c                   if        *inkl = '1'  and @@beg = ' '
     c                   eval      *in90 = '1'
     c                   endif

     c                   if        @@beg = 'Y'
     c                   eval      @@kc  = '1'
     c                   eval      *in90 = '0'
     c                   endif
      *
     C                   endsr
    C*================================================================
    C* KFDO - F6=Mult Emp processing
    C*================================================================
     C     kfdo          begsr
      *
     C                   if        wcmlte <> 'Y'
     C                   eval      ##mgid = '4'
      *
     c                   call      'WA727R'
     c                   parm                    ##mgid            1
     c                   parm                    ##rtcd            1
     c     *inkl         parm                    ##kl              1
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
      *
      * Dont allow if TICALC = blank
     c                   eval      i = 1
     c                   do        14
     c                   if        fd2(i) <> 0 and ca2(i) <> 'Y'
     C                   eval      ##mgid = '2'
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   eval      i = i + 1
     c                   enddo

     c                   call      'W0595R05'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    @@mode
     c                   parm                    @@rrn                          wmwcp
     c                   parm                    @@rrn1                         wmcmp

     c                   if        *inkl = '1'
     c                   eval      *in90 = '1'
     c                   endif
      *
     c                   endsr                                                  kfdo
    C*================================================================
    C* KGDO - F7=Calculate processing
    C*================================================================
     C     kgdo          begsr
     c                   exsr      valcalc
     c                   if        error = ' '
     c                   exsr      wrtupd

     c                   eval      schrtot = %xfoot(hr2)
     c                   eval      scwgtot = %xfoot(wg2)
     c*
     C     @@rrn         chain     pwc01                              99
     c                   eval      wcthrs =  schrtot
     c                   eval      wctwag =  scwgtot
     c                   eval      wccalc =  ' '
     c                   update    pwc01

     c                   endif                                                  error = ' '
     C                   endsr
    C*================================================================
      *=============================================================
      * prErrchk - Error check
      *=============================================================
     p prErrchk        b
     d prErrchk        pi             1a
     dstrdat                          8  0
     d lstdat                         8  0
     d hrswrk                         7  2
     d wgsern                         9  2
     d*
     d xxflg           S              1a   inz
     d*
     c                   eval      xxflg = ' '

     c*  Start date cannot be less than the Start Date (WCSTDT) on the wage calculation rec
     c                   if        strdat <> 0
     c                   eval      xxstdt = 0
     c                   z-add     strdat        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxstdt            7 0
     c                   if        xxstdt < wcstdt
     c                   eval      xxflg = '6'
     c                   return    xxflg
     c                   endif
     c                   endif

     c**** Edit for start date.
     c                   if        strdat <> 0
     c                   eval      srdti = strdat
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      xxflg = '1'
     c                   return    xxflg
     c                   else
     c                   movel     srdto         strdat
     c                   endif                                                  srerr = '1'
     c                   endif

     c**** Edit for last date.

     c                   if        strdat = 0 and lstdat <> 0
     c                   eval      xxflg = '2'
     c                   return    xxflg
     c                   endif

     c*  Last  date cannot be greater than the Lastt Date (WCLSDT) on the wage calculation rec
     c                   if        lstdat <> 0
     c                   eval      xxlsdt = 0
     c                   z-add     lstdat        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxlsdt            7 0
     c                   if        (xxlsdt > wclsdt) or
     c                             (xxlsdt < wcstdt)
     c                   eval      xxflg = '7'
     c                   return    xxflg
     c                   endif
     c                   endif

     c                   if        lstdat <> 0
     c                   eval      srdti = lstdat
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      xxflg = '3'
     c                   return    xxflg
     c                   else
     c                   movel     srdto         lstdat
     c                   endif                                                  srerr = '1'
     c                   endif                                                  lstdat <> 0

     c**** Edit for Hours worked.
     c                   if        strdat = 0 and hrswrk <> 0
     c                   eval      xxflg = '4'
     c                   return    xxflg
     c                   endif

     c**** Edit for Wages earned.
     c                   if        strdat = 0 and wgsern <> 0
     c                   eval      xxflg = '5'
     c                   return    xxflg
     c                   endif

     c                   return    xxflg
     c*
     p prErrchk        e
     C*** END   ADD ****** 05/26/11 ************************************
