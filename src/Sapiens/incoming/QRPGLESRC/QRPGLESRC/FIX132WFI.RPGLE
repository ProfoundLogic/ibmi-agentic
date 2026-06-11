      * (C) Copyright Progressive Data Solutions, Inc., 2006
      *
     h/title FIX132WFI - Initial Load 132WFI
      **============================================================**
      ** Program Description:                                       **
      **   - Transfer mid-term DARTS remark code to new fields.     **
      **   - Update Mid-Year Endorsement Table entries for          **
      **     Waivers of Subrogation.                                **
      **                                                            **
      ** Called from: WCPOSTCV                                      **
      **                                                            **
      ** Calls to:    None                                          **
      **                                                            **
      **============================================================**
      ** REVISIONS:                                                 **
      **                                                            **
      ** 07/19/06  C:61  support WA091 51985                        **
      **                 - Add Endorsement Table File Records for   **
      **                   WC890600B.                               **
      **                                                            **
      ** 04/26/06  C:61  Project 132WFI                             **
      **                 Created.                                   **
      **                                                            **
      **============================================================**


      *=============================================================================================
      *  Files
      *=============================================================================================

      * Group master
     fwmfnl     if   e           k disk

      * Endorsement/DARTS control
     fwmdxl     uf   e           k disk

      * Mid-Year Endorsement Menu Table
     fwtmyl     uf a e           k disk
      ******* beg add ***** 07/19/06 *************************************

      * Endorsement Table
     fwtenl     uf a e           k disk
      ******* end add ***** 07/19/06 *************************************


      *=============================================================================================
      *  Definitions
      *=============================================================================================

     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d psds          esds                  extname(szq1p)                       External prog info

     d Desc            s             60a   Dim(6) CTDATA

     d kksort          s                   like(mysort)
      ******* beg add ***** 07/19/06 *************************************
     d kkform          s                   like(enform)
      ******* end add ***** 07/19/06 *************************************


      *=============================================================================================
      *  KLIST
      *=============================================================================================

      *  Endorse/DARTS Table

     c     myky          klist
     c                   kfld                    fnco#
     c                   kfld                    fnfnd

     c     myky1         klist
     c                   kfld                    fnco#
     c                   kfld                    fnfnd
     c                   kfld                    kksort
      ******* beg add ***** 07/19/06 *************************************

      * Endorsement Table

     c     enky          klist
     c                   kfld                    fnco#
     c                   kfld                    fnfnd
     c                   kfld                    kkform
      ******* end add ***** 07/19/06 *************************************


      *=============================================================================================
      *  PLIST
      *=============================================================================================

     c     dtcymd        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      '*CYMD'       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1


      *=============================================================================================
      *  Main
      *=============================================================================================


      **   - Transfer mid-term DARTS remark code to new fields.     **

     c                   Read      pdx01

     c                   DoW       not %eof

     c                   if        dxmisc <> *blanks
     c                             and dxwvar = *blanks
     c                             and dxwvcr = *blanks
     c                             and dxwvdr = *blanks
     c                   eval      dxwvar = dxmisc
     c                   eval      dxwvcr = dxmisc
     c                   eval      dxwvdr = dxmisc

     c                   movel(p)  q1user        dxuser
     c                   movel(p)  q1pgm         dxpgm
     c                   time                    dsdttm
      *                                                                                  : :
     c                   eval      dxtime = dstime

     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         dxdt

     c                   update    pdx01
     c                   endif

     c                   Read      pdx01                                  60
     c                   EndDo                                                  *in60 = *off

     c                   unlock    wmdxl

      **   - Update Mid-Year Endorsement Table entries for          **
      **     Waivers of Subrogation.  All groups                    **

     c                   Read      pfn01

     c                   DoW       not %eof
     c     myky          setll     pmy01
     c     myky          reade     pmy01

     c                   DoW       not %eof

     c                   if        mypgm1 = 'W0051R24'
     c                             and mydesc <> desc(1)

      * add
     c                   eval      mydesc = desc(1)
     c                   eval      mydsc1 = desc(2)
     c                   movel(p)  q1user        myuser
     c                   movel(p)  q1pgm         mypgm
     c                   time                    dsdttm
     c                   eval      mytime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         mydt
     c                   update    pmy01

      * change
     c                   eval      kksort = mysort + 1
     c     myky1         chain     pmy01

     c                   if        not %found
     c                   eval      mypgm1 = 'W0051R25'
     c                   eval      mysort = kksort
     c                   eval      mydesc = desc(3)
     c                   eval      mydsc1 = desc(4)
     c                   movel(p)  q1user        myuser
     c                   movel(p)  q1pgm         mypgm
     c                   time                    dsdttm
     c                   eval      mytime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         mydt
     c                   write     pmy01
     c                   endif

      * delete
     c                   eval      kksort = mysort + 1
     c     myky1         chain     pmy01

     c                   if        not %found
     c                   eval      mypgm1 = 'W0051R26'
     c                   eval      mysort = kksort
     c                   eval      mydesc = desc(5)
     c                   eval      mydsc1 = desc(6)
     c                   movel(p)  q1user        myuser
     c                   movel(p)  q1pgm         mypgm
     c                   time                    dsdttm
     c                   eval      mytime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         mydt
     c                   write     pmy01
     c                   endif
      *
     c                   leave
     c                   endif

     c     myky          reade     pmy01
     c                   enddo

      ******* beg add ***** 07/19/06 *************************************
     c                   exsr      srwtenp
      ******* end add ***** 07/19/06 *************************************
     c                   Read      pfn01                                  60
     c                   EndDo                                                  *in60 = *off


     c                   SetOn                                        lr
     c                   Return
      ******* beg add ***** 07/19/06 *************************************


      *================================================================
      * SRWTENP - Update Endorsement Table Records
      *================================================================
     c     srwtenp       begsr

      * WC890600B

     c                   eval      kkform = 'WC890600B'
     c                   clear                   pen01

     c     enky          chain     pen01

     c                   if        %found
     c                   eval      enco# = fnco#
     c                   eval      enfnd = fnfnd
     c                   eval      enform = kkform
     c                   eval      endesc = 'NCCI MID-TERM ENDORSEMENT'
     c                   eval      enst = '  '
     c                   eval      eneddt = 1010707
     c                   eval      enmnau = 'M'
     c                   eval      enprnt = 'Y'
     c                   eval      ensngl = 'S'
     c                   eval      enfldr = 'WCPOLSV3'
     c                   eval      enpgm1 = *blanks
     c                   eval      enpgm2 = 'SR89600B'
     c                   eval      enpgm3 = *blanks
     c                   eval      enrpfm = *blanks
     c                   eval      enrpdt = 0

     c                   movel(p)  q1user        enuser
     c                   movel(p)  q1pgm         enpgm
     c                   time                    dsdttm
     c                   eval      entime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         endt
     c                   update    pen01

     c                   else

     c                   eval      enco# = fnco#
     c                   eval      enfnd = fnfnd
     c                   eval      enform = kkform
     c                   eval      endesc = 'NCCI MID-TERM ENDORSEMENT'
     c                   eval      enst = '  '
     c                   eval      eneddt = 1010707
     c                   eval      enmnau = 'M'
     c                   eval      enprnt = 'Y'
     c                   eval      ensngl = 'S'
     c                   eval      enfldr = 'WCPOLSV3'
     c                   eval      enpgm1 = *blanks
     c                   eval      enpgm2 = 'SR89600B'
     c                   eval      enpgm3 = *blanks
     c                   eval      enrpfm = *blanks
     c                   eval      enrpdt = 0

     c                   movel(p)  q1user        enuser
     c                   movel(p)  q1pgm         enpgm
     c                   time                    dsdttm
     c                   eval      entime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         endt
     c                   write     pen01
     c                   endif

      * WC040306

     c                   eval      kkform = 'WC040306'
     c                   clear                   pen01

     c     enky          chain     pen01

     c                   if        %found
     c                   eval      enst = 'CA'
     c                   eval      eneddt = 840401
     c                   eval      enmnau = 'M'
     c                   eval      enprnt = 'Y'
     c                   eval      ensngl = 'S'
     c                   eval      enfldr = 'WCPOLSV3'
     c                   eval      enpgm1 = *blanks
     c                   eval      enpgm2 = 'SR04306A'
     c                   eval      enpgm3 = 'SR04306A'
     c                   eval      enrpfm = *blanks
     c                   eval      enrpdt = 0

     c                   movel(p)  q1user        enuser
     c                   movel(p)  q1pgm         enpgm
     c                   time                    dsdttm
     c                   eval      entime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         endt
     c                   update    pen01
     c                   endif
      * zzz
     c                   endsr
      ******* end add ***** 07/19/06 *************************************

** CTDATA
ADDITION OF WAIVER OF SUBRO
ADDITION TO WAIVER OF SUBROGATION SCHEDULE
WAIVER OF SUBRO CHANGES
CHANGES TO WAIVER OF SUBROGATION SCHEDULE
DELETION OF WAIVER OF SUBRO
DELETION FROM WAIVER OF SUBROGATION SCHEDULE
