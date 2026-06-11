     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     H/TITLE W0003R02 - Vendor Information
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  Update/display vendor information       *
     H*                                                               *
     H* CALLED BY: W0003R                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 02/18/03  C:136 PROJECT WC1115                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/26/09 C:198 Project: WC1694                                *
     H*                Add two new flags to control additional export *
     H*                processing options.                            *
     H*                                                               *
     H* 12/19/05 C:65  Project: 139WBT                                *
     H*                Add two new flags to control additional export *
     H*                processing options.                            *
     H*                                                               *
     H* 10/20/05 C:53  Project: WC1324                                *
     H*                Add flags                                      *
     H*                                                               *
     H* 05/10/05 C:136 Support: WA135 46601                           *
     H*                Add edit to prevent duplicate vendor numbers   *
     H*                from being added.                              *
     H*                                                               *
     H* 04/19/04 C:136 Project: WC1115                                *
     H*                Add effective and expiration date to edit      *
     H*                against when assigning a vendor number to a    *
     H*                policy.                                        *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0003f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmvpp     uf a e             disk
     F                                     rename(pvp01:dbrcdf)
     Fstabl     if   e           k disk
     Fwtabl     if   e           k disk
      *** beg add ***** 04/19/04 **************************************
     Fwdvhl     if   e           k disk
      *** end add ***** 04/19/04 **************************************
      *** beg add ***** 05/10/05 **************************************
     Fwmvpl     if   e           k disk
      *** end add ***** 05/10/05 **************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Vendor'
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
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
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
     C                   parm                    @@rrn
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
     c     stky          klist
     c                   kfld                    kkkey            10

     c     tbky          klist
     c                   kfld                    ##co#
     c                   kfld                    k1key            10

      *** beg add ***** 04/19/04 **************************************
     c     vhky          klist
     c                   kfld                    ##co#
      *** end add ***** 04/19/04 **************************************

      *** beg add ***** 05/10/05 **************************************
     c     vpky          klist
     c                   kfld                    ##co#
     c                   kfld                    scvdr#
      *** end add ***** 05/10/05 **************************************
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

     c                   if        scvdr# =  0
     c                   seton                                        71  90
     c                   endif

      *** beg add *****05/10/05 ***************************************
     c                   if        @@mode = 'A'

     c  n90vpky          chain     pvp01
     c                   if        %found
     c                   seton                                        55  90
     c                   else
     c                   setoff                                       55
     c                   endif
     c                   endif
      *** end add *****05/10/05 ***************************************

     c  n90              if        scname =  *blanks
     c                   seton                                        72  90
     c                   endif

     c                   if        not *in90  and
     c                             scst <> *blanks
     c                   eval      kkkey = scst
     c     stky          chain     pst01
     c                   if        not %found
     c                   seton                                        76  90
     c                   endif
     c                   endif

      *** beg add ***** 04/19/04 ******************************
     c  n90              if        scefdt <> *zeros
     c                   eval      srdti = scefdt
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   seton                                        88  90
     c                   else
     c                   movel     srdto         scefdt
     c                   endif
     c                   endif

     c  n90              if        scexdt <> *zeros
     c                   eval      srdti = scexdt
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   seton                                        89  90
     c                   else
     c                   movel     srdto         scexdt
     c                   endif
     c                   endif

     c  n90              if        scexdt <> *zeros
     c                   eval      srdti = scefdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         vpefdt
     c                   eval      srdti = scexdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         vpexdt
     c                   if        vpexdt < vpefdt
     c                   seton                                        53  90
     c                   endif
     c                   endif

     c  n90              if        scexdt <> *zeros
     c     vhky          setll     pvh01
     c     vhky          reade     pvh01
     c                   dow       not %eof
     c                   if        vpexdt <= vhefdt or
     c                             vpexdt <  vhexdt
     c                   seton                                        54  90
     c                   leave
     c                   endif
     c     vhky          reade     pvh01
     c                   enddo
     c                   endif
      *** end add ***** 04/19/04 ******************************

     c*** begin add *** 12/15/05 ******************************

     c  n90              if        scexnp <> ' ' and scexnp <> 'Y'
     c                   seton                                        56  90
     c                   endif

     c  n90              if        scexal <> ' ' and scexal <> 'Y'
     c                   seton                                        57  90
     c                   endif
      *** end add ***** 12/15/05 ******************************

     c  n90              if        scpyfr <> *blanks
     c                   if        scpyfr <> 'W' and
     c                             scpyfr <> 'M' and
     c                             scpyfr <> 'A'
     c                   seton                                        73  90
     c                   endif
     c                   endif

     c  n90              if        scftyp <> *blanks
     c                   if        scftyp <> 'D' and
     c                             scftyp <> 'N' and
     c                             scftyp <> 'F'
     c                   seton                                        79  90
     c                   endif
     c                   endif

     c  n90              if        scpmfr <> *blanks
     c                   if        scpmfr <> 'W' and
     c                             scpmfr <> 'M'
     c                   seton                                        74  90
     c                   endif
     c                   endif

     c  n90              if        scdtyp <> *blanks
     c                   if        scdtyp <> 'N' and
     c                             scdtyp <> 'T'
     c                   seton                                        80  90
     c                   endif
     c                   endif

     c  n90              if        scdnlf <> *blanks
     c                   if        scdnlf <> 'D' and
     c                             scdnlf <> 'W' and
     c                             scdnlf <> 'M'
     c                   seton                                        75  90
     c                   endif
     c                   endif

     c  n90              if        scdfmt <> *blanks
     c                   if        scdfmt <> 'YYYYMMDD' and
     c                             scdfmt <> 'MMDDYYYY'
     c                   seton                                        81  90
     c                   endif
     c                   endif

     c  n90              if        scblnd <> *blanks
     c                   if        scblnd <> 'Y' and
     c                             scblnd <> 'N'
     c                   seton                                        77  90
     c                   endif
     c                   endif

     c  n90              if        scppcd <> *blanks
     c                   eval      k1key = scppcd
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'P' or tttype <> 'P'
     c                   seton                                        82  90
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        85  90
     c                   endif                                                  %found

     c                   endif                                                  scppcd

     c  n90              if        scnet  <> *blanks
     c                   if        scnet  <> 'Y' and
     c                             scnet  <> 'N'
     c                   seton                                        78  90
     c                   endif
     c                   endif

     c*** beg add ***10/20/05 **********************************

     c  n90              if        scautr <> 'N' and
     c                             scautr <> 'Y'
     c                   seton                                        41  90
     c                   endif

     c  n90              if        scpyup <> *blanks
     c                   if        scpyup <> 'Y'
     c                   seton                                        42  90
     c                   endif
     c                   endif

     c*** end add ***10/20/05 **********************************

     c  n90              if        sccpcd <> *blanks
     c                   eval      k1key = sccpcd
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'P' or  tttype <> 'P' or
     c                             ttexds <> 'Y'
     c                   seton                                        83  90
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        86  90
     c                   endif                                                  %found

     c                   endif                                                  sccpcd

     c  n90              if        scpbcd <> *blanks
     c                   eval      k1key = scpbcd
     c     tbky          chain     ptt01
     c                   if        %found  and ttstat = 'A'

     c                   if        ttctyp <> 'P' or  tttype <> 'B'
     c                   seton                                        84  90
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        87  90
     c                   endif                                                  %found

     c                   endif                                                  scpbcd

     c*** beg add *** 06/26/09 ******************************
     c*
     c  n90              if        scmthi <> ' ' and scmthi <> 'Y'
     c                   seton                                        43  90
     c                   endif
     c*
     c  n90              if        scptyc <> ' ' and scptyc <> 'Y'
     c                   seton                                        44  90
     c                   endif
     c*** end add *** 06/26/09 ******************************
     C                   endsr
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
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
     C     kjdo          begsr

     c                   select
     c                   when      hfld = 'SCPPCD' or
     c                             hfld = 'SCCPCD' or
     c                             hfld = 'SCPBCD'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks

     c                   select
     c                   when      hfld = 'SCPPCD'
     c                   eval      scppcd = sckey
     c                   seton                                        51
     c                   when      hfld = 'SCCPCD'
     c                   eval      sccpcd = sckey
     c                   seton                                        52
     c                   when      hfld = 'SCPBCD'
     c                   eval      scpbcd = sckey
     c                   endsl

     c                   else

     c     hfld          comp      'SCPPCD'                               50
     c     hfld          comp      'SCCPCD'                               51
     c     hfld          comp      'SCPBCD'                               52
     c                   endif                                                  *inkl <> *on

     c                   endsl

     c                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     c                   eval      scvdr# = vpvdr#
     c                   eval      scvdr# = vpvdr#
     c                   eval      scname = vpname
     c                   eval      scadr1 = vpadr1
     c                   eval      scadr2 = vpadr2
     c                   eval      sccity = vpcity
     c                   eval      scst   = vpst
     c                   movel     vpzip         sczip1
     c                   move      vpzip         sczip2
      *** beg add ***** 04/19/04 ******************************
     c                   eval      srdti = vpefdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scefdt
     c                   eval      srdti = vpexdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scexdt
      *** end add ***** 04/19/04 ******************************
     c                   eval      sciadr = vpiadr
     c                   eval      scemal = vpemal
     c                   eval      scpyfr = vppyfr
     c                   eval      scpmfr = vppmfr
     c                   eval      scdnlf = vpdnlf
     c                   eval      scblnd = vpblnd
     c                   eval      scnet  = vpnet
     c                   eval      scftyp = vpftyp
     c                   eval      scdtyp = vpdtyp
      *** begin add *** 12/15/05 ******************************
     c                   eval      scexnp = vpexnp
     c                   eval      scexal = vpexal
      *** end add ***** 12/15/05 ******************************
     c                   eval      scdfmt = vpdfmt
     c                   eval      scppcd = vpppcd
     c                   eval      sccpcd = vpcpcd
     c                   eval      scpbcd = vppbcd
     c*** beg add ***10/20/05 **********************************
     c                   eval      scautr = vpautr
     c                   eval      scpyup = vppyup
     c*** end add ***10/20/05 **********************************
      *** beg add *** 06/26/09 *********************************
     c                   eval      scmthi = vpmthi
     c                   eval      scptyc = vpptyc
      *** end add *** 06/26/09 *********************************

     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr

     c                   eval      vpco#  = ##co#
     c                   eval      vpvdr# = scvdr#
     c                   eval      vpvdr# = scvdr#
     c                   eval      vpname = scname
     c                   eval      vpadr1 = scadr1
     c                   eval      vpadr2 = scadr2
     c                   eval      vpcity = sccity
     c                   eval      vpst   = scst
     c                   movel     sczip1        vpzip
     c                   move      sczip2        vpzip
      *** beg add ***** 04/19/04 ******************************
     c                   eval      srdti = scefdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         vpefdt
     c                   eval      srdti = scexdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         vpexdt
      *** end add ***** 04/19/04 ******************************
     c                   eval      vpiadr = sciadr
     c                   eval      vpemal = scemal
     c                   eval      vppyfr = scpyfr
     c                   eval      vppmfr = scpmfr
     c                   eval      vpdnlf = scdnlf
     c                   eval      vpblnd = scblnd
     c                   eval      vpnet  = scnet
     c                   eval      vpftyp = scftyp
     c                   eval      vpdtyp = scdtyp
      *** begin add *** 12/15/05 ******************************
     c                   eval      vpexnp = scexnp
     c                   eval      vpexal = scexal
      *** end add ***** 12/15/05 ******************************
     c                   eval      vpdfmt = scdfmt
     c                   eval      vpppcd = scppcd
     c                   eval      vpcpcd = sccpcd
     c                   eval      vppbcd = scpbcd
     c*** beg add ***10/20/05 **********************************
     c                   eval      vpautr = scautr
     c                   eval      vppyup = scpyup
     c*** end add ***10/20/05 **********************************
      *** beg add *** 06/26/09 *********************************
     c                   eval      vpmthi = scmthi
     c                   eval      vpptyc = scptyc
      *** end add *** 06/26/09 *********************************

     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     c                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      vpuser = q1user
     C                   eval      vppgm = q1pgm
     C                   time                    dsdttm
     C                   eval      vptime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         vpdt
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      vpuser = q1user
     C                   eval      vpiusr = q1user
     C                   eval      vppgm = q1pgm
     C                   eval      vpipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      vptime = dstime
     C                   eval      vpitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         vpdt
     C                   movel(p)  srdto         vpidt
     C                   write     dbrcdf
     C                   feod      wmvpp
     C                   endsr
