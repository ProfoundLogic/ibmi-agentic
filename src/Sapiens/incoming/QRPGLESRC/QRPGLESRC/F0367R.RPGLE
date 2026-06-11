     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H/TITLE F0367R                                                                        *
     H**************************************************************************************
     H*  Description:  Screen to display breakout for payment for inpatient                *
     H*                                                                                    *
     H*  Created: 04/19/10   Programmer C:227   Project:NY020                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*
     H*                                                                                    *
     H*  08/11/21 C:295 Project: NY056                                                     *
    H*                 Add Hospital Name to the screen                                    *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff0367f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Ffddup     if   e             disk
     Ffaeul     if   e           k disk
     Fftadl1ny  if   e           k disk
     F*fteml1ny  if   e           k disk
     Ffteml2ny  if   e           k disk
     Ffdcal18   if   e           k disk
     Fststl     if   e           k disk
     Fftgtl     if   e           k disk
      *
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D ##cpt4          s              5    inz('DRG')
     D optnm           c                   const('Inpatient Payment -
     D                                     Information')
      *
     Dpsds           esds                  extname(szq1p)
     D*
     D kkkey           s                   like(stkey)
     D kkdesc          s                   like(emdesc)
     C*
     C     *like         define    duco#         ##co#
     C     *like         define    dusub#        ##sub#
     C     *like         define    dufnd         ##fnd
     C     *like         define    dufyr         ##fyr
     C     *like         define    ducase        ##case
     C     *like         define    durate        ##cpt4
     C     *like         define    dueob#        ##eob#
     C     *like         define    gttbid        kktbid
     C     *like         define    gtkey         kkkey
     C     *like         define    emopct        kkopct
     C*
     C     *entry        plist
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@mode            1
     C                   parm                    ##rrn             9 0
     C                   parm                    @@st              2
     C                   parm                    ##conm           40
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *Blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd1       PLIST
     C                   parm                    srdti             8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      *Blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C     euky          klist
     C                   kfld                    ##co#
     C                   kfld                    ##sub#
     C                   kfld                    ##fnd
     C                   kfld                    ##fyr
     C                   kfld                    ##case
     C                   kfld                    ##eob#
     C*
     C     adky          klist
     C                   kfld                    ##cpt4
     C                   kfld                    eudcdt
     C*
     C     adky1         klist
     C                   kfld                    ##cpt4
     C*
     C     emky          klist
     C                   kfld                    kkopct
     C**** Beg Chg ***** 08/11/21 **********************************
     C***                kfld                    eudcdt
     C                   kfld                    kkdesc
     C                   kfld                    eudcdt
     C**** End Chg ***** 08/11/21 **********************************
     C*
     C     emky1         klist
     C                   kfld                    kkopct
     C***** Beg Add **********08/11/21 ************************
     C                   kfld                    kkdesc
     C***** End Add **********08/11/21 ************************
     C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
     C     caky          klist
     C                   kfld                    ##co#
     C                   kfld                    ##sub#
     C                   kfld                    ##fnd
     C                   kfld                    ##fyr
     C                   kfld                    ##case
     C                   kfld                    ##eob#
     C                   kfld                    ##cpt4
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*----------------------------------------------------------------------
     C*  MAINLINE CODE
     C*----------------------------------------------------------------------
     C     ##rrn         chain(n)  pdu01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   eval      ##co# = duco#
     C                   eval      ##sub# = dusub#
     C                   eval      ##fnd = dufnd
     C                   eval      ##fyr = dufyr
     C                   eval      ##case = ducase
     C                   eval      ##eob# = dueob#
     C                   eval      ##cpt4 = durate
     C*
     C     euky          chain     peu01                              60
     C*
     C                   if        *in60 = *off and ##cpt4 <> *blanks
     C     adky          setgt     pad01
     C     adky1         readpe    pad01                                  60
     C                   endif
     C*
     C                   if        *in60 = *off
     C     caky          chain     pca01                              60
     C                   endif
     C*
     C                   exsr      pgmstr
     C                   exsr      mvdbsc
     C*
     C                   exfmt     scr01
     C*
     C                   endif
     C*
     C                   move      *on           *inlr
     C*================================================================
     C* mvdbsc - Move data base fields to screen fields
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     mksthd        begsr
     C*
     C* Display state code and name in header
     C*
     C                   eval      kkkey = @@st
     C                   eval      scst  = @@st
     C     stky          chain     pst01                              99
     C                   eval      scstds = stname
     C*
     C                   endsr
     C*================================================================
     C     mvdbsc        begsr
     C*---------------------------------------------------------------------
     C*    Get from FDDUP
     C*---------------------------------------------------------------------
     C                   movel     durate        scrate
     C                   if        durate = *blanks
     C                   eval      scdesc = *blanks
     C                   else
     C                   movel     dudesc        scdesc
     C                   endif
     C                   movel     dudgsv        scdgsv
     C*---------------------------------------------------------------------
     C*    Get from FAEUP
     C*---------------------------------------------------------------------
     C****** Beg Add ********** 08/11/21 **********************************
     C******             movel(p)  eumcar        scmcar
      /Free
         scmcar = %Trim(EUmcar) + '-' + %Trim(EUhnam);
      /End-Free
     C****** End Add ********** 08/11/21 **********************************
     C                   if        euicn = 'IC' or euicn = 'IM' or
     C                             euicn = 'IP' or euicn = 'IS' or
     C                             euicn = 'IW' or euicn = 'IX'
     C                   eval      *in37 = '1'
     C                   movel     'NYT1'        kktbid
     C                   movel     euicn         kkkey
     C                   movel     euicn         scicn
     C     gtky          chain     pgt01                              60
     C                   if        *in60 = *off
     C                   movel     gtdesc        scicd
     C                   endif
     C                   endif
     C                   z-add     eudcdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         scdcdt
     C*---------------------------------------------------------------------
     C*    Get from FTEMPNY
     C*---------------------------------------------------------------------
     C*
     C                   movel(p)  eumcar        kkopct
     C****** Beg Add ******* 08/11/21 ************************************
                         kkdesc = euhnam;
     C****** End Add ******* 08/11/21 ************************************
     C     emky          setgt     pem01
     C     emky1         readpe    pem01                                  61
     C                   if        *in61 = *off
     C                   z-add     emefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         scefdt
     C                   endif
     C*---------------------------------------------------------------------
     C*    Get from FTADPNY
     C*---------------------------------------------------------------------
     C                   select
     C                   when      dudgsv = '1'
     C                   eval      scsiw = adsiw1
     C                   eval      scalos = adalos
     C                   when      dudgsv = '2'
     C                   eval      scsiw = adsiw2
     C                   eval      scalos = adalos
     C                   when      dudgsv = '3'
     C                   eval      scsiw = adsiw3
     C                   eval      scalos = adalos
     C                   when      dudgsv = '4'
     C                   eval      scsiw = adsiw4
     C                   eval      scalos = adalos
     C                   endsl
     C*---------------------------------------------------------------------
     C*    Get from FDCAP
     C*---------------------------------------------------------------------
     C                   eval      scinlr = cainlr
     C                   eval      scoutl = caoutl
     C                   eval      sctran = catran
     C                   if        durate <> *blanks
     C                   eval      scpaid = capaid
     C                   else
     C                   eval      scexpy = capaid
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     pgmstr        begsr
     C*
     C                   movel(p)  ##conm        scconm
     C                   movel(p)  optnm         ##data
     C*
     C                   call      'SRCENTER'
     C                   parm                    ##data           80
     C                   parm      40            ##leng            3 0
     C*
     C                   movel     ##data        scprds
     C*                  move      *on           dsptyp
     C*
     C                   movel(p)  ##conm        ##data1
     C*
     C                   call      'SRCENTER'
     C                   parm                    ##data1          80
     C                   parm      40            ##leng1           3 0
     C*
     C                   MOVEL     ##data1       scconm
     C*                  move      *on           dsptyp
     C*
     C                   exsr      mksthd
     C*
     C                   endsr                                                  PGMSTR
