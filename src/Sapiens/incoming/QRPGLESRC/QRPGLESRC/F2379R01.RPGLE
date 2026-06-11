     H* Copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************
     H/TITLE - Work With SNF Provider Specific File display screen             *
     H*                                                                        *
     H*  DESCRIPTION: This program Shows the W/W detail screen for FMSKP       *
     H*                                                                        *
     H*   CALLED BY: MENU                                                      *
     H*                                                                        *
     H**************************************************************************
     H*  CREATED: 10/06/2011  C:251 PROJECT 114MZO                             *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*                                                                        *
     H**************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2379f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmskp     uf a e             disk
     F                                     rename(psk01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'SNF PROVIDER SPECIFIC FILE  '
     D comapi          c                   'FA007R'
     D #ofscr          c                   2
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
     D @@dtcvt         ds             8
     D @@yr                           4
     D @@mnth                         2
     D @@dt                           2
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
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY  '     srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
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
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   movel     sknpi#        scnpi#
     C                   movel     skprov        scprov
     C                   movel     skname        scname
     C*
     C                   z-add     skefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scefdt
     C*
     C                   Movel     skbgdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scbgdt
     C*
     C                   z-add     skrpdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scrpdt
     C*
     C                   z-add     sktrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         sctrdt
     C*
     C                   Movel     skwaiv        scwaiv
     C                   Movel     skint         scint
     C                   Movel     sktype        sctype
     C                   Movel     skcens        sccens
     C                   Movel     skcdwi        sccdwi
     C                   Movel     skamsa        scamsa
     C                   Movel     skwmsa        scwmsa
     C                   Movel     sksmsa        scsmsa
     C                   Movel     sksole        scsole
     C                   Movel     sklug         sclug
     C                   Movel     skrel         screl
     C                   Movel     skfble        scfble
     C                   Movel     skmix         scmix
     C                   Movel     skmix1        scmix1
     C                   Movel     skcola        sccola
     C                   Movel     skibed        scibed
     C                   Movel     skbeds        scbeds
     C                   Movel     skoccr        scoccr
     C                   Movel     skssi         scssi
     C                   Movel     skmrat        scmrat
     C                   Movel     skppsp        scppsp
     C                   Movel     skspfa        scspfa
     C                   Movel     skodsh        scodsh
     C                   Movel     sksppy        scsppy
     C                   z-add     skendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scendt
     c                   Movel     skhoqu        schoqu
     c                   Movel     skacbs        scacbs
     c                   Movel     skwcbs        scwcbs
     c                   Movel     sksalo        scsalo
     c                   Movel     skspwi        scspwi
     c                   Movel     skgaf         scgaf
     c                   Movel     skwind        scwind
     c                   Movel     skcpas        sccpas
     c                   Movel     skdpas        scdpas
     c                   Movel     skopas        scopas
     c                   Movel     sktpas        sctpas
     c                   Movel     skcpps        sccpps
     c                   Movel     skhsca        schsca
     c                   Movel     skohhr        scohhr
     c                   Movel     skchhr        scchhr
     c                   Movel     skcccr        sccccr
     c                   Movel     sknew         scnew
     c                   Movel     skcime        sccime
     c                   Movel     skscod        scscod
     c                   Movel     skcexc        sccexc
     c                   Movel     skstat        scstat
     C                   z-add     skffdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scffdt
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   movel     scnpi#        sknpi#
     C                   movel     scprov        skprov
     C                   movel     scname        skname
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         skefdt
     C                   z-add     scbgdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         skbgdt
     C                   z-add     scrpdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         skrpdt
     C                   z-add     sctrdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         sktrdt
     C                   Movel     scwaiv        skwaiv
     C                   Movel     scint         skint
     C                   Movel     sctype        sktype
     C                   Movel     sccens        skcens
     C                   Movel     sccdwi        skcdwi
     C                   Movel     scamsa        skamsa
     C                   Movel     scwmsa        skwmsa
     C                   Movel     scsmsa        sksmsa
     C                   Movel     scsole        sksole
     C                   Movel     sclug         sklug
     C                   Movel     screl         skrel
     C                   Movel     scfble        skfble
     C                   Movel     scmix         skmix
     C                   Movel     scmix1        skmix1
     C                   Movel     sccola        skcola
     C                   Movel     scibed        skibed
     C                   Movel     scbeds        skbeds
     C                   Movel     scoccr        skoccr
     C                   Movel     scssi         skssi
     C                   Movel     scmrat        skmrat
     C                   Movel     scppsp        skppsp
     C                   Movel     scspfa        skspfa
     C                   Movel     scodsh        skodsh
     C                   Movel     scsppy        sksppy
     C                   z-add     scendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         skendt
     c                   Movel     schoqu        skhoqu
     c                   Movel     scacbs        skacbs
     c                   Movel     scwcbs        skwcbs
     c                   Movel     scsalo        sksalo
     c                   Movel     scspwi        skspwi
     c                   Movel     scgaf         skgaf
     c                   Movel     scwind        skwind
     c                   Movel     sccpas        skcpas
     c                   Movel     scdpas        skdpas
     c                   Movel     scopas        skopas
     c                   Movel     sctpas        sktpas
     c                   Movel     sccpps        skcpps
     c                   Movel     schsca        skhsca
     c                   Movel     scohhr        skohhr
     c                   Movel     scchhr        skchhr
     c                   Movel     sccccr        skcccr
     c                   Movel     scnew         sknew
     c                   Movel     sccime        skcime
     c                   Movel     scscod        skscod
     c                   Movel     sccexc        skcexc
     C*
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
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
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
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      fmskp
     C                   endsr
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
    C*
     C     chgsts        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
     C                   if        *in99 = *off
    C*
     C                   if        skstat = 'I'
     C                   eval      skstat = 'A'
     C                   else
     C                   eval      skstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        skuser
     C                   movel     q1pgm         skpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         skdt
     C                   movel     dstime        sktime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
