     H* Copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************
     H/TITLE - Work With IRF Provider Specific File display screen             *
     H*                                                                        *
     H*  DESCRIPTION: This program Shows the W/W detail screen for FMIRP       *
     H*                                                                        *
     H*   CALLED BY: MENU                                                      *
     H*                                                                        *
     H**************************************************************************
     H*  CREATED: 10/17/2011  C:251 PROJECT 114MAR                             *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*                                                                        *
     H**************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2383f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmirp     uf a e             disk
     F                                     rename(pir01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IRF PROVIDER SPECIFIC FILE  '
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
     C                   movel     irnpi#        scnpi#
     C                   movel     irprov        scprov
     C                   movel     irname        scname
     C*
     C                   z-add     irefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scefdt
     C*
     C                   Movel     irbgdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scbgdt
     C*
     C                   z-add     irrpdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scrpdt
     C*
     C                   z-add     irtrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         sctrdt
     C*
     C                   Movel     irwaiv        scwaiv
     C                   Movel     irint         scint
     C                   Movel     irtype        sctype
     C                   Movel     ircens        sccens
     C                   Movel     ircdwi        sccdwi
     C                   Movel     iramsa        scamsa
     C                   Movel     irwmsa        scwmsa
     C                   Movel     irsmsa        scsmsa
     C                   Movel     irsole        scsole
     C                   Movel     irlug         sclug
     C                   Movel     irrel         screl
     C                   Movel     irfble        scfble
     C                   Movel     irmix         scmix
     C                   Movel     irmix1        scmix1
     C                   Movel     ircola        sccola
     C                   Movel     iribed        scibed
     C                   Movel     irbeds        scbeds
     C                   Movel     iroccr        scoccr
     C                   Movel     irssi         scssi
     C                   Movel     irmrat        scmrat
     C                   Movel     irppsp        scppsp
     C                   Movel     irspfa        scspfa
     C                   Movel     irodsh        scodsh
     C                   Movel     irsppy        scsppy
     C                   z-add     irendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scendt
     c                   Movel     irhoqu        schoqu
     c                   Movel     iracbs        scacbs
     c                   Movel     irwcbs        scwcbs
     c                   Movel     irsalo        scsalo
     c                   Movel     irspwi        scspwi
     c                   Movel     irgaf         scgaf
     c                   Movel     irwind        scwind
     c                   Movel     ircpas        sccpas
     c                   Movel     irdpas        scdpas
     c                   Movel     iropas        scopas
     c                   Movel     irtpas        sctpas
     c                   Movel     ircpps        sccpps
     c                   Movel     irhsca        schsca
     c                   Movel     irohhr        scohhr
     c                   Movel     irchhr        scchhr
     c                   Movel     ircccr        sccccr
     c                   Movel     irnew         scnew
     c                   Movel     ircime        sccime
     c                   Movel     irscod        scscod
     c                   Movel     ircexc        sccexc
     c                   Movel     irstat        scstat
     C                   z-add     irffdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scffdt
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   movel     scnpi#        irnpi#
     C                   movel     scprov        irprov
     C                   movel     scname        irname
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         irefdt
     C                   z-add     scbgdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         irbgdt
     C                   z-add     scrpdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         irrpdt
     C                   z-add     sctrdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         irtrdt
     C                   Movel     scwaiv        irwaiv
     C                   Movel     scint         irint
     C                   Movel     sctype        irtype
     C                   Movel     sccens        ircens
     C                   Movel     sccdwi        ircdwi
     C                   Movel     scamsa        iramsa
     C                   Movel     scwmsa        irwmsa
     C                   Movel     scsmsa        irsmsa
     C                   Movel     scsole        irsole
     C                   Movel     sclug         irlug
     C                   Movel     screl         irrel
     C                   Movel     scfble        irfble
     C                   Movel     scmix         irmix
     C                   Movel     scmix1        irmix1
     C                   Movel     sccola        ircola
     C                   Movel     scibed        iribed
     C                   Movel     scbeds        irbeds
     C                   Movel     scoccr        iroccr
     C                   Movel     scssi         irssi
     C                   Movel     scmrat        irmrat
     C                   Movel     scppsp        irppsp
     C                   Movel     scspfa        irspfa
     C                   Movel     scodsh        irodsh
     C                   Movel     scsppy        irsppy
     C                   z-add     scendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         irendt
     c                   Movel     schoqu        irhoqu
     c                   Movel     scacbs        iracbs
     c                   Movel     scwcbs        irwcbs
     c                   Movel     scsalo        irsalo
     c                   Movel     scspwi        irspwi
     c                   Movel     scgaf         irgaf
     c                   Movel     scwind        irwind
     c                   Movel     sccpas        ircpas
     c                   Movel     scdpas        irdpas
     c                   Movel     scopas        iropas
     c                   Movel     sctpas        irtpas
     c                   Movel     sccpps        ircpps
     c                   Movel     schsca        irhsca
     c                   Movel     scohhr        irohhr
     c                   Movel     scchhr        irchhr
     c                   Movel     sccccr        ircccr
     c                   Movel     scnew         irnew
     c                   Movel     sccime        ircime
     c                   Movel     scscod        irscod
     c                   Movel     sccexc        ircexc
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
     C                   feod      fmirp
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
     C                   if        irstat = 'I'
     C                   eval      irstat = 'A'
     C                   else
     C                   eval      irstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        iruser
     C                   movel     q1pgm         irpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         irdt
     C                   movel     dstime        irtime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
