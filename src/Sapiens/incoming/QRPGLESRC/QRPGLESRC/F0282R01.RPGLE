     H**************************************************************************************
     H copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -                         *
     H StoneRiver is a trademark of StoneRiver, Inc.')                                     *
     H**************************************************************************************
     H* H/TITLE - Case Provider Start/End Dates Maintenance                               **
     H*                                                                                   **
     H* DESCRIPTION: This program is used to update the provider start and enddates        *
     H*                                                                                    *
     H* CALLED BY: F0282R                                                                  *
     H*                                                                                    *
     H* CALLS TO :                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 12/12/11   Programmer C:277   Project:114MAW                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
    H*
     H actgrp(*caller)
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff0282f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffasel1    uf a e             disk
     F                                     rename(pse01:dbrcdf)
     Ffasel2    if a e           k disk
     F                                     rename(pse01:pse02)
    F*
    F*  Claim file
     Ffmcmp     if   e             disk
    F*
    F*  Claim Phys/Prov/Spec Trans File
     Ffappl1    if   e           k disk
    F*
    F*  WCMC Provider Master
     Ffmpdl3    if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Case Provider Start/End Dates'
     D comapi          c                   'FA007R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3  0
     D ##conm          s             40
     D @@fnam          s             40
     D @@fnd           s              3  0
     D @@sub#          s              5  0
     D @@fyr           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D xxfrun          s              1    inz('Y')
     D kkco#           s                   like(seco#)
     D kksub#          s                   like(sesub#)
     D kkfnd           s                   like(sefnd )
     D kkfyr           s                   like(sefyr )
     D kkcase          s                   like(secase)
     D kkseq#          s                   like(seseq#)
     D ##seq#          s                   like(seseq#)
     D kkfein          s                   like(sefein)
     D kkmod           s                   like(semod )
     D kkstdt          s                   like(sestdt)
     D kkendt          s                   like(seendt)
     D xxstdt          s                   like(sestdt)
     D xxendt          s                   like(seendt)
     D x1stdt          s                   like(sestdt)
     D x1endt          s                   like(seendt)
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
     C                   parm                    @@rrn2
     C                   parm                    ##seq#
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    ##conm
     C                   parm                    @@fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     seky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkstdt
     C*
     C     s1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C*
     C     ppky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*
     C     p1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
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
     C*
     C* Start Date must be required
     C*
     C                   if        scstdt = *zeros
     C                   seton                                        9072
     C                   seton                                          87
     C                   leavesr
     C                   endif
     C*
     C* Start date must be a valid date
     C*
     C                   z-add     scstdt        srdti
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   seton                                        9078
     C                   seton                                          87
     C                   leavesr
     C                   else
     C*
     C                   movel     srdto         scstdt
     C                   endif
     C*
     C* End Date must be required
     C*
     C                   if        scendt = *zeros
     C                   seton                                        9073
     C                   seton                                          89
     C                   leavesr
     C                   endif
     C*
     C* End Date must be a valid date
     C*
     C                   z-add     scendt        srdti
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   seton                                        9079
     C                   seton                                          89
     C                   leavesr
     C                   else
     C*
     C                   movel     srdto         scendt
     C                   endif
     C*
     C* End date must be greater that or eq to Start date
     C*
     C                   if        scendt < scstdt
     C                   seton                                        9080
     C                   leavesr
     C                   endif
     C*
     C* Date Must not be duplicate
     C*
     C* Start Date
     C*
     C                   z-add     scstdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         kkstdt
     C                   Movel     srdto         xxstdt
     C* End Date
     C                   z-add     scendt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         xxendt
     C*
     C* Date Must not be overlap
     C*
     C* Add Mode
     C                   if        @@mode = 'A'
     C*
     C     seky          chain     pse02                              92
     C                   if        *in92 = *off and xxstdt = sestdt
     C                             and xxendt = seendt
     C                   seton                                        9081
     C                   leavesr
     C                   else
     C*
     C                   setoff                                         81
E01  C                   endif
     C*
     C     s1ky          chain     pse02                              92
     C                   dow       *in92 = *off
     C                   if        xxstdt >= sestdt and xxstdt <= seendt
     C                             or xxendt >= sestdt and xxendt <= seendt
     C                   seton                                        9082
     C                   leave
     C                   else
     C*
     C                   setoff                                         82
E01  C                   endif
     C     s1ky          reade     pse02                                  92
E01  C                   enddo
     C                   else
     C*
     C* Change Mode
     C*
     C                   if        xxfrun = 'Y'
     C                   eval      x1stdt = sestdt
     C                   eval      x1endt = seendt
     C                   eval      xxfrun = 'N'
     C                   endif
     C     s1ky          chain     pse02                              92
     C                   dow       *in92 = *off
     C                   if        xxstdt >= sestdt and xxstdt <= seendt
     C                             or xxendt >= sestdt and xxendt <= seendt
     C                   if        x1stdt = sestdt and x1endt = seendt
     C                   goto      next
E01  C                   endif
     C                   seton                                        9082
     C                   leave
     C                   else
     C*
     C                   setoff                                         82
E01  C                   endif
     C     next          tag
     C     s1ky          reade     pse02                                  92
E01  C                   enddo
     C                   endif
     C*
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
     C* Start Date
     C                   z-add     sestdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scstdt
     C* End Date
     C                   z-add     seendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scendt
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   time                    dsdttm
     C                   eval      seco#  = cmco#
     C                   eval      sesub# = cmsub#
     C                   eval      sefnd  = cmfnd
     C                   eval      sefyr  = cmfyr
     C                   eval      secase = cmcase
     C                   eval      seseq# = ppseq#
     C                   eval      sefein = ppfein
     C                   eval      semod  = ppmod
     C                   eval      sestat = 'A'
     C* Start Date
     C                   z-add     scstdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         sestdt
     C* End Date
     C                   z-add     scendt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         seendt
     C* User
     C                   movel     q1user        seuser
     C                   movel     q1user        seiusr
     C                   Movel     q1pgm         sepgm
     C                   Movel     q1pgm         seipgm
     C                   Z-add     dstime        setime
     C                   Z-add     dstime        seitim
     C                   Z-add     dsdate        srdti
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         sedt
     C                   Movel     srdto         seidt
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
     C*
     C     @@mode        comp      'D'                                    09    Inquiry
     C     @@mode        comp      'A'                                    40    Add
     C     @@mode        comp      'C'                                    10    change
     C                   setoff                                           31
     C* GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        @@WCIF            1             WC INST
     C                   PARM      *BLANK        @@MCIF            1             MC INST
     C                   PARM      *BLANK        @@WCIN            1             WC INTERFACE
     C                   PARM      *BLANK        @@AR              1             MC AR INST
     C                   PARM      *BLANK        @@NTWK            1             NETWORK INST
     C                   PARM      *BLANK        @@UR              1             PRECERT INST
     C                   PARM      *BLANK        @@MED             1             MEDMGMT INST
     C                   PARM      *BLANK        @@EDI             1             EDI INST
     C                   PARM      *BLANK        @@WCDK            1             WC DARTS KEY
     C
     C     @@rrn2        chain     pcm01                              90
     C*
     C                   move      cmco#         kkco#
     C                   move      cmsub#        kksub#
     C                   move      cmfnd         kkfnd
     C                   move      cmfyr         kkfyr
     C                   move      cmcase        kkcase
     C                   eval      kkseq# = ##seq#
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   move      cmco#         #1co#             3
     C                   move      cmsub#        #1sub#            5
     C                   move      cmfnd         #1fnd             3
     C                   move      cmfyr         #1fyr             3
     C                   move      cmcase        #1case            7
     C*
     C                   CALL      'FA702R'
     C                   parm      *blank        @@rtcd            1
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C     scl1hd        parm      *blank        @@L1             78
     C     scl2hd        parm      *blank        @@L2             78
     C*
     C* Get provider information
     C*
     C     ppky          setgt     PPP01                              99
     C                   if        *in99 = *off
     C     p1ky          readpe    PPP01                                  99
     C                   if        *in99 = *off
     C                   move      ppfein        scfein
     C                   move      ppmod         scmod
     C* Set key list
     C                   move      ppfein        kkfein
     C                   move      ppmod         kkmod
     C*
     C* Get name and area information
     C*
     C     pdky          chain     ppd01                              90
     C                   movel     pdname        scname
     C                   Z-ADD     pdarea        scarea
     C                   movel     pdphn#        scphna
     C                   move      pdphn#        scphnb
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             90
B01  C                   if        *in90 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
     C*
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C*
     C                   write     dbrcdf
     C*
     C                   feod      fasel1
     C                   endsr
     C*================================================================
     C* CHGSTS - Subroutine for changing status
     C*================================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        sestat = 'I'
     C                   eval      sestat = 'A'
     C                   else
     C                   eval      sestat = 'I'
     C                   endif
     C*
     C                   movel     q1user        seuser
     C                   movel     q1pgm         sepgm
     C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sedt
     C                   movel     dstime        setime
     C*
     C                   update    dbrcdf
     C*
     C                   endif
     C*
     C                   endsr
     C*

