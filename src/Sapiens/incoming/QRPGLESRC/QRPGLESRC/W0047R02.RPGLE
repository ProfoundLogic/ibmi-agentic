     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0047R02 - Rate Change Information
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE STATE RATE CHAGNE FILE.  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0047R - WORK WITH STATE RATE CHANGE                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 11/10/10  PROGRAMMER: C:255 PROJECT: WC1939         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/13/11  C:190  PROJECT: WC2079                              *
     H*                  Added Flat rate Adjustment Stat Code.        *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0047f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmrtp     uf a e             disk
     F                                     rename(prt01:dbrcdf)
     Fwmrtl     if   e           k disk
     Fwmfnp     if   e             disk
     Fststl1    if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'State Rate Change'
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
     D @@rrn2          s              9  0
     D XXDT            S              7  0
     D kkco#           s                   like(rtco#)
     D kkfnd           s                   like(rtfnd)
     D kkst            s                   like(rtst)
     D kkefdt          s                   like(rtefdt)
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
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    stkey         @@st
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
     C*  If in Add Mode
     C                   if        *in40=*ON
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C  n90              if        srerr= '1'
     C                   seton                                        22  90
     C                   endif                                                  srerr=1
     C*
     C                   movel     scst          @@st
     C     @@st          chain     PST01                              60
     C                   if        *in60 = *on
     C                   seton                                        24  90
     C                   endif                                                  *in60=*on
     C                   endif                                                  *in40=*on
     C  n90              if        scopt<> *blanks and scopt <> 'Y'
     C                   seton                                        23  90
     C                   endif                                                  scopt
     c***  Begin Add  ***  10/13/11  **************************************
     C  n90              if        scstat < 0
     C                   seton                                        26  90
     C                   endif                                                  scstat
     c***  End   Add  ***  10/13/11  **************************************
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
     C                   movel     rtst          scst
     C*
     C                   z-add     rtefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   movel     rtopt         scopt
     c***  Begin Add  ***  10/13/11  **************************************
     c                   eval      scstat = rtstat
     c***  End   Add  ***  10/13/11  **************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   move      fnco#         rtco#
     C                   move      fnfnd         rtfnd
     C                   movel     scst          rtst
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rtefdt
     C                   movel     scopt         rtopt
     c***  Begin Add  ***  10/13/11  **************************************
     c                   eval      rtstat = scstat
     c***  End   Add  ***  10/13/11  **************************************
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
     C     @@mode        comp      '1'                                    09    Inquiry
     C     @@mode        comp      '2'                                    09    Inquiry
     C     @@mode        comp      '3'                                    09    Inquiry
     C     @@mode        comp      'D'                                    09    Inquiry
     C     @@mode        comp      'A'                                    40    Inquiry
     C*
     C*  GET GROUP INFORMATION
     C     @@rrn2        chain     wmfnp
     C                   z-add     fnfnd         scfnd
     C                   movel     fnname        scname
     C
     C
     C
     C
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
     C*
     C                   move      fnco#         kkco#
     C                   move      scfnd         kkfnd
     C                   movel     scst          kkst
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C     key1          chain     prt01                              60
     C*
     C                   if        *in60=*on
     C                   exsr      mvscdb
     C                   eval      RTUSER=Q1USER
     C                   eval      RTPGM =Q1PGM
     C*
     C                   eval      SRDTI = DSDATE
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     SRDTO         XXDT
     C*
     C                   move      XXDT          RTDT
     C                   move      DSTIME        RTTIME
     C                   eval      RTIUSR=Q1USER
     C                   eval      RTIPGM =Q1PGM
     C                   move      XXDT          RTIDT
     C                   move      DSTIME        RTITIM
     C                   write     dbrcdf
     C                   feod      wmrtp
     C                   else
     C                   seton                                        25  90
     C                   endif
     C*
     C                   endsr
