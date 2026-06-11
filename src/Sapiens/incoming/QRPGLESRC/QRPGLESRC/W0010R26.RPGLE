     h*  (C) Copyright Progressive Data Solutions, Inc., 2003
    h*
     h/TITLE W0010R26 - Group ACH Control Information
     h**************************************************************************
     h* PROGRAM DESCRIPTION:  Update/display  Group ACH Control Informationion *
     h*                                                                        *
     h* CALLED BY: W0010R                                                      *
     h*                                                                        *
     h* CALLS TO:  NONE                                                        *
     h*                                                                        *
     h* CREATED: 04/11/03  C:136 PROJECT WC1116                                *
     h**************************************************************************
     h* REVISIONS:                                                             *
     h*                                                                        *
     h*  03/21/2006   C:168   Support: WA139:51238                             *
     h*                       modified as not to go into "Add" mode if the     *
     h*                       user is only displaying                          *
     h*                                                                        *
     h**************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0010f26  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    f*
    f*  Physical file definition (for maintaining record)
     fwmdcp     uf a e             disk
     f                                     rename(pdc01:dbrcdf)
     fwmdcl     if   e           k disk    infds(dclfds)
     fwmfnp     if   e             disk
     fwtabl     if   e           k disk
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d optnm           c                   'Group ACH Control Info'
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
     d @@kl            s              1
     d @@mode          s              1
     d @@rrn           s              9  0
     d @@rrn1          s              9  0
     d dclfds          ds
     d  dcrrn                397    400B 0
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
     c/COPY SCOPYMT01C
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@rrn1                         wmfnp
    c*
    c*  Company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     tbky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkkey            10

     c     dcky          klist
     c                   kfld                    fnco#
     c                   kfld                    fnfnd
    c*================================================================
    c* DLTPRE - Pre-processing before delete occurs
    c*================================================================
     c     dltpre        begsr
     c                   endsr
    c*================================================================
    c* DSPLYR - Processing after displaying information
    c*================================================================
     c     dsplyr        begsr
     c                   endsr
    c*================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
    c*================================================================
     c     errck1        begsr

     c                   if        sccnam = *blank
     c                   seton                                        71  90
     c                   endif

     c  n90              if        scceds = *blank
     c                   seton                                        72  90
     c                   endif

     c  n90              if        scsvcl <= *zero
     c                   seton                                        73  90
     c                   endif

     c                   endsr                                                  errck1
    c*================================================================
    c* ERRCK2 - Error checking for srceen 2 (SCR02)
    c*================================================================
     c     errck2        begsr
     c                   endsr
    c*================================================================
    c* ERRCK3 - Error checking for srceen 3 (SCR03)
    c*================================================================
     c     errck3        begsr
     c                   endsr
    c*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr

     c                   eval      scgrp  = fnfnd
     c                   eval      scgpnm = fnname
     c                   eval      sccnam = dcconm
     c                   eval      scceds = dcceds
     c                   eval      scsvcl = dcsvcl

     c                   endsr
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr

     c                   eval      dcco#  =  fnco#
     c                   eval      dcgrp  =  fnfnd
     c                   eval      dcconm =  sccnam
     c                   eval      dcceds =  scceds
     c                   eval      dcsvcl =  scsvcl

     c                   endsr
    c*================================================================
    c* OTHERR - Processing for other modes
    c*================================================================
     c     otherr        begsr
     c                   endsr
    c*================================================================
    c* OTHFKS - Processing for other functions keys
    c*================================================================
     c     othfks        begsr
     c                   endsr
    c*================================================================
    c* PGMEND - End of program processing
    c*================================================================
     c     pgmend        begsr
     c                   seton                                        lr
     c                   endsr
    c*================================================================
    c* PGMSTR - Start of program processing
    c*================================================================
     c     pgmstr        begsr
     c
     c     @@rrn1        chain     pfn01
     c                   if        %found
     c
      *** BEG  CHG 03/21/06 ************************************************
     c                   if        @@mode <> 'D'
     c     dcky          chain     pdc01
     c                   if        %found
     c                   eval      @@mode = 'C'
     c                   eval      @@rrn = dcrrn
     c                   else
     c                   eval      @@mode = 'A'
     c                   endif
     c                   endif
      *** BEG  CHG 03/21/06 ************************************************
     c
     c                   else
     c                   seton                                        lr
     c                   endif
     c
     c                   endsr
    c*================================================================
    c* PREOTH - Pre-processing for other modes
    c*================================================================
     c     preoth        begsr
     c                   endsr
    c*================================================================
    c* UPDATR - Update data base record and after processing
    c*================================================================
     c     updatr        begsr
     c     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     c                   exsr      mvscdb
     c                   eval      dcuser = q1user
     c                   eval      dcpgm = q1pgm
     c                   time                    dsdttm
     c                   eval      dctime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         dcdt
     c                   update    dbrcdf
E01  c                   endif
     c                   endsr
    c*================================================================
    c* WRITER - Write data base record and after processing
    c*================================================================
     c     writer        begsr
     c                   clear                   dbrcdf
     c                   exsr      mvscdb
     c                   eval      dcuser = q1user
     c                   eval      dciusr = q1user
     c                   eval      dcpgm = q1pgm
     c                   eval      dcipgm = q1pgm
     c                   time                    dsdttm
     c                   eval      dctime = dstime
     c                   eval      dcitim = dstime
     c                   eval      srdti  = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         dcdt
     c                   movel(p)  srdto         dcidt
     c                   write     dbrcdf
     c                   eval      @@mode = ' '
     c                   feod      wmdcp
     c                   endsr
