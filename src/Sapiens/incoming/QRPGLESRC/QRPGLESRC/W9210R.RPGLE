     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*
     h/TITLE W9210R - JDE Control Maintenance
     h**************************************************************************
     h* PROGRAM DESCRIPTION:  JDE control maintenance                          *
     h*                                                                        *
     h* CALLED BY: S0001R                                                      *
     h*                                                                        *
     h* CALLS TO:  NONE                                                        *
     h*                                                                        *
     h* CREATED: 11/17/09  C:191 PROJECT 25WQI                                 *
     h**************************************************************************
     h* REVISIONS:                                                             *
     h*                                                                        *
     h**************************************************************************
    H*
     Fw9210f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmjep     uf a e             disk
     F                                     rename(pje01:dbrcdf)
     Fwmjel     if   e           k disk
     F                                     infds(jefds)
     fsmcop     if   e             disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'JDE Control Maintenance'
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
     d jefds           ds
     d  jerrn                397    400B 0
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
     c     key1          klist
     c                   kfld                    coco#
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
     C* Set error indicator, on first error-process will leave this subroutine
     C                   eval      *in90 = *on
     C* Interface status code is required
     C                   if        scstcd = *blanks
     C                   eval      *in71 = *on
     C                   leavesr
     C                   endif                                                  scstcd = *blanks
     C* Interface type is required
     C                   if        scityp = *blanks
     C                   eval      *in72 = *on
     C                   leavesr
     C                   endif                                                  scityp = *blanks
     C* Reached here so no errors found, set off 90
     C                   eval      *in90 = *off
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
     C                   eval      scstcd = jestcd
     C                   eval      scityp = jeityp
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      jeco# = coco#
     C                   eval      jestcd = scstcd
     C                   eval      jeityp = scityp
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
     C                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C
     c     @@rrn         chain     pco01
     c                   if        %found
     c                   move      coco#         scco#
     c                   eval      scname = coname
     C*
     c                   if        @@mode = 'D'
     c                   eval      *in09 = '1'
     c                   endif                                                  @@mode = ' '
     C*
     c     key1          chain     pje01
     c                   if        %found
     c                   eval      @@mode = 'C'
     c                   eval      @@rrn = jerrn
     c                   else
     c                   eval      @@mode = 'A'
     c                   endif
     c                   else
     c                   seton                                        lr
     c                   endif
     c*
     c                   if        *in09 = '1'
     c                   eval      @@mode= 'D'
     c                   endif                                                  @@mode = ' '
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
     C                   eval      jeuser = q1user
     C                   eval      jepgm = q1pgm
     C                   time                    dsdttm
     C                   eval      jetime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         jedt
     C                   update    dbrcdf
E01  C                   endif
     c                   eval      outflg = *on
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      jeuser  = q1user
     C                   eval      jeiusr = q1user
     C                   eval      jepgm  = q1pgm
     C                   eval      jeipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      jetime  = dstime
     C                   eval      jeitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         jedt
     C                   movel(p)  srdto         jeidt
     C                   write     dbrcdf
     C                   feod      wmjep
     c                   eval      outflg = *on
     C                   endsr
