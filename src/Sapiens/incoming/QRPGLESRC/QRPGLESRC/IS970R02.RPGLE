     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *========================================================================
     H/TITLE IS970R - Work With Printer Servers - Open Enrollment Process
      *========================================================================
      * DESCRIPTION:
      *  This program will allow the user to add, change, view or delete a
      *  printer server for the Open Enrollment Process.
      *
      * CREATED   : 04/03/00
      * PROGRAMMER: C:113
      * PROJECT   : 87MIW
      *========================================================================
      * REVISIONS:
      *
      *
      * ----------------------------------------------------------------------
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
      * ----------------------------------------------------------------------
      *========================================================================
      * NOTES:
      *
      *
      *========================================================================

      *========================================================================
      * File Definitions
      *========================================================================
     FIS970f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

      * Physical file definition (for maintaining record)
     Fimpsp     uf a e             disk
     F                                     rename(pps01:dbrcdf)

     Fimpsl1    if a e           k disk

      /COPY IS970RPR

      *================================================================
      *  /COPY required constants
      *================================================================
     D optnm           c                   'Printer Servers'
     D comapi          c                   'SA005R'
     D #ofscr          c                   1

      *========================================================================
      * Variable Definitions
      *========================================================================
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D rtnCode         s              1a
     D pType           s              7a   inz('*LIB   ')
     D pText           s             20a   inz('PC PRINT SERVER QUE')
    
     C/COPY SCOPYMT01C
    
      *========================================================================
      * PList Definitions
      *========================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    
      * Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm

      *
      *
      *========================================================================
      * KList Definitions
      *========================================================================
     C     psKey         klist
     C                   kfld                    scpsid

      *========================================================================
      * Subroutine Definitions
      *========================================================================

      *========================================================================
      * ChgSts - Change the status of the record
      *========================================================================
     C     ChgSts        begsr

     C     @@rrn         chain     dbrcdf                             60
     C                   if        *in60 = *off
     C                   select
     C                   when      psstat = 'I'
     C                   eval      psstat = 'A'

     C                   when      psstat = 'A'
     C                   eval      psstat = 'I'

     C                   endsl

     C                   time                    dsdttm
     C                   eval      pslusr = q1user
     C                   eval      pslpgm = q1pgm
     C                   eval      psltim = dstime
     c*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         psldt
     C
     c**********         eval      psldt  = Cvt2CYMD(dsdate)

     C                   update    dbrcdf

     C                   endif

     C                   endsr

      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     dltpre        begsr
     C                   endsr

      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     dsplyr        begsr
     C                   endsr

      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     errck1        begsr

      * id cannot be blank
     C                   if        scpsid = *blanks
     C                   seton                                        71  90
     C                   endif

      * in add mode, id cannot already exist
     C                   if        *in40 = *on
     C  n90pskey         chain     pps01                              60
     C  n60              seton                                        72  90
     C                   endif

      * dataq cannot be blank
     C  n90              if        scdataq = *blanks
     C                   seton                                        76  90
     C                   endif

      * library cannot be blank
     C  n90              if        scdataqlib = *blanks
     C                   seton                                        75  90
     C                   endif

      * library must exist on system
     C  n90              callp     CheckObj(scdataqlib:pType:rtnCode)
     C  n90              if        rtnCode <> '0'
     C                   seton                                        74  90
     C                   endif

      * shared folder drive cannot be blank
     C  n90              if        scdrive = *blanks
     C                   seton                                        79  90
     C                   endif

      * description cannot be blank
     C  n90              if        scdesc = *blanks
     C                   seton                                        73  90
     C                   endif

     C                   endsr

      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     errck2        begsr
     C                   endsr

      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     errck3        begsr
     C                   endsr

      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     mvdbsc        begsr

     C                   eval      scpsid = pspsid
     C                   eval      scdataq = pspque
     C                   eval      scdataqlib = pspqlb
     C                   eval      scdrive = psidrv
     C                   eval      scdesc = psdesc

     C                   if        *in40 = *on
     C                   eval      scstat = 'A'
     C                   else
     C                   eval      scstat = psstat
     C                   endif

     C                   endsr

      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     mvscdb        begsr

     C                   eval      pspsid = scpsid
     C                   eval      pspque = scdataq
     C                   eval      pspqlb = scdataqlib
     C                   eval      psidrv = scdrive
     C                   eval      psdesc = scdesc
     C                   eval      psstat = scstat

      * maint fields
     C                   if        *in40 = *on

     C                   eval      psiusr = q1user
     C                   eval      psipgm = q1pgm
     c*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         psidt
     C
     C******             eval      psidt  = Cvt2CYMD(dsdate)
     C                   eval      psitim = dstime

     C                   else

     C                   eval      pslusr = q1user
     C                   eval      pslpgm = q1pgm
     C                   eval      psltim = dstime
     c*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         psldt
     C
     C*****              eval      psldt  = Cvt2CYMD(dsdate)

     C                   endif

     C                   endsr

      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     otherr        begsr
     C                   endsr

      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     othfks        begsr
     C                   endsr

      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     pgmend        begsr
     C                   endsr

      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     pgmstr        begsr

     C                   select

     C                   when      @@mode = 'A'
     C                   seton                                        40

     C                   when      @@mode = 'D'
     C                   seton                                        09

     c                   endsl

     C                   endsr

      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     preoth        begsr

     C                   select

     C                   when      @@mode = 'S'
     C                   exsr      ChgSts
     C                   Return

     C                   endsl

     C                   endsr

      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     updatr        begsr

     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
     C                   callp     CreateQue(pspque:pspqlb:pText:rtnCode)
     C                   endif

     C                   endsr

      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     writer        begsr

     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   callp     CreateQue(pspque:pspqlb:pText:rtnCode)
     C                   feod      impsp

     C                   endsr

      *========================================================================
      * Procedure Definitions
      *========================================================================

      /copy IS970RPI
