      *  (C) Copyright Progressive Data Solutions, Inc., 2004

      /title W2122R02 - Maint Contractor Credit Factor Table Information
      *****************************************************************
      * Revised:
      *
      *
      * Created 01/27/04  C:61  Project WC1173
      *****************************************************************

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2122f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwtcdp     uf a e             disk
     F                                     rename(pcd01:dbrcdf)

     *  Contractor Credit (used to find if record has unique key)
     Fwtcdl     if   e           k disk
     F                                     rename(pcd01:pcd02)

     *  State table
     Fststl3    if   e           k disk
     Fststp     if   e             disk
     F                                     rename(pst01:pst02)



     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'Contractor Credit Factor'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1


     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0



     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     C/COPY SCOPYMT01C


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam


     *================================================================
     *  KLIST definitions
     *================================================================

     c     cdky          klist
     c                   kfld                    scco#
     c                   kfld                    scst
     c                   kfld                    scawul


     c     stky          klist
     c                   kfld                    kkkey            10



     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr


     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr

     *  entered state must be valid

     c                   if        (*in90 = *off) and
     c                             (*in40 = *on) and
     c                             (scst <> 'XX')
     c                   eval      kkkey = scst
     C     stky          chain     pst01

     c                   if        not %found
     c                   seton                                        71  90
     c                   endif

     c                   endif

     *  record key values must be unique

     c                   if        (*in90 = *off) and
     c                             (*in40 = *on)
     c     cdky          chain     pcd02

     c                   if        %found
     c                   seton                                        79  90
     c                   endif

     c                   endif

      * Check to be sure screen has not been updated since entry

     c  N90              if         (cddt > savedt) or
     c                              ((cddt = savedt) and (cdtime > savetm))
     c                   seton                                        89  90
     c                   endif

     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *====================================================================
     * KJDO - Searches
     *====================================================================
     C     kjdo          begsr

     c                   select

      *  State
     c                   when      hfld = 'SCST'
     c                   call      'SY601R'
     C                   parm      '2'           ##call            1
     C                   parm      *blank        ##kc              1
     C                   parm      *blank        ##kl              1
     C                   parm      *ZEROS        ##rrn             9 0

     c                   if        ##rrn = 0
     c                   eval      *in81 = *on

     c                   else

     c     ##rrn         chain     pst02
     c                   eval      scst = stkey
     c                   eval      *in82 = *on
     c                   endif

      *  Average Wage Upper Limit
     c                   when      hfld = 'SCAWUL'
     c                   eval      *in72 = *on

      *  Credit Factor
     c                   when      hfld = 'SCFACT'
     c                   eval      *in73 = *on

     c                   endsl

     c                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

     c                   eval      scco#  = cdco#
     c                   eval      scst   = cdst
     c                   eval      scawul = cdawul
     c                   eval      scfact = cdfact

     C                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     c                   eval      cdco#  = scco#
     c   40              eval      cdco#  = ##co#
     c                   eval      cdst   = scst
     c                   eval      cdawul = scawul
     c                   eval      cdfact = scfact

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     c                   seton                                        lr
     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
     c     @@mode        comp      'A'                                    40
     c     @@mode        comp      'D'                                    09
     C                   endsr


     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      cduser = q1user
     C                   eval      cdpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      cdtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         cddt
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      cduser = q1user
     C                   eval      cdiusr = q1user
     C                   eval      cdpgm = q1pgm
     C                   eval      cdipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      cdtime = dstime
     C                   eval      cditm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         cddt
     C                   movel(p)  srdto         cdidt
     C                   write     dbrcdf
     C                   feod      wtcdp
     C                   endsr
