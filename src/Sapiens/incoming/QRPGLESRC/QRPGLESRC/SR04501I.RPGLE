      *  (C) Copyright Fiserv Insurance Solution Inc., 2006          *
      *
     H/TITLE SR04501I - ADDITIONAL ENTRY PROGRAM FOR WC040501.C
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC040501.C *
      *                                                                   *
      *  CALLED FROM: SR04501                                             *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *  CREATED: 05/04/07      C:184    PROJECT: WC1506                  *
      *===================================================================*

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR04501f  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
      *  Physical file definition (for maintaining record)
     Fwxfmp1    uf a e             disk
     F                                     rename(pfm01:dbrcdf)

      ** Group Master File
     Fwxfml11   IF   E           K DISK    infds(f3fds)

      ** Group Master File
     FWMFNL     IF   E           K DISK

      ** Employer Master File
     FWMEML     IF   E           K DISK

     d f3fds           ds
     d  f3rrn                397    400B 0


      *================================================================
      *  /COPY required constants
      *================================================================
     D optnm           c                   const('WC040501C Additional +
     D                                     Information')
     D comapi          c                   'SA005R'
     D #ofscr          c                   4
     D*


      *---------------------------------------------------------------------
      * Stand Alone Fields
      *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D*---------------------------------------------------------------------
     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3
     D*---------------------------------------------------------------------
     D xxForm          s             10    inz('WC040501C ')
     D kkco#           s                   like(emco#)
     D kkfnd           s                   like(emfnd)
     D kkemp#          s                   like(ememp#)
     D kkdiv           s                   like(emdiv)
     D kkfyr           s              3  0
     D kkform          s             15
     D ##edn           s             80


     C/COPY SCOPYMT01C


      *================================================================
      *  PLIST definitions
      *================================================================


     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr

      *  Company file/fund (group) retrieve program parameter list

     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm


      *================================================================
      *  KLIST definitions
      *================================================================

      ** Employer Master file
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv


      ** Additional Information file
     C     f3ky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkform

      ** Group Master file
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    


      *================================================================
      *  *LIKE DEFN statements
      *================================================================


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


      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     errck1        begsr
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

     *================================================================
     * ERRCK4 - Error checking for srceen 4 (SCR04)
     *================================================================
     C     errck4        begsr
     C                   endsr

      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     mvdbsc        begsr

      * Default values if add mode
     c                   if        @@mode = 'A'
     c                   clear                   dbrcdf
      *
     c                   else

     c                   eval      SCC401 = F3C401
     c                   eval      SCC101 = F3C101
     c                   eval      SCC402 = F3C402
     c                   eval      SCC403 = F3C403
     c                   eval      SCC404 = F3C404
     c                   eval      SCC405 = F3C405
     c                   eval      SCC102 = F3C102
     c                   eval      SCC406 = F3C406
     c                   eval      SCC103 = F3C103
     c                   eval      SCC407 = F3C407
     c                   eval      SCC201 = F3C201
     c                   eval      SCC202 = F3C202
     c                   eval      SCC203 = F3C203
     c                   eval      SCC408 = F3C408
     c                   eval      SCA521 = F3A521
     c                   eval      SCA522 = F3A522
     C                   eval      SCAM01 = F3AM01
     C                   eval      SCAM03 = F3AM03
     C                   eval      SCA541 = F3A541
     C                   eval      SCA543 = F3A543
     C
     c                   endif

     C                   endsr


      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     mvscdb        begsr
     C                   eval      f3co# = kkco#
     C                   eval      f3fnd = kkfnd
     C                   eval      f3fyr = kkfyr
     C                   eval      f3emp# = kkemp#
     C                   eval      f3div = kkdiv
     C                   eval      f3form = kkform

     C                   eval      F3C401 = SCC401
     C                   eval      F3C101 = SCC101
     C                   eval      F3C402 = SCC402
     C                   eval      F3C403 = SCC403
     C                   eval      F3C404 = SCC404
     C                   eval      F3C405 = SCC405
     C                   eval      F3C102 = SCC102
     C                   eval      F3C406 = SCC406
     C                   eval      F3C103 = SCC103
     C                   eval      F3C407 = SCC407
     C                   eval      F3C201 = SCC201
     C                   eval      F3C202 = SCC202
     C                   eval      F3C203 = SCC203
     C                   eval      F3C408 = SCC408
     C                   eval      F3A521 = SCA521
     C                   eval      F3A522 = sCA522
     C                   eval      F3AM01 = SCAM01
     C                   eval      F3AM03 = SCAM03
     C                   eval      F3A541 = SCA541
     C                   eval      F3A543 = SCA543

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

     C                   if        *inkc = *on OR
     C                             *inkl = *on
     C                   eval      @@call = 'F'
     C                   endif

     C                   endsr


      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     pgmstr        begsr
    
      * Get Mode
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@emp#        kkemp#
     C                   move      @@div         kkdiv
     C                   move      @@fyr         kkfyr
     C                   eval      kkform = xxform
    
     C     f3ky1         chain     pfm01                              60
    
     c                   if        %found
     c                   eval      @@mode = 'C'
     c                   eval      @@rrn  = f3rrn

     c                   else

     c                   eval      @@mode = 'A'
     c                   eval      @@rrn  = 0
     c                   endif
    
      * Get Header
    
     C                   if        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif

     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng

     C                   movel     ##data        scprds
    
     C                   eval      scfnd = kkfnd
     C                   eval      scfyr = kkfyr
      *
     C     fnky          chain     pfn01                              60
     C                   eval      scfnnm = fnname
      *
     C     emky          chain     pem01                              60
      *
     C                   call      'WA005R'
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm                    emname
     C                   parm                    ##edn

     C                   eval      scedn = ##edn


     C                   endsr


      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     preoth        begsr
     C                   endsr


      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   exsr      mvscdb
     c                   eval      f3user = q1user
     c                   eval      f3pgm = q1pgm
     c                   time                    dsdttm
     c                   eval      f3time = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         f3dt
     C                   update    dbrcdf
     C                   endif
     c                   eval      outflg  = *on
     C                   endsr


      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     c                   eval      f3user = q1user
     c                   eval      f3iusr = q1user
     c                   eval      f3pgm = q1pgm
     c                   eval      f3ipgm = q1pgm
     c                   time                    dsdttm
     c                   eval      f3time = dstime
     c                   eval      srdti  = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         f3dt
     c                   movel(p)  srdto         f3idt
     C                   write     dbrcdf
     C                   feod      wxfmp1
     c                   eval      outflg = *on
     C                   endsr


