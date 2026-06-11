     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *
     H/TITLE SR00504I - ADDITIONAL ENTRY - Retro Three Year Plan
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC000504B. *
      *                                                                   *
      *  CALLED FROM: SR00504                                             *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *===================================================================*
      * REVISIONS:
      *
      * 10/30/09  C:190 PROJECT 132WMQ
      *                 Created.
      *
      *===================================================================*

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fsr00504f  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
      *  Physical file definition (for maintaining record)
     Fwxfmp2    uf a e             disk
     F                                     rename(pfm02:dbrcdf)

      ** Group Master File
     Fwxfml21   IF   E           K DISK    infds(f4fds)

      ** Group Master File
     FWMFNL     IF   E           K DISK

      ** Employer Master File
     FWMEML     IF   E           K DISK

     d f4fds           ds
     d  f4rrn                397    400B 0


      *================================================================
      *  /COPY required constants
      *================================================================
     D optnm           c                   const('Additional +
     D                                     Information for WC000504B')
     D comapi          c                   'SA005R'
     D #ofscr          c                   3


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
     D xxForm          s             10    inz('WC000504B ')
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
     C     f4ky1         klist
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


      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     mvdbsc        begsr

      * Default values if add mode
     c                   if        @@mode = 'A'
     c                   clear                   dbrcdf
      *
     c                   else

      ** SCR01 fields
     c                   z-add     f4llim        scllim                         LOSS LIMITIATION
     c                   z-add     f4lcon        sclcon                         LOSS CONV. FACTOR
     c                   z-add     f4minr        scminr                         MIN. RETROSPECTIVE
     c                   z-add     f4maxr        scmaxr                         MAX. RETROSPECTIVE

     c                   z-add     f4e050        sce050                         EST. STAND PR..50%
     c                   z-add     f4e100        sce100                         EST. STAND PR..100%
     c                   z-add     f4e150        sce150                         EST. STAND PR..150%
     c                   z-add     f4b050        scb050                         BAS. STAND PR..50%
     c                   z-add     f4b100        scb100                         BAS. STAND PR..100%
     c                   z-add     f4b150        scb150                         BAS. STAND PR..150%

      ** SCR02 fields

     c                   move      f4st01        scst1                          State 1
     c                   move      f4st02        scst2                          State 2
     c                   move      f4st03        scst3                          State 3
     c                   move      f4st04        scst4                          State 4
     c                   move      f4st05        scst5                          State 5
     c                   move      f4st06        scst6                          State 6
     c                   move      f4st07        scst7                          State 7
     c                   move      f4st08        scst8                          State 8
     c                   move      f4st09        scst9                          State 9
     c                   move      f4st10        scst10                         State 10

     c                   z-add     f4A541        scA541                         Generic amount 1
     c                   z-add     f4A542        scA542                         Generic amount 2
     c                   z-add     f4A543        scA543                         Generic amount 3
     c                   z-add     f4A544        scA544                         Generic amount 4
     c                   z-add     f4A545        scA545                         Generic amount 5
     c                   z-add     f4A546        scA546                         Generic amount 6
     c                   z-add     f4A547        scA547                         Generic amount 7
     c                   z-add     f4A548        scA548                         Generic amount 8
     c                   z-add     f4A549        scA549                         Generic amount 9
     c                   z-add     f45410        sca54a                         Generic amount 10
     c                   z-add     f45411        sc5411                         Generic amount 11
     c                   z-add     f45412        sc5412                         Generic amount 12
     c                   z-add     f45413        sc5413                         Generic amount 13
     c                   z-add     f45414        sc5414                         Generic amount 14
     c                   z-add     f45415        sc5415                         Generic amount 15
     c                   z-add     f45416        sc5416                         Generic amount 16
     c                   z-add     f45417        sc5417                         Generic amount 17
     c                   z-add     f45418        sc5418                         Generic amount 18
     c                   z-add     f45419        sc5419                         Generic amount 19
     c                   z-add     f45420        sc5420                         Generic amount 20
     c                   z-add     f45421        sc5421                         Generic amount 21
     c                   z-add     f45422        sc5422                         Generic amount 22
     c                   z-add     f45423        sc5423                         Generic amount 23
     c                   z-add     f45424        sc5424                         Generic amount 24
     c                   z-add     f45425        sc5425                         Generic amount 25
     c                   z-add     f45426        sc5426                         Generic amount 26
     c                   z-add     f45427        sc5427                         Generic amount 27
     c                   z-add     f45428        sc5428                         Generic amount 28
     c                   z-add     f45429        sc5429                         Generic amount 29
     c                   z-add     f45430        sc5430                         Generic amount 30
     c                   z-add     f45431        sc5431                         Generic amount 31
     c                   z-add     f45432        sc5432                         Generic amount 32
     c                   z-add     f45433        sc5433                         Generic amount 33
     c                   z-add     f45434        sc5434                         Generic amount 34
     c                   z-add     f45435        sc5435                         Generic amount 35
     c                   z-add     f45436        sc5436                         Generic amount 36
     c                   z-add     f45437        sc5437                         Generic amount 37
     c                   z-add     f45438        sc5438                         Generic amount 38
     c                   z-add     f45439        sc5439                         Generic amount 39
     c                   z-add     f45440        sc5440                         Generic amount 40
     c                   endif

      ** SCR03 fields
     c                   z-add     f45441        sc5441                         Generic amount 41
     c                   z-add     f45442        sc5442                         Generic amount 42
     c                   z-add     f45443        sc5443                         Generic amount 43
     c                   z-add     f45444        sc5444                         Generic amount 44
     c                   z-add     f45445        sc5445                         Generic amount 45
     c                   z-add     f45446        sc5446                         Generic amount 46
     c                   z-add     f45447        sc5447                         Generic amount 47
     c                   z-add     f45448        sc5448                         Generic amount 48
     c                   z-add     f45449        sc5449                         Generic amount 49
     c                   z-add     f45450        sc5450                         Generic amount 50
     c                   z-add     f45451        sc5451                         Generic amount 51
     c                   z-add     f45452        sc5452                         Generic amount 52
     c                   z-add     f45453        sc5453                         Generic amount 53
     c                   z-add     f45454        sc5454                         Generic amount 54
     c                   z-add     f45455        sc5455                         Generic amount 55
     c                   z-add     f45456        sc5456                         Generic amount 56
     c                   z-add     f45457        sc5457                         Generic amount 57
     c                   z-add     f45458        sc5458                         Generic amount 58
     c                   z-add     f45459        sc5459                         Generic amount 59
     c                   z-add     f45460        sc5460                         Generic amount 60
     c                   z-add     f45461        sc5461                         Generic amount 61
     c                   z-add     f45462        sc5462                         Generic amount 62
     c                   z-add     f45463        sc5463                         Generic amount 63
     c                   z-add     f45464        sc5464                         Generic amount 64
     c                   z-add     f45465        sc5465                         Generic amount 65
     c                   z-add     f45466        sc5466                         Generic amount 66
     c                   z-add     f45467        sc5467                         Generic amount 67
     c                   z-add     f45468        sc5468                         Generic amount 68
     c                   z-add     f45469        sc5469                         Generic amount 69
     c                   z-add     f45470        sc5470                         Generic amount 70

     C                   endsr                                                  mvdbsc


      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     mvscdb        begsr
     C                   eval      f4co# = kkco#
     C                   eval      f4fnd = kkfnd
     C                   eval      f4fyr = kkfyr
     C                   eval      f4emp# = kkemp#
     C                   eval      f4div = kkdiv
     C                   eval      f4form = kkform

      ** scr01 fields

     c                   z-add     scllim        f4llim                         Loss limitiation
     c                   z-add     sclcon        f4lcon                         Loss conv. factor
     c                   z-add     scminr        f4minr                         Min. retrospective
     c                   z-add     scmaxr        f4maxr                         Max. retrospective

     c                   z-add     sce050        f4e050                         Est. stand pr.50%
     c                   z-add     sce100        f4e100                         Est. stand pr.100%
     c                   z-add     sce150        f4e150                         Est. stand pr.150%
     c                   z-add     scb050        f4b050                         Bas. stand pr.50%
     c                   z-add     scb100        f4b100                         Bas. stand pr.100%
     c                   z-add     scb150        f4b150                         Bas. stand pr.150%

      ** SCR02 fields
     c                   move      scst1         f4st01                         State 1
     c                   move      scst2         f4st02                         State 2
     c                   move      scst3         f4st03                         State 3
     c                   move      scst4         f4st04                         State 4
     c                   move      scst5         f4st05                         State 5
     c                   move      scst6         f4st06                         State 6
     c                   move      scst7         f4st07                         State 7
     c                   move      scst8         f4st08                         State 8
     c                   move      scst9         f4st09                         State 9
     c                   move      scst10        f4st10                         State 10

     c                   z-add     sca541        f4a541                         Generic amount 1
     c                   z-add     sca542        f4a542                         Generic amount 2
     c                   z-add     sca543        f4a543                         Generic amount 3
     c                   z-add     sca544        f4a544                         Generic amount 4
     c                   z-add     sca545        f4a545                         Generic amount 5
     c                   z-add     sca546        f4a546                         Generic amount 6
     c                   z-add     sca547        f4a547                         Generic amount 7
     c                   z-add     sca548        f4a548                         Generic amount 8
     c                   z-add     sca549        f4a549                         Generic amount 9
     c                   z-add     sca54A        f45410                         Generic amount 10
     c                   z-add     sc5411        f45411                         Generic amount 11
     c                   z-add     sc5412        f45412                         Generic amount 12
     c                   z-add     sc5413        f45413                         Generic amount 13
     c                   z-add     sc5414        f45414                         Generic amount 14
     c                   z-add     sc5415        f45415                         Generic amount 15
     c                   z-add     sc5416        f45416                         Generic amount 16
     c                   z-add     sc5417        f45417                         Generic amount 17
     c                   z-add     sc5418        f45418                         Generic amount 18
     c                   z-add     sc5419        f45419                         Generic amount 19
     c                   z-add     sc5420        f45420                         Generic amount 20
     c                   z-add     sc5421        f45421                         Generic amount 21
     c                   z-add     sc5422        f45422                         Generic amount 22
     c                   z-add     sc5423        f45423                         Generic amount 23
     c                   z-add     sc5424        f45424                         Generic amount 24
     c                   z-add     sc5425        f45425                         Generic amount 25
     c                   z-add     sc5426        f45426                         Generic amount 26
     c                   z-add     sc5427        f45427                         Generic amount 27
     c                   z-add     sc5428        f45428                         Generic amount 28
     c                   z-add     sc5429        f45429                         Generic amount 29
     c                   z-add     sc5430        f45430                         Generic amount 30
     c                   z-add     sc5431        f45431                         Generic amount 31
     c                   z-add     sc5432        f45432                         Generic amount 32
     c                   z-add     sc5433        f45433                         Generic amount 33
     c                   z-add     sc5434        f45434                         Generic amount 34
     c                   z-add     sc5435        f45435                         Generic amount 35
     c                   z-add     sc5436        f45436                         Generic amount 36
     c                   z-add     sc5437        f45437                         Generic amount 37
     c                   z-add     sc5438        f45438                         Generic amount 38
     c                   z-add     sc5439        f45439                         Generic amount 39
     c                   z-add     sc5440        f45440                         Generic amount 40

      ** SCR03 fields
     c                   z-add     sc5441        f45441                         Generic amount 41
     c                   z-add     sc5442        f45442                         Generic amount 42
     c                   z-add     sc5443        f45443                         Generic amount 43
     c                   z-add     sc5444        f45444                         Generic amount 44
     c                   z-add     sc5445        f45445                         Generic amount 45
     c                   z-add     sc5446        f45446                         Generic amount 46
     c                   z-add     sc5447        f45447                         Generic amount 47
     c                   z-add     sc5448        f45448                         Generic amount 48
     c                   z-add     sc5449        f45449                         Generic amount 49
     c                   z-add     sc5450        f45450                         Generic amount 50
     c                   z-add     sc5451        f45451                         Generic amount 51
     c                   z-add     sc5452        f45452                         Generic amount 52
     c                   z-add     sc5453        f45453                         Generic amount 53
     c                   z-add     sc5454        f45454                         Generic amount 54
     c                   z-add     sc5455        f45455                         Generic amount 55
     c                   z-add     sc5456        f45456                         Generic amount 56
     c                   z-add     sc5457        f45457                         Generic amount 57
     c                   z-add     sc5458        f45458                         Generic amount 58
     c                   z-add     sc5459        f45459                         Generic amount 59
     c                   z-add     sc5460        f45460                         Generic amount 60
     c                   z-add     sc5461        f45461                         Generic amount 61
     c                   z-add     sc5462        f45462                         Generic amount 62
     c                   z-add     sc5463        f45463                         Generic amount 63
     c                   z-add     sc5464        f45464                         Generic amount 64
     c                   z-add     sc5465        f45465                         Generic amount 65
     c                   z-add     sc5466        f45466                         Generic amount 66
     c                   z-add     sc5467        f45467                         Generic amount 67
     c                   z-add     sc5468        f45468                         Generic amount 68
     c                   z-add     sc5469        f45469                         Generic amount 69
     c                   z-add     sc5470        f45470                         Generic amount 70

     C                   endsr                                                  mvscdb


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
     C     f4ky1         chain     pfm02                              60
    
     c                   if        %found
     c                   eval      @@mode = 'C'
     c                   eval      @@rrn  = f4rrn

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
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     c                   eval      f4user = q1user
     c                   eval      f4pgm = q1pgm
     c                   time                    dsdttm
     c                   eval      f4time = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         f4dt
     C                   update    dbrcdf
E01  C                   endif
     c                   eval      outflg  = *on
     C                   endsr


      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     c                   eval      f4user = q1user
     c                   eval      f4iusr = q1user
     c                   eval      f4pgm = q1pgm
     c                   eval      f4ipgm = q1pgm
     c                   time                    dsdttm
     c                   eval      f4time = dstime
     c                   eval      srdti  = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         f4dt
     c                   movel(p)  srdto         f4idt
     C                   write     dbrcdf
     C                   feod      wxfmp2
     c                   eval      outflg = *on
     C                   endsr

