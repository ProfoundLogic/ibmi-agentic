      * (C) Copyright Progressive Data Solutions, Inc., 2004
      *
     H/TITLE SR04306I - ADDITIONAL ENTRY PROGRAM FOR WC040306
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC040306 . *
      *                                                                   *
      *  CALLED FROM: SR04306                                             *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *  CREATED: 10/06/04      C:61     PROJECT: 132WAC                  *
      *===================================================================*

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR04306f  cf   e             workstn
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
     D optnm           c                   const('WC040306  Additional +
     D                                     Information')
     D comapi          c                   'SA005R'


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
     D xxForm          s             10    inz('WC040306  ')
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

      *  Get number of screens
     c                   exsr      get#scr
     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr

      *  Get number of screens
     c                   exsr      get#scr
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


      *================================================================
      * Get#Scr - Determine number of screens to display
      *================================================================
     C     get#scr       begsr

     c                   if        (scc407 <> *blanks)
     c                             or (scc207 <> *blanks)
     c                             or (scc408 <> *blanks)
     c                             or  (scc208 <> *blanks)
     c                             or (scc409 <> *blanks)
     c                             or  (scc209 <> *blanks)
     c                             or (scc410 <> *blanks)
     c                             or  (scc210 <> *blanks)
     c                             or (scc411 <> *blanks)
     c                             or  (scc211 <> *blanks)
     c                             or (scc412 <> *blanks)
     c                             or  (scc212 <> *blanks)
     c                             or (scc413 <> *blanks)
     c                             or  (scc213 <> *blanks)
     c                             or (scc414 <> *blanks)
     c                             or  (scc214 <> *blanks)
     c                             or (scc415 <> *blanks)
     c                             or  (scc215 <> *blanks)
     c                             or (scc416 <> *blanks)
     c                             or  (scc216 <> *blanks)
     c                   eval      #ofscr = 2

     c                   else

     c                   eval      #ofscr = 1
     c                   endif

     C                   endsr


      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     mvdbsc        begsr

      * Default values if add mode
     c                   if        @@mode = 'A'
     c                   clear                   dbrcdf

     c                   else

     c                   eval      sca921 = f3a921

     c                   eval      scc401 = f3c401
     c                   eval      scc402 = f3c402
     c                   eval      scc403 = f3c403
     c                   eval      scc404 = f3c404
     c                   eval      scc405 = f3c405
     c                   eval      scc406 = f3c406
     c                   eval      scc407 = f3c407
     c                   eval      scc408 = f3c408
     c                   eval      scc409 = f3c409
     c                   eval      scc410 = f3c410
     c                   eval      scc411 = f3c411
     c                   eval      scc412 = f3c412
     c                   eval      scc413 = f3c413
     c                   eval      scc414 = f3c414
     c                   eval      scc415 = f3c415
     c                   eval      scc416 = f3c416

     c                   eval      scc201 = f3c201
     c                   eval      scc202 = f3c202
     c                   eval      scc203 = f3c203
     c                   eval      scc204 = f3c204
     c                   eval      scc205 = f3c205
     c                   eval      scc206 = f3c206
     c                   eval      scc207 = f3c207
     c                   eval      scc208 = f3c208
     c                   eval      scc209 = f3c209
     c                   eval      scc210 = f3c210
     c                   eval      scc211 = f3c211
     c                   eval      scc212 = f3c212
     c                   eval      scc213 = f3c213
     c                   eval      scc214 = f3c214
     c                   eval      scc215 = f3c215
     c                   eval      scc216 = f3c216

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

     c                   eval      f3a921 = sca921

     c                   eval      f3c401 = scc401
     c                   eval      f3c402 = scc402
     c                   eval      f3c403 = scc403
     c                   eval      f3c404 = scc404
     c                   eval      f3c405 = scc405
     c                   eval      f3c406 = scc406
     c                   eval      f3c407 = scc407
     c                   eval      f3c408 = scc408
     c                   eval      f3c409 = scc409
     c                   eval      f3c410 = scc410
     c                   eval      f3c411 = scc411
     c                   eval      f3c412 = scc412
     c                   eval      f3c413 = scc413
     c                   eval      f3c414 = scc414
     c                   eval      f3c415 = scc415
     c                   eval      f3c416 = scc416

     c                   eval      f3c201 = scc201
     c                   eval      f3c202 = scc202
     c                   eval      f3c203 = scc203
     c                   eval      f3c204 = scc204
     c                   eval      f3c205 = scc205
     c                   eval      f3c206 = scc206
     c                   eval      f3c207 = scc207
     c                   eval      f3c208 = scc208
     c                   eval      f3c209 = scc209
     c                   eval      f3c210 = scc210
     c                   eval      f3c211 = scc211
     c                   eval      f3c212 = scc212
     c                   eval      f3c213 = scc213
     c                   eval      f3c214 = scc214
     c                   eval      f3c215 = scc215
     c                   eval      f3c216 = scc216

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
     c                   z-add     1             #ofscr            1 0
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
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     c                   eval      f3user = q1user
     c                   eval      f3pgm = q1pgm
     c                   time                    dsdttm
     c                   eval      f3time = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         f3dt
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


