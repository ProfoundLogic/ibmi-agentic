     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *
      *** zzz
     H/TITLE SRxxxxxxI - ADDITIONAL ENTRY - xxxxxxxxxxxxxxxxxxxxxxxxxxxx
      *===================================================================*
      *                                                                   *
      *** zzz
      *  CALLED FROM: SRxxxxxx                                            *
      *                                                                   *
      *===================================================================*
      * REVISIONS:
      *
      *** zzz
      * xx/xx/xx  C:000 PROJECT xxxxxx
      *                 Created.
      *
      *===================================================================*

      *** zzz
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSRsma1f   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
      *  Physical file definition (for maintaining record)
     Fwledp     uf a e             disk
     F                                     rename(ped01:dbrcdf)

     Fwledl     IF   E           K DISK    infds(edfds)

      ** Group Master File
     FWMFNL     IF   E           K DISK

      ** Employer Master File
     FWMEML     IF   E           K DISK

     d edfds           ds
     d  edrrn                397    400B 0


      *================================================================
      *  /COPY required constants
      *================================================================
     D AddInF          c                   const('Additional +
     D                                     Information for ')
     D comapi          c                   'SA005R'
     D #ofscr          c                   1


      *---------------------------------------------------------------------
      * Stand Alone Fields
      *---------------------------------------------------------------------
     D optnm           s             40
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
     D @@form          s                   like(fmform)
     D*---------------------------------------------------------------------
     D kkco#           s                   like(fmco#)
     D kkfnd           s                   like(fmfnd)
     D kkemp#          s                   like(fmemp#)
     D kkdiv           s                   like(fmdiv)
     D kkfyr           s              3  0
     D kkform          s                   like(fmform)
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
     C                   parm                    @@form

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
     C     edky          klist
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

      * Surcharge percentage must be entered.

      *** zzz
      ********
     c********           if        scsurc = *zero
     c********           eval      *in71 = *on
     c********           eval      *in90 = *on
     c********           endif

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

     C     edky          chain     ped01                              60
      *
     C                   if        %found
      *** zzz
     C***********        eval      scsurc = fma521
     C                   else
      *** zzz
     C***********        eval      scsurc = *zeros
     C                   endif

     c                   endif

     C                   endsr


      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     mvscdb        begsr
     C                   eval      fmco# = kkco#
     C                   eval      fmfnd = kkfnd
     C                   eval      fmfyr = kkfyr
     C                   eval      fmemp# = kkemp#
     C                   eval      fmdiv = kkdiv
     C                   eval      fmform = kkform

      *** zzz
     C************       eval      fma521 = scsurc

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
     C                   eval      kkform = @@form
    
     C     edky          chain     ped01                              60
    
     c                   if        %found
     c                   eval      @@mode = 'C'
     c                   eval      @@rrn  = edrrn

     c                   else

     c                   eval      @@mode = 'A'
     c                   eval      @@rrn  = 0
     c                   endif
    
      * Get Header
    
     c                   eval      optnm = AddInf + %trim(@@form)
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
     c                   eval      fmuser = q1user
     c                   eval      fmpgm = q1pgm
     c                   time                    dsdttm
     c                   eval      fmtime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         fmdt
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
     c                   eval      fmuser = q1user
     c                   eval      fmiusr = q1user
     c                   eval      fmpgm = q1pgm
     c                   eval      fmipgm = q1pgm
     c                   time                    dsdttm
     c                   eval      fmtime = dstime
     c                   eval      srdti  = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         fmdt
     c                   movel(p)  srdto         fmidt
     C                   write     dbrcdf
     C                   feod      wledp
     c                   eval      outflg = *on
     C                   endsr

