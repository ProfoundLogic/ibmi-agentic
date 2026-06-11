     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/TITLE SR04603IB - Additional Entry Program for WC040603B
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC040603B. *
      *                                                                   *
      *  CALLED FROM: SR04603B                                            *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *===================================================================*
      *  Created on 04/19/2018 by c:62 for Project 150WBL                 *
      *===================================================================*
      * Revisions:
      *
      *
      *===================================================================*

     FSR04603fb cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
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

      ** Employer Detail Discount File
     FWDELL     IF   E           K DISK    Rename(pel01:pel00)

      ** Large Deductible Discount File
     FWMLDL1    IF   E           K DISK

     d f3fds           ds
     d  f3rrn                397    400B 0


      *================================================================
      *  /COPY required constants
      *================================================================
     D optnm           c                   const('WC040603B Additional +
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
     D xxForm          s             10    inz('WC040603  ')
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


      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     mvdbsc        begsr

      * Default values if add mode
     c                   if        @@mode = 'A'
     c                   clear                   dbrcdf
     c                   eval      scam03 = 0
     c                   else
     c                   eval      scam03 = f3am03
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

     c                   eval      f3am03 = scam03

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


