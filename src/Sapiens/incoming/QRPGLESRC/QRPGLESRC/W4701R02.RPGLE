     H*  (C) Copyright Progressive Data Solutions, Inc., 2005
    H*
     F*****************************************************************
     F*  REVISIONS:
     F*
     F*  03/23/05  C:53 WC1196
     F*                 ADD AUDIT ASSIGNMENT INFO
     F*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2280f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwttgp     uf a e             disk
     F                                     rename(ptg01:dbrcdf)
     F*** BEG ADD *** 03/23/05 *****************************
     Fwttgl     If   e           k disk
     F*** END ADD *** 03/23/05 *****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Business Categories'
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
     D kkco#           s                   like(tgco#)
     D kkbctg          s                   like(tgbctg)
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
     C     tgky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkbctg
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99=*off
     C*
     C                   if        tgstat='A'
     C                   eval      tgstat =  'I'
     C                   else
     C                   eval      tgstat = 'A'
     C                   endif                                                  fnflag='I'
     C*
     C                   update    dbrcdf
     C*
     C                   endif                                                  *in99=*off
     C*
     C                   endsr                                                  chgsts
     C*
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
     C*
     C                   if        scbctg = *blanks
     C                   move      *on           *in71
     C     *in71         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   eval      kkbctg = scbctg
     C*
     C                   if        *in40 = *on
     C     tgky          chain     ptg01                              99
     C*
     C                   if        *in99 = *off
     C                   move      *on           *in72
     C     *in72         cabeq     *on           ender1                   90
     C                   endif
     C                   endif
     C*
     C                   if        scbcds = *blanks
     C                   move      *on           *in73
     C     *in73         cabeq     *on           ender1                   90
     C                   endif
     C*
     C     ender1        endsr
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
     C                   eval      scbctg = tgbctg
     C                   eval      scbcds = tgbcds
     C                   eval      scstat = tgstat
     C*
     C                   if        *in40 = *on
     C                   eval      scstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      tgco# = ##co#
     C                   eval      tgbctg = scbctg
     C                   eval      tgbcds = scbcds
     C                   eval      tgstat = scstat
     C*
     C                   eval      tguser = q1user
     C                   eval      tgpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      tgtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         tgdt
     C*
     C                   if        *in40 = *on
     C                   eval      tgiusr = q1user
     C                   eval      tgipgm = q1pgm
     C                   eval      tgitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         tgidt
     C                   endif
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
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@mode        comp      '1'                                    09
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C
     C                   endsl
     C*
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
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wttgp
     C                   endsr
