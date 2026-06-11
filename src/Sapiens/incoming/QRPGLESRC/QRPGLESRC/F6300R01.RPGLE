     H**************************************************************************************
     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H*  Description:  NCCI Medical Data Call Submission Maintenance                       *
     H*                                                                                    *
     H*  Created: 04/02/09   Programmer C:227   Project:MC331                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff6300f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffeu9p     uf a e             disk
     F                                     rename(pu901:dbrcdf)
     Ffenll3    uf a e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'NCCI Data Call Submissions'
     D comapi          c                   'FA007R'
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
     D @@co#           s              3
     D @@mth           s              2
     D @@qtr           s              1
     D @@year          s              4
     D @@ftcd          s              1
     D @@lgfl          s                   like(u9file)
     D @@sbdt          s                   like(u9sbdt)
     D CurDte          s                   like(u9sbdt)
     D @@sbtm          s                   like(u9sbtm)
     D @@prfl          s                   like(u9prfl)
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
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
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
     C     nlky          klist
     C                   kfld                    @@lgfl
     C                   kfld                    @@sbdt
     C                   kfld                    @@sbtm
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
     C                   move      *blanks       scpdsc
     C                   move      *blanks       scfdsc
     C                   move      *blanks       scofil
     C                   movel     u9file        scfile
     C                   movel     u9ofil        scofil
     C                   z-add     u9sbdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         scsbdt
     C                   movel     u9sbtm        scsbtm
     C                   movel     u9mth         scmth
     C                   movel     u9qtr         scqtr
     C                   z-add     u9year        scyear
     C                   movel     u9prfl        scprfl
     C                   if        u9prfl <> ' '
     C                   eval      *in88 = *on
     C                   select
     C                   when      u9prfl = 'P'
     C                   eval      scpdsc = 'Processed'
     C                   when      u9prfl = 'D'
     C                   eval      scpdsc = 'Deleted'
     C                   when      u9prfl = 'F'
     C                   eval      scpdsc = 'File Failure Delete'
     C                   endsl
     C                   endif
     C                   z-add     u9rcds        scrcds
     C                   movel     u9ftcd        scftcd
     C                   if        u9ftcd = 'O'
     C                   eval      scfdsc = 'Original'
     C                   elseif    u9ftcd = 'R'
     C                   eval      scfdsc = 'Replacement'
     C                   endif
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
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
     C                   if        @@mode = 'A'
     C                   call      'F6301R'
     C                   return
     C                   endif
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C     @@rrn         chain     dbrcdf
     C                   if        %found
     C                   movel     ##co#         @@co#
     C                   move      *all'0'       @@mth
     C                   move      u9mth         @@mth
     C                   movel     u9qtr         @@qtr
     C                   movel     u9year        @@year
     C                   eval      @@lgfl = u9file
     C                   eval      @@sbdt = u9sbdt
     C                   eval      @@sbtm = u9sbtm
     C                   eval      @@prfl = u9prfl
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   move      *blanks       srdto
     C                   move      *blanks       srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         CurDte
     C*
     C* Updating FEU9P and FENLP in case of Opt-7 and Opt-8
     C*
     C                   if        @@mode = 'X' or @@mode = 'F'
     C*
     C                   if        @@mode = 'X'
     C                   eval      u9prfl = 'D'
     C                   elseif    @@mode = 'F'
     C                   eval      u9prfl = 'F'
     C                   endif
     C*                  move      *blanks       srdto
     C*                  move      *blanks       srerr
     C*                  call      'SRCVTDAT'    dtcymd
     C                   movel     CurDte        u9dt
     C                   move      dstime        u9time
     C                   move      q1pgm         u9pgm
     C                   move      q1user        u9user
     C                   update    dbrcdf
     C     nlky          setll     pnl01
     C     nlky          reade     pnl01                                  88
     C                   dow       *in88 = *off
     C                   if        @@mode = 'X'
     C                   eval      nlprfl = 'D'
     C                   elseif    @@mode = 'F'
     C                   eval      nlprfl = 'F'
     C                   endif
     C                   movel     CurDte        nldt
     C                   move      dstime        nltime
     C                   move      q1pgm         nlpgm
     C                   move      q1user        nluser
     C                   update    pnl01
     C     nlky          reade     pnl01                                  88
     C                   enddo
     C*
     C                   endif
     C                   endif
     C*
     C                   select
     C                   when      @@mode = 'S'
     C                   eval      @@ftcd = 'R'
     C                   call      'F6301C'
     C                   parm                    @@co#
     C                   parm                    @@mth
     C                   parm                    @@qtr
     C                   parm                    @@year
     C                   parm                    @@ftcd
     C                   parm                    @@lgfl
     C                   return
     C                   when      @@mode = 'X'
     C                   eval      @@ftcd = 'D'
     C                   call      'F6301C'
     C                   parm                    @@co#
     C                   parm                    @@mth
     C                   parm                    @@qtr
     C                   parm                    @@year
     C                   parm                    @@ftcd
     C                   parm                    @@lgfl
     C                   return
     C                   when      @@mode = 'F'
     C                   return
     C                   endsl
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
     C                   feod      feu9p
     C                   endsr
