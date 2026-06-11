     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE F2368R01                                                                      *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H actgrp(*caller)
     H**************************************************************************************
     H*  Description:  Medicare Clinical Lab FS                                            *
     H*                                                                                    *
     H*  Created: 07/03/09   Programmer C:227   Project:AR038                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  01/09/18   C:079     Project : FS538                                              *
     H*                       After 1/1/2018 do not display the state fields               *
     H*                                                                                    *
     H*  01/06/16   C:212     Project : FS493                                              *
     H*                       Added processing for new field for OK                        *
     H*                                                                                    *
     H*  10/11/11   C:280    Project : SEQ78229                                            *
     H*                      Added option 20 for status change                             *
     H*                                                                                    *
     H*  06/22/11    C:198     Project : 114MYS                                            *
     H*                                  Added field for PA.                               *
     H*                                                                                    *
     H*  01/17/11    C:198     Project : FS388                                             *
     H*                                  Re-compiled.                                      *
     H*                                                                                    *
     H*  10/01/10    C:198     Project : WV009                                             *
     H*                                  Added field for West Virginia.                    *
     H*                                                                                    *
     H*  08/12/10    C:251     Project : 114MWS                                            *
     H*                                  Medicare Implementation :Add field for New Jersey *
     H*                                                                                    *
     H**************************************************************************************
    H*
     Ff2368f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftlbp     uf a e             disk
     F                                     rename(plb01:dbrcdf)
     Fftlbl1    if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare Clinical Lab FS'
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
     D kkefdt          s                   like(lbefdt)
     D kkhcpc          s                   like(lbhcpc)
     D kkmod           s                   like(lbmod)
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
     C     lbky          klist
     C                   kfld                    kkhcpc
     C                   kfld                    kkmod
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*** Beg Add *** 10/11/11 ****************************************
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
    C*
     C     chgsts        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
     C                   if        *in99 = *off
    C*
     C                   if        lbstat = 'I'
     C                   eval      lbstat = 'A'
     C                   else
     C                   eval      lbstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        lbuser
     C                   movel     q1pgm         lbpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         lbdt
     C                   movel     dstime        lbtime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*** End Add *** 10/11/11 ****************************************
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
     C* Procedure code must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             schcpc = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   endif
     C* Description must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scdesc = *blanks
     C                   seton                                        749086
     C                   leavesr
     C                   endif
     C* Modifier must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scmod = *blanks
     C                   seton                                        729088
     C                   leavesr
     C                   endif
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        739087
     C                   leavesr
     C                   else
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779087
     C                   leavesr
     C                   endif
     C                   endif
     C* Check for duplicate record in FTLBP(If in ADD mode)
     C                   eval      kkhcpc = schcpc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C                   eval      kkmod  = scmod
     C     lbky          chain     plb01
     C                   if        %found
     C                   seton                                        769089
     C                   leavesr
     C                   endif
     C                   endif
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
    C*** Beg Add *** 01/09/18 ****************************************
     C                   If        LBefDt >= 1180101
     C                   Eval      is2018 = *on
     C                   Else
     C                   Eval      is2018 = *off
     C                   Endif
    C*** End Add *** 01/09/18 ****************************************
     C                   clear                   schcpc
     C                   clear                   scmod
     C                   clear                   scefdt
     C                   clear                   sctx
     C                   clear                   sctn
     C                   clear                   scar
     C                   clear                   scdc
     C                   clear                   scdesc
     C                   clear                   scstat
     C**** BEG ADD **** 08/12/10 **************************************
     C                   clear                   scnj
     C**** END ADD **** 08/12/10 **************************************
     C**** Beg Add **** 01/09/18 **************************************
     C                   Clear                   scPayRate
     C**** End Add **** 01/09/18 **************************************
     C                   if        *in40 = *on
     C                   eval      schcpc = *blanks
     C                   eval      scmod = *blanks
     C                   eval      scefdt = *zeros
     C**                 eval      scnlim = *zeros
     C                   eval      sctx = *zeros
     C                   eval      sctn = *zeros
     C                   eval      scar = *zeros
     C                   eval      scdc = *zeros
     C                   eval      scdesc = *blanks
     C                   eval      scstat = 'A'
     C**** BEG ADD **** 08/12/10 **************************************
     C                   eval      scnj = *zeros
     C**** END ADD **** 08/12/10 **************************************
     C**** Beg Add **** 01/09/18 **************************************
     C                   eval      scPayRate = *zeros
     C**** End Add **** 01/09/18 **************************************
     C*
     C                   else
     C                   if        *in99 = *off
     C*
     C                   eval      schcpc = lbhcpc
     C                   eval      scmod = lbmod
     C                   z-add     lbefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C**                 eval      scnlim = lbnlim
     C                   eval      sctx = lbtx
     C                   eval      sctn = lbtn
     C                   eval      scar = lbar
     C                   eval      scdc = lbdc
     C                   movel     lbdesc        scdesc
     C                   eval      scstat = lbstat
     C**** BEG ADD **** 08/12/10 **************************************
     C                   eval      scnj = lbnj
     C**** END ADD **** 08/12/10 **************************************
     C*** Beg Add *** 10/01/10 ****************************************
     C                   eval      scwv = lbwv
     C*** End Add *** 10/01/10 ****************************************
     C*** Beg Add *** 01/06/16 ****************************************
     C                   eval      scok = lbok
     C*** End Add *** 01/06/16 ****************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C                   eval      scpa = lbpa
     C*** End Add *** 06/22/11 ****************************************
     C*** Beg Add *** 01/09/18 ****************************************
     C                   eval      scPayRate = LBnLim
     C*** End Add *** 01/09/18 ****************************************
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      lbhcpc = schcpc
     C                   eval      lbmod = scmod
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         lbefdt
     C**                 eval      lbnlim = scnlim
     C                   eval      lbtx = sctx
     C                   eval      lbtn = sctn
     C                   eval      lbar = scar
     C                   eval      lbdc = scdc
     C                   movel     scdesc        lbdesc
     C                   eval      lbstat = scstat
     C**** BEG ADD **** 08/12/10 **************************************
     C                   eval      lbnj = scnj
     C**** END ADD **** 08/12/10 **************************************
     C*** Beg Add *** 10/01/10 ****************************************
     C                   eval      lbwv = scwv
     C*** End Add *** 10/01/10 ****************************************
     C*** Beg Add *** 01/06/16 ****************************************
     C                   eval      lbok = scok
     C*** End Add *** 01/06/16 ****************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C                   eval      lbpa = scpa
     C*** End Add *** 06/22/11 ****************************************
     C*** Beg Add *** 01/09/18 ****************************************
     C                   eval      LBnLim = scPayRate
     C*** End Add *** 01/09/18 ****************************************
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
     C*** Beg Add *** 01/09/18 *************************************
     C                   eval      *inlr = *On
     C*** End Add *** 01/09/18 *************************************
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
    C*** Beg Add *** 10/11/11 ****************************************
     C     @@mode        comp      'C'                                    10
    C*** End Add *** 10/11/11 ****************************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
    C*
    C*** Beg Add *** 10/11/11 ****************************************
    C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
    C*
    C*** End Add *** 10/11/11 ****************************************
    C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        lbuser
     C                   movel     q1pgm         lbpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         lbdt
     c                   movel     dstime        lbtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        lbuser
     C                   movel     q1pgm         lbpgm
     C                   movel     q1pgm         lbipgm
     C                   movel     q1user        lbiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         lbdt
     C                   movel     srdto         lbidt
     c                   movel     dstime        lbtime
     c                   movel     dstime        lbitim
     C                   write     dbrcdf
     C                   feod      ftlbp
     C                   endsr
