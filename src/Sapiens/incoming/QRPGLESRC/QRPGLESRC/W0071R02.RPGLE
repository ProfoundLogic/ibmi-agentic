     h*  (c) Copyright Fiserv, Inc., 2003-2007
    h*
      /title Update/Display CiCenter NOI Interface Record
      *================================================================
     h* PROGRAM DESCRIPTION:
     h*
     h*   Update and/or display CiCenter NOI Interface Record
     h*
     h*   Called from: W0071R
     h*
     h*   Calls to:    None
     h*
     h* Created 04/03/02  C:136  Project WC1084
      *================================================================
     h* REVISIONS:                                                    *
      *                                                               *
      * 02/19/21 C:318 Support: 94631
      *                Mask SSN and make single field for ssn.
      *
      * 01/29/08 C:062 Support: WC132 60786                           *
      *                Make SS# char instead of numeric.              *
      *                                                               *
      * 06/25/07 C:191 Support: WC114 58501                           *
      *                Changed PGMSTR to seton *IN10 only when FRSTAT *
      *                is 'T'                                         *
      *                                                               *
      * 06/20/07 C:132 Support: WC114 58776                           *
      *                Change edit word on SS# to allow leading zero  *
      *                to show on screen. COMPILE ONLY-NO CHGS IN PGM *
      *                                                               *
      * 04/19/04 C:136 Support: WC099 42112                           *
      *                Prevent date check error if accident date is   *
      *                zero.                                          *
      *                                                               *
      * 03/22/04 C:136 Support: CI072 41717                           *
      *                Update the record for a status of 'D' when     *
      *                option 4 is used.                              *
      *================================================================
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0071f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    f*
    f*  Physical file definition (for maintaining record)
     fwhfrp     uf a e             disk
     f                                     rename(pfr01:dbrcdf)
      * Employer file (co/fnd/emp/div)
     fwmeml     if   e           k disk
      * Employer file
     fwmemp     if   e             disk    rename(pem01:pem02)
      * Fund file (co/fnd)
     fwmfnl     if   e           k disk    infds(dbfdsf)
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d optnm           c                   'CiCenter Internet Claims'
     d comapi          c                   'WA001R'
     d #ofscr          c                   1
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d dbfdsf          ds
     d   dbrrnf              397    400B 0
     d ##co#           s              3  0
     d ##conm          s             40
     d ##fnam          s             40
     d ##fnd           s              3  0
     d @@kc            s              1
     d @@kl            s              1
     d @@mode          s              1
     d @@rrn           s              9  0
     d qqkc            s              1
     d qqkl            s              1
     d qqcall          s              1
     d qqco#           s              3
     d qqfnd           s              3
     d qqfyr           s              3
     d qqview          s              1  0
     d qqrrn           s              9  0
     d qqrrn2          s              9  0
     d qqqte           s              1
     d usaDate         s               d   datfmt(*usa)
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
    c*
     c/COPY SCOPYMT01C
    c*
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@rrn
    c*
    c*  company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam

     c     dtusis        plist
     c                   parm                    srdti
     c                   parm      '*USA'        srffmt
     c                   parm      '*ISO'        srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     emky          klist
     c                   kfld                    ##co#
     c                   kfld                    scgrp
     c                   kfld                    scemp
     c                   kfld                    scdiv

     c     fnky          klist
     c                   kfld                    ##co#
     c                   kfld                    scgrp
    c*================================================================
    c* DLTPRE - Pre-processing before delete occurs
    c*================================================================
     c     dltpre        begsr

      *** beg add ***** 03/22/04 **************************************
     c     @@rrn         chain     dbrcdf
      *** end add ***** 03/22/04 **************************************
     c                   eval      FRSTAT = 'D'
     c                   time                    dsdttm
     c                   eval      FRUSER = Q1USER
     c                   eval      FRPGM  = Q1PGM
     c                   eval      FRTIME = dstime
     c                   eval      srdti  = dsdate
     c                   call      'SRCVTDAT'    dtusis
     c                   movel     srdto         FRDATE
      *** beg add ***** 03/22/04 **************************************
     c                   update    dbrcdf
      *** end add ***** 03/22/04 **************************************
     c                   seton                                        lr
     c                   return

     c                   endsr
    c*================================================================
    c* DSPLYR - Processing after displaying information
    c*================================================================
     c     dsplyr        begsr
     c                   endsr
    c*================================================================
    c* ERRCK1 - Error checking for screen 1 (SCR01)
    c*================================================================
     c     errck1        begsr

     c                   if        scstat <> 'D' and
     c                             scstat <> 'T' and
     c                             scstat <> 'P'
     c                   seton                                        71  90
     c                   endif

     c                   if        scgrp  <> *zero
     c     fnky          chain     pfn01
     c                   if        not %found
     c                   seton                                        72  90
     c                   endif
     c                   endif

     c  n90              if        scemp  <> *zero or
     c                             scdiv  <> *zero
     c     emky          chain     pem01
     c                   if        not %found
     c                   seton                                        73  90
     c                   endif
     c                   endif

     c  n90              if        scacdt <> *zero
     c     *usa          test(de)                scacdt

     c                   if        %error
     c                   seton                                        74  90
     c                   endif

     c                   endif

     c                   endsr
    c*================================================================
    c* ERRCK2 - Error checking for screen 2 (SCR02)
    c*================================================================
     c     errck2        begsr
     c                   endsr
    c*================================================================
    c* ERRCK3 - Error checking for screen 3 (SCR03)
    c*================================================================
     c     errck3        begsr
     c                   endsr
    c*================================================================
    c* KJDO - Search routine
    c*================================================================
     c     kjdo          begsr

     c                   if        hfld = 'SCGRP'                               search on group
     c                   move(p)   ##co#         qqco#
     c                   call      'W0010R'
     c     *inkc         parm                    qqkc
     c     *inkl         parm                    qqkl
     c                   parm      '2'           qqcall
     c                   parm                    qqco#
     c                   parm      *zero         qqfnd
     c                   parm                    qqfyr
     c                   parm      1             qqview
      *
     c                   if        qqfnd <> *zero
     c                   move(p)   qqfnd         scgrp
     c                   seton                                        76        position crsr scemp
     c                   else
     c                   seton                                        75        position crsr scgrp
     c                   endif                                                  qqfnd <> 0
     c                   endif                                                  hfld = 'SCGRP'
      *
     c                   if        hfld = 'SCEMP' or hfld = 'SCDIV'             search on emp or div
     c                   if        scgrp = *zero                                search if fund = 0
     c                   eval      qqqte = *blanks
     c                   call      'W0050R'
     c     *inkc         parm                    qqkc
     c     *inkl         parm                    qqkl
     c                   parm      '2'           qqcall
     c                   parm      0             qqrrn
     c                   parm      0             qqrrn2
     c                   parm                    qqqte
     c                   else                                                   search if fund <> 0
     c     fnky          chain     pfn01
     c                   call      'W0050R'
     c     *inkc         parm                    qqkc
     c     *inkl         parm                    qqkl
     c                   parm      '2'           qqcall
     c                   parm      0             qqrrn
     c                   parm      dbrrnf        qqrrn2
     c                   parm      *blank        qqqte
     c                   endif                                                  scgrp = 0
      *
     c                   if        qqrrn <> 0                                   if rcrd is selected
     c     qqrrn         chain     pem02                              60
     c                   if        not *in60
     c                   eval      scgrp = emfnd                                load screen fields
     c                   eval      scemp = ememp#
     c                   eval      scdiv = emdiv
     c                   seton                                        77        position to scss
     c                   endif                                                  not 60
     c                   else
     c                   if        *inkl = *on                                  position to scemp
     c                   seton                                        76        if F12 is selected
     c                   endif
     c                   endif                                                  qqrrn <> 0
     c                   endif                                                  hfld = 'SCEMP' or hf
      *
     c                   endsr
    c*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr

     c                   eval      scstat = FRSTAT
     c                   eval      scgrp  = FRFND
     c                   eval      scemp  = FREMP
     c                   eval      scdiv  = FRDIV
     c                   eval      scfsn  = FRFSN
     c                   eval      sclsn  = FRLSN
     c                   eval      scmid  = FRMID
     c                   eval      scadr1 = FRADR1
     c                   eval      sccty  = FRCTY
     c                   eval      scst   = FRST
     c*** begin chg *** 01/29/08 **************************************
     c**********         move(p)   FRSS          scss
     C***  Begin Chg  ***  02/19/21  *******************************
     c*                  eval      scss1=%subst(frss:1:3)
     c*                  eval      scss2=%subst(frss:4:2)
     c*                  eval      scss3=%subst(frss:6:4)
     c                   eval      scSocSec#=frss
     C***  End Chg  ***  02/19/21  *******************************
     C***  Begin Add  ***  02/19/21  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scSocSec#'   qqname           25
     C     scSocSec#     parm      scSocSec#     qqval           100
     C***  End Add  ***  02/19/21  *******************************
     c*** end   chg *** 01/29/08 **************************************
     c                   move(p)   FRZIP         sczip

     c     *iso0         test(de)                FRACDT
     c                   if        not %error
     c     *iso0         move      FRACDT        usaDate
     c                   move      usaDate       scacdt
     c                   endif

     c                   endsr
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr

     c                   eval      FRSTAT = scstat
     c                   eval      FRFND  = scgrp
     c                   eval      FREMP  = scemp
     c                   eval      FRDIV  = scdiv
     c                   eval      FRFSN  = scfsn
     c                   eval      FRLSN  = sclsn
     c                   eval      FRMID  = scmid
     c                   eval      FRADR1 = scadr1
     c                   eval      FRCTY  = sccty
     c                   eval      FRST   = scst
     c*** begin chg *** 01/29/08 **************************************
     c**********         move(p)   scss          FRSS
     C***  Begin Add  ***  02/19/21  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scSocSec#'   qqname           25
     C     scSocSec#     parm      scSocSec#     qqval           100
     C***  End Add  ***  02/19/21  *******************************
     C***  Begin Chg  ***  02/19/21  *******************************
     c*                  eval      frss=scss1+scss2+scss3
     c                   eval      frss=scSocSec#
     C***  End Chg  ***  02/19/21  *******************************
     c*** end   chg *** 01/29/08 **************************************
     c                   move(p)   sczip         FRZIP

      *** beg add ***** 04/19/04 **********************************
     c                   if        scacdt <> 0
      *** end add ***** 04/19/04 **********************************
     c     *usa          move      scacdt        usaDate
     c     *iso0         move      usaDate       FRACDT
      *** beg add ***** 04/19/04 **********************************
     c                   else
     c                   eval      FRACDT = *blanks
     c                   endif
      *** end add ***** 04/19/04 **********************************

     c                   endsr
    c*================================================================
    c* OTHERR - Processing for other modes
    c*================================================================
     c     otherr        begsr
     c                   endsr
    c*================================================================
    c* OTHFKS - Processing for other functions keys
    c*================================================================
     c     othfks        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     c                   endsr
    c*================================================================
    c* PGMEND - End of program processing
    c*================================================================
     c     pgmend        begsr
     c                   seton                                        lr
     c                   endsr
    c*================================================================
    c* PGMSTR - Start of program processing
    c*================================================================
     c     pgmstr        begsr

     c     @@rrn         chain     dbrcdf
     c*** BEG CHG ****** 06/25/07 ************************************************
     c************       if        FRSTAT <> 'P'
     c                   if        FRSTAT = 'T'
     c*** END CHG ****** 06/25/07 ************************************************
     c                   seton                                        10
     c                   endif

     c                   endsr
    c*================================================================
    c* PREOTH - Pre-processing for other modes
    c*================================================================
     c     preoth        begsr
     c                   endsr
    c*================================================================
    c* UPDATR - Update data base record and after processing
    c*================================================================
     c     updatr        begsr
     c     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     c                   exsr      mvscdb
     c                   time                    dsdttm
     c                   eval      FRUSER = Q1USER
     c                   eval      FRPGM  = Q1PGM
     c                   eval      FRTIME = dstime
     c                   move      dsdate        usaDate
     c     *iso0         move      usaDate       FRDATE
     c                   update    dbrcdf
E01  c                   endif
     c                   endsr
    c*================================================================
    c* WRITER - Write data base record and after processing
    c*================================================================
     c     writer        begsr
     c                   endsr
