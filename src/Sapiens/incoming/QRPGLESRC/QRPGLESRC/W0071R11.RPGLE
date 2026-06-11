     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    h*
      /title CiCenter Internet Claims - Quick Edit Option
      *****************************************************************
     h* PROGRAM DESCRIPTION:
     h*
     h*   CiCenter Claims - Quick Edit Option
     h*
     h*   Called from: W9302R (Opt 11)
     h*
     h*   Calls to:    None
     h*
     h* Created 04/07/03  C:136  Project: WC1084
      *
      *==================================================================================*
      *  REVISIONS:                                                                      *
     *                                                                                  *
      * 10/19/06 C:15   Project: 132WDQ                                                  *
      *                 Chgd pgm to support a "batch" mode and return a parm indicating  *
     *                 whether or not all edits where passed.                           *
     *                                                                                  *
      * 03/08/06 C: 132 PROJECT: WC071 - 51160                                           *
      *                - Change *DATE to use The Current Date from DSDTTM                *
      *                  Data Structure DSDATE.                                          *
      *                                                                                  *
      *==================================================================================*
      *
     f*** Begin chg *** 10/19/06 *********************************************************
     f*********w0071f11  cf   e             workstn
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0071f11  cf   e             workstn usropn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f*** End chg ***** 10/19/06 *********************************************************

    f*  CiCenter claims file
     fwhfrp     if   e             disk

    f*  Claims file
     fwmcmlb    if   e           k disk

    f*  Employer file
     fwmeml     if   e           k disk

    f*  Employer detail file
     fwdell2    if   e           k disk
    d*---------------------------------------------------------------------
    d* Arrays
    d*---------------------------------------------------------------------
     d msg             s             66    dim(10) ctdata
     d err             s             66    dim(10)
    d*---------------------------------------------------------------------
    d* Data Structures
    d*---------------------------------------------------------------------
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
    
     d dateymd         ds
     d   yyyy                  1      4
     d   mm                    5      6
     d   dd                    7      8

     d                 ds                  inz
     d  ed                     1     35  0
     d                                     dim(5) ascend
     d  emefd1                 1      7  0
     d  emefd2                 8     14  0
     d  emefd3                15     21  0
     d  emefd4                22     28  0
     d  emefd5                29     35  0

     d                 ds                  inz
     d  cd                     1     35  0
     d                                     dim(5) ascend
     d  emcad1                 1      7  0
     d  emcad2                 8     14  0
     d  emcad3                15     21  0
     d  emcad4                22     28  0
     d  emcad5                29     35  0

     d  display        ds
     d    scerr1               1     66
     d    scerr2              67    132
     d    scerr3             133    198
     d    scerr4             199    264
     d    scerr5             265    330
     d    scerr6             331    396
     d    scerr7             397    462
     d    scerr8             463    528
     d    scerr9             529    594
     d    scerr10            595    660
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d @@kc            s              1
     d @@kl            s              1
     d @@mode          s              1
     d @@rrn           s              9  0
     d kkco#           s                   like(cmco#)
     d kkss#           s                   like(cmss#)
     d kkacdt          s                   like(cmacdt)
     d kkfnd           s                   like(cmfnd)
     d kkemp           s                   like(cmemp#)
     d kkdiv           s                   like(cmdiv)
     d kkefdt          s                   like(cmacdt)
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d xxfnd           s                   like(cmfnd)
     d xxfyr           s                   like(cmfyr)
     d xxcase          s                   like(cmcase)
     d xxacdt          s                   like(cmacdt)
     d x               s              1  0
     d y               s              1  0
     d acc_date        s               d   datfmt(*usa)
     d sys_date        s               d   datfmt(*usa)
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@rrn

     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     cmky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkss#

     c     elky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp
     c                   kfld                    kkdiv
     c                   kfld                    kkefdt

     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp
     c                   kfld                    kkdiv

     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
    c*================================================================
    c*  Main
    c*================================================================
     c     @@rrn         chain     pfr01                              60

     c                   if        not *in60
      *
      * Check for duplicate claims
      *
     c                   eval      x = 1
     c                   eval      kkco# = FRCO
     c                   move      FRSS          kkss#
     c     cmky          chain     pcm01

     c                   if        %found
     c                   eval      err(x) = msg(1)
     c                   eval      x = x + 1
     c                   endif
      *
      * Validate employer number
      *
     c                   eval      kkco# = FRCO
     c                   eval      kkfnd = FRFND
     c                   eval      kkemp = FREMP
     c                   eval      kkdiv = FRDIV
     c     emky          chain     pem01                              60

     c                   if        *in60
     c                   eval      err(x) = msg(2)
     c                   eval      x = x + 1
     c                   endif
      *
      * Validate social security number (must exist)
      *
     c                   if        FRSS  = *blanks or
     c                             FRSS  = *zeros
     c                   eval      err(x) = msg(3)
     c                   eval      x = x + 1
     c                   endif
      *
      * Validate accident date (1) must exist (2) and be less or equal to system date
      *
     c     *iso0         test(de)                FRACDT
     c                   if        %error
     c                   eval      err(x) = msg(4)
     c                   eval      x = x + 1
     c                   else
     c     *iso0         move      FRACDT        acc_date
      *
      ***** BEG CHG ***** 03/08/06 *******************************************************
     c**** *usa*******   move      *date         sys_date
     c                   TIME                    DSDTTM
     c     *usa          move      DSDATE        sys_date                        mmddyyyy
      ***** END CHG ***** 03/08/06 *******************************************************

     c                   if        acc_date > sys_date
     c                   eval      err(x) = msg(5)
     c                   eval      x = x + 1
     c                   endif
      *
      * Coverage edits:
     *    Check the employer effective and cancellation dates. Get the most current eff date.
     *
     c                   eval      y = 1
     c     *zero         lookup    ed(y)                                  60
     c                   if        *in60 = *off
     c                   eval      y = 5
     c                   else
     c                   eval      y = y - 1
     c                   endif
     *
     *    Compare the accident date to the effective date. If acc date is >= to eff date,
     *    get out of the loop.
     *
     c     *cymd         move      acc_date      xxacdt
     c                   dow       y <> 0
     c                   if        xxacdt >= ed(y)
     c                   leave
     c                   else
     c                   eval      y = y - 1
     c                   endif
     c                   enddo
     *
     *    The accident date can't be greater than the cancel date,  unless overriden by the user.
     *
     c                   if        y = *zero
     c                   eval      y = 1
     c                   move      *on           *in83
     c                   else
     *
     c                   if        cd(y) <= xxacdt
     c                             and cd(y) > *zero
     c                   move      *on           *in83
     c                   endif
     *
     *    *in83=*on indicates no active coverage for accident date
     *
     c                   if        *in83 = *on
    
     c                   eval      err(x) = msg(6)
     c                   eval      x = x + 1
     c                   eval      err(x) = msg(7)
     c                   eval      x = x + 1
     c                   endif
    
     c                   endif
     *
     *    Verify policy year coverage (WDELP).
     *
     c                   eval      kkefdt = xxacdt
     c     elky          setgt     pel01
     c     emky          readpe    pel01                                  99
     *
     *    If accident date is not within the policy eff period, dsp window.
     *
     c                   if        elpfdt > xxacdt
     c                             or elptdt <= xxacdt
     c                             or elpfdt = *zero
    
     c                   eval      err(x) = msg(8)
     c                   eval      x = x + 1
     c                   eval      err(x) = msg(9)
     c                   eval      x = x + 1
    
     c                   endif
    
     c                   endif

      *
      * Display window
      *
     c*** Begin add *** 10/19/06 *********************************************************
     c                   if        @@mode = 'X'
     c                   if        x > 1
     c                   eval      @@mode = '1'
     c                   endif
     c                   else
     c                   open      w0071f11
     c*** End add ***** 10/19/06 *********************************************************

     c                   if        x = 1
     c                   eval      err(x) = msg(10)
     c                   endif

     c                   movea     err           display
     c                   exfmt     wdw01
     c                   endif

     c*** Begin add *** 10/19/06 *********************************************************
     c                   endif
     c*** End add ***** 10/19/06 *********************************************************

     c                   eval      *inlr = *on
** msg
Possible duplicate claim exists.
The employer number not valid or doesn't exist.
The social security number is blank.
The accident date is blank or equal to zeros.
The accident date must be less or equal to system date.
The accident date does not fall within the effective and
    cancellation date ranges in the Employer Master File.
The accident date does not fall within the policy start
    and end dates in the Fund Detail File.
No errors were found.
