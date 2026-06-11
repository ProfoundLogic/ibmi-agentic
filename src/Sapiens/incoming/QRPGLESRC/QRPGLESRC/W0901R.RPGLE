    H*  (C) Copyright Fiserv, Inc., 2003 - 2007
    H**************************************************************************************
    H/TITLE FROI Import Process
    H**************************************************************************************
    H*  Description: Prompts user to run the FROI Claim Import Process.                   *
    H*                                                                                    *
    H*  Called from: Menu                                                                 *
    H*                                                                                    *
    H*  Calls to:                                                                         *
    h*                                                                                    *
    h*  Created: 10/19/06      C:15      Project: 132WDQ                                  *
    h**************************************************************************************
    h*  Revisions:                                                                        *
     h*                                                                                    *
     h*                                                                                    *
    h**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0901f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     fwdihp     o    e             disk
     f                                     infds(dbfds)
    
     d/copy scopypr01d
     d dbfds           ds
     d  dbrrn                397    400b 0
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d optnm           c                   const('FROI Claim Import')
     d comapi          c                   const('WA001R')
     d @@call          s              1
     d @@kc            s              1
     d @@kl            s              1
     d ##co#           s              3  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d ##kc            s              1
     d ##kl            s              1
     d ##call          s              1
     d ##rrn           s              9  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d Sys_time        ds
     d  Time_date              1     14  0
     d  Time                   1      6  0
     d  Date                   7     14  0
   11d
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm                    @@call
    c*
    c*  Company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
    c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    c*================================================================
    c*  KLIST definitions
    c*================================================================

     c     upky          klist
     c                   kfld                    Q1USER

     c/copy scopypr01c
    c*====================================================================
    c* DSCAPI - Call API for getting descriptions (use 91 on error)
    c*====================================================================
     c     DSCAPI        begsr
     c                   call      comapi        compl                  91
     c                   endsr                                                  DSCAPI
    c*====================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
    c*====================================================================
     c     ERRCK1        begsr
     c                   exsr      othfks

     c                   endsr                                                  ERRCK1
    c*====================================================================
    c* KMDO   - Processing for F13=Change user parms
    c*====================================================================
     c     KMDO          begsr
     c                   call      'W0006R'                             99
     c                   exsr      getcof
     c                   endsr                                                  KMDO
    c*====================================================================
    c* KRDO   - Processing for F17 - Import History
    c*====================================================================
     c     KRDO          begsr

     c                   call      'W0900R'
     c     *inkc         parm                    ##kc
     c                   parm                    ##kl
     c                   parm      '1'           ##call
     c                   parm      *zero         ##rrn

     c                   endsr                                                  KJDO
    c*====================================================================
    c* OTHFKS - Processing for other functions keys
    c*====================================================================
     c     OTHFKS        begsr
     c     *inkm         caseq     *on           kmdo                     90
     c     *inkr         caseq     *on           krdo                     90
     c                   endcs
     c                   endsr                                                  OTHFKS
    c*====================================================================
    c* PGMEND - End of program processing
    c*====================================================================
     c     PGMEND        begsr
     c                   seton                                        lr
     c                   endsr                                                  PGMEND
    c*====================================================================
    c* PGMSTR - Used for any start of program processing (one-time)
    c*====================================================================
     c     PGMSTR        begsr
     C                   movel(p)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
     c                   endsr                                                  PGMSTR
    c*====================================================================
    c* PREDSP - Pre-processing before displaying (every time)
    c*====================================================================
     c     PREDSP        begsr

     c                   endsr                                                  PREDSP
    c*====================================================================
    c* PRSCTL - Process control after no errors found (Call Submitter)
    c*====================================================================
     c     PRSCTL        begsr

     c*  Write Import History record.
     c*
     c                   clear                   pih01

     c                   eval      ihco#  = ##co#
     c                   eval      ihiusr = q1user
     c                   eval      ihipgm = q1pgm

     c                   time                    Time_Date
     c                   move      date          srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         ihidt

     c                   eval      ihitim = time

     c                   write     pih01

     c*  Call submitter
     c*
     c                   move      dbrrn         qqrrn
     c                   call      'W0901C'
     c                   parm                    qqrrn             9

     c                   endsr                                                  PRSCTL
