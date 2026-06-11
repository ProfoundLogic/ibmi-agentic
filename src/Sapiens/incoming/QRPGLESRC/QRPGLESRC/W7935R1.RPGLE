      * (c) 2011 StoneRiver, Inc. All rights reserved.
      * StoneRiver is a trademark of StoneRiver, Inc.
    H**************************************************************************************
    H/TITLE Direct Debit A/R Batch Entry
    H**************************************************************************************
    H*  Description: Reimbursable Deductible Detail Report.                               *
    H*                                                                                    *
    H*  Called from: Menu                                                                 *
    H*                                                                                    *
    H*  Calls to:                                                                         *
    h*                                                                                    *
    h*  Created: 04/11/03      C:220     Project: 114WRM                                  *
    h**************************************************************************************
    h*  Revisions:                                                                        *
     H*                                                                                    *
     H* 03/29/12 C:136 Support: WA114 77875                                                *
    h*               -Add loss date and exclude policy fields and pass to submitter       *
    h*               -Correct edit for loss date                                          *
    h*                                                                                    *
     H* 11/24/11 C:282 Project WC2003                                                      *
     H*                Add prompt for print of SSN.                                        *
    h*                                                                                    *
     H* 03/02/11 C:189 PROJECT: WC1974                                                     *
     H*                          Add a new prompt entry that will allow the user to        *
     H*                          generate output for just Large Deductibles,               *
     H*                          just Small Deductibles or Both                            *
    h**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw7935f1   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
     fwmfnl     if   e           k disk
     fwmdbl     if   e           k disk
     fwmdcl     if   e           k disk
     fwmupl     if   e           k disk
     d/copy scopypr01d
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d optnm           c                   const('Reimbursable Deductible +
     d                                     Detail Report')
     d comapi          c                   const('WA001R')
     d @@call          s              1
     d @@kc            s              1
     d @@kl            s              1
     d xxcall          s                   like(@@call)
     d ##co#           s              3  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d ##kc            s              1
     d ##kl            s              1
     d ##call          s              1
     d ##rrn           s              9  0
     d qqco#           s              3
     d qqfnd           s              3
     d qqrndt          s              8
     d qqoutq          s             10
     d qqkc            s              1
     d qqkl            s              1
     d qqcall          s              1
     d qqfyr           s              3
     d qqview          s              1  0
     d kkgrp           s              3  0
     d runDate         s              8  0
     d toPODT8         s              8  0
     d svgrp           s              3  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
     d/copy SPRDATE
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm                    @@call
    
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    c*
    c*  Company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     fnky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkgrp

     c     upky          klist
     c                   kfld                    Q1USER
      **** BEGIN ADD **** 03/28/08 *********************************************
      *
      * KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     c     syky          klist
     c                   kfld                    xxsyk             2
      **** END   ADD **** 03/28/08 *********************************************

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
      *** beg chg ***** 03/29/12 *******************************************
     C***********        if        scprdt <> *zeros
     C***********        z-add     scprdt        srdti
     C***********        call      'SRCVTDAT'    datchk
     c***********        movel     srdto         scprdt
     C***********
     C***********        if        srerr = *off
     C***********        movel     scprdt        ##dt              8
     C***********
     C***********        else
     C***********        seton                                        3590
     C***********        endif                                                   scprdt <>valid
     C***********        endif                                                   scprdt <>blank

      *** Validate post date
     C                   z-add     scprdt        srdti
     C                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scprdt
     C                   if        srerr = *off
     C                   movel     scprdt        ##dt              8
     C                   else
     C                   seton                                        3590
     C                   endif
      *** Validate loss date
     C                   z-add     sclsdt        srdti
     C                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         sclsdt
     C                   if        srerr = *off
     C                   movel     sclsdt        ##lsdt            8
     C                   else
     C                   seton                                        3890
     C                   endif
      *** end chg ***** 03/29/12 *******************************************
     C
     C                   if        scpsts <> 'B' and scpsts <> 'N'
     C                             and scpsts <> 'A'
     C                   seton                                        3690
      ******* beg add ****** 11/24/11 *************************************
     c                   leavesr
      ******* end add ****** 11/24/11 *************************************
     C                   endif                                                   scpsts B,N,A
     C
     C***  Begin Add  ***  03/02/11  **************************************
     C                   if        scddtp <> 'L' and scddtp <> 'S'
     C                             and scddtp <> 'B'
     C                   seton                                        3790
      ******* beg add ****** 11/24/11 *************************************
     c                   leavesr
      ******* end add ****** 11/24/11 *************************************
     C                   endif                                                   scddtb L,S,B
     C
     C***  End   Add  ***  03/02/11  **************************************
      ******* beg add ****** 11/24/11 *************************************
     c                   if        *in44 = *off
      *
     c                   if        scy4n <> 'Y' and scy4n <> 'N'
     c                             and scy4n  <> '4'
      *** beg chg ***** 03/29/12 *******************************************
      *   Chg'd indicator assignment to *in41
     c                   seton                                        4190
      *** end chg ***** 03/29/12 *******************************************
     c                   leavesr
     c                   endif                                                  scy4n Y,N,4
      *
      * If user set "Print SSN" to "Y" then display info window.
     c                   if        (scy4n <> #1y4n) and
     c                             (scy4n = 'Y')
     c                   eval      *inkl = *off
     c                   exfmt     wdw01

     c                   if        *inkl = *on
     c                   eval      *inkl = *off
     C                   seton                                          90
     c                   leavesr
     c                   endif

     c                   endif                                                  (scy4n <> #1y4n)
      *
     c                   endif                                                  *in44 = *off
      ******* end add ****** 11/24/11 *********************************

      *** beg add ***** 03/29/12 *******************************************
     c                   if        scexfl <> *blank and scexfl <> 'Y'
     c                   seton                                        3990
     c                   endif

     c                   if        scexfl = 'Y' and
     c                             (scpsts = 'A' or scpsts = 'N')
     c                   seton                                        4090
     c                   endif
      *** end add ***** 03/29/12 *******************************************

     c                   endsr                                                  ERRCK1
    c*====================================================================
    c* KJDO   - Processing for F10
    c*====================================================================
     c     KJDO          begsr

     c                   endsr                                                  KJDO
    c*====================================================================
    c* KMDO   - Processing for F13=Change user parms
    c*====================================================================
     c     KMDO          begsr
     c                   endsr                                                  KMDO
    c*====================================================================
    c* KRDO   - Processing for F17 - Transmission History
    c*====================================================================
     c     KRDO          begsr

     c                   endsr                                                  KJDO
    c*====================================================================
    c* OTHFKS - Processing for other functions keys
    c*====================================================================
     c     OTHFKS        begsr
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
      ******* beg add ****** 11/24/11 *********************************

     c                   move      ##co#         #1co#             3

     c                   call      'WA097R'
     c                   parm                    #1co#
     c                   parm      'W7935R1'     #1rpid           10
     c     scy4n         parm      *blank        #1y4n             1
     c     *in44         parm      *blank        #1chg             1

      ******* end add ****** 11/24/11 *********************************
     c                   endsr                                                  PGMSTR
    c*====================================================================
    c* PREDSP - Pre-processing before displaying (every time)
    c*====================================================================
     c     PREDSP        begsr

     c                   time                    dsdttm
     c                   call      'SRPAST8'
     c                   parm      dsdate        datein            8 0
     c                   parm      1             numdays           5 0
     c                   parm                    err               1

     c*                  eval      scdate = datein

     c                   endsr                                                  PREDSP
    c*====================================================================
    c* PRSCTL - Process control after no errors found (Call Submitter)
    c*====================================================================
     c     PRSCTL        begsr

     c     upky          chain     pup01
     c                   move      ##co#         qqco#             3
     c                   move      scprdt        ##dt              8
     c                   move      scpsts        ##type            1
     c                   move      scddtp        ##ddtp            1

     c                   call      'W7935C1'
     c                   parm                    qqCo#
     c                   parm                    ##Dt
     c                   parm                    ##Type
     c                   parm      UPOUTQ        ##Outq           10
     c                   parm                    ##ddtp
      ******* beg add ****** 11/24/11 *********************************
     C                   PARM                    scy4n
      ******* end add ****** 11/24/11 *********************************
      *** beg add ***** 03/29/12 **************************************
     c                   parm                    ##lsdt
     c                   parm      scexfl        ##excl            1
      *** end add ***** 03/29/12 **************************************

     c                   endsr                                                  PRSCTL
