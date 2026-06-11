    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2007
     H/TITLE F8291R
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H**************************************************************************
    H*  F8271R     : MCMC Health State Reporting - File Download
    H*
    H*  Description: Populates the MCMC paid Date File (FAQ1P)
    H*
    H*  Called from: F8291C - MCMC State Reporting - Override program
    H*  Calls to   : None
    H*
     H**************************************************************************
     H*  Created on 03/12/2007 by C:062 for Project 139MHG
     H**************************************************************************
    H*  Revisions:
     H*
     H**************************************************************************
     Ffaeal17   uf   e           k disk
     Ffay1p     o    e             disk
     Ffaeol4    if   e           k disk
    F*
    D*
     D getPaymentDate  PR             8a
    D*
     D/COPY SCOPYWW01I
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D detail          ds            33
     D  sourceInvoice                14    overlay(detail)
     D  paymentDate                   8    overlay(detail:15)
     D  paymentAmount                11    overlay(detail:23)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@date          s              8
     D @@dcid          s             12
     D count           s              9  0 inz(0)
     D kkco#           s                   like(eoco#)
     D kksub#          s                   like(eosub#)
     D kkfnd           s                   like(eofnd)
     D kkfyr           s                   like(eofyr)
     D kkcase          s                   like(eocase)
     D kkeob#          s                   like(eoeob#)
     D date            s              6
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D #1trdt          s              8
     D #1cmdt          s              8
     D ##wcf#          s              3
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@date
     C                   parm                    @@dcid
     C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   PARM      '*YYMD'       srffmt
     C                   PARM      '*MDYY'       srtfmt
     C                   PARM                    srdto
     C                   PARM                    srerr
    C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyym2        plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*=====================================================================
    C* Execute main subroutine
    C*=====================================================================
    C*
     C                   exsr      mainsr
    C*
    C*================================================================
    C* MAINSR - MAIN SUBROUTINE
    C*================================================================
     C     mainsr        begsr
     C*
     C                   exsr      pgmstr
    C*
    C* Let's start reading thru these checks
    C*
     C                   eval      count = *zeros
     C                   reset                   detail
    C*
     C                   read      pea01                                  99
     C                   dow       *in99 = *off
    C*
     C                   eval      kkco#  = eaco#
     C                   eval      kksub# = easub#
     C                   eval      kkfnd  = eafnd
     C                   eval      kkfyr  = eafyr
     C                   eval      kkcase = eacase
     C                   eval      kkeob# = eaeob#
     C     eoky          chain     peo01
     C                   if        %found(faeol4)
     C                   if        eontwk = 'MCMC' and eawcpd <> 0
     C*
     C                   exsr      writeFAY1P
     C*
     C                   eval      eaedi  = 'S'
     C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C                   movel     q1pgm         eapgm
     C*
     C                   update    pea01
     C*
     C                   endif
     C                   endif
    C*
     C                   read      pea01                                  99
     C                   enddo
    C*
     C                   exsr      pgmend
    C*
     C                   endsr
    C*================================================================
    C* pgmend - End of Program Processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   clear                   @@dcid
     C                   if        count > *zeros
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         date
     C     'PD'          cat(p)    date:0        @@dcid
     C                   cat       '.DAT':0      @@dcid
     C                   endif
    C*
     C                   seton                                        lr
    C*
     C                   endsr
    C*================================================================
    C* writeFAY1P - Write the Data Record
    C*================================================================
     C     writeFAY1P    begsr
    C*
     C                   clear                   py101
    C*
     C                   eval      sourceInvoice = easinv
     C                   eval      paymentDate = getPaymentDate
     C                   eval      paymentAmount=%editc(eapaid:'4')
    C*
     C                   eval      y1data = detail
    C*
     C                   write     py101
     C                   eval      count = count + 1
    C*
     C                   endsr
     C****************************************************************
     C* pgmstr - Retrieve install flags, etc.
     C****************************************************************
     C     pgmstr        begsr
     C*
     C*  Check to see if WCMS is installed
     C*
     C                   call      'SA001R'
     C                   parm                    @@wcif            1
     C                   parm                    @@mcif            1
     C                   parm                    @@wcin            1
     C*
     C                   endsr
     C****************************************************************
     C* getPaymentDate - Get the Payment Date                        *
     C****************************************************************
     P getPaymentDate  B
     D getPaymentDate  PI             8a

     D payDate         s              8a
     C*
     C* Use the check mailing date if not blank
     C*
     C     @@wcif        IFEQ      'Y'
     C*
     C* Determine if standalone account
     C*
     C                   move      eaco#         #1co#             3
     C                   movel     easub#        ##sub#            5
     C                   movel     eafnd         #1fnd             3
     C*
     C                   call      'FA701R'
     C                   parm      ' '           ##rtcd            1
     C                   parm                    #1co#
     C                   parm                    ##sub#
     C                   parm                    #1fnd
     C                   parm      ' '           ##outs            1
     C*
     C                   if        ##outs = 'N'
     C*
     C* get WCMS group number
     C*
     C                   call      'FA700R'
     C                   parm      ' '           ##rtcd
     C                   parm                    #1co#
     C                   parm      *blanks       ##wcf#
     C                   parm                    ##sub#
     C                   parm                    #1fnd

     C                   z-add     eawcpd        srdti
     C                   call      'SRCVTDAT'    dtmdy2
     C                   movel     srdto         #1trdt

     C                   call      'WA050R'
     C                   parm                    #1co#
     C                   parm      ##wcf#        #1fnd
     C                   parm                    #1trdt
     C                   parm      *blanks       #1cmdt
     C*
     C                   if        #1cmdt <> *blanks
     C                   move(p)   #1cmdt        srdti
     C                   call      'SRCVTDAT'    dtyym2
     C                   movel     srdto         payDate
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   if        payDate = *blanks or payDate = '00000000'
     C                   if        eawcpd <> 0
     C                   move      eawcpd        payDate
     C                   else
     C                   z-add     eotrdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         payDate
     C                   endif
     C                   endif
     C*
     C                   return    payDate
     C*
     P getPaymentDate  E
