     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE WA081R - Payment request invoice# edit                   *
     H*****************************************************************
     H*  DESCRIPTION: Group EDI Info Work-with & Maintenance.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0080R - System generated check request                    *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 01/05/11  PROGRAMMER: C:257 PROJECT: 114WXM         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* XX/XX/XX   C:XXX   PROJECT: XXXXXX                            *
     H*****************************************************************
     Fwa081f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     Fwarplt    if   e           k disk
     F*
     Fwdpalt    if   e           k disk
     F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D @@co#           s                   like(paco#)
     D @@fnd           s                   like(pafnd)
     D @@fyr           s                   like(pafyr)
     D @@case          s                   like(pacase)
     D @@seq           s                   like(paseq#)
     D @@fein          s                   like(pafein)
     D @@inv#          s                   like(painv#)
     D @@fkey          s              2
     D @@i90           s              1a
     D xxi90           s              1a
    D*---------------------------------------------------------------------
     d* Procedure to check for duplicate payment
    D*---------------------------------------------------------------------
     d prDupPayment    pr             1a
     d @@co#                               like(paco#)
     d @@fnd                               like(pafnd)
     d @@fyr                               like(pafyr)
     d @@case                              like(pacase)
     d @@fein                              like(pafein)
     d @@inv#                              like(painv#)
     D*
    D*================================================================
     C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#                          company
     C                   parm                    @@fnd                          fund
     C                   parm                    @@fyr                          policy year
     C                   parm                    @@case                         case number
     C                   parm                    @@seq                          sequence number
     C                   parm                    @@fein                         payee fedral id
     C                   parm                    @@inv#                         invoice number
     C                   parm                    @@fkey                         function key
     C                   parm                    @@i90                          return ind (90)
    C*================================================================
     C* Main Processing
    C*================================================================
     C*
     C* Check invoice is not blank
     C*
     C                   exsr      srInVoice
     C*
     C* Check for duplicate payment
     C* If key entered is F12 then return to check request.
     C* If key entered is F7 then do not check for duplicate payment.
     C*
     C                   if        @@inv# <> *blanks
     C                   exsr      srDupPay
     C                   endif
     C                   seton                                        lr
     C                   eval      @@i90 = *in90
     C*
    C*================================================================
    C* srInVoice - Validate if invoice is not blank
    C*================================================================
     C     srInVoice     begsr
     C*
     C* If invoice is blank or zero then prompt window to notify.
     C*
     C                   if        @@inv# = *blanks and @@fkey <> '07'
     C                   exfmt     wdw01                                        soft edit
     C*
     C                   select
     C                   when      *inkl = *on                                  cancel (F12)
     C                   eval      @@fkey = '12'
     C                   eval      *in90 = *on
     C* Do not execute srDupPay if Inv is blank and F7 is pressed.
     C                   when      *inkg = *on                                  cancel (F07)
     C                   eval      @@fkey = '07'
     C                   other                                                  enter key
     C                   eval      @@fkey = '12'
     C                   eval      *in90 = *on
     C                   endsl
     C                   endif                                                  @@inv#
     C*
     C*
     C                   endsr
    C*================================================================
    C* srDupPay  - Check for duplicate payment
    C*================================================================
     C     srDupPay      begsr
     C*
     C                   eval      @@i90  = prDupPayment(@@co#:@@fnd:@@fyr
     C                                      :@@case:@@fein:@@inv#)
     C                   if        @@i90 = *on                                  duplicate record
     C                   dow       *inkl = *off
     C                   exfmt     wdw02                                        hard edit
     C                   enddo
     C                   if        *inkl = *on                                  cancel (F12)
     C                   eval      @@fkey = '12'
     C                   eval      *in90 = '1'
     C                   endif
     C                   endif
     C                   endsr
    c*====================================================================
      * prDupPay - procedure to check duplicate payment
     *====================================================================
     pprDupPayment     b
     dprDupPayment     pi             1a
     d@@co#                                like(paco#)
     d@@fnd                                like(pafnd)
     d@@fyr                                like(pafyr)
     d@@case                               like(pacase)
     d@@fein                               like(pafein)
     d@@inv#                               like(painv#)
     C***********************************
     C* klist
     C***********************************
     c     arky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@case
     c                   kfld                    @@fein
     c                   kfld                    @@inv#
     C*
     C     arky          chain(N)  prp01                                        warpp file
     C                   if        not %found                                   record found
     C     arky          chain(N)  ppa01                                        wdapp file
     C                   if        not %found                                   record found
     C                   eval      xxi90 = *off
     C                   else
     C                   eval      xxi90 = *on
     C                   endif
     C                   else
     C                   eval      xxi90 = *on
     C                   endif
     C*
     C                   return    xxi90
     pprDupPayment     e
