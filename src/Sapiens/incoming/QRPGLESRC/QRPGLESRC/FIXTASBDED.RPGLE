     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/title Fix program for Back To Base - Deductible Changes
     H*********************************************************************
     H* Created: 04/21/10    C:33    Project: 25WRB                       *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
     Fwdeal4    uf   e           k disk    rename(pea01:dea04)
     Fwtabl     if   e           k disk    include(ptt01)
     Fwmeal27   if   e           k disk    rename(pea01:mea27) prefix(m27)
     Fwdeal27   uf   e           k disk    rename(pea01:dea27)
     Fwmxal27   uf   e           k disk    rename(pxa01:mxa27)
     Fwdddl     if   e           k disk    rename(pdd01:pdd00)
     Fwdeap     o    e             disk    rename(pea01:dea0p)
     Fwmeap     o    e             disk    rename(pea01:mea0p) usropn
     Fqsysprt   o    f  132        printer oflind(*inof) usropn

     Dbat#             s                   like(EABAT#) inz(*hival)
     Dbatch#           s                   like(EABAT#)
     Dco#              s                   like(TTCO#) inz(25)
     DkkKey            s                   like(TTKEY)
     DkkCode           s                   like(EAARCD)
     Dlib              s             10
     Dx                s              3  0
     DsysDt7           s                   like(TTDT)
     DprtFnd           s                   like(EAFND)
     DprtEmp#          s                   like(EAEMP#)
     DprtDiv           s                   like(EADIV)
     DprtBilled        s                   like(EABATA)
     DprtARBill        s                   like(EABATA)
     DprtARPaid        s                   like(EABATA)
     DprtBillDif       s                   like(EABATA)
     DprtPaidDif       s                   like(EABATA)
     DttlBillDif       s                   like(EABATA) inz
     DttlPaidDif       s                   like(EABATA) inz

     D psds          esds                  extname(szq1p)

     D#ofDisBill       c                   4
     D disBill         s              2    dim(#ofDisBill) ctdata perrcd(1)
     D#ofDisPay        c                   3
     D disPay          s              2    dim(#ofDisPay) ctdata perrcd(1)

     DCrtQTempCmd      pr            80
     D file                          10    const

     DDisolveDed       pr

     DWorkWBat         pr
     D inBat#                              like(EABAT#) const
     D inType                         1                 const

     DOvrCmd           pr            80
     D file                          10    const
     D library                       10    const

     DDltOvrCmd        pr            80
     D file                          10    const

     D/copy sPrDate
     D/copy sPrRunCmd

     C     key           klist
     C                   kfld                    co#
     C                   kfld                    kkKey

     C     keyAR         klist
     C                   kfld                    co#
     C                   kfld                    batch#
     C     keyNextAR     klist
     C                   kfld                    co#
     C                   kfld                    bat#

     C     *entry        plist
     C                   parm                    batch#

     C                   eval      sysDt7 = DateTo7(SystemDate)
     C*
     C* Need to disolve deducible codes.
     C                   callp     DisolveDed
     C*
     C                   if        %open(qsysprt)
     C                   except    total
     C                   endif
     C*
     C                   eval      *inlr = *on
     C*=========
     C     OpenWMEAP     begsr
     C* Never used
     C                   if        %open(WMEAP)
     C                   open      WMEAP
     C                   endif
     C                   endsr
     OQsysprt   e            header         1 02
     O                                         +002 'Grp'
     O                                         +002 ' Employer'
     O                                         +002 '  Div'
     O                                         +002 '        Billed '
     O                                         +002 '    A/R Billed '
     O                                         +002 '      A/R Paid '
     O                                         +002 '   Billed Diff '
     O                                         +002 '     Paid Diff '
     O          e            header         1
     O                                         +002 '==='
     O                                         +002 '========='
     O                                         +002 '====='
     O                                         +002 '============== '
     O                                         +002 '============== '
     O                                         +002 '============== '
     O                                         +002 '============== '
     O                                         +002 '============== '
     O          e            detail         1
     O                       prtFnd        z   +002
     O                       prtEmp#       z   +002
     O                       prtDiv        z   +002
     O                       prtBilled     j   +002
     O                       prtARBill     j   +002
     O                       prtARPaid     j   +002
     O                       prtBillDif    j   +002
     O                       prtPaidDif    j   +002
     O          e            total       1  1
     O                                         +002 '   '
     O                                         +002 '         '
     O                                         +002 '     '
     O                                         +002 '               '
     O                                         +002 '               '
     O                                         +002 '               '
     O                       ttlBillDif    j   +002
     O                       ttlPaidDif    j   +002





     P*====================================================================
     P* Disolve deducible codes
     P*====================================================================
     PDisolveDed       b
     DDisolveDed       pi

     Dfnd              s                   like(EAFND)
     Demp#             s                   like(EAEMP#)
     Ddiv              s                   like(EADIV)
     Dfyr              s                   like(EAFYR)
     DsaveEAFND        s                   like(EAFND)
     DsaveEAEMP#       s                   like(EAEMP#)
     DsaveEADIV        s                   like(EADIV)
     DsaveEAFYR        s                   like(EAFYR)
     DsaveEAAMT        s                   like(EAAMT)
     DholdEAFND        s                   like(EAFND)
     DholdEAEMP#       s                   like(EAEMP#)
     DholdEADIV        s                   like(EADIV)
     DholdEAAMT        s                   like(EAAMT)
     DtotalBilled      s             +2    like(EAAMT)  inz
     DholdBilled       s                   like(totalBilled)
     DholdPaid         s                   like(totalBilled)
     DyrBilled         s             +2    like(EAAMT)  inz
     DyrPaid           s             +2    like(EAAMT)  inz
     DpaymentCode      s                   like(EAARCD) inz('PD')
     DpaymentType      s                   like(EAARTY)
     DpaymentCTyp      s                   like(EACTYP)
     DpaymentDesc      s                   like(EADESC)
     DoffsetCode       s                   like(EAARCD)
     DoffsetType       s                   like(EAARTY)
     DoffsetCTyp       s                   like(EACTYP)
     DoffsetDesc       s                   like(EADESC)
     DoffsetAmt        s                   like(totalBilled)
     DpaymentBat#      s                   like(EABAT#)
     DoffsetPBat#      s                   like(EABAT#)
     DoffsetBBat#      s                   like(EABAT#)
     DdtlAR            s                   like(totalBilled)
     DbillAR           s                   like(totalBilled)
     DpaidAR           s                   like(totalBilled)
     Dcode             s                   like(EAARCD)
     DmaxAmt           s              7  0 inz(*hival)

     C     keyWDDDL      klist
     C                   kfld                    co#
     C                   kfld                    fnd

     C     keyWMEAL27    klist
     C                   kfld                    co#
     C                   kfld                    code
     C                   kfld                    fnd
     C                   kfld                    emp#
     C                   kfld                    div
     C     key2WMEAL27   klist
     C                   kfld                    co#
     C                   kfld                    code
     C                   kfld                    fnd
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    fyr

     C                   eval      kkKey = paymentCode
     C                   clear                   TTTYPE
     C                   clear                   TTCTYP
     C                   clear                   TTDESC
     C     key           chain     PTT01
     C                   eval      paymentType = TTTYPE
     C                   eval      paymentCTyp = TTCTYP
     C                   eval      paymentDesc = TTDESC

     C* Get batch #'s for deductible payments and offsetting A/R batches
     C*  Write dummy records
     C                   clear                   EABAT#
     C                   eval      bat# = batch#
     C     keyNextAR     setgt     dea04
     C     co#           readpe(n) dea04
     C                   eval      paymentBat# = EABAT# + 1
     C                   eval      offsetBBat# = EABAT# + 2
     C                   eval      offsetPBat# = EABAT# + 3
     C                   callp     WorkWBat(paymentBat#:'W')
     C                   callp     WorkWBat(offsetBBat#:'W')
     C                   callp     WorkWBat(offsetPBat#:'W')

     C* Update transaction date for current deductible payments
     C*  This is done to keep the JDE process from picking up the records
     C     keyAR         setll     dea04
     C                   dou       %eof
     C     keyAR         reade     dea04
     C                   if        not %eof
     C                   eval      EATRDT = 1100430
     C                   update    dea04
     C                   endif
     C                   enddo

     C* Process the deductible records for payments and offsets
     C     keyAR         setll     dea04
     C                   dou       %eof(WDEAL4)
     C     keyAR         reade(n)  dea04
     C                   eval      holdEAFND  = EAFND
     C                   eval      holdEAEMP# = EAEMP#
     C                   eval      holdEADIV  = EADIV
     C                   eval      holdEAAMT  = EAAMT
     C                   if        %eof(WDEAL4)
     C                                or EAFND  <> saveEAFND
     C                                or EAEMP# <> saveEAEMP#
     C                                or EADIV  <> saveEADIV
     C                   if        totalBilled <> *zero
     C                   exsr      DisolveBill
     C                   exsr      DisolvePmnt
     C                   if        totalBilled <> billAR
     C                              or totalBilled <> paidAR
     C                   exsr      printErr
     C                   endif
     C                   clear                   totalBilled
     C                   endif
     C                   endif
     C                   eval      saveEAFND  = holdEAFND
     C                   eval      saveEAEMP# = holdEAEMP#
     C                   eval      saveEADIV  = holdEADIV
     C                   eval      saveEAAMT  = holdEAAMT
     C                   if        not %eof(WDEAL4)
     C                   exsr      WritePayment
     C                   endif
     C                   eval      totalBilled = totalBilled + holdEAAMT
     C                   enddo

     C* Update the batch amounts for the new batches created
     C                   callp     WorkWBat(paymentBat#:'U')
     C                   callp     WorkWBat(offsetBBat#:'U')
     C                   callp     WorkWBat(offsetPBat#:'U')

     C*=========
     C     DisolveBill   begsr
     C*
     C                   clear                   dtlAR
     C                   eval      holdBilled = totalBilled
     C                   eval      fnd  = saveEAFND
     C                   eval      emp# = saveEAEMP#
     C                   eval      div  = saveEADIV
     C                   eval      EABAT# = offsetBBat#
     C     1             do        #ofDisBill    x
     C                   clear                   yrBilled
     C                   clear                   m27EAFYR
     C                   clear                   m27EAAMT
     C                   clear                   saveEAFYR
     C                   eval      code = disBill(x)
     C                   exsr      GetOffset
     C     keyWMEAL27    setll     mea27
     C                   dou       %eof or holdBilled <= 0
     C     keyWMEAL27    reade     mea27
     C                   if        %eof or m27EAFYR <> saveEAFYR
     C                   if        yrBilled <> *zero
     C                   if        yrBilled < holdBilled
     C                   eval      offsetAmt = yrBilled
     C                   else
     C                   eval      offsetAmt = holdBilled
     C                   endif
     C                   exsr      WriteOffset
     C                   eval      holdBilled = holdBilled - yrBilled
     C                   clear                   yrBilled
     C                   endif
     C                   eval      saveEAFYR = m27EAFYR
     C                   endif
     C                   eval      yrBilled = yrBilled + m27EAAMT
     C                   enddo
     C                   if        holdBilled <= 0
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   eval      billAR = -dtlAR
     C*
     C                   endsr
     C*=========
     C     DisolvePmnt   begsr
     C*
     C                   clear                   dtlAR
     C                   eval      holdPaid = totalBilled
     C                   eval      fnd  = saveEAFND
     C                   eval      emp# = saveEAEMP#
     C                   eval      div  = saveEADIV
     C                   eval      EABAT# = offsetPBat#
     C     1             do        #ofDisPay     x
     C                   clear                   yrPaid
     C                   clear                   m27EAFYR
     C                   clear                   m27EAAMT
     C                   clear                   saveEAFYR
     C                   eval      code = disPay(x)
     C                   exsr      GetOffset
     C     keyWMEAL27    setll     mea27
     C                   dou       %eof or holdPaid <= 0
     C     keyWMEAL27    reade     mea27
     C                   if        %eof or m27EAFYR <> saveEAFYR
     C                   if        yrPaid <> *zero
     C                   if        yrPaid < holdPaid
     C                   eval      offsetAmt = yrPaid
     C                   else
     C                   eval      offsetAmt = holdPaid
     C                   endif
     C                   exsr      WriteOffset
     C                   eval      holdPaid = holdPaid - yrPaid
     C                   clear                   yrPaid
     C                   endif
     C                   eval      saveEAFYR = m27EAFYR
     C                   endif
     C                   eval      yrPaid = yrPaid + m27EAAMT
     C                   enddo
     C                   if        holdPaid <= 0
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   eval      paidAR = -dtlAR
     C*
     C                   endsr
     C*=========
     C     WriteOffset   begsr
     C*
     C                   dou       offsetAmt <= *zero
     C                   select
     C                   when      offsetAmt > maxAmt
     C                   eval      EAAMT = -maxAmt
     C                   other
     C                   eval      EAAMT = -offsetAmt
     C                   endsl
     C                   exsr      LoadOffset
     C                   exsr      writeWDEAP
     C                   eval      dtlAR = dtlAR + EAAMT
     C                   eval      offsetAmt = offsetAmt - maxAmt
     C                   enddo
     C*
     C                   endsr
     C*=========
     C     WritePayment  begsr
     C*
     C                   eval      EAFND  = saveEAFND
     C                   eval      EAEMP# = saveEAEMP#
     C                   eval      EADIV  = saveEADIV
     C                   eval      EAAMT  = saveEAAMT
     C*
     C                   eval      EABAT# = paymentBat#
     C*
     C                   eval      EAARCD = paymentCode
     C* * * *            eval      EAARTY = paymentType
     C                   eval      EACTYP = paymentCTyp
     C                   eval      EADESC = paymentDesc
     C*
     C                   exsr      WriteWDEAP
     C*
     C                   endsr
     C*=========
     C     WriteWDEAP    begsr
     C*
     C                   eval      EATFAP = 'N'
     C                   eval      EATFFL = 'N'
     C                   clear                   EATFDT
     C                   clear                   EALATE
     C                   clear                   EACASH
     C                   clear                   EAAUTR
     C                   clear                   EAONLI
     C                   clear                   EABATA
     C*
     C                   eval      EACKAM = EAAMT
     C*
     C                   eval      EAPGM  = q1Pgm
     C                   eval      EAUSER = q1User
     C                   eval      EADT   = sysDt7
     C                   eval      EATIME = TimeTo6(SystemTime)
     C                   eval      EAIPGM = EAIPGM
     C                   eval      EAIUSR = EAUSER
     C                   eval      EAIDT  = EADT
     C*
     C                   write     dea0p
     C*
     C                   endsr
     C*=========
     C     PrintErr      begsr
     C*
     C                   if        not %open(qsysprt)
     C                   open      qsysprt
     C                   eval      *inof = *on
     C                   endif
     C*
     C                   if        *inof
     C                   except    header
     C                   eval      *inof = *off
     C                   endif
     C*
     C                   eval      prtFnd     = saveEAFND
     C                   eval      prtEmp#    = saveEAEMP#
     C                   eval      prtDiv     = saveEADIV
     C                   eval      prtBilled  = totalBilled
     C                   eval      prtARBill  = billAR
     C                   eval      prtARPaid  = paidAR
     C                   eval      prtBillDif = totalBilled - billAR
     C                   eval      prtPaidDif = totalBilled - paidAR
     C*
     C                   except    detail
     C*
     C                   eval      ttlBillDif = ttlBillDif + prtBillDif
     C                   eval      ttlPaidDif = ttlPaidDif + prtPaidDif
     C*
     C                   endsr
     C*=========
     C     GetOffset     begsr
     C*
     C                   eval      kkKey = code
     C                   clear                   TTKEY
     C                   clear                   TTTYPE
     C                   clear                   TTCTYP
     C                   clear                   TTDESC
     C     key           chain     PTT01
     C                   eval      offsetCode = TTKEY
     C                   eval      offsetType = TTTYPE
     C                   eval      offsetCTyp = TTCTYP
     C                   eval      offsetDesc = TTDESC
     C*
     C                   endsr
     C*=========
     C     LoadOffset    begsr
     C*
     C                   eval      EAFND  = saveEAFND
     C                   eval      EAEMP# = saveEAEMP#
     C                   eval      EADIV  = saveEADIV
     C                   eval      EAFYR  = saveEAFYR
     C*
     C                   eval      EAARCD = offsetCode
     C* * * *            eval      EAARTY = offsetType
     C                   eval      EACTYP = offsetCTyp
     C                   eval      EADESC = 'Offset-' + offsetDesc
     C*
     C                   endsr

     PDisolveDed       e

     P*====================================================================
     P* Work with dummy records
     P*====================================================================
     PWorkWBat         b
     DWorkWBat         pi
     D inBat#                              like(EABAT#) const
     D inType                         1                 const

     DbatchTotal       s                   like(EABATA)
     DdummyFnd         s                   like(EAFND)  inz(*hival)
     DdummyEmp#        s                   like(EAEMP#) inz(*hival)
     DdummyDiv         s                   like(EADIV)  inz(*hival)
     DdummyFyr         s                   like(EAFYR)  inz(*hival)

     C     dummy         klist
     C                   kfld                    co#
     C                   kfld                    inBat#
     C                   kfld                    dummyFnd
     C                   kfld                    dummyEmp#
     C                   kfld                    dummyDiv
     C                   kfld                    dummyFyr

     C                   eval      bat# = inBat#

     C                   select
     C                   when      inType = 'W'
     C                   exsr      WriteDummy
     C                   when      inType = 'U'
     C                   exsr      UpdateBatch
     C                   when      inType = 'D'
     C                   exsr      DeleteDummy
     C                   endsl

     C*=========
     C     WriteDummy    begsr
     C                   clear                   dea0p
     C                   eval      EACO#  = co#
     C                   eval      EABAT# = inBat#
     C                   eval      EAFND  = dummyFnd
     C                   eval      EAEMP# = dummyEmp#
     C                   eval      EADIV  = dummyDiv
     C                   eval      EAFYR  = dummyFyr
     C                   eval      EAINV# = *hival
     C                   eval      EAACHF = 'D'
     C                   write     dea0p
     C                   endsr
     C*=========
     C     DeleteDummy   begsr
     C     dummy         delete    dea04                              99
     C                   endsr
     C*=========
     C     UpdateBatch   begsr
     C*
     C* Delete the dummy record
     C                   callp     WorkWBat(inBat#:'D')
     C*
     C                   clear                   batchTotal
     C*
     C     keyNextAR     setll     dea04
     C                   dou       %eof
     C     keyNextAR     reade(n)  dea04
     C                   if        not %eof
     C                   eval      batchTotal = batchTotal + EAAMT
     C                   endif
     C                   enddo
     C*
     C     keyNextAR     setll     dea04
     C                   dou       %eof
     C     keyNextAR     reade     dea04
     C                   if        not %eof
     C                   eval      EABATA = batchTotal
     C                   update    dea04
     C                   endif
     C                   enddo
     C*
     C                   endsr

     PWorkWBat         e

     P*====================================================================
     P* Create overrride command
     P*====================================================================
     POvrCmd           b
     DOvrCmd           pi            80
     D file                          10    const
     D library                       10    const

     D cmd             s             80

     C                   eval      cmd = 'OVRDBF FILE(' + %trim(file) +
     C                                               ')' +
     C                                       ' TOFILE(' + %trim(library) +
     C                                             '/' +  %trim(file) +
     C                                               ')'
     C                   return    cmd

     POvrCmd           e

     P*====================================================================
     P* Create delete overrride command
     P*====================================================================
     PDltOvrCmd        b
     DDltOvrCmd        pi            80
     D file                          10    const

     C                   return    'DLTOVR FILE(' + %trim(file) + ')'

     PDltOvrCmd        e

     P*====================================================================
     P* Create QTEMP duplication command
     P*====================================================================
     PCrtQTempCmd      b
     DCrtQTempCmd      pi            80
     D file                          10    const

     C                   return    'CRTDUPOBJ OBJ(' + %trim(file) + ')' +
     C                             ' FROMLIB(PDSFIL) OBJTYPE(*FILE)' +
     C                             ' TOLIB(QTEMP) DATA(*YES)'

     PCrtQTempCmd      e

** Disolve Deductible Billings
CL                             CL
CF                             CA
AR                             CF
                               WC
** Disolve Deductible Payments
CH                             CH
PT                             PT
                               TC
