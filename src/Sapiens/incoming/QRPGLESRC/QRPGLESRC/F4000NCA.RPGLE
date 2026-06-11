    H**********************************************************************
    H* (C)COPYRIGHT PROGRESSIVE DATA SOLUTIONS INC., 2002                 *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NCA                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR CA                           *
    H**********************************************************************
    H*  CREATED: 09/23/02  C:79   PROJECT: CA001                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
    H*                                                                    *
     H*  12/12/03 C:79  Project: FL048                                     *
    H*                 Populate CAHTOS                                    *
    H*                                                                    *
     H*  10/02/03 C:79  Project: FSMNT                                     *
    H*                 Move clear of PCAWK2 after update PCA01            *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
     Ffdcal13   uf   e           k disk
     Ffaeolg    if   e           k disk
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*
     Ffmprlca   if   e           k disk
    F*
     Ffmrcl     if   e           k disk
    F*
     Ffmsvl1    if   e           k disk
     Ffmrxl     if   e           k disk
     Ffdrul1    if   e           k disk
    F*
     Ffdfrl5    if   e           k disk
     Ffddhl     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffddul     if   e           k disk
     Fftuml     if   e           k disk
     Ffmp1l     if   e           k disk
     Fftfelca   if   e           k disk
     Ffadflca   if   e           k disk
     Ffaeul     if   e           k disk
    F*
    D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
    D*
     D                 ds
     D xxcmsv                        11
     D  xxsvcd                        3  0 overlay(xxcmsv)
     D  xxcpt4                        5    overlay(xxcmsv:4)
     D  xxunit                        3  0 overlay(xxcmsv:9)
    D*
    D*  Date/Time Data Structure.
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D cafolu          s              3  0
     D cainvl          s              1
     D camd50          s              3  2
     D camd51          s              3  2
     D camd54          s              3  2
     D camd55          s              3  2
     D camd56          s              3  2
     D camd62          s              3  2
     D camd66          s              3  2
     D camd80          s              3  2
     D camd83          s              3  2
     D catype          s              1
     D hlddat          s              7  0
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D kkymd           s                   like(frefdt)
     D kkmed#          s                   like(dfmed#)
     D kkdrg#          s                   like(fedrg#)
     D mod1            s              8
     D qqcase          s              7  0
     D qqco#           s              3  0
     D qqdcs#          s              5
     D qqeob#          s              9  0
     D qqfein          s              9  0
     D qqfnd           s              3  0
     D qqfyr           s              3  0
     D qqmod           s              3  0
     D qqspcd          s              3
     D qqst            s              2
     D qqsub#          s              5  0
     D srdate          s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srfutr          s              5  0
     D srrtcd          s              1
     D srtfmt          s              8
     D x               s              3  0
     D xcmra           s              9  2
     D xppmod          s                   like(capmod)
     D xxdcs#          s              5  0
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxinvl          s              1
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
      *** Begin Change *** 05/02/18 *********************
     D***xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 05/02/18 *********************
     D xxrlwt          s                   like(ferlwt)
     D xxcomp          s                   like(dfcomp)
     D xxdrg           s                   like(capaid)
     D xxcost          s                   like(capaid)
     D xxthrs          s                   like(capaid)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     caky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
    C*  Procedure Code file
    C*
     C     prky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    cafdat
    C*
     C     svky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
     C                   kfld                    cafdat
    C*
    C* Reasonable & Customary File
    C*
     C     rcky          klist
     C                   kfld                    qqco#
     C                   kfld                    cazipa
     C                   kfld                    xxcpt4
     C                   kfld                    kkefdt
     C*
     C*  Usual and Customary Modifier Percent File
     C*
     C     umky          klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
    C*
    C*  Link to FDRUP to get correct effective date
    C*
     C     ruky          klist
     C                   kfld                    qqst
     C                   kfld                    catype
     C                   kfld                    cafdat
    C*
     C     ruky1         klist
     C                   kfld                    qqst
     C                   kfld                    catype
    C*
    C*
     C     frky          klist
     C                   kfld                    qqst
     C                   kfld                    catype
     C                   kfld                    kkefdt
    C*
     C     frky1         klist
     C                   kfld                    qqst
     C                   kfld                    catype
    C*
    C* KEY LIST FOR EOMB FILE
    C*
     C     eoky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
     C     dhky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  DENTAL DETAIL BILL FILE (FDDDL)
    C*
     C     ddky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  PHARMACY DETAIL BILL FILE (FDDPL)
     C     dpky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  HOSPITAL DETAIL BILL FILE (FDDUL)
    C*
     C     duky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
    C*  NDC Drug File
    C*
     C     rxky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxndc
    C*
    C*  Composite Factors and CCR's File
    C*
     C     dfky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kkefdt
    C*
     C     dfky1         klist
     C                   kfld                    kkmed#
    C*
    C*  DRG Federal Register File
    C*
     C     feky          klist
     C                   kfld                    kkdrg#
    C*
     C*  Hospital Header File
     C*
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqsub#
     C                   parm                    qqfnd
     C                   parm                    qqfyr
     C                   parm                    qqcase
     C                   parm                    qqfein
     C                   parm                    qqmod
     C                   parm                    qqdcs#
     C                   parm                    qqst
     C                   parm                    qqspcd
     C                   parm                    qqeob#
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C                   move      qqdcs#        xxdcs#
    C*
     C                   open      facap
    C*
     C                   exsr      convrt
    C*
    C*  END PROGRAM AND CLOSE ALL FILES
    C*
     C                   close     facap
    C*
     C     term          tag
    C*
     C                   eval      xcmra = 0
     C                   return
     C                   move      *on           *inlr
    C*
    C********************************************************************
    C*  Convert fields into base fields used for determining payment
    C*  creates workfile with MRA and modifier amounts and prof/tech
    C*****************************************************************
     C     convrt        begsr
    C*
     C                   eval      x = 0
     C                   clear                   date
    C*
    C* Find the recon number.  If this is a recon.
    C*
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      xxrcn = eorcn
    C*
     C                   else
     C                   eval      xxrcn = *zero
    C*
     C                   endif
    C*
    C* Main read of detail charge file
    C*
     C     caky1         setll     pca01
     C     caky1         reade     pca01                                  54
    C*
     C                   dow       *in54 = *off
    C*
     C                   clear                   pdd01
     C                   clear                   pdh01
     C                   clear                   pdd01
     C                   clear                   pdu01
     C                   clear                   mod
     C                   clear                   mod1
    C*
    C*
     C                   if        xxrcn = carcn
    C*
     C                   eval      kkymd = cafdat
     C                   eval      kkdseq = cadseq
    C*
     C                   if        eobtyp = 'N'
     C                             and carcfl = 'Y'
    C*
    C* Chain out to the Physician Bill Detail file
    C*
     C     dhky          chain     pdh01                              60
    C*
     C                   if        *in60 = *off
     C                   move      dhprcd        cacpt4
     C                   move      dhprcd        xxcpt4
     C                   exsr      pmodld
     C                   endif
    C*
     C                   movel     'P'           catype
    C*
     C                   exsr      recu
    C*
     C                   else
    C*
     C                   if        eobtyp = 'N'
     C                             and carcfl = 'C'
    C*
    C* Else - treat line item as a procedure code from the CPT4 file
    C*
    C* Chain out to the Physician Bill Detail file
    C*
     C     dhky          chain     pdh01                              60
    C*
     C                   if        *in60 = *off
     C                   move      dhprcd        cacpt4
     C                   move      dhprcd        xxcpt4
     C                   exsr      pmodld
     C                   endif
    C*
     C                   movel     'P'           catype
    C*
     C                   exsr      proc
     C                   else
    C*
    C* Else treat line item as a dental code
    C* Chain out to the Dental Bill Detail file
    C*
     C                   if        eobtyp = 'D'
     C                             and carcfl = 'C'
     C     ddky          chain     pdd01                              60
    C*
     C                   if        *in60 = *off
     C                   move      ddprcd        cacpt4
     C                   move      ddprcd        xxcpt4
     C                   endif
    C*
     C                   movel     'D'           catype
    C*
     C                   exsr      dent
    C*
     C                   else
    C*
     C                   if        eobtyp = 'D'
     C                             and carcfl = 'Y'
    C*
     C     ddky          chain     pdd01                              60
    C*
     C                   if        *in60 = *off
     C                   move      ddprcd        cacpt4
     C                   move      ddprcd        xxcpt4
     C                   endif
    C*
     C                   movel     'D'           catype
    C*
     C                   exsr      recu
    C*
     C                   else
    C*
    C* Else treat line item as a Pharmacy code
    C* Chain out to the Pharmacy Bill Detail file
    C*
     C                   if        eobtyp = 'X'
     C     dpky          chain     pdp01                              60
    C*
     C                   if        *in60 = *off
     C                   move      dpndc         candc
     C                   endif
    C*
     C                   movel     'X'           catype
    C*
     C                   exsr      phar
    C*
     C                   else
    C*
    C* Else treat line item as a Hospital code
    C* Chain out to the Hospital Bill Detail file
    C*
     C     duky          chain     pdu01                              60
    C*
     C                   if        *in60 = *off
     C                   move      durvcd        casvcd
     C                   move      durate        cacpt4
     C                   move      durate        xxcpt4
     C                   endif
    C*
     C                   movel     'H'           catype
    C*
     C                   exsr      svcd
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      xxmra = camra
    C*
    C* If modifiers are present then adjust CAAMT accordingly.  CAAMT
    C* is the key field used to process FACAP in highest MRA order.
    C*
     C                   if        mod1 <> *blanks
    C*
     C                   exsr      pmod
    C*
     C                   endif
    C*
    C*   Add all workfile fields to workfile
    C*   Load MRA or Charge to CAAMT in highest to lowest MRA order
    C*
     C                   if        xxmra < cachrg
     C                   z-add     xxmra         caamt                92
     C                   else
     C                   eval      caamt = cachrg
     C                   endif
    C*
    C*  Setup correct effective date for rules
    C*
     C     frky          setgt     pfr01
     C     frky1         readpe    pfr01                                  60
    C*
     C                   if        *in60 = *off
    C*
     C                   eval      carudt = frefdt
    C*
     C                   else
     C                   move      *hival        carudt
    C*
     C                   endif
    C*
    C*   Only write this line item out if the date is greater than or
    C*   or equal to the initial eff. date of F4000N
    C*
    C*   Get the initial effective date of F4000N for this state
    C*
     C     ruky          setgt     pru01
     C     ruky1         readpe    pru01                                  56
    C*
     C                   if        *in56 = *off
     C                             and ruapgm = 'F4000N'
     C                   move      ruefdt        xxymd
    C*
     C                   else
     C                   move      *hival        xxymd
    C*
     C                   endif
    C*
     C                   if        kkymd >= xxymd
    C*
     C                   eval      catotc = cachrg
     C                   movel     qqspcd        caspcd
    C*
     C                   move      q1user        causer
     C                   move      q1user        caiusr
     C                   move      q1pgm         capgm
    C*
     C                   time                    dsdttm
    C*
     C                   move      dstime        catime
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cadt
     C                   movel     srdto         caidt
    C*
     C                   write     pcawk2
     C*
     C* Clear FACAP to prevent carryover of values to next record.
     C*
     C*** BEG DEL *** 10/02/03 ****************************************
     C****************** clear                   pcawk2
     C*** END DEL *** 10/02/03 ****************************************
    C*
    C*    Clear CAPAID in detail file to reprocess correctly
    C*    especially in the multiple surgical rule.
    C*
     C                   if        cacscd = '1  '
     C                             or cacscd = '001'
    C*
     C                   clear                   capaid
     C                   eval      camraa = xcmra
    C*
     C                   update    pca01
    C*
     C                   endif
    C*
     C*** BEG ADD *** 10/02/03 ****************************************
     C                   clear                   pcawk2
     C*** END ADD *** 10/02/03 ****************************************
    C*
     C                   else
    C*
    C* Release the record lock
    C*
     C                   except    rca01
    C*
     C                   endif
    C*
     C                   else
    C*
    C* Release the record lock
    C*
     C                   except    rca01
    C*
     C                   endif
    C*
     C     caky1         reade     pca01                                  54
    C*
     C                   enddo
    C*
     C     convre        endsr
    C********************************************************************
    C** DENTAL CODE MRA SEARCH                             D E N T      *
    C********************************************************************
     C     dent          begsr
    C*
    C*  Default to use service date for effective date for rules
    C*
     C                   move      cafdat        kkefdt
    C*
    C*  Process the fee scheduled codes here
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C     prky          setgt     ppr01
     C     nxtpr3        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
    C*
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C* Load amounts
    C*
     C                   eval      camra = prmra
     C                   eval      caprof= prprof
     C                   eval      catech= pr92n1
     C                   eval      caanam= pranam
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
     C                   eval      cafolu = prfolu
    C*
    C* Set flags for anes, assist surg, no cut, etc
    C*
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a5         canct
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
    C*
    C* Load modifier reductions for processing hierchy
    C*
     C                   exsr      modval
    C*
     C                   clear                   caplac
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr3
    C*
     C     dente         endsr
    C********************************************************************
    C** FOLLOW UP DAY LOAD SUBROUTINGE                                  *
    C********************************************************************
     C     folu          begsr
    C*
     C                   movel     'N'           cainvl
     C                   eval      srdti = cafdat
    C*
     C                   call      'SRCVTDAT'    dtmdyy
    C*
     C                   movel     srdto         srdate
    C*
     C                   eval      srfutr = cafolu
    C*
     C                   move      ' '           srrtcd
    C*
     C                   call      'SRFUTR8'
     C                   parm                    srdate
     C                   parm                    srfutr
     C                   parm                    srrtcd
    C*
     C                   eval      srdti = srdate
    C*
     C                   call      'SRCVTDAT'    dtcymd
    C*
     C                   movel     srdto         hlddat
    C*
    C*
    C*
     C     folue         endsr
    C*
    C********************************************************************
    C** MODVAL SUBROUTINE                               M O D V A L     *
    C********************************************************************
    C*
     C     modval        begsr
     C
     C                   if        prcpt4 >= '70000' and prcpt4 <= '79999'
     C                   eval      camd50 = 0.75
     C                   endif
    C*
     C                   eval      camd50 = 0.50
     C                   eval      camd51 = 0.50
     C                   eval      camd62 = 6.25
     C                   eval      camd80 = 0.20
     C                   eval      camd81 = 0.20
     C                   eval      camd82 = 0.20
     C                   eval      camd83 = 0.10
     C*
     C     modvale       endsr
    C*
    C********************************************************************
    C** PHARMACY SUBROUTINE                                  P H A R    *
    C********************************************************************
     C     phar          begsr
    C*
    C*  Default to service date for effective date for rules
    C*
     C                   eval      kkefdt = cafdat
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C                   movel     dpndc         xxndc
    C*
     C     rxky          setgt     prx01
     C     nxtpr4        tag
     C     rxky          readpe    prx01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
    C*
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
     C                   else
    C*
    C* Brand-name Pharmaceuticals
    C*
     C                   if        rxptyp = '01'
     C                             or rxptyp = '07'
    C*
    C* Determine if fill date is less than AWP effective date if so
    C* use previous AWP unit price else use current AWP unit price
     C                   if        cafdat < rxawpd
    C*
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rx1awp/1000
     C                   eval(h)   xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   else
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rxawp/1000
     C                   eval(h)   xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   endif
    C*
     C                   else
    C*
    C* Generic Pharmaceuticals
    C* Determine if fill date is less than AWP effective date if so
    C* use previous AWP unit price else use current AWP unit price
    C*
     C                   if        cafdat < rxawpd
    C*
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rx1awp/1000
     C                   eval(h)   xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   else
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rxawp/1000
     C                   eval(h)   xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      caunts = dpunts
     C                   move      rxstat        castat
     C                   move      'Y'           cafsr
     C                   eval      camra = xxawp * caunts
     C                   eval(h)   caawp = caunts * xxawp
     C                   eval      captyp = rxptyp
    C*
     C                   endif
    C*
     C     rxstat        cabeq     'I'           nxtpr4
    C*
     C     phare         endsr
    C********************************************************************
    C** PROCEDURE CODE MODIFIER CALCULATIONS                  P M O D   *
    C********************************************************************
     C     pmod          begsr
    C*
    C* Adjust MRA depending on MOD
    C*
     C     '50'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd50
    C*
     C                   endif
    C*
     C     '51'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd51
    C*
     C                   endif
    C*
     C     '62'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = (camra * camd62)/10
    C*
     C                   endif
    C*
     C     '80'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd80
    C*
     C                   endif
    C*
     C     '81'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd81
    C*
     C                   endif
    C*
     C     '82'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd82
    C*
     C                   endif
    C*
     C     pmode         endsr
    C*
    C********************************************************************
    C** MODIFIER LOADING                                  P M O D L D   *
    C********************************************************************
     C     pmodld        begsr
    C*
     C                   move      dhmod1        camod1
     C                   move      dhmod2        camod2
     C                   move      dhmod3        camod3
     C                   move      dhmod4        camod4
    C*
     C                   move      dhmod1        mod(1)
     C                   move      dhmod2        mod(2)
     C                   move      dhmod3        mod(3)
     C                   move      dhmod4        mod(4)
    C*
     C                   movea     mod           mod1
    C*
     C                   endsr
    C********************************************************************
    C** PROCEDURE CODE MRA SEARCH                             P R O C   *
    C********************************************************************
     C     proc          begsr
    C*
    C*  Default to service date for effective date for rules
    C*
     C                   eval      kkefdt = cafdat
    C*
    C*  Process the Fee Scheduled codes here
    C*
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
    C*
     C     prky          setgt     ppr01
     C     nxtpr2        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
    C*
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
     C* Load amounts
    C*
     C                   eval      camra = prmra
     C                   eval      caprof= prprof
     C                   eval      catech= pr92n1
     C                   eval      caanam= pranam
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
    C* Load flags for anes, asst surg, no cut, etc.
    C*
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a5         canct
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
     C                   move      pr1a3         cabmri
     C                   move      pr1a4         cabcas
    C*
    C* Load modifier reductions for processing hierchy
    C*
     C                   exsr      modval
    C*
     C                   move      dhpos         caplac
     C                   move      dhprcd        cacpt4
     C                   eval      caunts = dhunts
    C*
     C                   if        pr1a2 = 'Y'
     C                             or dhprcd >= '00100'
     C                             and dhprcd <= '01999'
     C                   eval      caanmn = dhunts
     C                   endif
    C*
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
    C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr2
    C*
     C     proce         endsr
    C********************************************************************
    C** REASONABLE AND CUSTOMARY FILE SEARCH                  R E C U   *
    C********************************************************************
     C     recu          begsr
    C*
    C*  Default to use service date for effective date for rules
    C*
     C                   eval      kkefdt = cafdat
    C*
     C     rcky          setgt     prc01
     C     nxrc1         tag
     C                   readp     prc01                                  60
    C*
     C                   if        *in60 = *on
     C                             or rcco# <> qqco#
     C                             or rczipa <> cazipa
     C                             or rccpt4 <> xxcpt4
    C*
     C                   movel     'Y'           xxinvl
    C*
     C                   else
     C                   eval      cafolu = rcfolu
     C*** BEG ADD *** 04/28/04 *********************************************
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
     C*** END ADD *** 04/28/04 *********************************************
    C*
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   eval      caprof = rcprof
     C                   eval      catech = rc92n1
     C*
     C*  Load the professional and technical component amount based on
     C*  percentages in FTUMP.
     C*
     C     umky          setgt     pum01
     C                   readp     pum01                                  60
     C*
     C                   if        *in60 = *off and cacpt4 <= umtocp
     C*
     C                   eval      xxmd26 = umod26/100
     C                   eval      xxmd27 = umod27/100
     C*
     C                   if        caprof = 0
     C                   eval      caprof = rcmra * xxmd26
     C                   endif                                                  caprof = 0
     C*
     C                   if        catech = 0
     C                   eval      catech = rcmra * xxmd27
     C                   endif                                                  catech = 0
     C*
     C                   endif                                                  end *in60
     C*
     C                   eval      caanam = rcanam
     C                   move      rcsurg        casurg
     C                   move      rcsupp        casupp
     C                   eval      caasc = rc92n1
     C                   move      rcstat        castat
     C                   move      rcrpt         carpt
     C                   move      rcfsr         cafsr
    C*
    C* Load modifier reductions for processing hierchy
    C*
     C                   exsr      modval
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C*
     C                   move      dhpos         caplac
     C                   eval      caunts = dhunts
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
     C*
     C                   when      eobtyp = 'D'
    C*
     C                   move      ddpos         caplac
     C                   eval      caunts = ddunts
     C                   move      ddpart        capart
     C                   move      ddrcfl        carcfl
    C*
     C                   endsl
    C*
     C                   endif
    C*
     C     rcstat        cabeq     'I'           nxrc1
    C*
     C     recue         endsr
    C********************************************************************
    C** SERVICE CODE SEARCH                                   S V C D   *
    C********************************************************************
     C     svcd          begsr
     C*
     C     euky          chain     peu01                              99
    C*
     C                   move      durvcd        xxsvcd
     C                   move      '000'         xxunit
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
     C                   eval      camra = 0
     C*** BEG ADD *** 12/12/03 *********************************************
     C                   eval      cahtos = euicn
     C*** END ADD *** 12/12/03 *********************************************
    C*
    C*  Default to use service date for effective date for rules
    C*
     C                   eval      kkmed# = eumcar
     C                   eval      kkefdt = cafdat
     C                   eval      kkdrg# = durvcd
     C                   select
     C*
     C                   when      euicn >= 'IM' and euicn <= 'IS'
    C*
     C*  Inpatient using DRG rates
    C*
     C     feky          chain     pfe01                              99
     C*
     C                   if        *in99 = *off
     C                   eval      xxrlwt = ferlwt
     C                   else
     C                   eval      xxrlwt = 0
     C                   endif
     C*
     C     dfky          setgt     pdf01
     C     dfky1         readpe    pdf01                                  99
     C*
     C                   if        *in99 = *off
     C                   eval      xxcomp = dfcomp
     C                   else
     C                   eval      xxcomp = 0
     C                   endif
     C*
     C                   eval      xxdrg = 1.20 * (xxcomp*xxrlwt)
    C*
     C*  Determine Cost Outlier
    C*
     C                   eval      xxcost = cachrg * dftccr
     C                   eval      xxthrs = xxdrg + dfoutl
    C*
     C                   if        xxcost > xxthrs
     C                   eval      camra = xxdrg + (.8 * (xxcost - xxthrs))
     C                   else
     C                   eval      camra = xxdrg
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
     C                   endif                                                  xxcost > xxthrs
     C*
     C                   when      euicn >= 'AO' and euicn <= 'HS'
    C*
     C*  Outpatient pays per the physician fee schedule
     C*
     C     svky          setgt     psv01
     C     nxtsv1        tag
     C                   readp     psv01                                  60
    C*
     C                   if        *in60 = *on
     C                   move      'Y'           cainvl
    C*
     C                   else
     C                   eval      camra = svalow
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
    C*
     C                   endif
    C*
     C     svstat        cabeq     'I'           nxtsv1
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
     C                   endsl
    C*
     C     svcde         endsr
    C* ==================================================================
     Opca01     e            rca01
