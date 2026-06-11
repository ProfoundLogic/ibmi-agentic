    H**********************************************************************
     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NMA                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR MA                           *
    H**********************************************************************
    H*  CREATED: 07/23/04  C:79   PROJECT: MA012                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  04/07/14 C:212 Support: MC129 84904                               *
    H*                 Fix payment for invalid code                       *
    H*                                                                    *
     H*  01/21/10 C:227 Support: MC129 71305                               *
    H*                 Restorative Services data file missing for         *
     H*                 Chiropractic Services billing.                     *
     H*                                                                    *
     H*  05/13/09 C:079 Project: MA018                                     *
    H*                 Condition old ASC logic                            *
    H*                                                                    *
     H*  04/30/09 C:227 Project: MA018                                     *
    H*                 MA 2009 Physicians FS Update                       *
     H*                                                                    *
     H*  05/09/07 C:079 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
     H*                                                                    *
     H*  02/14/06 C:79 Support: MC129 51271                                *
     H*                Add G0283 To Special Rates Programming              *
     H*                                                                    *
     H*  05/16/05 C:79 Support: MC129 45519                                *
     H*                Allow Chiro to bill for PT supportive services      *
     H*                                                                    *
     H*  05/15/05 C:79 Support: MC129 45824                                *
     H*                Allow Chiro to bill for PT                          *
     H*                                                                    *
     H*  01/11/05 C:79 Support: MC129 44887                                *
     H*                Load CAAMT for 97110-97150 using prmra * units      *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
     Ffdcal13   uf   e           k disk
     Ffaeolg    if   e           k disk
     Ffacap     uf a e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*
     Ffmprl     if   e           k disk
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
     Fftccl1    if   e           k disk
     Ffmp2lma   if   e           k disk
     Ffmp2l1ma  if   e           k disk
     F                                     rename(pp201:pp202)
     Ffmp1l     if   e           k disk
     Fftaslma   if   e           k disk
     Ffaeul     if   e           k disk
    D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D*** BEG ADD *** 01/11/04 *********************************************
     D ptprcd          s              5    dim(100) inz
     D ptamt           s              9  2 dim(100) inz
     D*** END ADD *** 01/11/04 *********************************************
     D*** END ADD *** 01/11/04 *********************************************
    D*
    D*
     D                 ds
     D xxcmsv                        11
     D  xxsvcd                        3  0 overlay(xxcmsv)
     D  xxcpt4                        5    overlay(xxcmsv:4)
     D  xxunit                        3  0 overlay(xxcmsv:9)
    D*  Date/Time Data Structure.
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
     D camd81          s              3  2
     D camd82          s              3  2
     D catype          s              1
     D hlddat          s              7  0
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D kkymd           s                   like(frefdt)
     D kkgrp           s                   like(asgrp)
     D kkarea          s                   like(asarea)
     D kkkey           s                   like(cckey)
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
      *** Begin Change *** 05/02/18 *********************
     D***xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 05/02/18 *********************
     D xxdcs#          s              5  0
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D*** BEG ADD *** 01/11/04 *********************************************
     D n               s              3  0
     D m               s              3  0
     D*** END ADD *** 01/11/04 *********************************************
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
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
    C*
    C*  Procedure Code file
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
     C     svky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
    C*
     C     p1ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
    C*
     C     p2ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    xxcmsv
     C                   kfld                    cafdat
    C*
     C     p2ky1         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
    C*
    C* Reasonable & Customary File
     C     rcky          klist
     C                   kfld                    qqco#
     C                   kfld                    cazipa
     C                   kfld                    xxcpt4
     C                   kfld                    kkymd
    C*
    C*  Usual and Customary Modifier Percent File
     C     umkey         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
    C*
    C*  Provider Master File
    C*
    C*  Link to FDRUP to get correct effective date
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
     C     rxky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxndc
    C*
    C*  COUNTY CODE FILE
    C*
     C     ccky          klist
     C                   kfld                    qqco#
     C                   kfld                    kkkey
    C*
    C*  ASC ALLOWANCE FILE
    C*
     C     asky          klist
     C                   kfld                    kkgrp
     C                   kfld                    kkarea
     C                   kfld                    kkefdt
    C*
     C     asky1         klist
     C                   kfld                    kkgrp
     C                   kfld                    kkarea
    C*
    C*  HOSPITAL HEADER BILL FILE (FAEUP)
    C*
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
     C                   close     facap
     C*** BEG ADD *** 01/11/04 *********************************************
     C                   if        ptprcd(1) <> *blanks
    C*
     C                   exsr      ptsort
    C*
     C                   endif
     C*** END ADD *** 01/11/04 *********************************************
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
     C*** BEG ADD *** 01/11/04 *********************************************
     C                   eval      n = 1
     C                   move      *blanks       ptprcd
     C                   move      *zeros        ptamt
     C*** END ADD *** 01/11/04 *********************************************
     C                   eval      x = 0
     C                   clear                   date
    C*
    C* Find the recon number.  If this is a recon.
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
    C*** END ADD *** 08/22/02 *********************************************
    C*
    C* Else treat line item as a Pharmacy code
    C* Chain out to the Pharmacy Bill Detail file
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
     C     euky          chain     peu01                              99
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
    C*
     C                   if        mod1 <> *blanks
    C*
     C                   exsr      pmod
    C*
     C                   endif
    C*
    C*   Add all workfile fields to workfile
    C*   Load MRA or Charge to CAAMT in highest to lowest MRA order
     C                   if        xxmra < cachrg
     C                   z-add     xxmra         caamt                92
     C                   else
     C                   eval      caamt = cachrg
     C                   endif
     C*** BEG ADD *** 01/11/04 *********************************************
     C                   if        dhprcd > '97110' and dhprcd <= '97150'
    C*
     C                   eval      m = 1
     C     dhprcd        lookup    ptprcd(m)                              99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      ptamt(m)  = ptamt(m) + camra
    C*
     C                   else
     C                   eval      ptprcd(n) = dhprcd
     C                   eval      ptamt(n)  = camra
     C                   eval      n = n + 1
    C*
     c                   endif
    C*
     c                   endif
     C*** END ADD *** 01/11/04 *********************************************
    C*
    C* For ASC surgeries without an allowance set caamt = charged
    C*
     C                   if        catype = 'H' and eublty >= '131'
     C                             and eublty <= '135' and
     C                             durate >= '10000' and durate <= '69999'
     C                             and caamt = 0
     C                   eval      caamt = cachrg
     C                   endif
    C*
    C*  Setup correct effective date for rules
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
    C*  If R&C or CPT4 it is Physician type. Else it is Hospital type
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
    C*
    C* Clear FACAP to prevent carryover of values to next record.
    C*
    C*
    C*    Clear CAPAID in detail file to reprocess correctly
    C*    especially in the multiple surgical rule.
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
     C                   clear                   pcawk2
    C*
     C                   else
    C*
    C* Release the record lock
     C                   except    rca01
    C*
     C                   endif
    C*
     C                   else
    C*
    C* Release the record lock
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
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C     prky          setgt     ppr01
     C     nxtpr3        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
    C*  Default to use service date for effective date for rules
     C                   move      cafdat        kkefdt
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
    C*  Setup correct effective date for rules
     C                   eval      kkefdt = prefdt
    C*
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n2
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a3         canct
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
     C                   eval      camd50 = *zero
     C                   eval      camd51 = *zero
     C                   eval      camd80 = *zero
     C                   eval      camd81 = *zero
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
     C     folue         endsr
    C*
    C********************************************************************
    C** LOAD AMOUNTS FROM PROC CODE FILE            L O A D A M T       *
    C********************************************************************
     C     loadamt       begsr
    C*
     C                   eval      xxcpt4 = durate
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  99
    C*
    C* Surgeries get ASC rate
     C*
     C                   select
     C                   when      *in99 = *off and durate >= '10000'
     C                             and durate <= '69999'
    C*
    C* Prior to 9/1/2004
    C*
     C                   if        cafdat < 1040901
    C*
     C                   if        eost = 'MA'
    C*
     C                   eval      camra = pr92n1
    C*
     C                   else
     C                   eval      camra = pr92n3
    C*
     C                   endif
    C*
     C                   endif
    C*
    C* On or after 9/1/2004 its based on the area billed
    C*
     C                   if        cafdat >= 1040901
     C*** BEG ADD *** 05/13/09 *********************************************
     C                             and cafdat <  1090401
     C*** END ADD *** 05/13/09 *********************************************
    C*
    C* Get the county from the hospital header file
    C*
     C     p1ky          chain     pp101                              99
     C****************   eval      kkkey = 'MA' + %subst(p1cnty:6:2)
     C                   eval      kkkey = 'MA' + eucnty
    C*
     C     ccky          chain     pcc01                              99
     C                   movel     cc10a1        xxarea            2
     C                   move      xxarea        kkarea
    C*
     C                   eval      kkgrp = pr92n1
    C*
     C     asky          setgt     pas01
     C     asky1         readpe    pas01                                  99
     C                   if        *in99 = *off
     C                   eval      camra = asfee
     C                   else
     C                   eval      camra = 0
     C                   endif
    C*
     C                   endif
    C*
    C* Radiology/lab gets tech component
    C*
     C                   when      *in99 = *off and durate >= '70000'
     C                             and durate <= '89999'
     C                   if        pr92n2 <> 0
     C                   eval      camra = pr92n2
     C                   else
     C                   eval      camra = prmra
     C                   endif
    C*
    C* Medicine pays MRA
    C*
     C                   when      *in99 = *off and durate >= '90000'
     C                             and durate <= '96999'
     C                   eval      camra = prmra
    C*
    C* Physical Medicine pays MRA or 1993 special rates
    C*
     C                   when      *in99 = *off and durate >= '97001'
     C                             and durate <= '97799' and eusprt = 'Y'
     C*** BEG ADD *** 02/14/06 *********************************************
     C                             or *in99 = *off and durate = 'G0283'
     C                             and eusprt = 'Y'
     C*** END ADD *** 02/14/06 *********************************************
     C*
     C                   eval      xxsvcd = 0
     C     svky          setgt     psv01
     C     svky1         readpe    psv01                                  60
     C*
     C                   if        *in60 = *off and svalow <> 0
     C                   eval      camra = svalow
     C                   else
     C                   eval      camra = prprof
     C                   endif
    C*
     C                   when      *in99 = *off and durate >= '97001'
     C                             and durate <= '97799' and eusprt <> 'Y'
     C*** BEG ADD *** 02/14/06 *********************************************
     C                             or *in99 = *off and durate = 'G0283'
     C                             and eusprt <> 'Y'
     C*** END ADD *** 02/14/06 *********************************************
     C                   eval      camra = prprof
    C*
    C* All others pay MRA
    C*
     C                   when      *in99 = *off and durate >= '97800'
     C                             and durate <= '99999'
     C                   eval      camra = prmra
    C*
     C                   endsl
    C*
     C                   endsr
    C*
    C********************************************************************
    C** LOAD AMOUNTS BASED ON SPECIALTY             L O D S P E C A M T *
    C********************************************************************
     C     lodspecamt    begsr
    C*
     C                   select
    C*
     C                   when      eospcd = 'ACP'
     C                   eval      camra = pr92n4
    C*
     C                   when      eospcd = 'CHI'
     C                   eval      camra = pr92n6
     C*** BEG ADD *** 05/15/05 *********************************************
     C                   if        dhprcd >= '97110' and prcpt4 <= '97799'
     C                             and prprof <> 0
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 05/15/05 *********************************************
     C*** BEG ADD *** 05/16/05 *********************************************
     C                   if        dhprcd >= '97010' and prcpt4 <= '97039'
     C                             and prprof <> 0
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 05/16/05 *********************************************
     C*** BEG ADD *** 04/30/09 *********************************************
     C                   if        cafdat >= 1090401
     C*
     C                   if        (dhprcd >= '70010' and prcpt4 <= '79999')
     C                   eval      camra = prmra
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n2
     C                   endif
     C*
     C*** BEG CHG *** 01/21/10 *********************************************
     C***********        if        (dhprcd >= '97110' and prcpt4 <= '97799') or
     C***********                  (dhprcd >= '97010' and prcpt4 <= '97039')
     C***********        eval      camra = 0
     C***********        eval      caprof = 0
     C***********        eval      catech = 0
     C***********        endif
     C                   if        (dhprcd >= '16000' and prcpt4 <= '16030') or
     C                             (dhprcd >= '92506' and prcpt4 <= '92534') or
     C                             (dhprcd >= '97001' and prcpt4 <= '97999') or
     C                             (dhprcd = 'G0237')
     C                   eval      camra = prprof
     C                   endif
     C*** END CHG *** 01/21/10 *********************************************
     C*
     C                   endif
     C*** END ADD *** 04/30/09 *********************************************
    C*
     C                   when      eospcd = 'PSY'
     C                   eval      camra = pr92n5
    C*
     C*** BEG CHG *** 04/30/09 *********************************************
     C***                when      eospcd = 'REH' or eospcd = 'PHT' or
     C***                          eospcd = 'OCC' or eospcd = 'OCM' or
     C***                          eospcd = 'OCT' or eospcd = 'OT' or
     C***                          eospcd = 'SPE' or eospcd = 'AUD' or
     C***                          eospcd = 'MD ' and dhprcd >= '97001' and
     C***                          dhprcd <= '97755' or
     C***                          eospcd = 'MD ' and dhprcd >= '97799' and
     C***                          dhprcd <= '98929' or
     C***                          eospcd = 'MED' and dhprcd >= '97001' and
     C***                          dhprcd <= '97755' or
     C***                          eospcd = 'MED' and dhprcd >= '97799' and
     C***                          dhprcd <= '98929'
    C*
     C***                eval      camra = prprof
    C*
     C                   when      eospcd = 'REH' or eospcd = 'PHT' or
     C                             eospcd = 'OCC' or eospcd = 'OCM' or
     C                             eospcd = 'OCT' or eospcd = 'OT' or
     C                             eospcd = 'SPE' or eospcd = 'AUD' or
     C**For MED and MD and cafdat < 1090401
     C                             eospcd = 'MD ' and cafdat < 1090401 and
     C                             dhprcd >= '97001' and dhprcd <= '97755' or
     C                             eospcd = 'MD ' and cafdat < 1090401 and
     C                             dhprcd <= '97799' and dhprcd >= '98929' or
     C                             eospcd = 'MED' and cafdat < 1090401 and
     C                             dhprcd >= '97001' and dhprcd <= '97755' or
     C                             eospcd = 'MED' and cafdat < 1090401 and
     C                             dhprcd <= '97799' and dhprcd >= '98929' or
     C**For MD and cafdat >=1090401
     C                             eospcd = 'MD ' and cafdat >= 1090401 and
     C                             dhprcd >= '16000' and dhprcd <= '16030' or
     C                             eospcd = 'MD ' and cafdat >= 1090401 and
     C                             dhprcd >= '92506' and dhprcd <= '92534' or
     C                             eospcd = 'MD ' and cafdat >= 1090401 and
     C                             dhprcd >= '97001' and dhprcd <= '97799' or
     C                             eospcd = 'MD ' and cafdat >= 1090401 and
     C                             dhprcd = 'G0237' or
     C**For MED and cafdat >=1090401
     C                             eospcd = 'MED' and cafdat >= 1090401 and
     C                             dhprcd >= '16000' and dhprcd <= '16030' or
     C                             eospcd = 'MED' and cafdat >= 1090401 and
     C                             dhprcd >= '92506' and dhprcd <= '92534' or
     C                             eospcd = 'MED' and cafdat >= 1090401 and
     C                             dhprcd >= '97001' and dhprcd <= '97799' or
     C                             eospcd = 'MED' and cafdat >= 1090401 and
     C                             dhprcd = 'G0237'
    C*
     C                   eval      camra = prprof
    C*
     C*** END CHG *** 04/30/09 *********************************************
     C*** BEG ADD *** 04/30/09 *********************************************
     C                   when      eospcd = 'ASC'
     C                   if        (dhprcd >= '10000' and dhprcd <= '69999')
     C                             or (dhprcd >= '88000' and dhprcd <= '88399')
     C                   eval      camra = pr92n1
     C                   endif
     C*** END ADD *** 04/30/09 *********************************************
     C                   endsl
    C*
     C                   endsr
    C*
    C********************************************************************
    C** PHARMACY SUBROUTINE                                  P H A R    *
    C********************************************************************
     C     phar          begsr
    C*
    C*  Default to service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
     C                   movel     dpndc         xxndc
    C*
     C     rxky          setgt     prx01
     C     nxtpr4        tag
     C     rxky          readpe    prx01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
     C                   else
    C*
    C* Brand-name Pharmaceuticals
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
     C                   eval(h)   camra = rx1awp
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
     C                   eval      camra = xxawp * caunts
     C*** BEG CHG *** 05/09/07 *********************************************
     C****************** eval(h)   caawp = caunts * xxawp
     C                   eval      caawp = xxawp
     C*** END CHG *** 05/09/07 *********************************************
     C                   move      rxstat        castat
     C                   move      'Y'           cafsr
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
    C* Adjust MRA depending on MOD
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
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = pr92n3
     C                   eval      camra = pr92n3
    C*
     C                   endif
    C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
    C*
     C                   eval      xxmra = pr92n3
     C                   eval      camra = pr92n3
    C*
     C                   endif
    C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = pranam
     C                   eval      camra = pranam
    C*
     C                   endif
    C*
     C     pmode         endsr
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
     C*** END ADD *** 10/17/02 *********************************************
    C*
    C********************************************************************
    C** PROCEDURE CODE MRA SEARCH                             P R O C   *
    C********************************************************************
     C     proc          begsr
    C*
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*
    C*  Default to service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C     prky          setgt     ppr01
     C     nxtpr2        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C***** beg chg *** 04/07/14 *******************************************
     C*****              if        *in60 = *on
    C*
     C                   if        *in60 = *on
     C                             or
     C                             (*in60 = *off and
     C                              prcpt4 <> cacpt4)
     C***** end chg *** 04/07/14 *******************************************
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caamt = prmra
     C                   eval      caprof = prprof
     C                   eval      cafacf = pr92n1
     C                   eval      catech = pr92n2
    C*
    C*  Load the professional and technical component amount based on
    C*  percentages in FTUMP.
    C*
     C*** BEG DEL *** 04/30/09 *********************************************
     C***  umkey         setgt     pum01
     C***                readp     pum01                                  60
    C*
     C***                if        *in60 = *off
     C***                          and cacpt4 <= umtocp
    C*
     C***                move      umod26        xxmd26
     C***                move      umod27        xxmd27
    C*
     C***                if        caprof = 0
     C***                eval      caprof = prmra * xxmd26
     C***                endif
    C*
     C***                if        catech = 0
     C***                eval      catech = prmra * xxmd27
     C***                endif
    C*
     C***                endif
     C*** END DEL *** 04/30/09 *********************************************
    C*
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   eval      caasc = pr92n1
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a3         canct
     C                   move      pr1ad         casepp
     C                   move      pr1a4         capcf
     C                   move      prfsr         cafsr
     C                   eval      camd50 = *zero
     C                   eval      camd51 = *zero
     C                   eval      camd54 = *zero
     C                   eval      camd55 = *zero
     C                   eval      camd56 = *zero
     C                   eval      camd62 = *zero
     C                   eval      camd66 = *zero
     C                   eval      camd80 = *zero
     C                   eval      camd81 = *zero
     C                   eval      camd82 = *zero
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
    C*
     C*** BEG CHG *** 04/30/09 *********************************************
     C***                if        dhprcd >= '00001'
     C                   if        dhprcd >= '00001' or dhprcd = 'G0237'
     C*** END CHG *** 04/30/09 *********************************************
     C                   exsr      lodspecamt
     C                   else
     C                   eval      camra = prmra
     C                   endif
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr2
    C*
     C     proce         endsr
     C*** BEG ADD *** 01/11/04 *********************************************
    C********************************************************************
    C** SORT PT PROCEDURES IN HIGHEST MRA ORDER MRA * UNITS
    C********************************************************************
     C     ptsort        begsr
    C*
     C                   open      facap
    C*
     C                   read      pcawk2                                 99
     C                   dow       *in99 = *off
    C*
     C                   eval      m = 1
     C     cacpt4        lookup    ptprcd(m)                              99
    C*
     C                   if        *in99 = *on
     C                   eval      caamt = ptamt(m)
     C                   update    pcawk2
     C                   endif
    C*
     C                   read      pcawk2                                 99
    C*
     C                   enddo
    C*
     C                   close     facap
    C*
     C                   endsr
     C*** END ADD *** 01/11/04 *********************************************
    C********************************************************************
    C** REASONABLE AND CUSTOMARY FILE SEARCH                  R E C U   *
    C********************************************************************
     C     recu          begsr
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
    C*
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
     C                   move      rcfsr         cafsr
     C                   eval      cafolu = rcfolu
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
    C*
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   eval      caprof = rcprof
     C                   eval      catech = rc92n2
    C*
    C*  Load the professional and technical component amount based on
    C*  percentages in FTUMP.
    C*
     C     umkey         setgt     pum01
     C                   readp     pum01                                  60
    C*
     C                   if        *in60 = *off
     C                             and cacpt4 <= umtocp
    C*
     C                   move      umod26        xxmd26
     C                   move      umod27        xxmd27
    C*
     C                   if        caprof = 0
     C                   eval      caprof = rcmra * xxmd26
     C                   endif
    C*
     C                   if        catech = 0
     C                   eval      catech = rcmra * xxmd27
     C                   endif
    C*
     C                   endif
    C*
    C*** END ADD *** 05/08/00 ****************************************
     C                   eval      caanam = rcanam
     C                   move      rcsurg        casurg
     C                   move      rcsupp        casupp
     C                   eval      caasc = rc92n1
     C                   move      rcstat        castat
     C                   move      rcrpt         carpt
    C*
     C                   eval      camd50 = *zeros
     C                   eval      camd51 = *zeros
     C                   eval      camd54 = *zeros
     C                   eval      camd55 = *zeros
     C                   eval      camd56 = *zeros
     C                   eval      camd62 = *zeros
     C                   eval      camd66 = *zeros
     C                   eval      camd80 = *zeros
     C                   eval      camd81 = *zeros
     C                   eval      camd82 = *zeros
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C*
     C                   move      dhpos         caplac
     C                   eval      caunts = dhunts
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
     C                   if        dhprcd >= '00100' and dhprcd <= '01999'
     C                   eval      camra = rcconf
     C                   endif
     C*
     C                   when      eobtyp = 'D'
    C*
     C                   move      ddpos         caplac
     C                   eval      caunts = ddunts
     C                   move      ddpart        capart
     C                   move      ddrcfl        carcfl
    C*
     C                   endsl
     C*** END ADD *** 08/22/02 *********************************************
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
    C*
     C                   eval      cahtos = euicn
     C                   move      durvcd        xxsvcd
     C                   move      '000'         xxunit
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
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
    C* Verify service date against provider effective date
    C*
     C     p2ky1         chain     pp202                              61
     C                   if        *in61 = *off
     C                   if        cafdat >= p2efdt
    C*
    C* Hard code service code so clients can enter rev codes but have
    C* the program use the HOSP cost to charge ratio set up for their
    C* providers.
     C                   movel     '000INPAT'    xxcmsv
     C                   move      '000'         xxcmsv
    C*
     C     p2ky          setgt     pp201
     C                   readp     pp201                                  62
    C*
     C                   if        *in62 = *off
     C                             and cafein = p2fein
     C                             and camod = p2mod
     C                             and xxcmsv = p2sv01
    C*
    C* Calculate payment on percentage
    C*
     C                   eval      cascam = p2am01
    C*
     C                   else
    C*
     C                   endif
    C*
    C* Service date doesn't match provider effective date
     C                   else
    C*
     C                   endif
    C*
    C* Provider not in state service code file
     C                   else
    C*
     C                   endif
    C*
     C                   endif
    C*
     C     svstat        cabeq     'I'           nxtsv1
    C*
    C* Load amounts from the procedure code file for outpatient bills
    C*
     C                   if        eublty >= '131' and eublty <= '135'
     C                   exsr      loadamt
     C                   endif
    C*
     C     svcde         endsr
    C* ==================================================================
     Opca01     e            rca01
