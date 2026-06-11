    H**********************************************************************
     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NIL                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR IL                           *
    H**********************************************************************
    H*  CREATED: 08/11/10  C:227  PROJECT: IL001                          *
    H*  COPIED FROM F4000NNJ                                              *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*   05/02/18 C:180 Project: MC420                                    *
    H*                  Increase xxWAP size, remove div by 1000.          *
    H*                                                                    *
     H*   02/01/16 C:079 Project: IL009                                    *
     H*                  Add logic to pay Dental POC                       *
    H*                                                                    *
     H*   02/27/13 C:284 Project: IL005                                    *
     H*                  Add logic to execute DENT subroutine when eff. dt *
     H*                  >=1130101.                                        *
     H*                                                                    *
     H*   02/21/12 C:277 Project: IL004                                    *
     H*                  Retrieve values from FMFSPIL when  Date <01/01/12 *
     H*                  else retrieve values from FMF1PIL                 *
     H*                                                                    *
     H*  01/31/11 C:257 Project: IL002                                     *
    H*                 Add logic for hospital and ASTC to process         *
    H*                 modifiers 50 and 51                                *
    H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  03/26/04 C:79  Project: 114MHH                                    *
    H*                 For UCR Anes Codes load CAMRA from RCCONF          *
    H*                                                                    *
     H*  10/02/03 C:79  Project: FSMNT                                     *
    H*                 Move clear of PCAWK2 after update PCA01            *
    H*                                                                    *
    H*  10/17/02 C:79  Project: FS239                                     *
    H*                 Load Pharmacy allowance into CAAWP and CAMRA       *
    H*                                                                    *
    H*  10/02/02 C:79  Support: MC010 33679                               *
    H*                 Populate specialty code at the detail level not    *
    H*                                                                    *
    H*  08/22/02 C:79  Support: MC025 33965                               *
    H*                 Process UCR for Dental Bills.                      *
    H*                                                                    *
    H*  08/20/02 C:79  Support: MC025 33914                               *
    H*                 Set FACAP(CAFSR) for UCR codes in RECU subroutine  *
    H*                                                                    *
    H*  08/13/02 C:79  Support: MC010 33679                               *
    H*                 Clear rcd fmt PCAWK2 after writing                 *
    H*                                                                    *
    H*  05/30/01 C:62  Project: FS228                                     *
    H*                 GET ZIPA FROM CAZIPA NOT FROM PROVIDER FILE.       *
    H*                                                                    *
    H*  03/28/01 C:62  Project: UC002                                     *
    H*                 1. Add logic to populate Prof/Tech components      *
    H*                                                                    *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
     Ffdcal13   uf   e           k disk
     Ffaeolg    if   e           k disk
     F                                     infds(dbfds)
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*
     Ffmfsl1il  if   e           k disk
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
     Ffaeul     if   e           k disk
     Ffmp1l     if   e           k disk
     F*** Beg Add *** 02/21/12 *************************************************
     Ffmf1l1il  if   e           k disk
     F*** End Add *** 02/21/12 *************************************************
    D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
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
     D*
     D dbfds           ds
     D dbrrn                 397    400b 0
     D*
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
     D kkcat           s                   like(fscat)
     D kkzip           s                   like(fszip)
     D**** Beg Add *** 02/21/12 ************************************************
     D kkrgn#          s                   like(f1rgn#)
     D**** End Add *** 02/21/12 ************************************************
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
     D kkmod           s                   like(capmod)
      *** Begin Change *** 05/02/18 *********************
     D***xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 05/02/18 *********************
     D xxdcs#          s              5  0
     D xxefdt          s                   like(fsefdt)
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
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
    C*  Procedure Code new file FMFSP
     C     fsky          klist
     C                   kfld                    kkzip
     C                   kfld                    xxcpt4
     C                   kfld                    kkcat
     C                   kfld                    cafdat
    C*
     C     fsky1         klist
     C                   kfld                    kkzip
     C                   kfld                    xxcpt4
     C                   kfld                    kkcat
    C*
    C* Reasonable & Customary File
     C     rcky          klist
     C                   kfld                    qqco#
     C                   kfld                    cazipa
     C                   kfld                    xxcpt4
     C                   kfld                    kkefdt
    C*
    C*  Usual and Customary Modifier Percent File
    C*
     C     umky          klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
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
    C*  Provider Master File
     C     p1key         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C*** Beg Add *** 02/21/12 *************************************************
    C*  Procedure Code new file FMF1P
     C     f1ky          klist
     C                   kfld                    kkrgn#
     C                   kfld                    xxcpt4
     C                   kfld                    kkcat
     C                   kfld                    cafdat
    C*
     C*** End Add *** 02/21/12 *************************************************
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
     C                   movel     qqspcd        caspcd
    C*
    C* Find the recon number.  If this is a recon.
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      xxrcn = eorcn
     C                   movel     eoflic        kkzip
     C*** Beg Add *** 02/21/12 *************************************************
     C                   movel     eorgn#        kkrgn#
     C*** End Add *** 02/21/12 *************************************************
    C*
     C                   else                                                   *in60 = *off
     C                   eval      xxrcn = *zero
    C*
     C                   endif                                                  *in60 = *off
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
     C                   movel     eoflic        cazipa
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
     C*** Beg add *** 02/27/13 *************************************************
     C                             and cafdat < 1130101
     C*** End add *** 02/27/13 *************************************************
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
     C*** Beg Add *** 02/27/13 *************************************************
     C                   if        eobtyp = 'D'
     C                             and cafdat >= 1130101
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
     C*** End Add *** 02/27/13 *************************************************
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
     C*** BEGIN ADD *** 01/31/11 **************************************
     C                   move      dupmod        camod1
     C                   move      dumod2        camod2
     C                   move      dupmod        mod(1)
     C                   move      dumod2        mod(2)
     C                   movea     mod           mod1
     C*** END   ADD *** 01/31/11 **************************************
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
     C*** Beg Add *** 02/27/13 *************************************************
     C                   endif
     C*** End Add *** 02/27/13 *************************************************
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
     C                   if        xxmra < cachrg
     C                   z-add     xxmra         caamt                92
     C                   else
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
    C*  Default to use service date for effective date for rules
     C                   move      cafdat        kkefdt
    C*
     C                   if        (xxcpt4 >= '00100' and xxcpt4 <= '01999')
     C***********                  (xxcpt4 >= '99100' and xxcpt4 <= '99140')
     C                   eval      kkcat = 'Anesthesia'
     C                   elseif    xxcpt4 >= 'A0000' and xxcpt4 <= 'V9999'
     C                   eval      kkcat = 'HCPCS'
     C                   else
     C                   eval      kkcat = 'Professional'
     C                   endif
     C*** Beg Add *** 02/27/13 *************************************************
     C                   if        (xxcpt4 >= 'D0100' and xxcpt4 <= 'D9999')
     C                   if        cafdat < 1130101
     C                   eval      kkcat = 'DENTAL'
     C                   else
     C                   eval      kkcat = 'Dental'
     C                   endif
     C                   endif
     C*** End Add *** 02/27/13 *************************************************
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        cafdat < 1120101
     C*** End Add *** 02/21/12 ************************************************
     C     fsky          setgt     pfs01
     C     nxtpr3        tag
     C                   readp     pfs01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      cafolu = fsglob
    C*
     C                   eval      xcmra = fsptot
     C                   eval      camra = fsptot
     C                   eval      caprof = fsppc
     C                   eval      catech = fsptc
     C                   move      fsstat        castat
     C                   move      fsrpt         carpt
     C                   move      fsfsr         cafsr
     C                   eval      camd50 = *zero
     C                   eval      camd51 = *zero
     C                   eval      camd80 = *zero
     C                   eval      camd81 = *zero
     C                   clear                   caplac
    C*
     C                   endif
    C*
     C     fsstat        cabeq     'I'           nxtpr3
     C*** Beg Add *** 02/21/12 *************************************************
     C                   else
     C*
     C     f1ky          setgt     pf101
     C     nxtpr6        tag
     C                   readp     pf101                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      cafolu = f1glob
    C*
     C*** Beg Chg *** 02/01/16 ******************************************
     C*****************  eval      xcmra = f1ptot
     C*****************  eval      camra = f1ptot
     C                   if        f1tppo > 0
     C                   eval      camra = f1tppo * cachrg / 100
     C                   else
     C                   eval      camra = f1ptot
     C                   endif
     C                   eval      xcmra = camra
     C*** End Chg *** 02/01/16 ******************************************
     C                   eval      caprof = f1ppc
     C                   eval      catech = f1ptc
     C                   move      f1stat        castat
     C                   move      f1rpt         carpt
     C                   move      f1fsr         cafsr
     C                   eval      camd50 = *zero
     C                   eval      camd51 = *zero
     C                   eval      camd80 = *zero
     C                   eval      camd81 = *zero
     C                   clear                   caplac
    C*
     C                   endif
    C*
     C     f1stat        cabeq     'I'           nxtpr6
     C                   endif
     C*** End Add *** 02/21/12 *************************************************
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
     C                   eval      caunts = dpunts
     C                   eval      xxefdt = rxawpd
    C*
    C* Use previous average wholesale price(AWP)
     C                   if        cafdat < xxefdt
    C*
      *** Begin Change *** 05/02/18 *********************
     C***                eval      xxawp = rx1awp / 1000
     C                   eval      xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
    C*
    C* Use current average wholesale price(AWP)
     C                   else
      *** Begin Change *** 05/02/18 *********************
     C***                eval      xxawp = rxawp / 1000
     C                   eval      xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   endif
    C*
     C                   eval(h)   camra = caunts * xxawp
     C                   eval      caawp = xxawp
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
     C     'NU'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        cafdat < 1120101
     C*** End Add *** 02/21/12 ************************************************
     C                   if        fsnwpo > 0
     C                   eval      camra = fsnwpo * cachrg /100
     C                   else
     C                   eval      camra = fsnew
     C                   endif
     C*** Beg Add *** 02/21/12 ************************************************
     C                   else
     C*
     C                   if        f1nwpo > 0
     C                   eval      camra = f1nwpo * cachrg /100
     C                   else
     C                   eval      camra = f1new
     C                   endif
     C                   endif
     C*** End Add *** 02/21/12 ************************************************
     C                   eval      xxmra = camra
    C*
     C                   endif
    C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        cafdat < 1120101
     C*** End Add *** 02/21/12 ************************************************
    C*
     C                   if        fsuepo > 0
     C                   eval      camra = fsuepo * cachrg /100
     C                   else
     C                   eval      camra = fsused
     C                   endif
     C*** Beg Add *** 02/21/12 ************************************************
     C                   else
     C*
     C                   if        f1nwpo > 0
     C                   eval      camra = f1nwpo * cachrg /100
     C                   else
     C*
     C                   if        f1uepo > 0
     C                   eval      camra = f1uepo * cachrg /100
     C                   else
     C                   eval      camra = f1used
     C                   endif
     C                   endif
     C                   endif
     C*** End Add *** 02/21/12 ************************************************
     C                   eval      xxmra = camra
    C*
     C                   endif
    C*
     C     'RR'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        cafdat < 1120101
     C*** End Add *** 02/21/12 ************************************************
    C*
     C                   if        fsrnpo > 0
     C                   eval      caanam = fsrnpo * cachrg /100
     C                   else
     C                   eval      caanam = fsrnt
     C                   endif
     C*** Beg Add *** 02/21/12 ************************************************
     C                   else
     C*
     C                   if        f1nwpo > 0
     C                   eval      caanam = f1nwpo * cachrg /100
     C                   else
     C*
     C                   if        f1rnpo > 0
     C                   eval      caanam = f1rnpo * cachrg /100
     C                   else
     C                   eval      caanam = f1rnt
     C                   endif
     C                   endif
     C                   endif
     C*** End Add *** 02/21/12 ************************************************
    C*
     C                   endif
    C*
     C     'KF'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   if        fsc3po > 0
     C                   eval      camra = fsc3po * cachrg / 100
     C                   else
     C                   eval      camra = fsciii
     C                   endif
     C                   eval      xxmra = camra
    C*
     C                   endif
    C*
     C*** BEGIN ADD *** 01/31/11 **************************************
    C*
     C     '51'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd51
    C*
     C                   endif
     C*
     C     '50'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd50
    C*
     C                   endif
     C*** END   ADD *** 01/31/11 **************************************
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
     C                   if        (xxcpt4 >= '00100' and xxcpt4 <= '01999')
     C*************                (xxcpt4 >= '99100' and xxcpt4 <= '99140')
     C                   eval      kkcat = 'Anesthesia'
     C                   elseif    xxcpt4 >= 'A0000' and xxcpt4 <= 'V9999'
     C                   eval      kkcat = 'HCPCS'
     C                   else
     C                   eval      kkcat = 'Professional'
     C                   endif
    C*
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        cafdat < 1120101
     C*** End Add *** 02/21/12 ************************************************
     C     fsky          setgt     pfs01
     C     nxtpr2        tag
     C                   readp     pfs01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   if        mod1 <> *blanks
     C                   eval      kkmod = %subst(%trim(mod1):1:2)
     C                   else
     C                   eval      kkmod = *blanks
     C                   endif

     C                   select

     C                   when      kkcat = 'Anesthesia'
     C                   eval      xcmra = fsancv
     C                   eval      camra = fsancv
     C                   eval      caanam = fsanba
     C                   eval      caanmn = dhunts

     C                   when      kkcat = 'HCPCS'
     C                   if        fsnwpo > 0
     C                   eval      camra = fsnwpo * cachrg /100
     C                   else
     C                   eval      camra = fsnew
     C                   endif

     C                   when      kkcat = 'Professional'
     C                   if        fstppo > 0
     C                   eval      camra = fstppo * cachrg / 100
     C                   else
     C                   eval      camra = fsptot
     C                   endif
     C                   if        fspppo > 0
     C                   eval      caprof = fspppo * cachrg / 100
     C                   else
     C                   eval      caprof = fsppc
     C                   endif
     C                   if        fsptpo > 0
     C                   eval      catech = fsptpo * cachrg / 100
     C                   else
     C                   eval      catech = fsptc
     C                   endif

     C                   endsl

     C                   eval      xcmra = camra
     C                   eval      caanmn = dhunts
    C*
     C                   eval      cafolu = fsglob
    C*
     C                   movel     fsane         caanes
     C                   move      fsstat        castat
     C                   move      fsrpt         carpt
     C                   move      fsfsr         cafsr
     C                   eval      camd54 = *zero
     C                   eval      camd55 = *zero
     C                   eval      camd56 = *zero
     C                   eval      ca50in = fsbila
     C                   eval      ca51in = fsmult
     C                   eval      ca62in = fscosg
     C                   eval      ca66in = fstesg
     C                   eval      ca80in = fsasst
     C                   eval      camd81 = *zero
     C                   eval      camd82 = *zero
     C                   move      dhpos         caplac
     C                   move      dhprcd        cacpt4
     C                   eval      caunts = dhunts
    C*
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
    C*
     C                   endif
    C*
     C     fsstat        cabeq     'I'           nxtpr2
     C*** Beg Add *** 02/21/12 *************************************************
     C                   else
     C*
     C     f1ky          setgt     pf101
     C     nxtpr7        tag
     C                   readp     pf101                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   if        mod1 <> *blanks
     C                   eval      kkmod = %subst(%trim(mod1):1:2)
     C                   else
     C                   eval      kkmod = *blanks
     C                   endif

     C                   select

     C                   when      kkcat = 'Anesthesia'
     C                   eval      xcmra = f1ancv
     C                   eval      camra = f1ancv
     C                   eval      caanam = f1anba
     C                   eval      caanmn = dhunts

     C                   when      kkcat = 'HCPCS'
     C                   if        f1nwpo > 0
     C                   eval      camra = f1nwpo * cachrg /100
     C                   else
     C                   eval      camra = f1new
     C                   endif

     C                   when      kkcat = 'Professional'
     C                   if        f1tppo > 0
     C                   eval      camra = f1tppo * cachrg / 100
     C                   else
     C                   eval      camra = f1ptot
     C                   endif
     C                   if        f1pppo > 0
     C                   eval      caprof = f1pppo * cachrg / 100
     C                   else
     C                   eval      caprof = f1ppc
     C                   endif
     C                   if        fsptpo > 0
     C                   eval      catech = f1ptpo * cachrg / 100
     C                   else
     C                   eval      catech = f1ptc
     C                   endif

     C                   endsl

     C                   eval      xcmra = camra
     C                   eval      caanmn = dhunts
    C*
     C                   eval      cafolu = f1glob
    C*
     C                   movel     f1ane         caanes
     C                   move      f1stat        castat
     C                   move      f1rpt         carpt
     C                   move      f1fsr         cafsr
     C                   eval      camd54 = *zero
     C                   eval      camd55 = *zero
     C                   eval      camd56 = *zero
     C                   eval      ca50in = f1bila
     C                   eval      ca51in = f1mult
     C                   eval      ca62in = f1cosg
     C                   eval      ca66in = f1tesg
     C                   eval      ca80in = f1asst
     C                   eval      camd81 = *zero
     C                   eval      camd82 = *zero
     C                   move      dhpos         caplac
     C                   move      dhprcd        cacpt4
     C                   eval      caunts = dhunts
    C*
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
    C*
     C                   endif
    C*
     C     f1stat        cabeq     'I'           nxtpr7
     C                   endif
     C*** End Add *** 02/21/12 *************************************************
    C*
     C     proce         endsr
    C********************************************************************
    C** REASONABLE AND CUSTOMARY FILE SEARCH                  R E C U   *
    C********************************************************************
     C     recu          begsr
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
    C* Now that the Provider is established, get its Geographic Zip Area
    C* 1st - establish the first 3-bytes of the zip code
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
     C                   eval      caanes = rc1a2
     C                   eval      cafolu = rcfolu
    C*
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   eval      caanam = rcanam
     C                   move      rcsurg        casurg
     C                   move      rcsupp        casupp
     C                   eval      caprof = rcprof
     C                   eval      catech = rc92n1
    C*
    C*  Load the professional and technical component amount based on
    C*  percentages in FTUMP.
    C*
     C     umky          setgt     pum01
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
     C                   if        (dhprcd >= '00100' and dhprcd <= '01999')
     C***************              (dhprcd >= '99100' and dhprcd <= '99140')
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
    C*
    C* Get Peer Group from the provider file
     C     p1key         chain     pp101                              60
    C*
     C                   if        *in60 = *off
     C     5             subst(p)  p1cnty:10     capgrp
     C                   endif
    C*
     C                   move      durvcd        xxsvcd
     C                   move      '000'         xxunit
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
     C     eoky          chain     peu01
     C                   if        %found
     C                   movel(p)  euicn         cahtos
     C                   movel(p)  eudia1        caicd9
     C                   select
     C                   when      euicn = 'IM' or euicn = 'IP'
     C                             or euicn = 'IR' or euicn = 'IS'
     C*
     C                   movel     dbrrn         @@rrn             9 0
     C*
     C                   eval      @@desc = 'All incl room and board'
     C*
     C                   call      'FA761R'
     C                   parm                    @@rrn
     C                   parm      100           @@rvcd            3 0
     C                   parm      *blanks       @@hcpc            5
     C                   parm      *blanks       @@ndc             3
     C                   parm                    @@desc           25
     C                   parm      *blanks       @@rtcd            1
     C*
     C                   when      euicn = 'AO' or euicn = 'AS'
     C*
     C                   eval      kkcat = 'ASTC'
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        cafdat < 1120101
     C*** End Add *** 02/21/12 ************************************************
     C     fsky          setgt     pfs01
     C     nxtfs1        tag
     C                   readp     pfs01                                  60
     C                   if        *in60 = *on
     C                   move      'Y'           cainvl
     C                   else
     C                   if        fsaspo > 0
     C                   eval      camra = cachrg * fsaspo / 100
     C                   else
     C                   eval      camra = fsastc
     C                   endif
     C                   eval      xxmra = camra
     C*** BEGIN ADD *** 01/31/11 **************************************
     C                   eval      ca50in = fsbila
     C                   eval      ca51in = fsmult
     C                   eval      ca62in = fscosg
     C                   eval      ca66in = fstesg
     C                   eval      ca80in = fsasst
     C                   eval      cafolu  = fsglob
     C                   if        ca51in = '2'
     C                   eval      camd51 = 0.50
     C                   endif
     C                   if        ca50in = '1'
     C                   eval      camd50 = 1.50
     C                   endif
     C*** END   ADD *** 01/31/11 **************************************
     C                   endif
     C     fsstat        cabeq     'I'           nxtfs1
     C*
     C*** Beg Add *** 02/21/12 *************************************************
     C                   else
     C*
     C     f1ky          setgt     pf101
     C     nxtfs8        tag
     C                   readp     pf101                                  60
     C                   if        *in60 = *on
     C                   move      'Y'           cainvl
     C                   else
     C                   if        f1aspo > 0
     C                   eval      camra = cachrg * f1aspo / 100
     C                   else
     C                   eval      camra = f1astc
     C                   endif
     C                   eval      xxmra = camra
     C                   eval      ca50in = f1bila
     C                   eval      ca51in = f1mult
     C                   eval      ca62in = f1cosg
     C                   eval      ca66in = f1tesg
     C                   eval      ca80in = f1asst
     C                   eval      cafolu  = f1glob
     C                   if        ca51in = '2'
     C                   eval      camd51 = 0.50
     C                   endif
     C                   if        ca50in = '1'
     C                   eval      camd50 = 1.50
     C                   endif
     C*
     C                   endif
     C     f1stat        cabeq     'I'           nxtfs8
     C                   endif
     C*** End Add *** 02/21/12 *************************************************
     C                   when      euicn = 'HO' or euicn = 'HS'
     C*
     C                   eval      kkcat = 'Outpatient'
     C*** Beg Add *** 02/21/12 ************************************************
     C                   if        cafdat < 1120101
     C*** End Add *** 02/21/12 ************************************************
     C     fsky          setgt     pfs01
     C     nxtfs2        tag
     C                   readp     pfs01                                  60
     C                   if        *in60 = *on
     C                   move      'Y'           cainvl
     C                   else
     C                   if        fsotpo > 0
     C                   eval      camra = cachrg * fsotpo / 100
     C                   else
     C                   eval      camra = fsoutp
     C                   endif
     C                   eval      xxmra = camra
     C*** BEGIN ADD *** 01/31/11 **************************************
     C                   eval      ca50in = fsbila
     C                   eval      ca51in = fsmult
     C                   eval      ca62in = fscosg
     C                   eval      ca66in = fstesg
     C                   eval      ca80in = fsasst
     C                   eval      cafolu  = fsglob
     C*** END   ADD *** 01/31/11 **************************************
     C                   endif
     C     fsstat        cabeq     'I'           nxtfs2
     C*
     C*** Beg Add *** 02/21/12 *************************************************
     C                   else
     C*
     C     f1ky          setgt     pf101
     C     nxtfs9        tag
     C                   readp     pf101                                  60
     C                   if        *in60 = *on
     C                   move      'Y'           cainvl
     C                   else
     C                   if        f1otpo > 0
     C                   eval      camra = cachrg * f1otpo / 100
     C                   else
     C                   eval      camra = f1outp
     C                   endif
     C                   eval      xxmra = camra
     C                   eval      ca50in = f1bila
     C                   eval      ca51in = f1mult
     C                   eval      ca62in = f1cosg
     C                   eval      ca66in = f1tesg
     C                   eval      ca80in = f1asst
     C                   eval      cafolu = f1glob
     C                   endif
     C     f1stat        cabeq     'I'           nxtfs9
     C                   endif
     C*** End Add *** 02/21/12 *************************************************
     C                   endsl
    C*
     C                   endif
    C*
     C     svcde         endsr
    C* ==================================================================
     Opca01     e            rca01
