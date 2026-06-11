    H**********************************************************************
     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NDE                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR DE                           *
    H**********************************************************************
    H*  CREATED: 01/31/01  C:79   PROJECT: DE001                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  02/29/24 C:079 Support: 99/115160                                 *
    H*                 Load anesthesia allowance for outpatient           *
    H*                                                                    *
     H*  02/08/21 C:079 Project: DE018                                     *
    H*                 Fix facility logic for place of service            *
    H*                 Fix Anes code range                                *
    H*                                                                    *
     H*  02/14/19 C:212 Project: DE016                                     *
    H*                 Fix to pay ASTC POC codes                          *
    H*                                                                    *
     H*  04/27/18 C:180 Project: C114MJS                                   *
    H*                 Increase xxWAP size.                               *
    H*                                                                    *
     H*  02/16/17 C:079 Project: DE014                                     *
    H*                 Load Facility rates                                *
    H*                                                                    *
     H*  08/04/16 C:079 Support: 114/42552                                 *
    H*                 If paying POC then divide MRA / Units              *
    H*                                                                    *
     H*  02/24/16 C:079 Support: 114/39441                                 *
    H*                 Do not load allowances for rev code only lines     *
    H*                                                                    *
     H*  02/22/16 C:212 Project: DE013                                     *
    H*                 DE 2016 FS                                         *
    H*                                                                    *
     H*  04/06/15 C:079 Project: DE012                                     *
    H*                 Add outpatient and ASC processing                  *
    H*                                                                    *
     H*  03/16/15 C:296 Project: DE012                                     *
    H*                 Add edits for EUicn and DE state.                  *
    H*                                                                    *
     H*  04/18/14 C:079 Project: DE011                                     *
    H*                 Chain to FAEUP                                     *
    H*                                                                    *
     H*  03/26/14 C:212 Project: DE011                                     *
    H*                 DE FS 14: Add logic in SR SVCD to load % for ASCs  *
    H*                                                                    *
     H*  02/19/10 C:227 Project: DE006                                     *
    H*                 Change logic in Proc sub routine                   *
    H*                                                                    *
     H*  06/11/09 C:079 Project: DE005                                     *
    H*                 Change logic in calculations when effective        *
    H*                 date >= 06/01/2009                                 *
    H*                                                                    *
     H*  03/04/09 C:227 Project: DE004                                     *
    H*                 Change logic in calculations when effective        *
    H*                 date >= 11/14/2008                                 *
    H*                                                                    *
     H*  06/16/08 C:198 Project: DE002
    H*                 Allow calculations when PRRPT = 'P'                *
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
    H*  INDICATOR USAGE:
    H*****************************************************************
     Ffdcal13   uf   e           k disk
     Ffaeolg    if   e           k disk
     F                                     infds(dbfds)
     Ffacap     o    e           k disk    usropn
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
     Ffmr1l     if   e           k disk
    F*
     Ffmp2lde   if   e           k disk
     Ffaeul     if   e           k disk
     Fftanel1de if   e           k disk
     D/copy sPrModule
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
     D kkcmsv          s                   like(svcmsv)
     D kkymd           s                   like(frefdt)
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
     D xxawp           s                   like(CAawp)
     D xxdcs#          s              5  0
     D xxefdt          s                   like(prefdt)
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D kkGZip          s              3s 0 inz
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
    D*---------------------------------------------------------------------
     D* Retrieve inpatient allowance.
    D*---------------------------------------------------------------------
     D Rtv_inpat_Alw   pr                  extpgm('FA756RDE')
     D inrrn                          9  0
     D outrtcd                        1
     D outallow1                      9  2
     D outallow2                      9  2
     D*
     D inrrn           s              9  0
     D outrtcd         s              1
     D outallow1       s              9  2
     D outallow2       s              9  2
    D*---------------------------------------------------------------------
     D*
     D dbfds           ds
     D dbrrn                 397    400b 0
    C*
     C     caky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
    C* Reasonable & Customary File
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
     C     p2ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    kkcmsv
     C                   kfld                    kkefdt
    C*
     C     p2ky1         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    kkcmsv
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
    C*
    C*
    C*
    C*
     C                   move      qqdcs#        xxdcs#
    C*
     C                   open      facap
    C*
     C                   exsr      convrt
    C*
    C*  END PROGRAM AND CLOSE ALL FILES
     C                   close     facap
      /Free
       //*Calculate medicare inpatient bill
       if EORcn = 0 and EOBtyp = 'H';
          if EUIcn = 'IS' or EUIcn = 'IP'
          or EUIcn = 'IM' or EUIcn = 'IR';
             ExSr Medicare_inpt;
          endif;
       endif;

      /End-Free
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
      /Free
                         Chain euky faeul;
      /End-Free
    C*
     C                   movel     'H'           catype
     C                   exsr      pmodld
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
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
    C*
     C     prky          setgt     ppr01
     C     nxtpr3        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
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
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
    C*
     C                   select
     C*
     C                   when      prrpt = 'P'
     C*
     C                   if        %subst(eoflic:1:3) = '197'
     C                             or %subst(eoflic:1:3) = '198'
     C                   eval      xcmra = (pr92n4/100) * cachrg
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)* xcmra
     C                   eval      catech = (pr92n2/100)* xcmra
     C                   endif
     C*
     C                   if        %subst(eoflic:1:3) = '199'
     C                   eval      xcmra = (pr92n5/100) * cachrg
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)* xcmra
     C                   eval      catech = (pr92n2/100)* xcmra
     C                   endif
     C                   if        isModEnabled('Multiple Units')
     C                   eval      CAmra = CaMra / CAunts
     C                   eval      xcMra = xcMra / CAunts
     C                   eval      CAprof = CAprof / CAunts
     C                   eval      CAtech = CAtech / CAunts
     C                   endif
     C*
     C                   when      prrpt = 'N'
     C*
     C                   if        %subst(eoflic:1:3) = '197'
     C                             or %subst(eoflic:1:3) = '198'
     C                   eval      xcmra = prmra
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)*prmra
     C                   eval      catech = (pr92n2/100)*prmra
     C                   endif
     C*
     C                   if        %subst(eoflic:1:3) = '199'
     C                   eval      xcmra = pr92n3
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)*pr92n3
     C                   eval      catech = (pr92n2/100)*pr92n3
     C                   endif
     C*
     C                   endsl
     C*
     C                   endif
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
     C                   eval      captyp = rxptyp
    C*
    C* Use previous average wholesale price(AWP)
     C                   if        cafdat < xxefdt
    C*
     C                   eval      xxawp = rx1awp
    C*
    C* Use current average wholesale price(AWP)
     C                   else
     C                   eval      xxawp = rxawp
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
     C     '26'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = caprof
     C                   eval      camra = caprof
    C*
     C                   endif
    C*
     C     '27'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = catech
     C                   eval      camra = catech
    C*
     C                   endif
    C*
     C     'TC'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = catech
     C                   eval      camra = catech
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
    C*
     C                   if        eobtyp = 'N'
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
     C                   elseif    eobtyp = 'H'
    C*
     C                   move      dupmod        camod1
     C                   move      dumod2        camod2
     C                   move      dumod3        camod3
     C                   move      dumod4        camod4
    C*
     C                   move      dupmod        mod(1)
     C                   move      dumod2        mod(2)
     C                   move      dumod3        mod(3)
     C                   move      dumod4        mod(4)
    C*
     C                   movea     mod           mod1
    C*
     C                   endif
    C*
     C                   endsr
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
    C*
     C     prky          setgt     ppr01
     C     nxtpr2        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caamt = prmra
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prprof        caprof
     C                   eval      catech = pr92n1
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
                         if        pr1a2 = 'Y'
                                   or dhprcd >= '00100'
                                   and dhprcd <= '01999'
       //*** Beg Add *** 02/08/21 *****************************************
                                   and %Check('0123456789':DHprCd) = 0 ;
       //*** End Add *** 02/08/21 *****************************************
     C                   eval      caanmn = dhunts
     C                   endif
    C*
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
    C*
     C                   endif
    C*
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
    C*
     C*

     C                   if        cafdat > 0 and cafdat < 1081114
     C                   exsr      load08
     C                   endif

     C                   if        cafdat >= 1081114 and cafdat < 1090129
     C                   exsr      load08a
     C                   endif

     C                   if        cafdat >= 1090129
     C                   exsr      load09
     C                   endif
     C*** Beg Chg *** 02/08/21 ********************************************
     C****
     C****               if        cafdat >= 1170131 and (DHpos = '21' or
     C****                          DHpos= '22' or DHpos = '23' or
     C****                          DHpos= '24' or DHpos = '26' or
     C****                          DHpos= '31' or DHpos = '34' or
     C****                          DHpos= '41' or DHpos = '42' or
     C****                          DHpos= '51' or DHpos = '52' or
     C****                          DHpos= '53' or DHpos = '56' or
     C****                          DHpos= '61')
       if CAfDat >= 1170131 and (DHpos = ' 21' or
         DHpos= ' 22' or DHpos = ' 23' or
         DHpos= ' 24' or DHpos = ' 26' or
         DHpos= ' 31' or DHpos = ' 34' or
         DHpos= ' 41' or DHpos = ' 42' or
         DHpos= ' 51' or DHpos = ' 52' or
         DHpos= ' 53' or DHpos = ' 56' or
         DHpos= ' 61');
     C*** End Chg *** 02/08/21 ********************************************
     C                   exsr      loadFacil
     C                   endif
     C*
     C                   endif
     C*
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
     C                   eval      kkefdt = cafdat
    C*
    C*
    C*
    C*
    C*
    C* Now that the Provider is established, get its Geographic Zip Area
    C* 1st - establish the first 3-bytes of the zip code
    C*
    C*
    C* 2nd - Get the corresponding Geographic Zip Area record:
    C*
    C*
    C* USE GEO-ZIP 430 IF ZIP CODE IS OUT OF STATE
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
                         if        dhprcd >= '00100' and dhprcd <= '01999'
       //*** Beg Add *** 02/08/21 *****************************************
                                   and %Check('0123456789':DHprCd) = 0 ;
       //*** End Add *** 02/08/21 *****************************************
                         eval      camra = rcconf;
                         endif;
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
     C*
     C                   if        qqst = 'DE'
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC IST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
     C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
     C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
     C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
     C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
     C                   PARM      *BLANK        ##KCIF            1            Keep Claimant info
     C                   PARM      *BLANK        ##CURS            1            PBILLS entry cursor
     C                   PARM      *BLANK        ##PCIB            1            Prov Check in Batch
     C                   PARM      *BLANK        ##CLT#            3            Client Number
     C                   PARM      *BLANK        ##UCPF            1            UCR Percentile
     C*
     C                   if        ##UCPF = 'Y'
     C*
     C     rcky          setgt     pr101
     C     nxr11         tag
     C                   readp     pr101                                  60
    C*
     C                   if        *in60 = *on
     C                             or r1co# <> qqco#
     C                             or r1zipa <> cazipa
     C                             or r1prcd <> xxcpt4
    C*
     C                   movel     'Y'           xxinvl
    C*
     C                   else
    C*
     C                   eval      xcmra = r1mr75
     C                   eval      camra = r1mr75
     C                   eval      caamt = r1mr75
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
     C                   eval      caprof = r1mr75 * xxmd26
     C                   endif
    C*
     C                   if        catech = 0
     C                   eval      catech = r1mr75 * xxmd27
     C                   endif
    C*
     C                   endif
     C                   move      r1stat        castat
    C*
     C                   endif
    C*
     C     r1stat        cabeq     'I'           nxr11
    C*
     C                   endif
    C*
     C                   endif                                                  qqst = 'DE'
    C*
     C     recue         endsr
    C********************************************************************
    C** SERVICE CODE SEARCH                                   S V C D   *
    C********************************************************************
     C     svcd          begsr
    C*
     C                   move      durvcd        xxsvcd
     C                   move      '000'         xxunit
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
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
     C     prstat        cabeq     'I'           nxtsv1
     C*
     C* Load percent for ASC's
     C*
     C                   if        euicn >= 'AO' and euicn <= 'AS' and
     C                             CAfdat < 1150131
     C*
     C                   eval      kkcmsv = '   ASC     '
     C*
     C     p2ky          setgt     pp201
     C     p2ky1         readpe    pp201                                  99
     C*
     C                   if        *in99 = *off
     C                             and cafdat >= p2efdt
     C*
     C                   eval      camra = (p2am01 * cachrg) / 1000000
     C                   move      *blank        cainvl
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
     C                   eval      cahtos = euicn
     C                   else
     C                   move      'Y'           cainvl
     C*
     C                   endif
     C*
     C                   endif                                                  euicn >= AO
     C                   if        DUrate <> *blanks
    C*
    C*  Default to service date for effective date for rules
    C*
     C                   eval      kkefdt = cafdat
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C     prky          setgt     ppr01
     C     nxtpr5        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
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
     C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr5
    C*
     C                   Endif
     C*
     C*** Process Outpatient
     C*
     C                   if        (euicn >= 'HO' and euicn <= 'HS') and
     C                             CAfdat >= 1150131
    C*
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
     C                   if        DUrate <> *blanks
     C                   exsr      LoadOut
     C                   Endif
    C*
     C                   move      *blank        cainvl
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
     C                   eval      cahtos = euicn
     C                   else
     C                   move      'Y'           cainvl
     C*
     C                   endif                                                  euicn >= AO
     C*
     C                   endif                                                  euicn >= AO
     C*
     C*** Process ASC's
     C*
     C                   if        (euicn >= 'AO' and euicn <= 'AS') and
     C                             CAfdat >= 1150131
    C*
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
    C*
     C                   exsr      LoadAsc
    C*
     C                   move      *blank        cainvl
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
     C                   eval      cahtos = euicn
     C                   else
     C                   move      'Y'           cainvl
     C*
     C                   endif                                                  euicn >= AO
     C*
     C                   endif                                                  euicn >= AO
     C*
     C*//*Calculate medicare inpatient bill for type of service 'IS' or 'IP'.
     C*****
    C*
     C     svcde         endsr
    C* ==================================================================
    C**********************************************************************
    C* LOAD08 Sub routine                                                 *
    C**********************************************************************
     C     load08        begsr
    C*
     C                   select
     C*
     C                   when      prrpt = 'P'
     C*
     C                   if        %subst(eoflic:1:3) = '197'
     C                             or %subst(eoflic:1:3) = '198'
     C                   eval      xcmra = (pr92n4/100) * cachrg
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)* xcmra
     C                   eval      catech = (pr92n2/100)* xcmra
     C                   endif
     C*
     C                   if        %subst(eoflic:1:3) = '199'
     C                   eval      xcmra = (pr92n5/100) * cachrg
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)* xcmra
     C                   eval      catech = (pr92n2/100)* xcmra
     C                   endif
     C                   if        isModEnabled('Multiple Units')
     C                   eval      CAmra = CaMra / CAunts
     C                   eval      xcMra = xcMra / CAunts
     C                   eval      CAprof = CAprof / CAunts
     C                   eval      CAtech = CAtech / CAunts
     C                   endif
     C*
     C                   when      prrpt = 'N'
     C*
     C                   if        %subst(eoflic:1:3) = '197'
     C                             or %subst(eoflic:1:3) = '198'
     C                   eval      xcmra = prmra
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)*prmra
     C                   eval      catech = (pr92n2/100)*prmra
     C                   endif
     C*
     C                   if        %subst(eoflic:1:3) = '199'
     C                   eval      xcmra = pr92n3
     C                   eval      camra = xcmra
     C                   eval      caprof = (prprof/100)*pr92n3
     C                   eval      catech = (pr92n2/100)*pr92n3
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr
    C**********************************************************************
    C* LOAD08A Sub routine                                                *
    C**********************************************************************
     C     load08a       begsr
    C*
     C                   Select

     C                   when      %subst(eoflic:1:3) = '197' or
     C                             %subst(eoflic:1:3) = '198'
     C*** Anesthesia code range 00100 - 01999
                         if        (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
                                   and %Check('0123456789':CAcpt4) = 0
       //*** End Add *** 02/08/21 *****************************************
                                   or cacpt4 = '99100' or cacpt4 = '99116'
                                   or cacpt4 = '99135' or cacpt4 = '99140';

     C                   if        pr92nc <> 0
     C                   eval      xcmra = pr92nc
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nd <> 0
     C                   eval      xcmra = pr92nd/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif

     C*** HCPC code range A0021 - V5363
     C                   if        cacpt4 >= 'A0021' and cacpt4 <= 'V5363'

     C                   if        capmod = 'UE'
     C                   if        pr92na <> 0
     C                   eval      xcmra = pr92na
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nb <> 0
     C                   eval      xcmra = pr92nb/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   if        capmod = 'RR'
     C                   if        pr92nc <> 0
     C                   eval      xcmra = pr92nc
     C                   endif
     C                   if        pr92nd <> 0
     C                   eval      xcmra = pr92nd/100 * cachrg
     C                   endif
     C                   eval      caanam = xcmra
     C                   endif
     C                   endif

     C*** Not Anesthesia
                         if        (cacpt4 < '00100' or cacpt4 > '01999')
                                   and cacpt4 <>'99100' and cacpt4 <>'99116'
                                   and cacpt4 <>'99135' and cacpt4 <>'99140'
       //*** Beg Add *** 02/08/21 *****************************************
                                   or (cacpt4 >=  '00100' and cacpt4 <= '01999')
                                   and %Check('0123456789':CAcpt4) = 0;
       //*** End Add *** 02/08/21 *****************************************
     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        prmra <> 0
     C                   eval      xcmra = prmra
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92n4 <> 0
     C                   eval      xcmra = pr92n4/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   endif

     C                   when      %subst(eoflic:1:3) = '199'
     C*** Anesthesia code range 00100 - 01999
                         if        (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
                                   and %Check('0123456789':CAcpt4) = 0
       //*** End Add *** 02/08/21 *****************************************
                                   or cacpt4 = '99100' or cacpt4 = '99116'
                                   or cacpt4 = '99135' or cacpt4 = '99140';

     C                   if        pr92ng <> 0
     C                   eval      xcmra = pr92ng
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nh <> 0
     C                   eval      xcmra = pr92nh/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif

     C*** HCPC code range A0021 - V5363
     C                   if        cacpt4 >= 'A0021' and cacpt4 <= 'V5363'

     C                   if        capmod = 'UE'
     C                   if        pr92ne <> 0
     C                   eval      xcmra = pr92ne
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nf <> 0
     C                   eval      xcmra = pr92nf/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   if        capmod = 'RR'
     C                   if        pr92ng <> 0
     C                   eval      xcmra = pr92ng
     C                   endif
     C                   if        pr92nh <> 0
     C                   eval      xcmra = pr92nh/100 * cachrg
     C                   endif
     C                   eval      caanam = xcmra
     C                   endif
     C                   endif

     C*** Not Anesthesia
                         if        (cacpt4 < '00100' or cacpt4 > '01999')
                                   and cacpt4 <>'99100' and cacpt4 <>'99116'
                                   and cacpt4 <>'99135' and cacpt4 <>'99140'
       //*** Beg Add *** 02/08/21 *****************************************
                                   or (cacpt4 >=  '00100' and cacpt4 <= '01999')
                                   and %Check('0123456789':CAcpt4) > 0;
       //*** End Add *** 02/08/21 *****************************************
     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        pr92n3 <> 0
     C                   eval      xcmra = pr92n3
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92n5 <> 0
     C                   eval      xcmra = pr92n5/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   endif

     C                   endsl
     C*
     C                   endsr
    C**********************************************************************
    C* LOAD09 Sub routine                                                 *
    C**********************************************************************
     C     load09        begsr
    C*
     C                   Select

     C                   when      %subst(eoflic:1:3) = '197' or
     C                             %subst(eoflic:1:3) = '198'
     C*** Anesthesia code range 00100 - 01999
                         if        (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
                                   and %Check('0123456789':CAcpt4) = 0
       //*** End Add *** 02/08/21 *****************************************
                                   or cacpt4 = '99100' or cacpt4 = '99116'
                                   or cacpt4 = '99135' or cacpt4 = '99140';

     C                   if        pr92nc <> 0
     C                   eval      xcmra = pr92nc
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nd <> 0
     C                   eval      xcmra = pr92nd/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   if        cafdat >= 1140131
     C/free
                         monitor;
                          kkGZip = %dec(%subst(EOflic:1:3):3:0);
                         on-error;
                          kkGZip = *zeros;
                         endmon;

                         Setgt (kkGZip:CAfdat) pan01;
                         Readpe (kkGZip) pan01;
                         Dow not %eof(ftanel1de);
      /end-free
     C                   if        %subst(eoflic:1:3) = %editc(anegzip:'X')
     C                   eval      camra = aneconv
     C                   eval      xcmra = camra
     C                   leave
     C                   endif
     C/free
                         Readpe (kkGZip) pan01;
      /end-free
     C                   enddo
     C                   endif
     C                   endif

     C                   if        cacpt4 >= 'A0021' and cacpt4 <= 'V5363'

     C                   if        capmod = 'UE'
     C                   if        pr92na <> 0
     C                   eval      xcmra = pr92na
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nb <> 0
     C                   eval      xcmra = pr92nb/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   if        capmod = 'RR'
     C                   if        pr92nc <> 0
     C                   eval      xcmra = pr92nc
     C                   endif
     C                   if        pr92nd <> 0
     C                   eval      xcmra = pr92nd/100 * cachrg
     C                   endif
     C                   eval      caanam = xcmra
     C                   endif
     C                   endif

     C*** Not Anesthesia
                         if        (cacpt4 < '00100' or cacpt4 > '01999')
                                   and cacpt4 <>'99100' and cacpt4 <>'99116'
                                   and cacpt4 <>'99135' and cacpt4 <>'99140'
       //*** Beg Add *** 02/08/21 *****************************************
                                   or (cacpt4 >=  '00100' and cacpt4 <= '01999')
                                   and %Check('0123456789':CAcpt4) > 0;
       //*** End Add *** 02/08/21 *****************************************
     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        prmra <> 0
     C                   eval      xcmra = prmra
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92n4 <> 0
     C                   eval      xcmra = pr92n4/100 * cachrg
     C                   eval      camra = xcmra
     C*
     C* If paying a percent of charged for physician codes then
     C* divide by units so it pays correctly for multiple units
     C*
     C                   if        isModEnabled('Multiple Units') and
     C                             (cacpt4 >= '10000')
     C                   eval      CAmra = CaMra / CAunts
     C                   eval      xcMra = xcMra / CAunts
     C                   endif
     C                   endif
     C*Professional Component
     C                   if        prprof <> 0
     C                   eval      caprof = prprof
     C                   endif
     C                   if        pr92nk <> 0
     C                   eval      caprof = pr92nk/100 * cachrg
     C                   endif
     C*Technical Component
     C                   if        pr92n2 <> 0
     C                   eval      catech = pr92n2
     C                   endif
     C                   if        pr92nl <> 0
     C                   eval      catech = pr92nl/100 * cachrg
     C                   endif
     C                   endif
     C                   endif

     C                   when      %subst(eoflic:1:3) = '199'
     C*** Anesthesia code range 00100 - 01999
                         if        (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
                                   and %Check('0123456789':DHprCd) = 0
       //*** End Add *** 02/08/21 *****************************************
                                   or cacpt4 = '99100' or cacpt4 = '99116'
                                   or cacpt4 = '99135' or cacpt4 = '99140';

     C                   if        pr92ng <> 0
     C                   eval      xcmra = pr92ng
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nh <> 0
     C                   eval      xcmra = pr92nh/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   if        cafdat >= 1140131
     C/free
                         monitor;
                          kkGZip = %dec(%subst(EOflic:1:3):3:0);
                         on-error;
                          kkGZip = *zeros;
                         endmon;

                         Setgt (kkGZip:CAfdat) pan01;
                         Readpe (kkGZip) pan01;
                         Dow not %eof(ftanel1de);
      /end-free
     C                   if        %subst(eoflic:1:3) = %editc(anegzip:'X')
     C                   eval      camra = aneconv
     C                   eval      xcmra = camra
     C                   leave
     C                   endif
     C/free
                         Readpe (kkGZip) pan01;
      /end-free
     C                   enddo
     C                   endif
     C                   endif

     C*** HCPC code range A0021 - V5363
     C                   if        cacpt4 >= 'A0021' and cacpt4 <= 'V5363'

     C                   if        capmod = 'UE'
     C                   if        pr92ne <> 0
     C                   eval      xcmra = pr92ne
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nf <> 0
     C                   eval      xcmra = pr92nf/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   if        capmod = 'RR'
     C                   if        pr92ng <> 0
     C                   eval      xcmra = pr92ng
     C                   endif
     C                   if        pr92nh <> 0
     C                   eval      xcmra = pr92nh/100 * cachrg
     C                   endif
     C                   eval      caanam = xcmra
     C                   endif
     C                   endif

     C*** Not Anesthesia
                         if        (cacpt4 < '00100' or cacpt4 > '01999')
                                   and cacpt4 <>'99100' and cacpt4 <>'99116'
                                   and cacpt4 <>'99135' and cacpt4 <>'99140'
       //*** Beg Add *** 02/08/21 *****************************************
                                   or (cacpt4 >=  '00100' and cacpt4 <= '01999')
                                   and %Check('0123456789':CAcpt4) > 0;
       //*** End Add *** 02/08/21 *****************************************
     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        pr92n3 <> 0
     C                   eval      xcmra = pr92n3
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92n5 <> 0
     C                   eval      xcmra = pr92n5/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C*Professional Component
     C                   if        pr92ni <> 0
     C                   eval      caprof = pr92ni
     C                   endif
     C                   if        pr92nm <> 0
     C                   eval      caprof = pr92nm/100 * cachrg
     C                   endif
     C*Technical Component
     C                   if        pr92nj <> 0
     C                   eval      catech = pr92nj
     C                   endif
     C                   if        pr92nn <> 0
     C                   eval      catech = pr92nn/100 * cachrg
     C                   endif
     C                   endif
     C                   endif

     C                   endsl
     C*
     C                   endsr
    C**********************************************************************
    C* LoadFacil Subroutine                                               *
    C**********************************************************************
     C     loadFacil     begsr
    C*
     C                   Select

     C                   when      %subst(eoflic:1:3) = '197' or
     C                             %subst(eoflic:1:3) = '198'
     C*** HCPC code range A0021 - V5363
     C                   if        cacpt4 >= 'A0021' and cacpt4 <= 'V5363'

     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        pr92nw <> 0
     C                   eval      xcmra = pr92nw
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nz <> 0
     C                   eval      xcmra = pr92nz/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif

     C                   if        capmod = 'UE'
     C                   if        pr92o2 <> 0
     C                   eval      xcmra = pr92o2
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92o4 <> 0
     C                   eval      xcmra = pr92o4/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   if        capmod = 'RR'
     C                   if        pr92o6 <> 0
     C                   eval      xcmra = pr92o6
     C                   endif
     C                   if        pr92o8 <> 0
     C                   eval      xcmra = pr92o8/100 * cachrg
     C                   endif
     C                   eval      caanam = xcmra
     C                   endif
     C                   endif

     C*** Not Anesthesia
                         if        (cacpt4 < '00100' or cacpt4 > '01999')
                                   and cacpt4 <>'99100' and cacpt4 <>'99116'
                                   and cacpt4 <>'99135' and cacpt4 <>'99140'
       //*** Beg Add *** 02/08/21 *****************************************
                                   or (cacpt4 >=  '00100' and cacpt4 <= '01999')
                                   and %Check('0123456789':CAcpt4) > 0;
       //*** End Add *** 02/08/21 *****************************************
     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        pr92nw <> 0
     C                   eval      xcmra = pr92nw
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92nz <> 0
     C                   eval      xcmra = pr92nz/100 * cachrg
     C                   eval      camra = xcmra
     C*
     C* If paying a percent of charged for physician codes then
     C* divide by units so it pays correctly for multiple units
     C*
     C                   if        isModEnabled('Multiple Units') and
     C                             (cacpt4 >= '10000')
     C                   eval      CAmra = CaMra / CAunts
     C                   eval      xcMra = xcMra / CAunts
     C                   endif
     C                   endif
     C*Professional Component
     C                   if        prprof <> 0
     C                   eval      caprof = prprof
     C                   endif
     C                   if        pr92nk <> 0
     C                   eval      caprof = pr92nk/100 * cachrg
     C                   endif
     C*Technical Component
     C                   if        pr92n2 <> 0
     C                   eval      catech = pr92n2
     C                   endif
     C                   if        pr92nl <> 0
     C                   eval      catech = pr92nl/100 * cachrg
     C                   endif
     C                   endif
     C                   endif

     C                   when      %subst(eoflic:1:3) = '199'
     C*** HCPC code range A0021 - V5363
     C                   if        cacpt4 >= 'A0021' and cacpt4 <= 'V5363'

     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        pr92ny <> 0
     C                   eval      xcmra = pr92ny
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92o1 <> 0
     C                   eval      xcmra = pr92o1/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   if        capmod = 'UE'
     C                   if        pr92o3 <> 0
     C                   eval      xcmra = pr92o3
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92o5 <> 0
     C                   eval      xcmra = pr92o5/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C                   endif
     C                   if        capmod = 'RR'
     C                   if        pr92o7 <> 0
     C                   eval      xcmra = pr92o7
     C                   endif
     C                   if        pr92o9 <> 0
     C                   eval      xcmra = pr92o9/100 * cachrg
     C                   endif
     C                   eval      caanam = xcmra
     C                   endif
     C                   endif

     C*** Not Anesthesia
                         if        (cacpt4 < '00100' or cacpt4 > '01999')
                                   and cacpt4 <>'99100' and cacpt4 <>'99116'
                                   and cacpt4 <>'99135' and cacpt4 <>'99140'
       //*** Beg Add *** 02/08/21 *****************************************
                                   or (cacpt4 >=  '00100' and cacpt4 <= '01999')
                                   and %Check('0123456789':CAcpt4) > 0;
       //*** End Add *** 02/08/21 *****************************************
     C                   if        capmod <> 'RR' and capmod <> 'UE'
     C                   if        pr92ny <> 0
     C                   eval      xcmra = pr92ny
     C                   eval      camra = xcmra
     C                   endif
     C                   if        pr92o1 <> 0
     C                   eval      xcmra = pr92o1/100 * cachrg
     C                   eval      camra = xcmra
     C                   endif
     C*Professional Component
     C                   if        pr92ni <> 0
     C                   eval      caprof = pr92ni
     C                   endif
     C                   if        pr92nm <> 0
     C                   eval      caprof = pr92nm/100 * cachrg
     C                   endif
     C*Technical Component
     C                   if        pr92nj <> 0
     C                   eval      catech = pr92nj
     C                   endif
     C                   if        pr92nn <> 0
     C                   eval      catech = pr92nn/100 * cachrg
     C                   endif
     C                   endif
     C                   endif

     C                   endsl
     C*
     C                   endsr
    C**********************************************************************
    C* LoadOut sub routine                                                *
    C**********************************************************************
      /Free
           BegSr LoadOut;
               //*** Beg Add *** 02/29/24 *****************************
               // Anesthesia
               if (cacpt4 >= '00100' and cacpt4 <= '01999')
                  and %Check('0123456789':CAcpt4) = 0
                  or cacpt4 = '99100' or cacpt4 = '99116'
                  or cacpt4 = '99135' or cacpt4 = '99140';


                  monitor;
                  kkGZip = %dec(%subst(EOflic:1:3):3:0);
                  On-error;
                     kkGZip = *zeros;
                  Endmon;

                  // If paying POC do not set the Anesthesia flag
                  Select;
                  When (kkGzip = 197 or kkGzip = 198) and
                   PR92n8 > 0;
                     CAanes = *blank;
                     xcmra = pr92n8/100 * cachrg;
                     camra = xcmra;
                     CAanam = 0;
                     CAanMn = 0;
                  When kkGzip = 199 and PR92n9 > 0;
                     CAanes = *blank;
                     xcmra = pr92n9/100 * cachrg;
                     camra = xcmra;
                     CAanam = 0;
                     CAanMn = 0;
                  Other;
                     CAanes = PR1a2;
                     CAanam = PRanam;
                     CAanMn = DUunts;
                  Endsl;

                  // Load the anesthesia conversion factor based on zip
                  If CAanes = 'Y';
                     Setgt (kkGZip:CAfdat) pan01;
                     Readpe (kkGZip) pan01;
                     Dow not %eof(ftanel1de);
                        If %Subst(EOflic:1:3) = %Editc(ANEgzip:'X');
                           CAmra = ANEconv;
                           xcMra = CAmra;
                           Leave;
                        Endif;
                        Readpe (kkGZip) pan01;
                     Enddo;
                  Endif;
               Endif;
               //*** End Add *** 02/29/24 *****************************

             Select;

             when %subst(eoflic:1:3) = '197' or %subst(eoflic:1:3) = '198';

          //** Anesthesia code range 00100 - 01999

               //*** Beg Del *** 02/29/24 **********************************
               //***if (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
               //***   and %Check('0123456789':CAcpt4) = 0
       //*** End Add *** 02/08/21 *****************************************
               //***   or cacpt4 = '99100' or cacpt4 = '99116'
               //***   or cacpt4 = '99135' or cacpt4 = '99140';

               //***   eval xcmra = 0;
               //***   eval camra = xcmra;

               //***else;
               //*** End Del *** 02/29/24 **********************************

          //** All other codes

               if pr92n6 <> 0;
                     eval xcmra = pr92n6;
                     eval camra = xcmra;
               endif;

               if pr92n8 <> 0;
                     eval xcmra = pr92n8/100 * cachrg;
                     eval camra = xcmra;
               endif;

               //*** Beg Del *** 02/29/24 **********************************
               //***endif;
               //*** End Del *** 02/29/24 **********************************


             when %subst(eoflic:1:3) = '199';

          //** Anesthesia code range 00100 - 01999

               //*** Beg Del *** 02/29/24 **********************************
               //***if (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
               //***   and %Check('0123456789':CAcpt4) = 0
       //*** End Add *** 02/08/21 *****************************************
               //***   or cacpt4 = '99100' or cacpt4 = '99116'
               //***   or cacpt4 = '99135' or cacpt4 = '99140';

               //*** eval xcmra = 0;
               //*** eval camra = xcmra;

               //***else;
               //*** End Del *** 02/29/24 **********************************

          //** All other codes (non-Anesthesia)

               if pr92n6 <> 0;
                     eval xcmra = pr92n7;
                     eval camra = xcmra;
               endif;

               if pr92n8 <> 0;
                     eval xcmra = pr92n9/100 * cachrg;
                     eval camra = xcmra;
               endif;

               //*** Beg Del *** 02/29/24 **********************************
               //***endif;
               //*** End Del *** 02/29/24 **********************************

           endsl;

        endsr;
      /End-Free
    C**********************************************************************
    C* LoadAsc sub routine                                                *
    C**********************************************************************
      /Free
           BegSr LoadAsc;

             Select;

             when %subst(eoflic:1:3) = '197' or %subst(eoflic:1:3) = '198';

          //** Anesthesia code range 00100 - 01999

               if (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
                  and %Check('0123456789':CAcpt4) = 0
       //*** End Add *** 02/08/21 *****************************************
                  or cacpt4 = '99100' or cacpt4 = '99116'
                  or cacpt4 = '99135' or cacpt4 = '99140';


             //** check for ASTC POC

                  if pr92ns <> 0;

                     eval xcmra = (pr92ns/100) * cachrg;
                     eval camra = xcmra;

                  else;


                     eval xcmra = 0;
                     eval camra = xcmra;

                  endif;

               else;

          //** All other codes (non-Anesthesia)

               if pr92nq <> 0;
                     eval xcmra = pr92nq;
                     eval camra = xcmra;
               endif;

               if pr92ns <> 0;
                     eval xcmra = pr92ns/100 * cachrg;
                     eval camra = xcmra;
               endif;

               endif;


             when %subst(eoflic:1:3) = '199';

          //** Anesthesia code range 00100 - 01999

               if (cacpt4 >= '00100' and cacpt4 <= '01999')
       //*** Beg Add *** 02/08/21 *****************************************
                  and %Check('0123456789':CAcpt4) = 0
       //*** End Add *** 02/08/21 *****************************************
                  or cacpt4 = '99100' or cacpt4 = '99116'
                  or cacpt4 = '99135' or cacpt4 = '99140';


             //** check for ASTC POC

                  if pr92nt <> 0;

                     eval xcmra = (pr92nt/100) * cachrg;
                     eval camra = xcmra;

                  else;


                     eval xcmra = 0;
                     eval camra = xcmra;

                  endif;

               else;

          //** All other codes (non-Anesthesia)

               if pr92nr <> 0;
                     eval xcmra = pr92nr;
                     eval camra = xcmra;
               endif;

               if pr92nt <> 0;
                     eval xcmra = pr92nt/100 * cachrg;
                     eval camra = xcmra;
               endif;

               endif;

           endsl;

        endsr;
      /End-Free
     C*
      /Free
          //********************************************************************
          //*  Medicare Inpatient Subroutine                                   *
          //********************************************************************
       Begsr medicare_inpt;

       inrrn = dbrrn;
       outrtcd = *blanks;
       outAllow1 = 0;
       outAllow2 = 0;

          callp Rtv_inpat_Alw(inrrn:outrtcd:outAllow1:outAllow2);
          camra = outAllow1;

       EndSr;
      /End-Free
     Opca01     e            rca01
