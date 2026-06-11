     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
    H**********************************************************************
    H* PROGRAM: F4000NP2                                                  *
     H*                                                                    *
    H* DESCRIPTION: ADJUDICATION WORKFILE BUILD FOR P2                    *
    H**********************************************************************
    H*  CREATED: 10/06/11  C:251  PROJECT: 114MZO                         *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
     H*  07/18/23 C:342 Project: NY060                                     *
    H*                 NY Compliance: CARC/RARC Update                    *
    H*                                                                    *
     H*  04/27/18 C:180 Project: C114MJS                                   *
    H*                 Increase xxWAP size.                               *
    H*                                                                    *
     H*   05/30/16 C:309 Project: FS497                                    *
     H*                  Changed field ##si size to 2A and                 *
     H*                  populated "CAapcSI" with ##si                     *
     H*                                                                    *
     H*   04/10/12 C:251 Support: 78981 MC114                              *
    H*                  Rectify rule date logic for DOS < 07/01/10        *
    H*                                                                    *
     H*   10/17/11 C:251 Project: 114MAR                                   *
     H*                  Add call for IRF API FA767R                       *
     H*                                                                    *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H*                                                                    *
    H*                                                                    *
    H**********************************************************************
    F*
     Ffdcalh    uf   e           k disk    infds(cafds) usropn
     Ffddul     if   e           k disk
     Fftuml     if   e           k disk
     Ffddhl     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffmrxl     if   e           k disk
     Ffaeolg    if   e           k disk    infds(eofds)
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*
     Ffdrul1    if   e           k disk
     Ffdfrl5    if   e           k disk
     Fftrvl3    if   e           k disk
     Fftrtl2    if   e           k disk
     Ffaeul     if   e           k disk
    D*
     Dcafds            ds
     Dcarrn                  397    400b 0
     D*
     Deofds            ds
     Deorrn                  397    400b 0
     D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D xxmd62          s              4  3
     D camd82          s              3  2
     D                 ds
     D xxcmsv                        11
     D  xxsvcd                        3  0 overlay(xxcmsv)
     D  xxcpt4                        5    overlay(xxcmsv:4)
     D  xxunit                        3  0 overlay(xxcmsv:9)
    D*
    D*  Date/Time Data Structure.
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D psds          esds                  extname(szq1p)
    D*
    D*  Common service code date structure
    D*
     D                 ds
     D k1cmsv                        11
     D  k1rvcd                        3  0 overlay(k1cmsv)
     D  k1hcpc                        5    overlay(k1cmsv)
     D  k1unts                        3    overlay(k1cmsv)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D cafolu          s              3  0
     D camd50          s              3  2
     D camd51          s              3  2
     D camd54          s              3  2
     D camd55          s              3  2
     D camd56          s              3  2
     D camd62          s              3  2
     D camd66          s              3  2
     D camd80          s              3  2
     D camd81          s              3  2
     D catype          s              1
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D kkymd           s              7  0
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
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0
     D xcmra           s              9  2
     D xppmod          s                   like(capmod)
      *** Begin Change *** 04/27/18 *********************
     D*** xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 04/27/18 *********************
     D xxefdt          s                   like(cafdat)
     D xxfdat          s                   like(cafdat)
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D kkst            s                   like(rtst)
     D kktype          s                   like(rttype)
     D*
     D*** Beg Add *** 10/17/11 ************************************************
     D ##irfb          s                   like(cairfb)
     D ##outl          s                   like(caoutl)
     D ##lip           s                   like(calip)
     D ##tch           s                   like(catch)
     D #1code          s                   like(durate)
     D*** End Add *** 10/17/11 ************************************************
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*  Claim Detail File by date to setup follow up days
    C*
     C     caky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    kkymd
     C                   kfld                    qqfein
     C                   kfld                    qqmod
    C*
    C*  Claim Detail File read by EOB#
     C     cakyh         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkymd
    C*  Claim Detail File read by EOB#
     C     cakyh1        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
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
    C*  NDC Drug File
     C     rxky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxndc
    C*
    C*  Procedure Code file
     C     prky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
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
     C     svky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
     C                   kfld                    cafdat
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
     C     p2ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    xxcmsv
     C                   kfld                    kkefdt
    C*
     C     p2ky1         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    xxcmsv
    C*
     C     rtky1         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    kkefdt
     C*
     C     rtky2         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C*
     C     rvky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C                   kfld                    rtmrel
     C*
     C     rvky2         klist
     C                   kfld                    cacpt4
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
     C                   open      facap
    C*
     C                   exsr      convrt
    C*
    C*  END PROGRAM AND CLOSE ALL FILES
     C                   close     facap
    C*
     C                   close     fdcalh
     C     term          tag
    C*
     C                   eval      xcmra = 0
    C*
     C                   return
    C*
     C                   move      *on           *inlr
    C********************************************************************
    C*  Convert fields into base fields used for determining payment
    C*  creates workfile with MRA and modifier amounts and prof/tech
    C*****************************************************************
    C*
     C     convrt        begsr
    C*
     C                   eval      x = 0
     C                   clear                   date
    C*
     C     eoky          chain     peu01                              99
    C* Find the recon number.  If this is a recon.
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      xxrcn = eorcn
    C*
     C                   else
     C                   eval      xxrcn = *zero
    C*
     C                   endif                                                  *in60 = *off
    C*
     C                   eval      kkymd = *zero
     C*
     C*** call FA756R
     C                   if        euicn = 'IP'
     C                             or euicn = 'IM'
     C                             or euicn = 'IR'
     C                             or euicn = 'IS'
     C*
     C                   move      eorrn         ##rrn#
     C*
     C                   call      'FA756R'
     C                   parm                    ##rrn#            9 0           RRN of FAEOP
     C                   parm                    ##rtcd            1
     C                   parm                    ##allow           9 2
     C                   parm      'F'           ##call#           1
     C**** Beg Chg *** 07/18/23 **************************************
     C****               parm                    ##eob#            3
     C                   parm                    ##eob#            6
     C**** End Chg *** 07/18/23 **************************************
     C                   parm                    ##msg#            7
     C                   parm                    ##code            3
     C*
     C                   endif                                                  euicn = 'I/P'
     C*
     C                   open      fdcalh
    C*
    C* Main read of detail charge file
     C     cakyh         setll     pca01
     C     cakyh1        reade     pca01                                  54
    C*
     C                   dow       *in54 = *off
    C*
     C                   if        carcn = xxrcn
    C*
     C                   clear                   pdh01
     C                   clear                   pdu01
     C                   clear                   pdd01
     C                   clear                   pdp01
     C                   clear                   mod
     C                   clear                   mod1
    C*
     C                   eval      kkdseq = cadseq
     C                   eval      xxfdat = cafdat
    C*
    C*   If line item is in the R&C file call subroutine RECU
    C*   Else - treat line item as a procedure code from the CPT4 file
     C                   if        eobtyp = 'N'
     C                             and carcfl = 'C'
     C                             or eobtyp='N' and carcfl = 'M'
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
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 1.00
     C                   eval      camd80 = 1.00
     C                   eval      camd81 = 1.00
    C*
     C*** Beg Del *** 04/10/12 *******************************************
     C*******            if        cafdat >= 1100701
     C*** End Del *** 04/10/12 *******************************************
    C*
     C                   exsr      medicarePhy
    C*
     C*** Beg Del *** 04/10/12 *******************************************
     C******             endif
     C*** End Del *** 04/10/12 *******************************************
    C*
     C                   else
    C*
     C                   if        eobtyp = 'D'
     C                             and carcfl = 'M'
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
     C                   exsr      medicarePhy
    C*
     C                   else
    C*
    C*   Else - treat line item as a pharmacy code
     C                   if        eobtyp = 'X'
    C*
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
    C*   Else - treat line item as a service code
     C     duky          chain     pdu01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      casvcd = durvcd
     C                   move      durate        cacpt4
     C                   move      durate        xxcpt4
     C                   exsr      pmodldhos
     C                   endif
    C*
     C                   movel     'H'           catype
    C*
     C                   select
     C                   when      eospcd = 'ASC'
     C                   exsr      medicareASC
     C                   when      CASRC  = 'H'
     C                   exsr      Snf_Pmnt
     C*** Beg Add *** 10/17/11 *****************************************************
     C                   when      CASRC  = 'C'
     C                   exsr      Irf_Pmnt
     C*** End Add *** 10/17/11 *****************************************************
     C                   other
     C                   exsr      svcd
     C                   endsl
    C*
     C                   endif                                                  eobtyp = 'X'
    C*
     C                   endif                                                  eobtyp = 'D'
    C*
     C                   endif                                                  eobtyp = 'P'
    C*
    C* Initialize work fields
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
     C                   if        xxmra = 0
     C                   eval      carpt = 'Y'
     C                   else
     C                   z-add     xxmra         caamt                92
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
    C*   Get the initial effective date of F4000R for this state
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
     C                   if        xxfdat >= xxymd
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
     C                   write     pcawk2
    C*
    C* Clear FACAP to prevent carryover of values to next record.
    C*
    C*
    C*    Clear CAPAID in detail file to reprocess correctly.
    C*    Provider bills is supposed to do this before adjudication
     C                   if        cacscd = '1  '
     C                             or cacscd = '001'
    C*
     C                   clear                   capaid
     C                   eval      camraa = xcmra
     C                   update    pca01
    C*
     C                   endif
    C*
     C                   clear                   pcawk2
     C                   endif
    C*
     C                   endif
    C*
     C     cakyh1        reade     pca01                                  54
    C*
     C                   enddo
    C*
     C     convre        endsr
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
      *** Begin Change *** 04/27/18 **********************
     C***                eval      xxawp = rx1awp / 1000
     C                   eval      xxawp = rx1awp
      *** End  Change *** 04/27/18 ***********************
    C*
    C* Use current average wholesale price(AWP)
     C                   else
      *** Begin Change *** 04/27/18 **********************
     C***                eval      xxawp = rxawp / 1000
     C                   eval      xxawp = rxawp
      *** End  Change *** 04/27/18 ***********************
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
    C*
    C* Adjust MRA depending on MOD
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
    C** MODIFIER LOADING                             P M O D L D H O S  *
    C********************************************************************
     C     pmodldhos     begsr
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
     C                   endsr
    C*
    C********************************************************************
    C** SERVICE CODE SEARCH                                   S V C D   *
    C********************************************************************
     C     svcd          begsr
    C*
     C                   eval      kkefdt = cafdat
     C*** Chain to ftrvp so that we can load follow up days, indicators etc.
     C                   movel     eoast         kkst
     C                   movel     'O'           kktype
     C     rtky1         setgt     prt01
     C     rtky2         readpe    prt01                                  99
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky2         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   select
     C                   when      euicn = 'HO'
     C                             or euicn = 'HS'
     C                             or euicn = 'AO'
     C                             or euicn = 'AS'
     C*
     C                   move      carrn         ##rrn
     C*
     C                   call      'FA762R'
     C                   parm                    ##rrn             9 0
     C                   parm      'F'           ##call            1
     C***** Beg Chg *** 05/30/16 *********************************
     C*****************  parm                    ##si              1
     C                   parm                    ##si              2
     C***** End Chg *** 05/30/16 *********************************
     C                   parm                    ##eob             3
     C                   parm                    ##msg             7
     C                   parm                    ##modr            1
     C                   parm                    ##alow            9 2
     C                   parm                    ##rtcd            1
     C                   parm                    ##rsch            9 2
     C                   parm                    ##outl            9 2
     C*** load fdcap fields
     C                   if        *in99 = *off
     C                   if        cacpt4 >= '00100' and cacpt4 <= '01999'
     C                              and %subst(cacpt4:5:1) <> 'T' and
     C                              %subst(cacpt4:5:1) <> 'F'
     C                              or cacpt4 = '99100'
     C                              or cacpt4 = '99116'
     C                              or cacpt4 = '99135'
     C                              or cacpt4 = '99140'
     C                   else
     C                   eval(h)   ##alow = rtmul1 * ##alow
     C                   endif
     C                   endif
     C                   move      ##alow        camra
     C                   move      ##alow        xcmra
     C***** Beg Chg *** 05/30/16 *********************************
     C***************    movel     ##si          caapsi
     C                   movel     ##si          caapcsi
     C***** End Chg *** 05/30/16 *********************************
     C                   movel     'A'           castat
     C*** set carpt to 'T' ot 'F' when last char of proc code is 'T' or 'F' respectively
     C                   select
     C                   when       %subst(cacpt4:5:1) = 'T'
     C                   movel     'T'           carpt
     C                   when       %subst(cacpt4:5:1) = 'F'
     C                   movel     'F'           carpt
     C                   other
     C                   movel     'N'           carpt
     C                   endsl
     C                   movel     'Y'           cafsr
     C                   move      dupos         caplac
     C                   movel     durate        cacpt4
     C                   eval      casvcd = durvcd
     C                   move      duunts        caunts
     C*
     C                   when      euicn = 'IP'
     C                             or euicn = 'IR'
     C                             or euicn = 'IM'
     C                             or euicn = 'IS'
     C*
     C                   move      *zeros        camra
     C                   move      *zeros        xcmra
     C                   movel     'A'           castat
     C                   movel     'N'           carpt
     C                   movel     'Y'           cafsr
     C                   move      dupos         caplac
     C                   movel     durate        cacpt4
     C                   eval      casvcd = durvcd
     C                   move      duunts        caunts
     C                   endsl
    C*
     C                   eval      cahtos = euicn
    C*
     C                   if        durvcd = 100
     C                   eval      camra = capaid
     C                   endif
    C*
     C     svcde         endsr
     C*---------------------------------------------------------------------
     C     MedicareASC   begsr
     C*
     C                   move      carrn         ##rrn
     C*
     C                   call      'FA765R'
     C                   parm                    ##rrn             9 0
     C                   parm      'F'           ##call            1
     C                   parm      'M'           ##type            1
     C                   parm                    ##pi              2
     C                   parm                    ##eob             3
     C                   parm                    ##msg             7
     C                   parm                    ##alw1            9 2
     C                   parm                    ##alw2            9 2
     C                   parm                    ##alw3            9 2
     C                   parm                    ##alw4            9 2
     C                   parm                    ##rtcd            1
     C*
     C                   move      cafdat        kkefdt
     C                   movel     ##pi          caaspi
     C                   movel     'A'           castat
     C                   select
     C                   when       %subst(cacpt4:5:1) = 'T'
     C                   movel     'T'           carpt
     C                   when       %subst(cacpt4:5:1) = 'F'
     C                   movel     'F'           carpt
     C                   other
     C                   movel     'N'           carpt
     C                   endsl
     C                   movel     'Y'           cafsr
     C                   move      dupos         caplac
     C                   movel     durate        cacpt4
     C                   move      duunts        caunts
     C*
     C                   if        cacpt4 >= 'A0000' and cacpt4 <= 'Z9999'
     C                   movel     'U'           caanes
     C                   endif
     C*
     C                   movel     eoast         kkst
     C                   movel     'M'           kktype
     C     rtky1         setgt     prt01
     C     rtky2         readpe    prt01                                  99
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky2         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C                   if        cacpt4 >= '00100' and cacpt4 <= '01999'
     C                              and %subst(cacpt4:5:1) <> 'T' and
     C                              %subst(cacpt4:5:1) <> 'F'
     C                              or cacpt4 = '99100'
     C                              or cacpt4 = '99116'
     C                              or cacpt4 = '99135'
     C                              or cacpt4 = '99140'
     C                   else
     C                   eval(h)   ##alw1 = rtmul1 * ##alw1
     C                   endif
     C                   endif
     C*
     C                   move      ##alw1        camra
     C                   move      ##alw1        xcmra
     C*
     C                   endsr
     C*---------------------------------------------------------------------
     C     MedicarePhy   begsr
     C*
     C                   move      carrn         ##rrn
     C*
     C                   call      'FA763R'
     C                   parm                    ##rrn             9 0
     C                   parm      'F'           ##call            1
     C                   parm                    ##eob             3
     C                   parm                    ##msg             7
     C                   parm      ' '           ##modr            1
     C                   parm      'M'           ##rnd             1
     C                   parm                    ##alw1            9 2
     C                   parm                    ##alw2            9 2
     C                   parm                    ##alw3            9 2
     C                   parm                    ##rtcd            1
     C*
     C                   move      cafdat        kkefdt
     C                   movel     'A'           castat
     C                   select
     C                   when       %subst(cacpt4:5:1) = 'T'
     C                   movel     'T'           carpt
     C                   when       %subst(cacpt4:5:1) = 'F'
     C                   movel     'F'           carpt
     C                   other
     C                   movel     'N'           carpt
     C                   endsl
     C                   movel     'Y'           cafsr
     C                   move      dhpos         caplac
     C                   movel     dhprcd        cacpt4
     C                   movel     dharea        capart
     C                   move      dhunts        caunts
     C                   if        cacpt4 >= 'A0000' and cacpt4 <= 'Z9999'
     C                   movel     'U'           caanes
     C                   endif
     C                   if        cacpt4 >= '00100' and cacpt4 <= '01999'
     C                              and %subst(cacpt4:5:1) <> 'T' and
     C                              %subst(cacpt4:5:1) <> 'F'
     C                             or cacpt4 = '99100'
     C                             or cacpt4 = '99116'
     C                             or cacpt4 = '99135'
     C                             or cacpt4 = '99140'
     C                   eval      caanmn = dhunts
     C                   eval      caanes = 'Y'
     C                   eval      caanam = ##alw2
     C                   if        caanam <> 0
     C                   eval      camra  = ##alw1
     C                   else
     C                   eval      camra  = 0
     C                   eval      ##alw1 = 0
     C                   endif
     C                   endif
     C*
     C*** Chain to ftrvp so that we can load follow up days, indicators etc.
     C                   movel     eoast         kkst
     C                   movel     'M'           kktype
     C     rtky1         setgt     prt01
     C     rtky2         readpe    prt01                                  99
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky2         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C                   if        cacpt4 >= '00100' and cacpt4 <= '01999'
     C                              and %subst(cacpt4:5:1) <> 'T' and
     C                              %subst(cacpt4:5:1) <> 'F'
     C                              or cacpt4 = '99100'
     C                              or cacpt4 = '99116'
     C                              or cacpt4 = '99135'
     C                              or cacpt4 = '99140'
     C                   else
     C                   eval(h)   ##alw1 = rtmul1 * ##alw1
     C                   endif
     C                   endif
     C                   move      ##alw1        camra
     C                   move      ##alw1        xcmra
     C     rvky1         setgt     prv01
     C     rvky2         readpe    prv01                                  99
     C                   dow       *in99 = *off
     C                   if        rvstat = 'I'
     C     rvky2         readpe    prv01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*
     C                   eval      camd50 = 0.00
     C                   eval      camd51 = 0.00
     C                   eval      camd54 = 0.00
     C                   eval      camd55 = 0.00
     C                   eval      camd56 = 0.00
     C                   eval      camd62 = 0.00
     C                   eval      camd66 = 0.00
     C                   eval      camd80 = 0.00
     C                   eval      camd81 = 0.00
     C                   eval      camd82 = 0.00
     C                   eval      caenba = rvenba
     C                   move      rvglob        cafolu
     C                   eval      ca50in = rvbila
     C                   eval      ca51in = rvmult
     C                   eval      ca62in = rvcosg
     C                   eval      ca66in = rvtesg
     C                   eval      ca80in = rvasst
     C*
     C                   if        ca50in = '1'
     C                   eval      camd50= 1.5
     C                   endif
     C*
     C                   if        ca51in = '1'
     C                   eval      camd51= 0.25
     C                   endif
     C*
     C                   if        ca51in = '2'
     C                   eval      camd51= 0.5
     C                   endif
     C*
     C                   if        ca51in = '3'
     C                   eval      camd51= 0.5
     C                   endif
     C*
     C                   if        ca62in = '1' or ca62in = '2'
     C                   eval      camd62 = 6.25
     C                   eval      xxmd62 = 0.625
     C                   endif
    C*
     C                   if        ca66in = '1' or ca66in = '2'
     C                   eval      camd66 = 1.00
     C                   endif
    C*
     C                   if        ca80in = '2'
     C                   eval      camd80 = 0.16
     C                   eval      camd81 = 0.25
     C                   eval      camd82 = 0.13
     C                   endif
     C*
     C                   eval      camd54 = rvintr/100
     C                   eval      camd55 = rvpost/100
     C                   eval      camd56 = rvpre/100
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   endsr
     C*---------------------------------------------------------------------
     C*---------------------------------------------------------------------
     C     Snf_Pmnt      begsr
     C*
     C                   move      carrn         ##rrn
     C*
     C                   call      'FA772R'
     C                   parm                    ##rrn             9 0
     C                   parm      'F'           ##call            1
     C                   parm                    ##alw1            9 2
     C                   parm                    ##alw2            9 2
     C                   parm                    ##alw3            9 2
     C                   parm                    ##rtcd            1
     C*
     C                   move      cafdat        kkefdt
     C                   movel     'A'           castat
     C                   movel     'N'           carpt
     C                   movel     'Y'           cafsr
     C                   move      dupos         caplac
     C                   movel     durate        cacpt4
     C                   eval      casvcd = durvcd
     C                   move      duunts        caunts
     C*
     C                   movel     eoast         kkst
     C                   movel     'S'           kktype
     C*
     C     rtky1         setgt     prt01
     C     rtky2         readpe    prt01                                  99
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky2         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   eval(h)   ##alw1 = rtmul1 * ##alw1
     C*
     C                   move      ##alw1        camra
     C                   move      ##alw1        xcmra
     C*
     C                   endsr
     C*** Beg Add *** 10/17/11 *****************************************************
     C*---------------------------------------------------------------------
     C*---------------------------------------------------------------------
     C     Irf_Pmnt      Begsr
     C*
     C                   eval      ##irfb = *zeros
     C                   eval      ##outl = *zeros
     C                   eval      ##tch = *zeros
     C                   eval      ##lip = *zeros
     C*
     C                   move      carrn         ##rrn
     C*
     C                   call      'FA767R'
     C                   parm                    ##rrn
     C                   parm      'F'           ##call
     C                   parm                    ##alw1
     C                   parm                    ##alw2
     C                   parm                    ##alw3
     C                   parm                    ##outl
     C                   parm                    ##irfb
     C                   parm                    ##tch
     C                   parm                    ##lip
     C                   parm      *blanks       #1code
     C                   parm                    ##rtcd
     C*
     C                   move      cafdat        kkefdt
     C                   movel     'A'           castat
     C                   movel     'N'           carpt
     C                   movel     'Y'           cafsr
     C                   move      dupos         caplac
     C                   movel     durate        cacpt4
     C                   eval      casvcd = durvcd
     C*
     C                   if        #1code <> *blanks
     C*
     C                   movel     #1code        cacpt4
     C                   eval      k1rvcd = durvcd
     C                   eval      k1hcpc = durate
     C                   eval      k1unts = *blanks
     C                   eval      cacode = k1cmsv
     C*
     C                   endif
     C*
     C                   eval      cairfb = ##irfb
     C                   eval      caoutl = ##outl
     C                   eval      catch  = ##tch
     C                   eval      calip  = ##lip
     C*
     C                   move      duunts        caunts
     C*
     C                   movel     eoast         kkst
     C                   movel     'R'           kktype
     C*
     C     rtky1         setgt     prt01
     C     rtky2         readpe    prt01                                  99
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky2         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   eval(h)   ##alw1 = rtmul1 * ##alw1
     C*
     C                   move      ##alw1        camra
     C                   move      ##alw1        xcmra
     C*
     C                   endsr
     C*** End Add *** 10/17/11 *****************************************************
