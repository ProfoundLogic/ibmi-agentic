    H**********************************************************************
     H copyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    Hdebug(*yes) option(*srcstmt:*nodebugio)
    H**********************************************************************
    H* PROGRAM: F4000NTN                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR TN                           *
    H**********************************************************************
    H*  CREATED: 12/06/99  C:79   PROJECT: 48MDA                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  07/26/12 C:277 Project: TN016                                     *
    H*                 Apply changes of TN016                             *
    H*                                                                    *
     H*  04/21/10 C:198 Project: TN014                                     *
    H*                 Added logic for temporary codes.                   *
    H*                                                                    *
     H*  09/29/09 C:227 Project: TN013                                     *
    H*                 Add logic for DME procesing.                       *
    H*                                                                    *
     H*  08/25/09 C:079 Support: 69594                                     *
    H*                 Load mod RR and UE allowances                      *
    H*                 Load drug fees correctly                           *
    H*                                                                    *
     H*  12/03/08 C:189 Project: FS320                                     *
    H*                 2008 Q4 Rx FS - SC,TN, TX                          *
    H*                                                                    *
     H*  07/25/08 C:47  Project: FS319                                     *
    H*                 add cpt code range when loading camra from pn92nr  *
    H*                                                                    *
     H*  04/15/08 C:191 Project: TN008                                     *
    H*                 TN 2008 FS Changes                                 *
    H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  12/28/06 C:182 Support: 56185                                     *
    H*                 Added logic for TN Fee Schedule-J HCPCS            *
    H*                 code Allowances                                    *
     H*  06/20/06 C:79  Project: TN003                                     *
    H*                 Add logic for loading POS Rental,NU,Used amounts   *
    H*                 Exclude radiology/pathology from stop loss         *
    H*                 Load outpatient amount for type of bill 83x        *
    H*                 Load neurosurgeon rate for spec. NUE.              *
    H*                                                                    *
     H*  03/24/06 C:79  Support: MC010 52023                               *
    H*                 1. Use rev code range 100-219 for per diem         *
    H*                 2. Change to allow more than 1 room and board code *
    H*                 3. Change what codes are excluded from charges     *
    H*                                                                    *
     H*  07/03/05 C:79  Project: TN002                                     *
    H*                 Add logic for new fee schedule                     *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
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
    H*  10/05/01 C:79  Support: MC010 29131                               *
    H*                 Use CAFDAT for rules effective date for all        *
    H*                 bill types.                                        *
    H*                                                                    *
    H*  05/30/01 C:62  Project: FS228                                     *
    H*                 GET ZIPA FROM CAZIPA NOT FROM PROVIDER FILE.       *
    H*                                                                    *
    H*  03/28/01 C:62  Project: UC002                                     *
    H*                 1. Add logic to populate Prof/Tech components      *
    H*                                                                    *
    H*  02/02/00 C:79  Support: MC044/20183                               *
    H*                 1. Dont pass FUF, FUT arrays. F4000N loads         *
    H*                    these now.                                      *
    H*                                                                    *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
     F*** Beg Chg *** 07/26/12 *********************************************
     F***fdcal13   uf   e           k disk
     Ffdcal13   uf   e           k disk    infds(cafds)
     F*** End Chg *** 07/26/12 *********************************************
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
     Ffaeul     if   e           k disk
     Fftuml     if   e           k disk
     Ffmdmltn   if   e           k disk
     Ffmprltn   if   e           k disk    rename(ppr01:lpr01)
     F*** Beg Add *** 07/26/12 ****************************************
     Fftrvl3    if   e           k disk
     Fftrtl2    if   e           k disk
     Fftapl     if   e           k disk
     F*** End Add *** 07/26/12 ****************************************
    D*
     D drugs           s              5    dim(41) ctdata perrcd(1)
    D*
    D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D*** Beg Chg *** 07/26/12 ****************************************
     D*** mod             s              2    dim(4) ascend
     D mod             s              2    dim(4)
     D*** End Chg *** 07/26/12 ****************************************
     D l               s              1  0
     D j               s              1  0
     D k               s              1  0
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
     D*** Beg Add *** 07/26/12 ****************************************
     Dcafds            ds
     Dcarrn                  397    400b 0
     D*** End Add *** 07/26/12 ****************************************
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
     D camd82          s              3  2
     D catype          s              1
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D kkymd           s                   like(frefdt)
     D*** Beg Add *** 07/26/12 ****************************************
     D kkst            s                   like(rtst)
     D kktype          s                   like(rttype)
     D xxIdx           s              3  0
     D*** End Add *** 07/26/12 ****************************************
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
     D xxdme           s              1    inz('N')
     D totallchrg      s             11  2
     D xxsstay         s              1
     D xxloss          s              1
     D xxdays          s              3  0
     D I               s              2  0
     D Founddrug       s              1
     D*** Beg Add *** 07/26/12 ****************************************
     D ##rrn           s              9  0
     D ##call          s              1
     D ##eob           s              3
     D ##msg           s              7
     D ##modr          s              1
     D ##rnd           s              1
     D ##alw1          s              9  2
     D ##alw2          s              9  2
     D ##alw3          s              9  2
     D ##rtcd          s              1
     D***
     D* Calculate Medicare Physician API
     D Calc_Phy_Pmnt   pr                  extpgm('FA763RTN')
     D ##rrn                          9  0
     D ##call                         1
     D ##eob                          3
     D ##msg                          7
     D ##modr                         1
     D ##rnd                          1
     D ##alw1                         9  2
     D ##alw2                         9  2
     D ##alw3                         9  2
     D ##rtcd                         1
     D*** End Add *** 07/26/12 ****************************************
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
    C* Reasonable & Customary File
    C*
    C*  Procedure Code file
     C     prky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    cafdat
    C*
    C*  Procedure Code file
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
    C*
     C     dmky2         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    xxefdt            7 0
     C                   kfld                    xxmod1            2
     C                   kfld                    xxmod2            2
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
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*** Beg Add 07/26/12 *********************************************
     C*
     C     rtky1         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C                   kfld                    kkefdt
     C*
     C     rtky2         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
    C*
     C     rvky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C                   kfld                    rtmrel
     C*
     C     rvky2         klist
     C                   kfld                    cacpt4
     C*
     C     apky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C*
     C     apky2         klist
     C                   kfld                    cacpt4
     C*** End Add 07/26/12 *********************************************
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
     C                   close     facap
    C*
    C* Determine per diem and stop loss for inpatient hospital
    C*
     C                   if        eublty>='111' and eublty<='115'
     C                   open      facap
     C*
     C                   read      pcawk2                                 99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        casvcd >= 100 and casvcd <= 219
     C                   exsr      hospperdiem
     C                   update    pcawk2
     C                   leave
     C                   endif
     C*
     C                   read      pcawk2                                 99
     C*
     C                   enddo
     C*
     C                   close     facap
     C*
     C                   endif
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
     C                   eval      totallchrg = 0
     C                   eval      xxsstay = *blank
     C                   eval      caslos = *blank
     C                   eval      xxdays = 0
     C                   eval      xxloss = *blank
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
     C*** Beg Chg *** 07/26/12 ****************************************
     C***                if        eobtyp = 'N'
     C***                          and carcfl = 'C'
     C                   if        eobtyp = 'N'
     C                             and (carcfl = 'C' or carcfl = 'M')
     C*** End Chg *** 07/26/12 ****************************************
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
     C                   exsr      pmodldhos
     C                   endif
     C     euky          chain     peu01                              99
    C*
     C                   movel     'H'           catype
    C*
     C                   exsr      svcd
     C*** Beg Add *** 07/26/12 ****************************************
      /Free
          If cafdat < 1120610;
      /End-Free
     C*** End Add *** 07/26/12 ****************************************
     C                   if        euicn = 'AO' or euicn = 'AS' or
     C                             euicn = 'HO' or euicn = 'HS'
     C                   exsr      proc
     C                   eval      cahtos = euicn
     C                   eval      cacpt4 = durate
     C                   eval      caunts = duunts
     C                   move      dupos         caplac
     C                   endif
     C*** Beg Add *** 07/26/12 ****************************************
      /Free
          Endif;
      /End-Free
     C*** End Add *** 07/26/12 ****************************************
     C*
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
     C*** Beg Add *** 07/26/12 *********************************************
     C                             and cafdat < 1120610
     C*** End Add *** 07/26/12 *********************************************
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
    C** DENTAL CODE MRA SEARCH                             D E N T      *
     C     dent          begsr
    C*
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*
    C*  Default to use service date for effective date for rules
     C                   move      cafdat        kkefdt
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
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
    C*
     C                   if        cafdat>=1080304 and
     C                             ddpos <> '11'
     C                   eval      xcmra = pr92nu
     C                   eval      camra = pr92nu
     C                   else
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   endif                                                  cafdat<>1080304
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
    C** FOLLOW UP DAY LOAD SUBROUTINGE                                  *
    C** HOSPPERDIEM SUBROUTINE
     C     hospperdiem   begsr
     C*
     C                   select
     C*
     C                   when      eospcd = 'HOS'
     C*
     C                   if        xxsstay = 'Y'
     C*
     C*
     C                   if        xxdays <= 7
     C                   eval      camra = 1800 * xxdays
     C                   else
     C                   eval      camra = 1800 * 7 + ((xxdays - 7) * 1500)
     C                   endif
     C*
     C                   endif
     C*
     C                   if        xxsstay <> 'Y'
     C*
     C                   if        xxdays <= 7
     C                   eval      camra = 1500 * xxdays
     C                   else
     C                   eval      camra = 1500 * 7 + ((xxdays - 7) * 1250)
     C                   endif
     C*
     C                   endif
     C*
     C                   when      eospcd = 'REH'
     C*
     C                   if        xxdays <= 7
     C                   eval      camra = 1000 * xxdays
     C                   else
     C                   eval      camra = 1000 * 7 + ((xxdays - 7) * 800)
     C                   endif
     C*
     C                   when      eospcd = 'PSY'
     C*
     C                   eval      camra = 700 * xxdays
     C*
     C                   when      eospcd = 'TC '
     C*
     C                   eval      camra = 3000 * xxdays
     C*
     C                   other
     C                   eval      camra = 0
     C*
     C                   endsl
     C*
     C* Calculate stop loss if any
     C*
     C                   if        totallchrg - camra > 15000
     C                             and xxloss <> 'Y'
     C                   eval      camra = camra +
     C                             .80 * (totallchrg - (camra + 15000))
     C                   eval      caslos = 'Y'
     C                   eval      xxloss = 'Y'
     C                   endif
     C*
     C                   if        camra > totallchrg
     C                   eval      camra = totallchrg
     C                   endif
     C*
     C                   endsr
    C********************************************************************
    C** LOADINPHAMT SUBROUTINE                     L O A D I N P H A M T*
    C********************************************************************
     C     loadinphamt   begsr
     C*
     C* Surgical implants will be reimbursed separately.
     C* DME will be reimbursed separately
     C* Ambulance services will be reimbursed separately
     C* Orthotics/Prosthetics will be reimbursed separately
     C* Take home medications will be reimbursed separately
     C*
     C                   if        durate >= 'E0001' and durate <= 'E9999'
     C                             or durate >= 'L0001' and durate <= 'L9999'
     C                             or durvcd = 253
     C                             or durvcd >= 273 and durvcd <= 278
     C                             or durvcd >= 290 and durvcd <= 299
     C                             or durvcd >= 540 and durvcd <= 549
     C                             or durvcd >= 600 and durvcd <= 604
     C                             or durvcd >= 822 and durvcd <= 823
     C                             or durvcd = 832 or durvcd = 833
     C                             or durvcd = 842 or durvcd = 843
     C                             or durvcd = 852 or durvcd = 853
     C                             or durvcd >= 300 and durvcd <= 409
     C                             or durate >= '70000' and durate <= '89999'
     C                   else
     C                   add       cachrg        totallchrg
     C*
     C                   endif
     C*
     C                   if        durvcd >= 360 and durvcd <= 369
     C                             or durvcd >= 490 and durvcd <= 499
     C                             or durvcd >= 760 and durvcd <= 769
     C                             or euicn = 'IS'
     C                   eval      xxsstay = 'Y'
     C                   endif
     C*
     C* Accumulate room and board days
     C*
     C                   if        durvcd >= 100 and durvcd <= 219
     C                   eval      xxdays  = caunts + xxdays
     C                   endif
     C*
     C* Load FS amount for radiology
     C*
     C                   if        durvcd >= 320 and durvcd <= 329
    C*
     C                   eval      xxcpt4 = durate
     C*
     C     prky          setgt     ppr01
     C     nxtpr6        tag
     C     prky1         readpe    ppr01                                  99
     C                   if        *in99 = *off and cafdat >= prefdt and
     C                             prstat = 'A'
     C*
     C                   eval      cafacf = pr92ns
     C                   eval      catech = pr92n1
     C                   eval      camra  = pr92nr
     C                   eval      carpt  = prrpt
     C*
     C                   endif
     C*
     C                   endif
     C*
     C*
     C                   endsr
    C** LOADOUTHAMT SUBROUTINE                  L O A D O U T A M T     *
    C********************************************************************
     C     loadouthamt   begsr
    C*
     C                   eval      xxcpt4 = durate
     C*
     C     prky          setgt     ppr01
     C     nxtpr5        tag
     C     prky1         readpe    ppr01                                  99
     C                   if        *in99 = *off and cafdat >= prefdt and
     C                             prstat = 'A'
     C*
     C                   eval      cafacf = pr92ns
     C                   eval      catech = pr92n1
     C                   eval      camra  = pr92nr
     C                   eval      carpt  = prrpt
     C*
     C* PAY PT/OT RATES
     C*
     C                   if        durate >= '97001' and durate <= '98999'
     C*
     C                   eval      cappt1 = pr92n9
     C                   eval      cappt2 = pr92nb
     C                   eval      cappt3 = pr92nd
     C                   eval      cappt4 = pr92na
     C                   eval      cappt5 = pr92nc
     C                   eval      cappt6 = pr92ne
     C*
     C                   endif
     C*
     C                   else
     C                   if        prstat = 'I' and *in99 = *off
     C                   goto      nxtpr5
     C                   endif
     C*
     C                   endif
     C*
     C* PAY UCR FOR PATHOLOGY
     C*
     C                   if        durate >= '80000' and durate <= '89999'
     C                   exsr      recu
     C                   endif
     C
     C                   endsr
    C********************************************************************
    C** LOADSKNHAMT SUBROUTINE                  L O A D O U T A M T     *
    C********************************************************************
     C     loadsknhamt   begsr
    C*
     C                   eval      xxcpt4 = durate
     C*
     C     prky          setgt     ppr01
     C     nxtpr7        tag
     C     prky1         readpe    ppr01                                  99
     C                   if        *in99 = *off and cafdat >= prefdt and
     C                             prstat = 'A'
     C*
     C                   eval      cafacf = pr92oc
     C                   eval      catech = pr92n1
     C                   eval      camra  = pr92nr
     C                   eval      carpt  = prrpt
     C*
     C* PAY PT/OT RATES
     C*
     C                   if        durate >= '97001' and durate <= '98999'
     C*
     C                   eval      cappt1 = pr92n9
     C                   eval      cappt2 = pr92nb
     C                   eval      cappt3 = pr92nd
     C                   eval      cappt4 = pr92na
     C                   eval      cappt5 = pr92nc
     C                   eval      cappt6 = pr92ne
     C*
     C                   endif
     C*
     C                   else
     C                   if        prstat = 'I' and *in99 = *off
     C                   goto      nxtpr7
     C                   endif
     C*
     C                   endif
     C*
     C* PAY UCR FOR PATHOLOGY
     C*
     C                   if        durate >= '80000' and durate <= '89999'
     C                   exsr      recu
     C                   endif
     C
     C                   endsr
    C********************************************************************
    C** LOADAMT SUBROUTINE                              L O A D A M T   *
    C********************************************************************
     C     loadamt       begsr
     C*
     C                   select
     C*
     C* Surgery
     C*
     C                   when      prcpt4 >= '10000' and prcpt4 <= '69999'
     C*
     C                   eval      camra = prmra
     C*
     C                   if        eospcd = 'ORT' or eospcd = 'OR1' or
     C                             eospcd = 'OR2' or eospcd = 'NES'
     C                             or eospcd = 'NEU'
     C                   eval      camra = pr92n5
     C                   endif
     C*
     C* Radiology
     C*
     C                   when      prcpt4 >= '70001' and prcpt4 <= '79999'
     C*
     C                   eval      camra = pr92n6
     C*
     C* PT/OT
     C*
     C                   when      prcpt4 >= '97001' and prcpt4 <= '98999'
     C*
     C                   if        eospcd = 'CHI'
     C                   eval      camra = pr92n2
     C                   endif
     C*
     C                   eval      cappt1 = pr92n9
     C                   eval      cappt2 = pr92nb
     C                   eval      cappt3 = pr92nd
     C                   eval      cappt4 = pr92na
     C                   eval      cappt5 = pr92nc
     C                   eval      cappt6 = pr92ne
     C*
     C* Medicine-excluding psychiatry
     C*
     C                   when      prcpt4 >= '90000' and prcpt4 <= '90799'
     C                             or prcpt4 >= '90900' and prcpt4 <= '99999'
     C                             and prcpt4 <> '95883' and prcpt4 <> '96100'
     C                             and prcpt4 <> '96117'
ks4  C                             and  prcpt4 <> '90371'
     C*
     C                   if        dhpos <> ' 23'
     C                   eval      camra = pr92n3
     C                   else
     C                   eval      camra = pr92n4
     C                   endif
    C*
     C                   if        prcpt4 >= '90281' and prcpt4 <= '90749'
     C                             and pr92nr <> 0
    C*
     C                   eval      camra  = pr92nr
    C*
     C                   endif
     C
    C*
     C*
     C* Medicine- psychiatry
     C*
     C                   when      prcpt4 >= '90801' and prcpt4 <= '90899'
     C                             or prcpt4 = '95883' or prcpt4 = '96100'
     C                             or prcpt4 = '96117'
     C*
     C                   if        eospcd = 'PSY'
     C*
     C                   if        dhpos <> ' 23'
     C                   eval      camra = pr92n3
     C                   else
     C                   eval      camra = pr92n4
     C                   endif
     C*
     C                   else
     C                   eval      camra = pr92nr
     C                   endif
     C*
    C                   endsl
     C*
    C*
     C                   if        (prcpt4 >= 'J0120' and prcpt4 <= 'J9999') Or
     C                             (prcpt4 >= 'P9041' and prcpt4 <= 'P9048') Or
     C                             (prcpt4 >= 'Q0035' and prcpt4 <= 'Q9999')
     C                             or (prcpt4 >= 'A0001' and prcpt4 <= 'A9999')
    C*
     C                   eval      camra  = pr92nr
    C*
     C                   endif
     C
    C*
    C*
     C     loadamte      endsr
    C********************************************************************
    C** LOADAMT1 SUBROUTINE                              L O A D A M T   *
    C********************************************************************
     C     loadamt1      begsr
     C*
     C                   if        %subst(prcpt4:5:1) = 'T'
     C                   eval      carpt = 'T'
     C                   endif
     C*
     C                   if        %subst(prcpt4:5:1) = 'F'
     C                   eval      carpt = 'F'
     C                   endif
     C                   if        dhpos = ' 11'
     C                   select
     C* Chiro
     C                   when      prcpt4 >= '97001' and prcpt4 <= '98943'
     C                   eval      camra = pr92n2
     C* Surgery
     C                   when      prcpt4 >= '10000' and prcpt4 <= '69999'
     C                   eval      camra = prmra
     C*
     C* Surgery Ortho & Neuro
     C                   if        eospcd = 'ORT' or eospcd = 'OR1' or
     C                             eospcd = 'OR2' or eospcd = 'NES'
     C                             or eospcd = 'NEU'
     C                   eval      camra = pr92n5
     C                   endif
     C* dental
     C                   when      prcpt4 >= 'D0000' and prcpt4 <= 'D9999'
     C                   eval      camra = prmra
     C* General Medicine non-emergency
     C                   when      prcpt4 >= '90281' and prcpt4 <= '96999'
     C                             or prcpt4 >= '99000' and prcpt4 <= '99499'
ks4  C                             and  prcpt4 <> '90371'
     C                   if        dhpos <> ' 23'
     C                   eval      camra = pr92n3
     C                   else
     C* General Medicine emergency
     C                   eval      camra = pr92n4
     C                   endif
     C* PT/OT
     C                   when      prcpt4 >= '97001' and prcpt4 <= '97799'
     C                             or prcpt4 >= '97850' and prcpt4 <= '98929'
     C                   eval      camra = pr92nt
     C* Radiology
     C                   when      prcpt4 >= '70001' and prcpt4 <= '79999'
     C                   eval      camra = pr92n6
    C                   endsl
     C*
     C                   else                                                   dhpos = ' 11'
     C*
     C                   select
     C* Chiro
     C                   when      prcpt4 >= '97001' and prcpt4 <= '98943'
     C                   eval      camra = pr92ny
     C* Surgery
     C                   when      prcpt4 >= '10000' and prcpt4 <= '69999'
     C                   eval      camra = pr92nu
     C* Surgery Ortho & Neuro
     C                   if        eospcd = 'ORT' or eospcd = 'OR1' or
     C                             eospcd = 'OR2' or eospcd = 'NES'
     C                             or eospcd = 'NEU'
     C                   eval      camra = pr92nv
     C                   endif
     C* dental
     C                   when      prcpt4 >= 'D0000' and prcpt4 <= 'D9999'
     C                   eval      camra = pr92nu
     C* General Medicine non-emergency
     C                   when      prcpt4 >= '90281' and prcpt4 <= '96999'
     C                             or prcpt4 >= '99000' and prcpt4 <= '99499'
     C                   if        dhpos <> ' 23'
     C                   eval      camra = pr92nw
     C                   else
     C                   eval      camra = pr92nx
     C* General Medicine emergency
     C     'TC'          lookup    mod(1)                                 99
     C                   if        *in99 = *on
     C                   eval      catech = pr92oa
     C                   endif
    C*
     C     '26'          lookup    mod(1)                                 99
     C                   if        *in99 = *on
     C                   eval      caprof = pr92o8
     C                   endif
     C                   endif
     C* PT/OT
     C                   when      prcpt4 >= '97001' and prcpt4 <= '97799'
     C                             or prcpt4 >= '97850' and prcpt4 <= '98929'
     C                   eval      camra = pr92nz
     C* Radiology
     C                   when      prcpt4 >= '70001' and prcpt4 <= '79999'
     C                   eval      camra = pr92n6
    C                   endsl
     C*
     C                   endif                                                  dhpos = ' 11'
     C                   eval      I=1
     C                   eval      founddrug = *blank
    C*
     C                   dow       I <= 41
    C*
     C                   if        prcpt4 = drugs(I)
     C                   eval      founddrug = 'Y'
     C                   leave
     C                   endif
    C*
     C                   eval      i=i+1
    C*
     C                   enddo
    C*
     C                   if        (prcpt4 >= 'J0120' and prcpt4 <= 'J9999') Or
     C                             (prcpt4 >= 'P9041' and prcpt4 <= 'P9048') Or
     C                             (prcpt4 >= 'Q0035' and prcpt4 <= 'Q9999')
     C                             or (prcpt4 >= 'A0001' and prcpt4 <= 'A9999')
     C                             or founddrug = 'Y'
     C                   if        pr92nr <> 0
     C                   eval      camra  = pr92nr
     C                   endif
     C                   endif
    C*
     C                   if        prcpt4 >= 'K0001' and prcpt4 <= 'L9999'
    C*
     C     'RR'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = caanam
    C*
     C                   endif
    C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = caanam
    C*
     C                   endif
    C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = pr92n2
    C*
     C                   endif
    C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = pr92n2
    C*
     C                   endif
    C*
     C                   endif
    C*
     C     loadamt1e     endsr
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
     C***                eval(h)   xxawp = rx1awp / 1000
     C                   eval(h)   xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   else
      *** Begin Change *** 05/02/18 *********************
     C****               eval(h)   xxawp = rxawp / 1000
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
     C****               eval(h)   xxawp = rx1awp / 1000
     C                   eval(h)   xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   else
      *** Begin Change *** 05/02/18 *********************
     C****               eval(h)   xxawp = rxawp / 1000
     C                   eval(h)   xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      caunts = dpunts
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
    C** PROCEDURE CODE MODIFIER CALCULATIONS                  P M O D   *
    C********************************************************************
     C     pmod          begsr
     C                   eval      xxdme = ' '
     C     prky          setgt     lpr01                              99
     C     prky1         readpe    lpr01                                  99
     C                   eval      xxefdt = prefdt
     C                   eval      l = 0
     C*
     C                   dou       *in99 = *off
     C                   eval      l = l + 1
     C*
     C                   if        l  <  5
     C                   dow       mod(l) = ' '
     C                   eval      l = l + 1
     C                   if        l > 4
     C                   leave
     C                   endif
     C                   enddo                                                  mod(l) = ' '
     C                   endif                                                  l < 5
     C                   if        l >=5
     C                   leave
     C                   endif                                                  l >= 5
     C*
     C                   eval      j = 0
     C*
     C                   dow       j <> 5 and j + 1 <> 5
     C                   eval      j = j + 1
     C*
     C                   dow       mod(j) = ' '
     C                   eval      j = j + 1
     C                   if        j > 4
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        j <> l
     C*
     C                   if        l <> 5  and j <> 5
     C*
     C                   if        mod(l) <> ' ' and mod(j) <>  ' '
     C                   eval      xxmod1 = mod(l)
     C                   eval      xxmod2 = mod(j)
     C*
     C     dmky2         chain     pdm01                              99
     C                   if        *in99 = *off
     C                   eval      xxmra = dmmra
     C                   eval      camra = dmmra
     C                   eval      xxmod2 = ' '
     C                   eval      xxdme = 'Y'
     C                   leave
     C                   endif                                                  if *in99 = *off
     C                   endif                                                  mod(l) & mod(j)<>' '
     C*
     C                   endif                                                  l<>5 & j<>5
     C*
     C                   endif                                                  j <> l
     C*
     C                   enddo                                                  j <> 5
     C*
     C                   enddo                                                  dou *in99 = *off
     C*
     C                   if        xxdme <> 'Y'
     C*
     C                   eval      xxmod1 = ' '
     C                   select
     C                   when      mod(1) <> ' ' and mod(2) = ' ' and mod(3) =
     C                             ' ' and mod(4) = ' '
     C                   eval      xxmod1 = mod(1)
     C                   when      mod(1) = ' ' and mod(2) <> ' ' and mod(3) =
     C                             ' ' and mod(4) = ' '
     C                   eval      xxmod1 = mod(2)
     C                   when      mod(1) = ' ' and mod(2) = ' ' and mod(3) <>
     C                             ' ' and mod(4) = ' '
     C                   eval      xxmod1 = mod(3)
     C                   when      mod(1) = ' ' and mod(2) = ' ' and mod(3) =
     C                             ' ' and mod(4) <> ' '
     C                   eval      xxmod1 = mod(4)
     C                   endsl
     C                   eval      xxmod2 = ' '
     C     dmky2         chain     pdm01                              99
     C                   if        *in99 = *off
     C                   eval      xxmra = dmmra
     C                   eval      camra = dmmra
     C                   eval      xxdme = 'Y'
     C                   endif                                                  if *in99 = *off
     C                   endif                                                  if xxdme <> 'Y'
     C                   if        xxdme <> 'Y'
     C*
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
     C     'RR'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = caanam
    C*
     C                   endif
    C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = pr92n2
    C*
     C                   endif
    C* To make correct payment based on Components
    C*
     C     '26'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = caprof
     C                   eval      xxmra = camra
    C*
     C                   endif
    C*
     C     'TC'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = catech
     C                   eval      xxmra = camra
    C*
     C                   endif
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
    C********************************************************************
    C** MODIFIER LOADING                            P M O D L D H O S   *
    C********************************************************************
     C     pmodldhos     begsr
    C*
     C                   move      dupmod        camod1
     C                   move      dhmod2        camod2
    C*
     C                   move      dupmod        mod(1)
     C                   move      dumod2        mod(2)
    C*
     C                   movea     mod           mod1
    C*
     C                   endsr
    C*
    C** PROCEDURE CODE MRA SEARCH                             P R O C   *
    C********************************************************************
     C     proc          begsr
    C*
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*
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
     C                   if        *in60 = *on
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
     C                   eval      catech = pr92n1
     C                   eval      caapsi = pr1a4
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
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
     C                   move      dhpos         caplac
     C                   move      dhprcd        cacpt4
     C                   eval      caunts = dhunts
    C*
     C                   if        pr1a2 = 'Y'
     C                             or dhprcd >= '00100'
     C                             and dhprcd <= '01999'
     C                             and %subst(prcpt4:5:1) <>'F'
     C                             and %subst(prcpt4:5:1) <>'T'
     C                   eval      caanmn = dhunts
     C                   endif
     C                   if        cafdat >= 1050701 and cafdat < 1080304
     C                   exsr      loadamt
     C                   endif
     C*** Beg Chg *** 07/26/12 *****************************************
     C***                if        cafdat >= 1080304
     C                   if        cafdat >= 1080304 and cafdat < 1120610
     C*** End Chg *** 07/26/12 *****************************************
     C                   exsr      loadamt1
     C                   endif                                                  cafdat>=1080304
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
    C*
    C*   MODIFIERS
    C*
     C                   if        pr1a9 = '1'
     C                   eval      camd50 = 1.00
     C                   endif
     C                   if        pr1a8 = '2'
     C                   eval      camd51 = 0.50
     C                   endif
     C                   if        pr1a8 = '3'
     C                   eval      camd51 = 0.50
     C                   endif
     C                   eval      camd54 = pr52n2
     C                   eval      camd55 = pr52n3
     C                   eval      camd56 = pr52n1
     C                   if        pr1ab = '2'
     C                   eval      camd62 = 6.25
     C                   endif
     C                   if        pr1ac = '2'
     C                   eval      camd66 = 1.00
     C                   endif
     C                   if        pr1aa = '2'
     C                   eval      camd80 = 1.00
     C                   eval      camd81 = 1.00
     C                   eval      camd82 = 1.00
     C                   endif
    C*
     C                   endif
     C*** Beg Add *** 07/26/12 *****************************************
      /free
          If cafdat >= 1120610;
             exsr Medicare_phy;
          Endif;
      /end-free
     C*** End Add *** 07/26/12 *****************************************
     C     prstat        cabeq     'I'           nxtpr2
    C*
     C     proce         endsr
    C********************************************************************
    C** REASONABLE AND CUSTOMARY FILE SEARCH                  R E C U   *
    C********************************************************************
     C     recu          begsr
    C*
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
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
     C                   else
     C                   move      rcfsr         cafsr
     C                   eval      cafolu = rcfolu
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
    C*
    C*
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
     C                             and %subst(prcpt4:5:1) <>'F'
     C                             and %subst(prcpt4:5:1) <>'T'

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
    C* REASONABLE AND CUSTOMARY DO NOT CONTAIN A VALID EFFECTIVE
    C* DATE FOR THE PROCEDURE CODE
    C*
    C*
     C                   endif
     C     rcstat        cabeq     'I'           nxrc1
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
     C                   move      dupos         caplac
     C*
     C*** Beg Chg *** 07/26/12 ****************************************
     C***                if        cafdat >= 1050701
     C                   if        cafdat >= 1050701 and cafdat < 1120610
     C*** End Chg *** 07/26/12 ****************************************
     C*
     C                   select
     C*
     C                   when      eublty >= '111' and eublty <= '115'
     C                   exsr      loadinphamt
     C*
     C                   when      eublty >= '131' and eublty <= '135'
     C                             or eublty >= '831' and eublty <= '839'
     C                   exsr      loadouthamt
     C*
     C                   when      eublty >= '23A' and eublty <= '239'
     C                   exsr      loadsknhamt
     C*
     C                   endsl
     C*
     C                   endif
     C*** Beg Add *** 07/26/12 ****************************************
      /free
          If cafdat >= 1120610;
             kktype = eobtyp;
             kkst = eoast;

             Select;

             When eublty >= '111' and eublty <= '115';
             exsr medicare_inpt;

             when eublty >= '131' and eublty <= '135'
                  or eublty >= '831' and eublty <= '839';
             exsr medicare_outp;

             when eublty >= '23A' and eublty <= '239';
             exsr medicare_skn;

             Endsl;

          Endif;
      /end-free
     C*** End Add *** 07/26/12 ****************************************
    C*
     C     svcde         endsr
     C*** Beg Add *** 07/26/12 ********************************************
      /free
          //*********************************************************
          // Subroutine Medicare_phy
          //*********************************************************
          Begsr Medicare_phy;

             ##rrn  = carrn;
             ##call = 'A';
             ##modr = *blanks;
             ##rnd  = 'M';

             callp Calc_phy_pmnt(##rrn:##call:##eob:##msg:##modr:
                   ##rnd:##alw1:##alw2:##alw3:##rtcd);

             kkefdt = cafdat;
             castat = 'A';

             Select;

             When %subst(cacpt4:5:1) = 'T';
             carpt  = 'T';

             When %subst(cacpt4:5:1) = 'F';
             carpt  = 'F';

             other;
             carpt  = 'N';
             Endsl;

             cafsr  = 'Y';
             caplac = dhpos ;
             cacpt4 = dhprcd;
             capart = dharea;
             caunts = dhunts;

             If  cacpt4 >= 'A0000' and cacpt4 <= 'Z9999';
                caanes = 'U';
             Endif;

             If  cacpt4 >= '00100' and cacpt4 <= '01999'
                and %subst(cacpt4:5:1) <> 'T' and
                %subst(cacpt4:5:1) <> 'F'
                or cacpt4 = '99100'
                or cacpt4 = '99116'
                or cacpt4 = '99135'
                or cacpt4 = '99140';
                caanmn = dhunts;
                caanes = 'Y';
                caanam = ##alw2;
                   If caanam <> 0;
                      camra  = ##alw1;
                   Else;
                      camra  = 0;
                      ##alw1 = 0;
                   Endif;
             Endif;
           //Chain to ftrvp so that we can load follow up days, indicators etc.
             kkst = eoast;
             kktype = 'M';
             Setgt rtky1 prt01;
             Readpe rtky2 prt01;
             Dow not %eof(ftrtl2);
                If rtstat = 'I';
                   Readpe rtky2 prt01;
                Else;
                   Leave;
                Endif;
             Enddo;

             If not %Eof(ftrtl2);
                If cacpt4 >= '00100' and cacpt4 <= '01999'
                   and %subst(cacpt4:5:1) <> 'T' and
                   %subst(cacpt4:5:1) <> 'F'
                   or cacpt4 = '99100'
                   or cacpt4 = '99116'
                   or cacpt4 = '99135'
                   or cacpt4 = '99140';
                Else;
                   Select;

                   When casrc = 'L';
                   ##alw1 = ##alw1 * rtlbcf;

                   When casrc = 'F';
                   ##alw1 = ##alw1 * 1.0;

                   When casrc = 'A';
                   ##alw1 = ##alw1 * rtamcf;

                   When casrc = 'D' and %subst(cacpt4:1:1) <> 'L';
                   ##alw1 = ##alw1 * rtdmcf;

                   When casrc = 'D' and %subst(cacpt4:1:1) = 'L';
                   ##alw1 = ##alw1 * rtpocf;

                   When casrc = 'R';
                   ##alw2 = rtmul2 * 1;
                      Select;

                      When cacpt4 >= '10000' and cacpt4 <= '69999' and
                           (eospcd = 'ORT' or eospcd = 'NEU');
                      ##alw1 = ##alw1 * rtoncf;

                      When cacpt4 >= '10000' and cacpt4 <= '69999' and
                           (eospcd <> 'ORT' or eospcd <> 'NEU');
                      ##alw1 = ##alw1 * rtgscf;

                      When cacpt4 >= '70000' and cacpt4 <= '79999';
                      ##alw1 = ##alw1 * rtrdcf;

                      When cacpt4 >= '98940' and cacpt4 <= '98943';
                      ##alw1 = ##alw1 * rtchcf;

                      When cacpt4 >= '90000' and cacpt4 <= '99999';
                         Select;
                         When (cacpt4 >= '97001' and cacpt4 <= '97006') or
                              (cacpt4 >= '97010' and cacpt4 <= '97028') or
                              (cacpt4 >= '97032' and cacpt4 <= '97039') or
                              (cacpt4 >= '97110' and cacpt4 <= '97456') or
                              (cacpt4 >= '97750' and cacpt4 <= '97799') or
                              (cacpt4 >= '97800' and cacpt4 <= '98999');
                         ##alw1 = ##alw1 * rtptcf;

                         Other;
                            Select;
                            When dhpos <> ' 23';
                            ##alw1 = ##alw1 * rtmdcf;

                            When dhpos = ' 23';
                            ##alw1 = ##alw1 * rtemcf;
                            Endsl;
                         Endsl;
                      Endsl;

                   Endsl;
                Endif;
             Endif;
             camra = ##alw1;
             xcmra = ##alw1;

           //Professional and Technical Component
             xxIdx = 0;
             xxIdx = %lookup('26':mod);
             If xxIdx <> 0;
             caprof = ##alw1;
             xxmra = camra;
             Endif;

             xxIdx = 0;
             xxIdx = %lookup('TC':mod);
             If xxIdx <> 0;
             catech = ##alw1;
             xxmra = camra;
             Endif;

             Setgt rvky1 prv01;
             Readpe rvky2 prv01;
                Dow not %Eof(ftrvl3);
                   If rvstat = 'I';
                      Readpe rvky2 prv01;
                   Else;
                      Leave;
                   Endif;
                Enddo;

             If not %Eof(ftrvl3);

                camd50 = 0.00;
                camd51 = 0.00;
                camd54 = 0.00;
                camd55 = 0.00;
                camd56 = 0.00;
                camd62 = 0.00;
                camd66 = 0.00;
                camd80 = 0.00;
                camd81 = 0.00;
                camd82 = 0.00;
                caenba = rvenba;
                If  %check(' 0123456789': rvglob) > 0;
                Else;
                  cafolu = %int(rvglob);
                Endif;
                ca50in = rvbila;
                ca51in = rvmult;
                ca62in = rvcosg;
                ca66in = rvtesg;
                ca80in = rvasst;

                   If ca50in = '1';
                      camd50= 1.5;
                   Endif;

                   If ca51in = '1';
                      camd51= 0.25;
                   Endif;

                   If ca51in = '2';
                      camd51= 0.5;
                   Endif;

                   If ca51in = '3';
                      camd51= 0.5;
                   Endif;

                   If ca62in = '1' or ca62in = '2';
                      camd62 = 6.25;
                   Endif;
    
                   If ca66in = '1' or ca66in = '2';
                      camd66 = 1.00;
                   Endif;
    
                   If  ca80in = '2';
                      camd80 = 0.16;
                      camd81 = 0.25;
                      camd82 = 0.13;
                   Endif;

                   camd54 = rvintr/100;
                   camd55 = rvpost/100;
                   camd56 = rvpre/100;

             Endif;
          Endsr;
         //*****************************************************************
         //MEDICARE INPUT SUBROUTINE                  L O A D I N P H A M T*
         //*****************************************************************
          Begsr Medicare_inpt;

             If durate >= 'E0001' and durate <= 'E9999'
                or durate >= 'L0001' and durate <= 'L9999'
                or durvcd = 253
                or durvcd >= 273 and durvcd <= 278
                or durvcd >= 290 and durvcd <= 299
                or durvcd >= 540 and durvcd <= 549
                or durvcd >= 600 and durvcd <= 604
                or durvcd >= 822 and durvcd <= 823
                or durvcd = 832 or durvcd = 833
                or durvcd = 842 or durvcd = 843
                or durvcd = 852 or durvcd = 853
                or durvcd >= 300 and durvcd <= 409
                or durate >= '70000' and durate <= '89999';
             Else;
                totallchrg = cachrg + totallchrg;

             Endif;

             If durvcd >= 360 and durvcd <= 369
                or durvcd >= 490 and durvcd <= 499
                or durvcd >= 760 and durvcd <= 769
                or euicn = 'IS';
                xxsstay = 'Y';
             Endif;

           //Accumulate room and board days

             If durvcd >= 100 and durvcd <= 219;
                xxdays  = caunts + xxdays;
             Endif;

             kkst = eoast;
             kktype = 'M';
             Setgt rtky1 prt01;
             Readpe rtky2 prt01;
             Dow not %eof(ftrtl2);
                If rtstat = 'I';
                   Readpe rtky2 prt01;
                Else;
                   Leave;
                Endif;
             Enddo;

           //Load FS amount for radiology

             ##rrn  = carrn;
             ##call = 'F';
             ##modr = *blanks;
             ##rnd  = 'M';

             callp Calc_phy_pmnt(##rrn:##call:##eob:##msg:##modr:
                   ##rnd:##alw1:##alw2:##alw3:##rtcd);

             kkefdt = cafdat;
             castat = 'A';
             cafacf = pr92ns;
             catech = pr92n1;
             camra  = pr92nr;
             carpt  = prrpt;

             Select;

             When durate >= 'L0000' and durate <= 'L9999';
             ##alw1 = ##alw1 * rtpocf;

             When durate >= '70000' and durate <= '79999';
             ##alw1 = ##alw1 * rtrdcf;
             Endsl;
             camra = ##alw1;
             xcmra = ##alw1;

          Endsr;
         //*****************************************************************
         //MEDICARE OUTPUT SUBROUTINE                 L O A D I N P H A M T*
         //*****************************************************************
          Begsr Medicare_outp;

             kkefdt = cafdat;
             Setgt apky1 pap01;
             Readpe apky2 pap01;
             Dow not %Eof(ftapl);
                If apstat = 'I';
                   Readpe apky2 pap01;
                Else;
                   Leave;
                Endif;
             Enddo;
             caapsi = apsi;

             kkst = eoast;
             kktype = 'O';
             Setgt rtky1 prt01;
             Readpe rtky2 prt01;
             Dow not %eof(ftrtl2);
                If rtstat = 'I';
                   Readpe rtky2 prt01;
                Else;
                   Leave;
                Endif;
             Enddo;
          camra = apalow * rtopcf;
          xcmra = apalow * rtopcf;

          Endsr;
         //*****************************************************************
         //MEDICARE SKN SUBROUTINE                    L O A D I N P H A M T*
         //*****************************************************************
          Begsr Medicare_skn;

             kkefdt = cafdat;
             Setgt apky1 pap01;
             Readpe apky2 pap01;
             Dow not %Eof(ftapl);
                If apstat = 'I';
                   Readpe apky2 pap01;
                Else;
                   Leave;
                Endif;
             Enddo;

             kkst = eoast;
             kktype = 'M';
             Setgt rtky1 prt01;
             Readpe rtky2 prt01;
             Dow not %eof(ftrtl2);
                If rtstat = 'I';
                   Readpe rtky2 prt01;
                Else;
                   Leave;
                Endif;
             Enddo;
          camra = apalow * 1.0;
          xcmra = apalow * 1.0;

          Endsr;
      /end-free
     C*** End Add *** 07/26/12 ********************************************
    C* ==================================================================
     Opca01     e            rca01
** DRUGS
90371
90375
90376
90385
90585
90586
90632
90633
90645
90647
90648
90655
90656
90657
90658
90660
90669
90675
90691
90700
90702
90703
90704
90705
90706
90707
90713
90714
90715
90716
90717
90718
90721
90732
90733
90735
90740
90743
90744
90746
90747
