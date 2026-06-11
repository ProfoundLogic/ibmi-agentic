    H**********************************************************************
    H* (C)COPYRIGH
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NCT                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR CT                           *
    H**********************************************************************
    H*  CREATED: 08/24/04  C:79   PROJECT: CT001 FROM F4000NUC            *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  08/20/15 C:079 Project: CT011                                     *
    H*                 Pay modifiers KM and KN for L-codes                *
    H*                                                                    *
     H*  11/25/13 C:274 Project: CT009                                     *
    H*                 Add fee rate schedule for CT.                      *
    H*                                                                    *
     H*  08/16/11 C:274 Project: CT007                                     *
    H*                 Add fee rate schedule for CT.                      *
    H*                                                                    *
     H*  08/31/09 C:227 Project: CT005                                     *
    H*                 Include processing for DMEPOS modifiers.           *
    H*                                                                    *
     H*  02/05/09 C:079 Project: CT004                                     *
    H*                 PC/TC percents are not calculated from FTUMP for   *
    H*                 non UCR bills.                                     *
    H*                                                                    *
     H*  01/13/09 C:220 Project: CT004                                     *
    H*                 CT 2008 FS                                         *
    H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
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
    H*  05/30/01 C:62  Project: FS228                                     *
    H*                 GET ZIPA FROM CAZIPA NOT FROM PROVIDER FILE.       *
    H*                                                                    *
    H*  05/08/00 C:79  Project: FSMNT                                     *
    H*                 1. Add logic to populate Prof/Tech components      *
    H*                                                                    *
    H*  02/02/00 C:79  Support: MC044/20183                               *
    H*                 1. Dont pass FUF, FUT arrays. F4000N loads         *
    H*                    these now.                                      *
    H*                                                                    *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
    F*** Beg Chg *** 08/16/11 ****************************************
     F*****fdcal13   uf   e           k disk
    Ffdcal13   uf   e           k disk    infds(cafds)
    F*** End Chg *** 08/16/11 ****************************************
     Ffaeolg    if   e           k disk
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*** BEGIN DEL *** 05/30/01 **************************************
    F**********FMP1L   IF  E           K        DISK
    F**********FMGZL   IF  E           K        DISK
    F*** END   DEL *** 05/30/01 **************************************
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
    F*** BEG ADD *** 05/08/00 ****************************************
     Fftuml     if   e           k disk
    F*** END ADD *** 05/08/00 ****************************************
    F*** BEG ADD *** 08/31/09 ****************************************
     Ffmdmlct   if   e           k disk
     Ffmprlct   if   e           k disk    rename(ppr01:lpr01)
     Ffaeul     if   e           k disk
    F*** END ADD *** 08/31/09 ****************************************
    F*** BEG ADD *** 08/16/11 ****************************************
    Fftrtl2    if   e           k disk
    F*** END ADD *** 08/16/11 ****************************************
    D*
     D qqf             s              7    dim(50)
    D*
    D*** BEG DEL *** 02/02/00 ****************************************
    D******************* FUF       250  7 0             FOLLOW UP FRM DY
    D******************* FUT       250  7 0             FOLLOW UP TO DAY
    D*** END DEL *** 02/02/00 ****************************************
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
    D*** BEG ADD *** 08/31/09 ****************************************
     D i               s              1  0
     D j               s              1  0
     D k               s              1  0
    D*** END ADD *** 08/31/09 ****************************************
    D*
    D*** Beg Add *** 08/16/11 ****************************************
    Dcafds            ds
    Dcarrn                  397    400b 0
    D*** End ADD *** 08/16/11 ****************************************
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
     D*** BEG ADD *** 10/17/02 *********************************************
      *** Begin Change *** 05/02/18 *********************
     D***xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 05/02/18 *********************
     D*** END ADD *** 10/17/02 *********************************************
     D xxdcs#          s              5  0
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D*** BEG ADD *** 08/16/11 *********************************************
     D xxtype          s              1
     D*** END ADD *** 08/16/11 *********************************************
     D xxymd           s                   like(ruefdt)
     D*** BEG ADD *** 08/31/09 ****************************************
     D xxdme           s              1    inz('N')
     D*** END ADD *** 08/31/09 ****************************************
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
    C*** BEGIN DEL *** 05/30/01 **************************************
    C*
    C* Reasonable & Customary File
    C********** GZKY      KLIST
    C**********           KFLD           QQCO#
    C**********           KFLD           XXFRZP
    C*** END   DEL *** 05/30/01 **************************************
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
     C                   kfld                    kkymd
    C*** BEG ADD *** 05/08/00 ****************************************
    C*
    C*  Usual and Customary Modifier Percent File
     C     umkey         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
    C*** END ADD *** 05/08/00 ****************************************
    C*** BEGIN DEL *** 05/30/01 **************************************
    C*
    C*  Provider Master File
    C********** P1KEY     KLIST
    C**********           KFLD           QQFEIN
    C**********           KFLD           QQMOD
    C*** END   DEL *** 05/30/01 **************************************
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
    C*
    C*** BEG ADD *** 08/16/11 ****************************************
    C*  Fee Schedule Rate Table.
     C     rtky1         klist
     C                   kfld                    qqst
     C                   kfld                    xxtype
     C*
     C     rtky2         klist
     C                   kfld                    qqst
     C                   kfld                    xxtype
     C                   kfld                    cafdat
    C*** END ADD *** 08/16/11 ****************************************
    C*
    C*** BEG ADD *** 08/31/09 ****************************************
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    cafdat
     C*
     C     prky2         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C*
     C     dmky2         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    xxefdt            7 0
     C                   kfld                    xxmod1            2
     C                   kfld                    xxmod2            2
    C*** END ADD *** 08/31/09 ****************************************
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
    C*** BEG DEL *** 02/02/00 ****************************************
    C******************** PARM           FUF
    C******************** PARM           FUT
    C*** END DEL *** 02/02/00 ****************************************
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
    C*** BEGIN DEL *** 05/30/01 **************************************
    C********** *LIKE     DEFN GZFRZP    XXFRZP
    C********** *LIKE     DEFN GZZIPA    XXZIPA
    C*** END   DEL *** 05/30/01 **************************************
    C*
    C*
    C*
    C*
     C                   move      qqdcs#        xxdcs#
    C*** BEG DEL *** 02/02/00 ****************************************
    C******************** MOVE *ZERO     FUF
    C******************** MOVE *ZERO     FUT
    C******************** Z-ADD1         U       50
    C*** END DEL *** 02/02/00 ****************************************
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
     C*** BEG DEL *** 10/02/02 *********************************************
     C****************** movel     qqspcd        caspcd
     C*** END DEL *** 10/02/02 *********************************************
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
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   clear                   mod
     C                   clear                   mod1
     C*** END ADD *** 10/17/02 *********************************************
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
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      dhprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   exsr      pmodld
     C*** END ADD *** 10/17/02 *********************************************
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
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      dhprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   exsr      pmodld
     C*** END ADD *** 10/17/02 *********************************************
     C                   endif
    C*
     C                   movel     'P'           catype
    C*
    C*** BEG ADD *** 08/16/11 *********************************************
     C                   if        casrc='D' and cafdat>=1110715
     C                   exsr      MedDME
     C                   else
    C*** END ADD *** 08/16/11 *********************************************
     C                   exsr      proc
    C*** BEG ADD *** 08/16/11 *********************************************
     C                   endif
    C*** END ADD *** 08/16/11 *********************************************
     C                   else
    C*
    C* Else treat line item as a dental code
    C* Chain out to the Dental Bill Detail file
     C                   if        eobtyp = 'D'
    C*** BEG ADD *** 08/22/02 *********************************************
     C                             and carcfl = 'C'
    C*** END ADD *** 08/22/02 *********************************************
     C     ddky          chain     pdd01                              60
    C*
     C                   if        *in60 = *off
     C                   move      ddprcd        cacpt4
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      ddprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C                   endif
    C*
     C                   movel     'D'           catype
    C*
     C                   exsr      dent
    C*
     C                   else
    C*
    C*** BEG ADD *** 08/22/02 *********************************************
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
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      durate        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C*** BEG ADD *** 08/31/09 *********************************************
     C                   exsr      pmodldhos
     C*** END ADD *** 08/31/09 *********************************************
     C                   endif
    C*
     C                   movel     'H'           catype
    C*
     C                   exsr      svcd
     C*** BEG ADD *** 08/31/09 *********************************************
     C                   if        euicn = 'AO' or euicn = 'AS' or
     C                             euicn = 'HO' or euicn = 'HS'
     C                   exsr      proc
     C                   eval      cahtos = euicn
     C                   eval      cacpt4 = durate
     C                   eval      caunts = duunts
     C                   move      dupos         caplac
     C                   endif
     C*
     C*** END ADD *** 08/31/09 *********************************************
    C*
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   endif
    C*** END ADD *** 08/22/02 *********************************************
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
     C*** BEG DEL *** 10/17/02 *********************************************
    C******************
    C****************** move      dhmod1        camod1
    C****************** move      dhmod2        camod2
    C****************** move      dhmod3        camod3
    C****************** move      dhmod4        camod4
    C******************
    C****************** move      dhmod1        mod(1)
    C****************** move      dhmod2        mod(2)
    C****************** move      dhmod3        mod(3)
    C****************** move      dhmod4        mod(4)
    C******************
    C****************** movea     mod           mod1
     C*** END DEL *** 10/17/02 *********************************************
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
     C*** BEG ADD *** 10/02/02 *********************************************
     C                   movel     qqspcd        caspcd
     C*** END ADD *** 10/02/02 *********************************************
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
    C*** BEG ADD *** 08/13/02 ****************************************
     C*** BEG DEL *** 10/02/03 ****************************************
     C****************** clear                   pcawk2
     C*** END DEL *** 10/02/03 ****************************************
    C*** END ADD *** 08/13/02 ****************************************
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
     C*** BEG ADD *** 10/02/03 ****************************************
     C                   clear                   pcawk2
     C*** END ADD *** 10/02/03 ****************************************
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
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     ddprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
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
    C*** BEG DEL *** 02/02/00 ****************************************
    C********** CAFOLU    IFGT 0
    C**********           EXSR FOLU
    C**********           ENDIF                           END CAFOLU
    C*** END DEL *** 02/02/00 ****************************************
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n2
    C*** BEG ADD *** 08/31/09 ****************************************
     C                   if        eobtyp = 'H'
     C                   IF        DUPOS <>  ' 11'
     C                   eval      xcmra = pr92n3
     C                   eval      camra = pr92n3
     C                   eval      caprof = pr92n4
     C                   eval      catech = pr92n5
     C                   ENDIF
     C                   else
    C*** END ADD *** 08/31/09 ****************************************
    C*** BEG ADD *** 01/13/09 ****************************************
     C                   IF        DHPOS <>  ' 11'
     C                   eval      xcmra = pr92n3
     C                   eval      camra = pr92n3
     C                   eval      caprof = pr92n4
     C                   eval      catech = pr92n5
     C                   ENDIF
    C*** END ADD *** 01/13/09 ****************************************
    C*** BEG ADD *** 08/31/09 ****************************************
     C                   endif
    C*** END ADD *** 08/31/09 ****************************************
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a3         canct
     C*** Beg Add *** 11/25/13 *********************************************
     C                   if        pr1a4 = 'Y'
     C                   eval      canct = 'Y'
     C                   endif
     C*** End Add *** 11/25/13 *********************************************
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
    C*** BEG DEL *** 02/02/00 ****************************************
    C******************** MOVE *ZERO     FUF
    C******************** MOVE *ZERO     FUT
    C******************** Z-ADD1         U       50
    C*** END DEL *** 02/02/00 ****************************************
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
    C* Brand-name Pharmaceuticals
     C                   if        rxptyp = '01'
     C                             or rxptyp = '07'
    C*
    C* Determine if fill date is less than AWP effective date if so
    C* use previous AWP unit price else use current AWP unit price
     C                   if        cafdat < rxawpd
    C*
     C*** BEG CHG *** 10/17/02 *********************************************
     C****************** eval(h)   camra = rx1awp
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rx1awp
     C                   eval(h)   xxawp = rx1awp/1000
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 10/17/02 *********************************************
    C*
     C                   else
     C*** BEG CHG *** 10/17/02 *********************************************
     C****************** eval(h)   camra = rxawp
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rxawp/1000
     C                   eval(h)   xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 10/17/02 *********************************************
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
     C*** BEG CHG *** 10/17/02 *********************************************
     C****************** eval(h)   camra = rx1awp
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rx1awp/1000
     C                   eval(h)   xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 10/17/02 *********************************************
    C*
     C                   else
     C*** BEG CHG *** 10/17/02 *********************************************
     C****************** eval(h)   camra = rxawp
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rxawp/1000
     C                   eval(h)   xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 10/17/02 *********************************************
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      caunts = dpunts
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   eval      camra = xxawp * caunts
     C*** BEG CHG *** 05/09/07 *********************************************
     C****************** eval(h)   caawp = caunts * xxawp
     C                   eval      caawp = xxawp
     C*** END CHG *** 05/09/07 *********************************************
     C*** END ADD *** 10/17/02 *********************************************
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
     C*** BEG ADD *** 08/31/09 ******************************************
     C                   eval      xxdme = ' '
     C     prky1         setgt     lpr01                              99
     C     prky2         readpe    lpr01                                  99
     C                   eval      xxefdt = prefdt
     C                   eval      i = 0
     C*
     C                   dou       *in99 = *off
     C                   eval      i = i + 1
     C*
     C                   if        i  <  5
     C                   dow       mod(i) = ' '
     C                   eval      i = i + 1
     C                   if        i > 4
     C                   leave
     C                   endif
     C                   enddo                                                  mod(i) = ' '
     C                   endif                                                  i < 5
     C                   if        i >=5
     C                   leave
     C                   endif                                                  i >= 5
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
     C                   if        j <> i
     C*
     C                   if        i <> 5  and j <> 5
     C*
     C                   if        mod(i) <> ' ' and mod(j) <>  ' '
     C                   eval      xxmod1 = mod(i)
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
     C                   endif                                                  mod(i) & mod(j)<>' '
     C*
     C                   endif                                                  i<>5 & j<>5
     C*
     C                   endif                                                  j <> i
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
     C*
     C                   if        xxdme <> 'Y'
     C*** Beg Add *** 08/20/15 *******************************************
     C                             or PRmra <> 0
    C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = PR92N4
     C                   eval      CAmRA = xxMRA
    C*
     C                   endif
    C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = PR92N5
     C                   eval      CAmRA = xxMRA
    C*
     C                   endif
     C*** End Add *** 08/20/15 *******************************************
     C*
     C*** END ADD *** 08/31/09 ******************************************
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
     C*** BEG ADD *** 01/13/09 **********************************************
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
     C*** END ADD *** 01/13/09 **********************************************
     C*** BEG ADD *** 08/31/09 *********************************************
     C                   endif
     C*** END ADD *** 08/31/09 *********************************************
     C     pmode         endsr
     C*** BEG ADD *** 10/17/02 *********************************************
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
     C*** BEG ADD *** 08/31/09 *********************************************
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
     C*** END ADD *** 08/31/09 *********************************************
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
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     dhprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
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
    C*** BEG DEL *** 02/02/00 ****************************************
    C********** CAFOLU    IFGT 0
    C**********           EXSR FOLU
    C**********           ENDIF                           END CAFOLU
    C*** END DEL *** 02/02/00 ****************************************
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caamt = prmra
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n2
    C*** BEG ADD *** 08/31/09 ****************************************
     C                   if        eobtyp = 'H'
     C                   IF        DUPOS <>  ' 11'
     C                   eval      xcmra = pr92n3
     C                   eval      camra = pr92n3
     C                   eval      caprof = pr92n4
     C                   eval      catech = pr92n5
     C                   ENDIF
     C                   if        pr1a2 = 'Y'
     C                             or durate >= '00100'
     C                             and durate <= '01999'
     C                   eval      caanmn = duunts
     C                   endif
     C                   else
    C*** END ADD *** 08/31/09 ****************************************
    C*** BEG ADD *** 01/13/09 ****************************************
     C                   IF        DHPOS <>  ' 11'
     C                   eval      xcmra = pr92n3
     C                   eval      camra = pr92n3
     C                   eval      caamt = pr92n3
     C                   eval      caprof = pr92n4
     C                   eval      catech = pr92n5
     C                   ENDIF
    C*** END ADD *** 01/13/09 ****************************************
    C*** BEG ADD *** 08/31/09 ****************************************
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
     C                   ENDIF
    C*** END ADD *** 08/31/09 ****************************************
    C*** BEG ADD *** 05/08/00 ****************************************
    C*** BEG DEL *** 02/05/09 ****************************************
    C******************
    C*  Load the professional and technical component amount based on
    C*  percentages in FTUMP.
    C******************
    C*****umkey******** setgt     pum01
    C****************** readp     pum01                                  60
    C******************
    C****************** if        *in60 = *off
    C******************           and cacpt4 <= umtocp
    C******************
    C****************** move      umod26        xxmd26
    C****************** move      umod27        xxmd27
    C******************
    C****************** if        caprof = 0
    C****BEG*ADD*****01/13/09 ****************************************
    C****************** if        dhpos  = ' 11'
    C****END*ADD*****01/13/09 ****************************************
    C****************** eval      caprof = prmra * xxmd26
    C****BEG*ADD*****01/13/09 ****************************************
    C****************** else
    C****************** eval      caprof = pr92n3 * xxmd26
    C****************** endif
    C****END*ADD*****01/13/09 ****************************************
    C****************** endif
    C******************
    C****************** if        catech = 0
    C****BEG*ADD*****01/13/09 ****************************************
    C****************** if        dhpos  = ' 11'
    C****END*ADD*****01/13/09 ****************************************
    C****************** eval      catech = prmra * xxmd27
    C****BEG*ADD*****01/13/09 ****************************************
    C****************** else
    C****************** eval      catech = pr92n3 * xxmd27
    C****************** endif
    C****END*ADD*****01/13/09 ****************************************
    C****************** endif
    C******************
    C****************** endif
    C*** END DEL*** 02/05/09 ****************************************
    C*
    C*** END ADD *** 05/08/00 ****************************************
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   eval      caasc = pr92n1
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a3         canct
     C*** Beg Add *** 11/25/13 *********************************************
     C                   if        pr1a4 = 'Y'
     C                   eval      canct = 'Y'
     C                   endif
     C*** End Add *** 11/25/13 *********************************************
     C                   move      pr1ad         casepp
     C*** Beg Del *** 11/25/13 *********************************************
     C************       move      pr1a4         capcf
     C*** End Del *** 11/25/13 *********************************************
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
     C*** BEG CHG *** 08/31/09 *********************************************
     C*** This has been moved above where eobtyp is being checked **********
     C**********************************************************************
     C******             move      dhpos         caplac
     C******             move      dhprcd        cacpt4
     C******             eval      caunts = dhunts
    C*
     C******             if        pr1a2 = 'Y'
     C******                       or dhprcd >= '00100'
     C******                       and dhprcd <= '01999'
     C******             eval      caanmn = dhunts
     C******             endif
    C*
     C******             move      dharea        capart
     C******             move      dhrcfl        carcfl
     C*** BEG CHG *** 08/31/09 *********************************************
    C*** BEG DEL *** 05/09/00 ****************************************
    C**********
    C****MODIFIERS
    C**********
    C********** PR1A9     IFEQ '1'
    C**********           Z-ADD1.00      CAMD50  32
    C**********           ENDIF
    C********** PR1A8     IFEQ '2'
    C**********           Z-ADD0.50      CAMD51  32
    C**********           ENDIF
    C********** PR1A8     IFEQ '3'
    C**********           Z-ADD0.50      CAMD51  32
    C**********           ENDIF
    C**********           Z-ADDPR52N2    CAMD54  32
    C**********           Z-ADDPR52N3    CAMD55  32
    C**********           Z-ADDPR52N1    CAMD56  32
    C********** PR1AB     IFEQ '2'
    C**********           Z-ADD6.25      CAMD62  32
    C**********           ENDIF
    C********** PR1AC     IFEQ '2'
    C**********           Z-ADD1.00      CAMD66  32
    C**********           ENDIF
    C********** PR1AA     IFEQ '2'
    C**********           Z-ADD1.00      CAMD80  32
    C**********           Z-ADD1.00      CAMD81  32
    C**********           Z-ADD1.00      CAMD82  32
    C**********           ENDIF
    C**********
    C**********           END                             END *IN60
    C*** END DEL *** 05/09/00 ****************************************
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr2
    C*
     C     proce         endsr
    C*
    C*** BEG ADD *** 08/16/11 *********************************************
    C********************************************************************
    C** Medicare DME                                      M e d D M E   *
    C********************************************************************
     C     MedDME        begsr
     C
     C                   move      carrn         ##rrn
    C*  Default to service date for effective date for rules
     C                   eval      kkefdt = cafdat
     C
     C* Call API to calculate Medicare Physicain Amt
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
     C                   select
     * For codes beginning with "K" or "L" get the allowance percentage
     * from the FTRTP (LF=FTRTL2) Fee Schedule Rate Table
     C                   when       %subst(cacpt4:1:1) = 'K'
     C                              or %subst(cacpt4:1:1) = 'L'
     C
    C* Get Fee Schedule Rate for CT
     C                   eval      xxtype = 'M'
     C     rtky2         setgt     prt01
     C     rtky1         readpe    prt01                                  99
    C*
     C                   if        *in99 = *off
     *   Codes beginning with "K" or "L"
     C                   eval      camra = ##alw1 * RTMUL1
     C                   eval      xcmra = ##alw1 * RTMUL1
     C                   else
    C*   No Fee Schedule Rate record for CT found
     C                   eval      camra = ##alw1
     C                   eval      xcmra = ##alw1
     C                   endif
     C                   other
     *   Codes NOT beginning with "K" or "L"
     C                   eval      camra = ##alw1
     C                   eval      xcmra = ##alw1
     C                   endsl
     C
     C                   if        cacpt4 >= 'A4206'
     C                             and cacpt4 <= 'A6457'
     C                   movel     'F'           carpt
     C                   else
     C                   movel     'N'           carpt
     C                   endif
     C                   movel     'Y'           cafsr
     C                   move      dhpos         caplac
     C                   movel     dhprcd        cacpt4
     C                   movel     dharea        capart
     C                   move      dhunts        caunts
     C                   if        cacpt4 >= 'A0000'
     C                             and cacpt4 <= 'Z9999'
     C                   movel     'U'           caanes
     C                   endif
     C
     C     MedDMEe       endsr
    C*** END ADD *** 08/16/11 *********************************************
    C*
    C********************************************************************
    C** REASONABLE AND CUSTOMARY FILE SEARCH                  R E C U   *
    C********************************************************************
     C     recu          begsr
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     dhprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
    C*
    C*** BEGIN DEL *** 05/30/01 **************************************
    C**********           MOVE *BLANKS   XXFRZP
    C**********           MOVE *BLANKS   XXZIPA
    C*
    C********** P1KEY     CHAINPP101                60
    C********** *IN60     IFEQ '0'
    C*
    C*
    C* Now that the Provider is established, get its Geographic Zip Area
    C* 1st - establish the first 3-bytes of the zip code
    C**********           MOVELP1ZIP     XXFRZP
    C**********           END                             END 0 *IN60 - PPD01
    C*
    C*
    C* 2nd - Get the corresponding Geographic Zip Area record:
    C********** GZKY      SETGTPGZ01
    C**********           READPPGZ01                    60
    C********** *IN60     IFEQ '0'
    C********** XXFRZP    ANDLEGZTOZP
    C**********           MOVE GZZIPA    XXZIPA
    C*
    C**********           ELSE
    C*
    C* USE GEO-ZIP 430 IF ZIP CODE IS OUT OF STATE
    C**********           MOVEL'430'     XXZIPA
    C*
    C**********           END                             END 0 *IN60 - PGZ01
    C*** END   DEL *** 05/30/01 **************************************
    C*
     C     rcky          setgt     prc01
     C     nxrc1         tag
     C                   readp     prc01                                  60
    C*
     C                   if        *in60 = *on
     C                             or rcco# <> qqco#
    C*** BEGIN CHG *** 05/30/01 **************************************
    C********** RCZIPA    ORNE XXZIPA
     C                             or rczipa <> cazipa
    C*** END   CHG *** 05/30/01 **************************************
     C                             or rccpt4 <> xxcpt4
    C*
     C                   movel     'Y'           xxinvl
    C*
     C                   else
    C*** BEG ADD *** 08/20/02 ****************************************
     C                   move      rcfsr         cafsr
    C*** END ADD *** 08/20/02 ****************************************
     C                   eval      cafolu = rcfolu
     C*** BEG ADD *** 04/28/04 *********************************************
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
     C*** END ADD *** 04/28/04 *********************************************
    C*
    C*** BEG DEL *** 02/02/00 ****************************************
    C********** CAFOLU    IFGT 0
    C**********           EXSR FOLU
    C**********           ENDIF                           END CAFOLU
    C*** END DEL *** 02/02/00 ****************************************
    C*
    C*
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   eval      caprof = rcprof
     C                   eval      catech = rc92n2
    C*** BEG ADD *** 05/08/00 ****************************************
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
     C*** BEG ADD *** 08/22/02 *********************************************
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C*
     C*** END ADD *** 08/22/02 *********************************************
     C                   move      dhpos         caplac
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** move      dhprcd        cacpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   eval      caunts = dhunts
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
     C*** BEG ADD *** 04/28/04 *********************************************
     C                   if        dhprcd >= '00100' and dhprcd <= '01999'
     C                   eval      camra = rcconf
     C                   endif
     C*** END ADD *** 04/28/04 *********************************************
     C*** BEG ADD *** 08/22/02 *********************************************
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
     C                   move      durvcd        xxsvcd
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     durate        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   move      '000'         xxunit
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
     C*** BEG ADD *** 08/31/01 *********************************************
    C*
     C                   if        eobtyp = 'H'
     C     eoky          chain     peu01
     C                   endif
     C*** END ADD *** 08/31/01 *********************************************
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
    C*
     C     svcde         endsr
    C* ==================================================================
     Opca01     e            rca01
