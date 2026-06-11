    H**********************************************************************
     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NMS                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR MS                           *
    H**********************************************************************
    H*  CREATED: 01/16/98  C:86   PROJECT: MS005                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  08/19/19 C:079 Support: 111/82230                                 *
    H*                 Load PC and TC for outpatient/ASC                  *
    H*                                                                    *
     H*  07/05/19 C:079 Project: MS036                                     *
    H*                 Load Add-on code                                   *
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  12/09/16 C:309 Project: MS032                                     *
     H*                 Load Status Indicator in FACAP from PR5A5          *
     H*                                                                    *
     H*  02/03/14 C:296 Project: MS027                                     *
     H*                 1) Populate CANCT when pr1a7 = 'Y'.                *
     H*                                                                    *
     H*  07/19/11 C:274 Project: MS024                                     *
    H*                 Use PRMRA and outlier (XXOUTL) = zero if place     *
    H*                 of service is 23 (DUPOS) & lab code 80000-89999.   *
    H*                                                                    *
     H*  03/10/11 C:251 Support: 75981 MC133                               *
    H*                 Units being multiplied twice for NDC codes         *
    H*                                                                    *
     H*  10/28/10 C:079 Project: MS019 Priss-15                            *
    H*                 Use cachrg for outlier instead of totchg           *
    H*                                                                    *
     H*  10/20/10 C:227 Project: MS019 Priss-15                            *
    H*                 The codes with 0 mra should not be processsed for  *
    H*                 outlier processing                                 *
    H*                                                                    *
     H*  09/27/10 C:227 Project: MS019                                     *
    H*                 MS 2010 Fee Schedule Update                        *
    H*                                                                    *
     H*  12/08/08 C:189 Project: MS016                                     *
    H*                 MS 2008 Physician MS                               *
    H*                                                                    *
     H*  11/28/08 C:191 Project: MS017                                     *
    H*                 Add DRG payment processing                         *
    H*                                                                    *
     H*  12/02/08 C:079 Project: MS015                                     *
    H*                 Do not use revenue code when looking up line items *
    H*                 for hospital bills                                 *
    H*                                                                    *
     H*  01/21/08 C:047 Project: MS015                                     *
    H*                 Load facility fee and MRA amount.
    H*                                                                    *
     H*  10/13/07 C:079 Project: MS014                                     *
    H*                 Populate hospital billed modifier                  *
    H*                                                                    *
     H*  10/01/07 C:079 Project: MS014                                     *
    H*                 Populate hospital place of service correctly       *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
    H*                                                                    *
     H*  10/30/03 C:79  Project: FS271                                     *
    H*                 Use RXAWPD instead of KKEFDT to select AWP         *
    H*                                                                    *
     H*  10/02/03 C:79  Project: FSMNT                                     *
    H*                 Move clear of PCAWK2 after update PCA01            *
    H*                                                                    *
    H*  10/25/02 C:79  Project: MS011                                     *
    H*                 Load used amount when modifier = UE                *
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
    H*  06/19/00 C:79 Support:MC095/23189                                 *
    H*                1. Populate CANCT with PR1A6                        *
    H*                                                                    *
    H*  04/08/00 C:79 Support:MC095/22094                                 *
    H*                1. Populate CABMRI with PR1A3                       *
    H*                                                                    *
    H*  02/02/00 C:79  Support: MC044/20183                               *
    H*                 1. Dont pass FUF, FUT arrays. F4000N loads         *
    H*                    these now.                                      *
    H*                                                                    *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
     Ffdcalh    uf   e           k disk
     F*** BEG ADD *** 09/27/10 *********************************************
     Ffdcal26   if   e           k disk
     F                                     rename(pca01:pca26) prefix(p4:2)
     F*** BEG ADD *** 09/27/10 *********************************************
     Ffaeolg    if   e           k disk
     F*** BEG ADD *** 11/28/08 *********************************************
     F                                     infds(dbfds)
     F*** END ADD *** 11/28/08 *********************************************
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*** BEGIN DEL *** 05/30/01 **************************************
    F**********FMP1L   IF  E           K        DISK
    F**********FMGZL   IF  E           K        DISK
    F*** END   DEL *** 05/30/01 **************************************
    F*
     Ffmprl     if   e           k disk
    F*
     Ffmsvl1    if   e           k disk
    F*
     Ffmrcl     if   e           k disk
     Ffdrul1    if   e           k disk
    F*
     Ffdfrl5    if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
    F*** BEGIN ADD *** 03/28/01 **************************************
     Fftuml     if   e           k disk
    F*** END   ADD *** 03/28/01 **************************************
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffmrxl     if   e           k disk
     F*** BEG ADD *** 11/28/08 *********************************************
     Ffaeul     if   e           k disk
     D*** END ADD *** 11/28/08 *********************************************
     F*** BEG ADD *** 09/27/10 *********************************************
     Ffmzrlms   if   e           k disk
     Fftrtl2    if   e           k disk
     F*** END ADD *** 09/27/10 *********************************************
    D*
     F*** BEG ADD *** 11/28/08 *********************************************
     D dbfds           ds
     D dbrrn                 397    400b 0
     D*** END ADD *** 11/28/08 *********************************************
     D qqf             s              7    dim(50)
    D*
    D*
    D*** BEG DEL *** 02/02/00 ****************************************
    D******************* FUF       250  7 0             FOLLOW UP FRM DY
    D******************* FUT       250  7 0             FOLLOW UP TO DAY
    D*** END DEL *** 02/02/00 ****************************************
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
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
    D*
     D*** RMN ADD *** 09/27/10 *********************************************
     D                 ds
     D xxblty                         3
     D  xxblt1                        1    overlay(xxblty)
     D  xxblt2                        2    overlay(xxblty:1)
     D  xxblt3                        3    overlay(xxblty:1)
     D*** RMN ADD *** 09/27/10 *********************************************
    D*
     D psds          esds                  extname(szq1p)
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
     D xppmod          s                   like(dhmod1)
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
     D*** BEG ADD *** 09/27/10 *********************************************
     D xxzip           s                   like(zrfzcd)
     D xxloc#          s                   like(zrrgn#)
     D xxccr           s                   like(rtrccr)
     D xxoutl          s                   like(rtoutl)
     D xxout1          s                   like(camra)
     D xxout2          s                   like(camra)
     D impchg          s                   like(cachrg)
     D totchg          s                   like(cachrg)
     D*** END ADD *** 09/27/10 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     cakyh         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkymd
    C*
     C     cakyh1        klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*** BEG ADD *** 11/28/08 ****************************************
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*** END ADD *** 11/28/08 ****************************************
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
    C* Reasonable & Customary File
     C     rcky          klist
     C                   kfld                    qqco#
     C                   kfld                    cazipa
     C                   kfld                    xxcpt4
    C*** BEGIN CHG *** 03/28/01 **************************************
    C**********           KFLD           KKYMD
     C                   kfld                    kkefdt
    C*** END   CHG *** 03/28/01 **************************************
    C*** BEGIN ADD *** 03/28/01 **************************************
    C*
    C*  Usual and Customary Modifier Percent File
    C*
     C     umky          klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
    C*** END   ADD *** 03/28/01 **************************************
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
     C     duky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkdseq
    C*
     C     svky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
     C                   kfld                    cafdat
    C*
     C     svky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
    C*  NDC Drug File
     C     rxky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxndc
     C*** BEG ADD *** 09/27/10 *********************************************
     C*
     C     zrky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxzip
     C                   kfld                    kkefdt
     C*
     C     rtky          klist
     C                   kfld                    @@st              2
     C                   kfld                    @@type            1
     C                   kfld                    @@bgdt
     C*
     C*
     C     rtky1         klist
     C                   kfld                    @@st
     C                   kfld                    @@type
     C*
     C*** END ADD *** 09/27/10 *********************************************
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
     C*** BEG ADD *** 11/28/08 *********************************************
     C                   if        eorcn = 0
     C                   if        eobtyp = 'H'
     C                   if        euicn = 'IM' or euicn = 'IP' or euicn = 'IR'
     C                             or euicn = 'IS'
     C*
     C                   Movel     dbrrn         @@rrn             9 0
     C*
     C                   Call      'FA756RMS'
     C                   parm                    @@rrn             9 0
     C                   parm                    @@rtcd            1
     C                   parm                    @@allow           9 2
     C*
     C                   endif                                                  EUICN = IM,IP,IR,IS
     C                   endif                                                  eobtyp='H'
     C                   endif                                                  eorcn=0
     C*** END ADD *** 11/28/08 *********************************************
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
     C*** BEG ADD *** 11/28/08 ****************************************
     C     euky          chain     peu01                              99
     C*** END ADD *** 11/28/08 ****************************************
     C                   eval      kkymd = *zeros
    C*
    C* Main read of detail charge file
     C     cakyh         setll     pca01
     C     cakyh1        reade     pca01                                  54
    C*
     C                   dow       *in54 = *off
    C*
    C* Initialization
    C*
     C                   clear                   pdd01
     C                   clear                   pdh01
     C                   clear                   pdd01
     C                   clear                   pdu01
     C*** BEG ADD *** 10/25/02 *********************************************
     C                   clear                   mod
     C                   clear                   mod1
     C*** END ADD *** 10/25/02 *********************************************
    C*
     C                   if        xxrcn = carcn
    C*
     C                   eval      kkymd = cafdat
     C                   eval      kkdseq = cadseq
    C*
    C*   If line item is in the reasonable and customary file - call
    C*   subroutine RECU to process all possibilities that may occur
    C*   ie.  BY-RPT, Individual consideration, determine MRA
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
     C*** BEG ADD *** 10/25/02 *********************************************
     C                   exsr      pmodld
     C*** END ADD *** 10/25/02 *********************************************
     C                   endif
    C*
     C                   movel     'P'           catype
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
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      dhprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C*** BEG ADD *** 10/25/02 *********************************************
     C                   exsr      pmodld
     C*** END ADD *** 10/25/02 *********************************************
     C                   endif
    C*
     C                   movel     'P'           catype
     C                   exsr      proc
    C*
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
    C* Else - treat line item as a hospital service code
    C* Chain out to the Hospital service code file
     C*** BEG ADD *** 09/27/10 *********************************************
     C                   exsr      tot_chg
     C*** END ADD *** 09/27/10 *********************************************
     C     duky          chain     pdu01                              60
    C*
     C                   if        *in60 = *off
     C                   move      durvcd        casvcd
     C                   move      durate        cacpt4
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      durate        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C*** BEG ADD *** 10/13/07 *********************************************
     C                   exsr      pmodldh
     C*** END ADD *** 10/13/07 *********************************************
     C                   endif
    C*
     C                   movel     'H'           catype
     C                   exsr      svcd
     C*** BEG ADD *** 12/08/08 ********************************************
     C
    C*  Get Hospital Header information
     C     euky          chain     peu01                              60
    C*
     C                   if        *in60 = *off
     C                   if        EUICN = 'HO' or EUICN = 'HS'
     C                             or EUICN = 'AO' or EUICN = 'AS'
    C*
     C                   If        (DUPMOD = 'T' or  DUPMOD = 'D') or
     C                             (DUMOD2 = 'T' or  DUMOD2 = 'D')
    C*
     C                   eval      cafacf = sv92a4
     C                   endif
     C                   endif
     C                   endif
     C*** END ADD *** 12/08/08 ********************************************
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
     C*** BEG ADD *** 01/21/08 *********************************************
    C* Hosp Bill with a facility fee
    C*
     C                   if        eobtyp = 'H'
     C                             and cafacf <> 0
     C                   eval      xxmra = cafacf
     C                   endif
     C*** end ADD *** 01/21/08 *********************************************
    C*
    C*
    C* If modifiers are present then adjust CAAMT accordingly.  CAAMT
    C* is the key field used to process FACAP in highest MRA order.
    C*
     C*** BEG DEL *** 10/25/02 *********************************************
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
     C*** END DEL *** 10/25/02 *********************************************
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
     C     cakyh1        reade     pca01                                  54
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
     C                   move      cafdat        kkefdt
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
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n2
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C*** BEG CHG *** 02/03/14 ****************************************
     C*****              move      pr1a3         canct
     C                   if        Pr1A7 = 'Y'
     C                   eval      canct = 'Y'
     C                   else
     C                   eval      canct = pr1a6
     C                   endif
     C*** END CHG *** 02/03/14 ****************************************
     C***** Beg Add *** 07/05/19 *****************************************
     C                   move      pr1a7         CAaddon
     C***** End Add *** 07/05/19 *****************************************
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
     C*** BEG ADD *** 10/01/07 ********************************************
    C********************************************************************
    C** PHYSICIAN AMOUNT SUBROUTINE                       GETPHYSAMT    *
    C********************************************************************
     C     getphysamt    begsr
     C*
     C                   eval      xxcpt4 = durate
     C                   eval      kkefdt = cafdat
    C*
     C     prky          setgt     ppr01
     C     nxtpr5        tag
     C                   readp     ppr01                                  60
    C*
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      camra = prmra
     C                   eval      xcmra = prmra
    C*
     C                   eval      cafacf = pr92n3
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n1
     C                   eval      caanam = pranam
    C*
     C                   if        durate >= '00100' and durate <= '01999'
     C                   eval      caanes = 'Y'
     C                   eval      carcfl = 'C'
     C                   endif
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr5
    C*
     C     getphysamte   endsr
     C*** END ADD *** 10/01/07 ********************************************
    C*** BEG DEL *** 02/02/00 ****************************************
    C********************************************************************
    C** FOLLOW UP DAY LOAD SUBROUTINGE                                  *
    C********************************************************************
    C**********
    C********** FOLU      BEGSR
    C**********
    C**********           MOVEL'N'       CAINVL  1
    C**********           Z-ADDCAFDAT    SRDTI
    C**********
    C**********           CALL 'SRCVTDAT'DTMDYY
    C**********
    C**********           MOVELSRDTO     SRDATE  80
    C**********
    C**********           Z-ADDCAFOLU    SRFUTR  50
    C**********
    C**********           MOVE ' '       SRRTCD  1
    C**********
    C**********           CALL 'SRFUTR8'
    C**********           PARM           SRDATE
    C**********           PARM           SRFUTR
    C**********           PARM           SRRTCD
    C**********
    C**********           Z-ADDSRDATE    SRDTI
    C**********
    C**********           CALL 'SRCVTDAT'DTCYMD
    C**********
    C**********           MOVELSRDTO     HLDDAT  70
    C**********
    C**********           Z-ADDCAFDAT    FUF,U
    C**********           Z-ADDHLDDAT    FUT,U
    C**********           ADD  1         U
    C**********
    C**********
    C********** FOLUE     ENDSR
    C*** END DEL *** 02/02/00 ****************************************
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
     C                   eval      camra = rxawp
     C                   move      rxptyp        captyp
     C                   move      rxstat        castat
     C                   move      'Y'           cafsr
    C*
    C* Use previous average wholesale price(AWP)
     C*** BEG CHG *** 10/30/03 *********************************************
     C****************** if        cafdat < kkefdt
     C                   if        cafdat < rxawpd
     C*** END CHG *** 10/30/03 *********************************************
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
     C*** BEG ADD *** 10/17/02 *********************************************
     C*** BEG CHG *** 03/10/11 *********************************************
     C**********         eval(h)   caawp = caunts * xxawp
     C                   eval      caawp = xxawp
     C*** END CHG *** 03/10/11 *********************************************
     C*** END ADD *** 10/17/02 *********************************************
     C                   move      rxstat        castat
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
     C*** BEG ADD *** 01/21/08 *********************************************
    C*
     C     'T '          lookup    mod(1)                                 99
     C                   if        *in99 = *on
    C*** Beg Add *** 12/09/16 ****************************************
     C                   If        CAfdat >= 1161112 and EObtyp <> 'H'
     C                   Eval      xxmra = PR92n4
     C                   Else
    C*** End Add *** 12/09/16 ****************************************
     C                   Eval      xxmra = cafacf
    C*** Beg Add *** 12/09/16 ****************************************
     C                   Endif
    C*** End Add *** 12/09/16 ****************************************
     C                   endif
    C*
     C     'D '          lookup    mod(1)                                 99
     C                   if        *in99 = *on
    C*** Beg Add *** 12/09/16 ****************************************
     C                   If        cafdat >= 1161112 and EObtyp <> 'H'
     C                   Eval      xxmra = PR92n4
     C                   Else
    C*** End Add *** 12/09/16 ****************************************
     C                   eval      xxmra = cafacf
    C*** Beg Add *** 12/09/16 ****************************************
     C                   Endif
    C*** End Add *** 12/09/16 ****************************************
     C                   endif
    C*
     C*** END ADD *** 01/21/08 *********************************************
    C*
     C     pmode         endsr
     C*** BEG ADD *** 10/25/02 *********************************************
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
     C*** END ADD *** 10/25/02 *********************************************
     C*** BEG ADD *** 10/13/07 *********************************************
    C********************************************************************
    C** MODIFIER LOADING HOSPITAL                         P M O D L D H *
    C********************************************************************
     C     pmodldh       begsr
    C*
     C                   move      dupmod        mod(1)
     C                   move      dupmod        camod1
    C*
     C                   endsr
     C*** END ADD *** 10/13/07 *********************************************
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
    C*** BEG ADD *** 10/25/02 ****************************************
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = pr92n2
     C                   eval      xcmra = pr92n2
    C*
     C                   endif
    C*
    C*** END ADD *** 10/25/02 ****************************************
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n1
     C                   movel     pr1a2         caanes
    C*** BEG ADD *** 04/08/00 ****************************************
     C                   move      pr1a3         cabmri
    C*** END ADD *** 04/08/00 ****************************************
    C*** BEG ADD *** 04/08/00 ****************************************
     C                   move      'Y'           cabcas
    C*** END ADD *** 04/08/00 ****************************************
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
    C*** BEG CHG *** 06/19/00 ****************************************
    C******************** MOVE PR1AE     CANCT
     C*** BEG CHG *** 02/03/14 ****************************************
     C                   if        Pr1A7 = 'Y'
     C                   eval      canct = 'Y'
     C                   else
     C*** END CHG *** 02/03/14 ****************************************
     C                   move      pr1a6         canct
     C*** BEG CHG *** 02/03/14 ****************************************
     C                   endif
     C*** END CHG *** 02/03/14 ****************************************
    C*** END CHG *** 06/19/00 ****************************************
     C***** Beg Add *** 07/05/19 *****************************************
     C                   move      pr1a7         CAaddon
     C***** End Add *** 07/05/19 *****************************************
     C                   move      pr1ad         casepp
     C                   move      pr1a4         capcf
     C                   move      prfsr         cafsr
    C*
    C*   MODIFIERS
    C*
     C                   eval      camd50 = 1.5
     C                   eval      camd51 = .50
     C                   eval      camd80 = .20
     C                   eval      camd81 = .10
    C*
     C                   if        prsurg = 'Y'
    C*
     C                   eval      camd80 = 0.20
     C                   eval      camd81 = 0.10
    C*
     C                   endif
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
    C*
     C                   endif
     C     prstat        cabeq     'I'           nxtpr2
    C*
     C     proce         endsr
    C********************************************************************
    C** REASONABLE AND CUSTOMARY FILE SEARCH                  R E C U   *
    C********************************************************************
     C     recu          begsr
    C*
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
    C*** BEGIN ADD *** 03/28/01 **************************************
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
    C*** END   ADD *** 03/28/01 **************************************
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
     C                   move      dhpos         capos
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** move      dhprcd        cacpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   eval      caunts = dhunts
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
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
     C*** BEG CHG *** 12/02/08 *********************************************
     C****************** move      durvcd        xxsvcd
     C                   move      *ZEROS        xxsvcd
     C*** END CHG *** 12/02/08 *********************************************
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     durate        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   move      '000'         xxunit
     C                   move      durvcd        casvcd
     C                   move      durate        cacpt4
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
     C     svky          setgt     psv01
     C     nxtsv1        tag
     C     svky1         readpe    psv01                                  60
    C*
     C                   if        *in60 = *on
    C*
     C                   move      'Y'           cainvl
    C*
     C                   else
     C                   eval      camra = svalow
     C                   eval      xcmra = svalow
     C                   move      dupos         caplac
    C*
     C                   move      'Y'           cafsr
     C                   eval      cafacf = sv92a1
    C*
     C*** BEG ADD *** 01/21/08 ********************************************
     C                   if        caplac <> ' 24'
     C                   eval      cafacf = sv92a4
     C                   endif
     C*** END ADD *** 01/21/08 ********************************************
    C*
     C                   eval      caprof = sv92a2
     C                   eval      catech = sv92a3
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtsv1
    C*
     C*** BEG ADD *** 10/01/07 ********************************************
     C                   move      dupos         caplac
     C                   move      'Y'           cafsr
    C*
     C*** BEG ADD *** 09/27/10 ********************************************
     C                   if        cafdat >= 1100701
     C                   exsr      OPPS_ASC
     C                   else
     C*** END ADD *** 09/27/10 ********************************************
     C                   if        *in60 = *on or
     C                             (durate >= '00100' and durate <= '01999')
     C                   exsr      getphysamt
     C                   endif
     C*** BEG ADD *** 09/27/10 ********************************************
     C                   endif
     C*** END ADD *** 09/27/10 ********************************************
    C*
     C*** END ADD *** 10/01/07 ********************************************
    C*
     C     svcde         endsr
     C*** BEG ADD *** 09/27/10 ********************************************
    C*********************************************************************
    C** ASC and Outpatient Rates subroutine               OPPS_ASC       *
    C*********************************************************************
     C     OPPS_ASC      begsr
     C*
     C*** RMN ADD *** 09/27/10 *********************************************
     C                   movel     eublty        xxblty
     C                   if        xxblt2 <> '33'
     C*** RMN ADD *** 09/27/10 *********************************************
     C                   if        euicn = 'HO' or euicn = 'HS' or
     C                             euicn = 'AO' or euicn = 'AS'
     C*
     C                   eval      xxcpt4 = durate
     C                   eval      kkefdt = cafdat
    C*
     C     prky          setgt     ppr01
     C     nxtpr7        tag
     C                   readp     ppr01                                  60
    C*
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C*** BEG ADD *** 07/19/11 ****************************************
     C*** Beg Chg *** 08/19/19 ****************************************
     C****************   if        durate >= '80000' and durate <= '89999'
     C****************              and dupos = ' 23'
     C                   if        durate >= '80000' and durate <= '89999'
     C                              and dupos = ' 23' and CAfDat < 1190615
     C*** End Chg *** 08/19/19 ****************************************
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   else
     C*** END ADD *** 07/19/11 ****************************************
    C*
     C                   eval      camra = pr92n3
     C                   eval      xcmra = pr92n3
     C*** Beg Add *** 08/19/19 ****************************************
     C                   eval      CAprof = PRprof
     C                   eval      CAtech = PR92n1
     C*** End Add *** 08/19/19 ****************************************
    C*
     C*** BEG ADD *** 07/19/11 ****************************************
     C                   endif
     C*** END ADD *** 07/19/11 ****************************************
    C*
    C*** Beg Add *** 12/09/16 ****************************************
     C                   Eval      CAapcsi = PR5a5
    C*** End Add *** 12/09/16 ****************************************
     C*** BEG ADD *** 10/20/10 ****************************************
     C                   if        xcmra > 0
     C*** END ADD *** 10/20/10 ****************************************
     C* Outlier calculation Step 1
     C* Get is area is Rural or Urban. Default to locality 99 and Urban area
     C*
     C                   eval      xxloc# = 99
     C                   move      'U'           caarea
     C                   movel     eoflic        xxzip
     C*
     C     zrky          setgt     pzr01
     C                   readp     pzr01                                  99
     C*
     C                   if        *in99 = *off and xxzip >= zrfzcd
     C                             and xxzip <= zrtzcd
     C*
     C                   eval      xxloc# = zrrgn#
     C                   move      zrarea        caarea
     C*
     C                   endif                                                  *in99 = *off
     C*Set the pointer for the FTRTP to get the CCR values
     C*
     C                   Movel     'MS'          @@st
     C                   Movel     'O'           @@type
     C                   movel     eudcdt        @@bgdt            7 0
     C*
     C     rtky          setgt     prt01
     C     rtky1         readpe    prt01                                  99
     C                   if        caarea = 'U'                                 caarea = 'U'
     C                   eval      xxccr = rtuccr
     C                   else
     C                   eval      xxccr = rtrccr
     C                   endif                                                  caarea = 'U'
    C*
     C*** BEG CHG *** 10/28/10 *********************************************
     C****************** eval      xxout1 = totchg * xxccr
     C                   eval      xxout1 = cachrg * xxccr
     C*** END CHG *** 10/28/10 *********************************************
    C*
     C* Step 2 Deduct implantable cost
    C*
     C                   eval      xxout2 = xxout1 - impchg
    C*
     C* Step 3 Test if outlier meets 1.75 condition
    C*
     C                   eval      xxoutl = 0
     C*
     C                   if        xxout2 > (xcmra * 1.75)
    C*
     C* Step 4 Test to check if outlier meets threshold test
    C*
     C                   if        xxout2 > (xcmra + rtoutl)
     C*** BEG ADD *** 07/19/11 ****************************************
     C                   if        durate >= '80000' and durate <= '89999'
     C                              and dupos = ' 23'
     C***                ** Leave field xxoutl = zero **
     C                   else
     C*** END ADD *** 07/19/11 ****************************************
     C*
     C                   eval      xxoutl = (xxout2 - (xcmra * 1.75)) / 2
     C*
     C*** BEG ADD *** 07/19/11 ****************************************
     C                   endif
     C*** END ADD *** 07/19/11 ****************************************
     C                   endif                                                  xxout2>xcmra+rtoutl
     C*
     C                   endif                                                  xxout2>xcmra*1.75
    C*
     C* Step 5 Calculate total payment
    C*
     C                   eval      camra = camra + xxoutl
     C                   eval      xcmra = xcmra + xxoutl
     C*
     C*** BEG ADD *** 10/20/10 ****************************************
     C                   endif                                                  xcmra > 0
     C*** END ADD *** 10/20/10 ****************************************
     C                   endif                                                  *in60 = *off
    C*
     C     prstat        cabeq     'I'           nxtpr7
    C*
     C                   endif                                                  euicn = 'HO','HS'
     C*** RMN ADD *** 09/27/10 *********************************************
     C                   endif                                                  xxblt2 = '33'
     C*** RMN ADD *** 09/27/10 *********************************************
     C*
     C     oppse         endsr
    C*********************************************************************
    C** Calculate total charges and charges for inplantibles TOT_CHG     *
    C*********************************************************************
     C     tot_chg       begsr
     C*
     C                   eval      impchg = 0
     C                   eval      totchg = 0
     C*
     C     cakyh1        setll     pca26
     C     cakyh1        reade(N)  pca26                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C*
     C                   if        p4svcd = 278
     C                   eval      impchg = impchg + p4chrg
     C                   endif
     C                   eval      totchg = totchg + p4chrg
     C*
     C     cakyh1        reade(N)  pca26                                  99
     C*
     C                   enddo
     C*
     C     tote          endsr
     C*** END ADD *** 09/27/10 ********************************************
    C* ==================================================================
     Opca01     e            rca01
