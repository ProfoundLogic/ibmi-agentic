    H**********************************************************************
     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000RWV                                                  *
    H* DESCRIPTION: ADJUDICATION WORKFILE BUILD FOR WV                    *
    H**********************************************************************
    H*  CREATED: 12/29/99  C:79   PROJECT: 65MAM                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  07/18/23 C:342 Project: NY060                                     *
    H*                 NY Compliance: CARC/RARC Update                    *
    H*                                                                    *
     H*  04/27/18 C:180 Project: C114MJS                                   *
    H*                 Increase xxWAP size.                               *
    H*                                                                    *
     H*  08/16/17  C:079  Project: WV016                                   *
     H*                   Change call of FA756R to blank                   *
    H*                                                                    *
     H*  05/30/16  C:309  Project: FS497                                   *
     H*                   Changed field ##si size to 2A and                *
     H*                   populated "CAapcSI" with ##si                    *
     H*                                                                    *
     H*  10/01/10 C:198 Project: WV009                                     *
     H*             079 Changed to calculate the Medicare Reimbursement    *
     H*                 Amount.                                            *
     H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
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
    H*  08/13/02 C:79  Support: MC010 33679                               *
    H*                 Clear rcd fmt PCAWK2 after writing                 *
    H*                                                                    *
    H*  03/19/02 C:079 PROJECT: WV005                                     *
    H*                 1. If In-facility amount does not exist use        *
    H*                    out of facility amount.                         *
    H*                 2. Load CATECH with PR92N1 not PR92N2              *
    H*                    out of facility amount.                         *
    H*                                                                    *
    H*  11/30/01 C:079 PROJECT: WV002                                     *
    H*                 1. Get provider service code info.                 *
    H*                                                                    *
    H*  08/30/01 C:079 PROJECT: WV003                                     *
    H*                 1. Use CAFDAT to get the eff date for the          *
    H*                    rules                                           *
    H*                 2. If place of service = 11 then use PRMRA         *
    H*                    otherwise use PR92N2 (Facility amount).         *
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
    F*
     F*** Beg Chg *** 10/01/10 ****************************************
     F*****fdcalh    uf   e           k disk
     Ffdcalh    uf   e           k disk    infds(cafds) usropn
     F*** End Chg *** 10/01/10 ****************************************
     Ffddul     if   e           k disk
    F*** BEGIN ADD *** 03/28/01 **************************************
     Fftuml     if   e           k disk
    F*** END   ADD *** 03/28/01 **************************************
     Ffddhl     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffmrxl     if   e           k disk
     F*** Beg Chg *** 10/01/10 ****************************************
     F****faeolg    if   e           k disk
     Ffaeolg    if   e           k disk    infds(eofds)
     F*** End Chg *** 10/01/10 ****************************************
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
     Ffdrul1    if   e           k disk
    F*
     Ffmsvl1    if   e           k disk
    F*
     Ffdfrl5    if   e           k disk
    F*
    F*** BEG ADD *** 11/30/01 ****************************************
     Ffmp2lwv   if   e           k disk
    F*** END ADD *** 11/30/01 ****************************************
     F*** Beg Add *** 10/01/10 ****************************************
     Fftrvl3    if   e           k disk
     Fftrtl2    if   e           k disk
     Ffaeul     if   e           k disk
     F*** End Add *** 10/01/10 ****************************************
    D*
     D*** Beg Add *** 10/01/10 ****************************************
     Dcafds            ds
     Dcarrn                  397    400b 0
     D*
     Deofds            ds
     Deorrn                  397    400b 0
     D*
     D*** End Add *** 10/01/10 ****************************************
     D qqf             s              7    dim(50)
    D*
    D*** BEG DEL *** 02/02/00 ****************************************
    D******************* FUF       250  7 0             FOLLOW UP FRM DY
    D******************* FUT       250  7 0             FOLLOW UP TO DAY
    D*** END DEL *** 02/02/00 ****************************************
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D*** Beg Add *** 10/01/10 ****************************************
     D xxmd62          s              4  3
     D camd82          s              3  2
     D*** End Add *** 10/01/10 ****************************************
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
     D***xxawp           s              9  5
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
     D*** Beg Add *** 10/01/10 *************************************
     D kkst            s                   like(rtst)
     D kktype          s                   like(rttype)
     D*** End Add *** 10/01/10 *************************************
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
    C*** BEG ADD *** 11/30/01 ****************************************
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
    C*** END ADD *** 11/30/01 ****************************************
     C*** Beg Add *** 10/01/10 ****************************************
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
     C*** End Add *** 10/01/10 ****************************************
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
    C*
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
     C*** Beg Add *** 10/01/10 ****************************************
     C                   close     fdcalh
     C*** End Add *** 10/01/10 ****************************************
     C     term          tag
    C*
     C                   eval      xcmra = 0
     C                   return
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
     C*** BEG DEL *** 10/02/02 *********************************************
     C****************** movel     qqspcd        caspcd
     C*** END DEL *** 10/02/02 *********************************************
    C*
     C*** Beg Add *** 10/01/10 *********************************************
     C     eoky          chain     peu01                              99
     C*** End Add *** 10/01/10 *********************************************
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
     C                   eval      kkymd = *zero
     C*** Beg Add *** 10/01/10 ****************************************
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
     C*** Beg Chg *** 08/16/17 ****************************************
     C****************   parm      'F'           ##call#           1
     C                   parm      ' '           ##call#           1
     C*** End Chg *** 08/16/17 ****************************************
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
     C*** End Add *** 10/01/10 ****************************************
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
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   clear                   mod
     C                   clear                   mod1
     C*** END ADD *** 10/17/02 *********************************************
    C*
     C                   eval      kkdseq = cadseq
     C                   eval      xxfdat = cafdat
    C*
    C*   If line item is in the R&C file call subroutine RECU
     C                   if        eobtyp = 'N'
     C                             and carcfl = 'Y'
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
     C                   exsr      recu
    C*
     C                   else
    C*
    C*   Else - treat line item as a procedure code from the CPT4 file
     C                   if        eobtyp = 'N'
     C                             and carcfl = 'C'
     C*** Beg Add *** 10/01/10 *********************************************
     C                             or eobtyp='N' and carcfl = 'M'
     C*** End Add *** 10/01/10 *********************************************
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
     C*** Beg Add *** 10/01/10 *********************************************
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 1.00
     C                   eval      camd80 = 1.00
     C                   eval      camd81 = 1.00
    C*
     C                   if        cafdat >= 1100701
    C*
     C                   if        eospcd = 'ASC'
     C                   exsr      medicareASC
     C                   else
     C                   exsr      medicarePhy
     C                   endif
    C*
     C                   else
     C*** End Add *** 10/01/10 *********************************************
    C*
     C                   exsr      proc
    C*
     C*** Beg Add *** 10/01/10 *********************************************
     C                   endif
     C*** End Add *** 10/01/10 *********************************************
    C*
     C                   else
    C*
    C*   Else - treat line item as a dental code
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
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      durate        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C                   endif
    C*
     C                   movel     'H'           catype
    C*
     C                   exsr      svcd
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
    C* Initialize work fields
     C                   eval      xxmra = camra
    C*
    C* If modifiers are present then adjust CAAMT accordingly.  CAAMT
    C* is the key field used to process FACAP in highest MRA order.
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
     C*** Beg Add *** 10/01/10 *********************************************
     C                   if        caamt = 0 and
     C                             (carpt = 'N' or carpt = ' ')
     C                   eval      carpt = 'Y'
     C                   endif
     C*** End Add *** 10/01/10 *********************************************
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
     C*** BEG ADD *** 10/02/03 ****************************************
     C                   clear                   pcawk2
     C*** END ADD *** 10/02/03 ****************************************
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
    C** DENTAL CODE MRA SEARCH                             D E N T      *
    C********************************************************************
     C     dent          begsr
    C*** BEG ADD *** 08/30/01 ****************************************
    C*
    C*  Default to use service date for effective date for rules
     C                   move      cafdat        kkefdt
    C*** END ADD *** 08/30/01 ****************************************
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
    C*** BEG DEL *** 08/30/01 ****************************************
    C********************
    C** Default to use service date for effective date for rules
    C******************** MOVE CAFDAT    KKEFDT
    C*** END DEL *** 08/30/01 ****************************************
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*** BEG DEL *** 08/30/01 ****************************************
    C********************
    C*  Setup correct effective date for rules
    C******************** Z-ADDPREFDT    KKEFDT
    C*** END DEL *** 08/30/01 ****************************************
    C*
    C*** BEG DEL *** 02/02/00 ****************************************
    C********** CAFOLU    IFGT 0
    C**********           EXSR FOLU
    C**********           ENDIF                           END CAFOLU
    C*** END DEL *** 02/02/00 ****************************************
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
    C*
    C* If procedure is performed in a facility and there is a facility
    C* amount then load that.
    C*
    C*** BEG ADD *** 08/30/01 ****************************************
     C                   if        dhpos <> ' 11'
    C*** BEG ADD *** 03/19/02 ****************************************
     C                             and pr92n2 <> 0
    C*** END ADD *** 03/19/02 ****************************************
     C                   eval      xcmra = pr92n2
     C                   eval      camra = pr92n2
     C                   endif
    C*** END ADD *** 08/30/01 ****************************************
    C*
     C                   eval      caprof = prprof
    C*** BEG CHG *** 03/19/02 ****************************************
    C******************** Z-ADDPR92N2    CATECH
     C                   eval      catech = pr92n1
    C*** END CHG *** 03/19/02 ****************************************
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a5         canct
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
     C*** BEG ADD *** 10/17/02 *********************************************
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
    C*
    C********************************************************************
    C** PROCEDURE CODE MRA SEARCH                             P R O C   *
    C********************************************************************
     C     proc          begsr
    C*** BEG ADD *** 08/30/01 ****************************************
    C*
    C*  Default to service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*** END ADD *** 08/30/01 ****************************************
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
    C*** BEG DEL *** 08/30/01 ****************************************
    C********************
    C** Default to service date for effective date for rules
    C******************** Z-ADDCAFDAT    KKEFDT
    C*** END DEL *** 08/30/01 ****************************************
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*** BEG DEL *** 08/30/01 ****************************************
    C********************
    C** Setup correct effective date for rules
    C******************** Z-ADDPREFDT    KKEFDT
    C*** END DEL *** 08/30/01 ****************************************
    C*
    C*** BEG DEL *** 02/02/00 ****************************************
    C********** CAFOLU    IFGT 0
    C**********           EXSR FOLU
    C**********           ENDIF                           END CAFOLU
    C*** END DEL *** 02/02/00 ****************************************
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
    C*
    C* If procedure is performed in a facility load diff amount
    C*
    C*** BEG ADD *** 09/20/01 ****************************************
     C                   if        dhpos <> ' 11'
     C                   eval      xcmra = pr92n2
     C                   eval      camra = pr92n2
     C                   endif
    C*** END ADD *** 09/20/01 ****************************************
    C*
     C                   eval      caprof = prprof
    C*** BEG CHG *** 09/20/01 ****************************************
    C******************** Z-ADDPR92N2    CATECH
     C                   eval      catech = pr92n1
    C*** END CHG *** 09/20/01 ****************************************
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a5         canct
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 1.00
     C                   eval      camd80 = 1.00
     C                   eval      camd81 = 1.00
     C                   move      dhpos         caplac
    C*
     C                   endif
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
    C* Use GEO-ZIP 430 if zip code is out of state
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
     C                   eval      cafolu = rcfolu
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
     C                   eval      caprof = rcprof
     C                   eval      catech = rc92n2
     C*** BEG ADD *** 04/28/04 *********************************************
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
     C*** END ADD *** 04/28/04 *********************************************
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
     C                   move      rcfsr         cafsr
     C                   move      dhpos         caplac
    C*
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 1.00
     C                   eval      camd54 = *zeros
     C                   eval      camd55 = *zeros
     C                   eval      camd56 = *zeros
     C                   eval      camd62 = *zeros
     C                   eval      camd66 = *zeros
     C                   eval      camd80 = *zeros
     C*
     C*** BEG ADD *** 08/22/02 *********************************************
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
     C*** END ADD *** 08/22/02 *********************************************
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
     C*** Beg Add *** 10/01/10 *********************************************
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
     C*****              parm                    ##si              1
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
     C*****              movel     ##si          caapsi
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
     C*** End Add *** 10/01/10 *********************************************
     C*** Beg Del *** 10/01/10 *********************************************
     C*******            move      durvcd        xxsvcd
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     durate        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
     C*******            move      '000'         xxunit
    C*
    C*  Default to use service date for effective date for rules
     C*******            eval      kkefdt = cafdat
    C*
     C***  svky          setgt     psv01
     C***  nxtsv1        tag
     C***                readp     psv01                                  60
    C*
     C***                if        *in60 = *on
     C***                move      'Y'           cainvl
    C*
     C***                else
     C***                eval      camra = svalow
     C***                move      'Y'           cafsr
     C***                move      dupos         caplac
    C*
     C***                endif
    C*
     C***  prstat        cabeq     'I'           nxtsv1
    C*** BEG ADD *** 11/30/01 ****************************************
    C*
    C* Verify service date against provider effective date
    C*
     C***  p2ky          setgt     pp201
     C***  p2ky1         readpe    pp201                                  99
    C*
     C***                if        *in99 = *off
     C***                          and cafdat >= p2efdt
    C*
    C* If P2FL01 EQ A calculate payment on DRG rate
    C*
     C***                if        p2fl01 = 'A'
     C***                eval      camra = p2am01
     C***                move      ' '           cainvl
    C*
     C***                else
     C***                eval      camra = *zeros
     C***                move      'Y'           cainvl
    C*
     C***                endif
    C*
     C***                else
     C***                move      'Y'           cainvl
     C***                eval      camra = *zeros
    C*
     C***                endif
    C*** END ADD *** 11/30/01 ****************************************
     C*** End Del *** 10/01/10 *********************************************
     C*** Beg Add *** 10/01/10 *********************************************
     C                   eval      cahtos = euicn
    C*
     C                   if        durvcd = 100
     C                   eval      camra = capaid
     C                   endif
     C*** End Add *** 10/01/10 *********************************************
    C*
     C     svcde         endsr
     C*** Beg Add *** 10/01/10 *********************************************
     C*---------------------------------------------------------------------
     C     MedicareASC   begsr
     C*
     C                   move      carrn         ##rrn
     C*
     C                   call      'FA765R'
     C                   parm                    ##rrn             9 0
     C                   parm      'F'           ##call            1
     C                   parm      'W'           ##type            1
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
     C                   move      dhpos         caplac
     C                   movel     dhprcd        cacpt4
     C                   movel     dharea        capart
     C                   move      dhunts        caunts
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
     C*** End Add *** 10/01/10 *********************************************
