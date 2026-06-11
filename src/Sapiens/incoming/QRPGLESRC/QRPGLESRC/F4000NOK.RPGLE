    H**********************************************************************
    H* (C)COPYRIGHT PROGRESSIVE DATA SOLUTIONS INC., 2001                 *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NOK                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR OK                           *
    H**********************************************************************
    H*  CREATED: 01/16/98  C:86   PROJECT: OK033                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  09/20/16 C:079 Project: OK022                                     *
     H*                 Add processing for Rad/Path/Lab/Med for OPPS       *
    H*                                                                    *
     H*  04/04/16 C:212 079                                                *
     H*                 Project: OK022                                     *
     H*                 Add processing, multiple changes                   *
    H*                                                                    *
     H*  01/16/09 C:227 Project: OK019                                     *
     H*                 Change to load SVRELW instead of SVALOW.           *
     H*                                                                    *
     H*  08/25/08 C:212 Project: OK019                                     *
     H*                 Include processing for DMEPOS modifiers.           *
     H*                 Add new conversion factor of 4000 for DRG calcs.   *
     H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  02/15/05 C:79  Project: OK017                                     *
    H*                 For tech comp load PR92N1                          *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
    H*                                                                    *
     H*  10/02/03 C:79  Project: FSMNT                                     *
    H*                 Move clear of PCAWK2 after update PCA01            *
    H*                                                                    *
    H*  02/27/03 C:79  Project: OK016                                     *
    H*                 Add modifier 62                                    *
    H*                                                                    *
    H*  10/17/02 C:79  Project: FS239                                     *
    H*                 Load Pharmacy allowance into CAAWP and CAMRA       *
    H*                                                                    *
    H*  10/02/02 C:79  Support: MC010 33679                               *
    H*                 Populate specialty code at the detail level not    *
    H*                                                                    *
    H*  08/13/02 C:79  Support: MC010 33679                               *
    H*                 Clear rcd fmt PCAWK2 after writing                 *
    H*                                                                    *
    H*  01/22/02 C:79  Project: OK015                                     *
    H*                 Load CAMRA with correct data for modifiers         *
    H*                 KM and KN.                                         *
    H*                                                                    *
    H*  01/04/01 C:79  Support: MC095 30602                               *
    H*                 Use the service date for the rules eff date.       *
    H*                 Load RCFSR into CAFSR                              *
    H*                                                                    *
    H*  05/30/01 C:62  Project: FS228                                     *
    H*                 GET ZIPA FROM CAZIPA NOT FROM PROVIDER FILE.       *
    H*                                                                    *
    H*  03/28/01 C:62  Project: UC002                                     *
    H*                 1. Add logic to populate Prof/Tech components      *
    H*                                                                    *
    H*  08/08/00 C:79 PROJECT: OK010                                      *
    H*                1. Populate CAMRA, CAANAM based on modifiers        *
    H*                                                                    *
    H*  02/02/00 C:79  Support: MC044/20183                               *
    H*                 1. Dont pass FUF, FUT arrays. F4000N loads         *
    H*                    these now.                                      *
    H*                                                                    *
    H*  06/08/99 C:79 PROJ: OK008                                         *
    H*                Modified to work with Provider Bills                *
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
     Ffdcalh    uf   e           k disk
     F***** Beg Add *** 04/04/16 **************************************
     F                                     infds(cafds)
     F***** End Add *** 04/04/16 **************************************
     Ffaeolg    if   e           k disk
     F***** Beg Add *** 04/04/16 **************************************
     F                                     infds(dbfds)
     F***** End Add *** 04/04/16 **************************************
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
    F*
     Ffmrxl     if   e           k disk
     F*** BEG ADD *** 08/25/08 ****************************************
     Ffmdmlok   if   e           k disk
     Ffmprlok   if   e           k disk    rename(ppr01:lpr01)
     F*** END ADD *** 08/25/08 ****************************************
     F***** Beg Add *** 04/04/16 **************************************
     Fftrvl3    if   e           k disk
     Fftrtl2    if   e           k disk
     Ffaeul     if   e           k disk
     F***** End Add *** 04/04/16 **************************************
    D*
     D qqf             s              7    dim(50)
    D*
    D*
    D*** BEG DEL *** 02/02/00 ****************************************
    D******************* FUF       250  7 0             FOLLOW UP FRM DY
    D******************* FUT       250  7 0             FOLLOW UP TO DAY
    D*** END DEL *** 02/02/00 ****************************************
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D*** BEG ADD *** 08/25/08 ****************************************
     D i               s              1  0
     D j               s              1  0
     D k               s              1  0
     D xxdme           s              1    inz('N')
     D*** END ADD *** 08/25/08 ****************************************
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
     D***** Beg Add *** 04/04/16 **************************************
     D cafds           ds
     D carrn                 397    400b 0
     D*
     D dbfds           ds
     D dbrrn                 397    400b 0
     D*
     D***** End Add *** 04/04/16 **************************************
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
     D****xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 05/02/18 *********************
     D xxdcs#          s              5  0
     D xxefdt          s                   like(ruefdt)
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D*** BEG ADD *** 08/25/08 ****************************************
     D xxcf            s              4  0 inz(4000)
     D*** END ADD *** 08/25/08 ****************************************
     D***** Beg Add *** 04/04/16 **************************************
     D kkst            s                   like(rtst)
     D kktype          s                   like(rttype)
     D ind756call      s               n   inz(*off)
     D***** End Add *** 04/04/16 **************************************
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
     C     svky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
     C                   kfld                    cafdat
    C*
     C     svky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcmsv
    C*
     C*** BEG ADD *** 08/25/08 ****************************************
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
     C*** END ADD *** 08/25/08 ****************************************
     D***** Beg Add *** 04/04/16 **************************************
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
     C*
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     D***** End Add *** 04/04/16 **************************************
     D*
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
     C***** Beg Add *** 04/04/16 *******************************************
     C                   eval      ind756call = *off
     C                   If        EUdcDt >= 1120101

     C                   if        eorcn = 0
     C                   if        eobtyp = 'H'
     C                   if        euicn = 'IM' or euicn = 'IP' or euicn = 'IR'
     C                             or euicn = 'IS'
     C                   eval      @@mode = 'I'
     C                   eval      ind756call = *on
     C                   endif
     C                   if        euicn = 'AS' or euicn = 'AO'
     C                   eval      @@mode = 'A'
     C                   eval      ind756call = *on
     C                   endif
     C                   endif                                                  EUICN = IM,IP,IR,IS
     C                   endif                                                  eorcn=0
     C*
     C                   if        ind756call = *on
     C                   Movel     dbrrn         @@rrn             9 0
     C*
     C                   Call      'FA756ROK'
     C                   parm                    @@rrn             9 0
     C                   parm                    @@mode            1
     C                   parm                    @@rtcd            1
     C                   parm                    @@allow           9 2
     C*
     C                   endif                                                  eorcn=0
     C                   Endif                                                  eorcn=0
     C***** End Add *** 04/04/16 *******************************************
7   C*
    C*  END PROGRAM AND CLOSE ALL FILES
     C                   close     facap
     C*** Beg Add *** 04/04/16 ************************************
     C*
     C* Is a 2nd pass needed to reprocess ASC line items
     C*
     C                   if        (euicn = 'AS' or euicn = 'AO')
     C                             and eudcdt >= 1120101 and @@rtcd = '2'
    C*
     C                   open      facap
     C                   exsr      convrt
     C                   close     facap
    C*
     C                   clear                   @@rtcd
    C*
     C                   Endif                                                  eorcn=0
     C*** End Add *** 04/04/16 ************************************
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
     C***** Beg Add *** 04/04/16 **************************************
     C     euky          chain     peu01                              99
     C***** End Add *** 04/04/16 **************************************
    C*
     C                   eval      kkymd = *zeros
    C*
    C* Main read of detail charge file
     C     cakyh         setll     pca01
     C     cakyh1        reade     pca01                                  54
    C*
     C                   dow       *in54 = *off
    C*
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   clear                   pdd01
     C                   clear                   pdh01
     C                   clear                   pdd01
     C                   clear                   pdu01
     C                   clear                   mod
     C                   clear                   mod1
     C*** END ADD *** 10/17/02 *********************************************
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
     C***** Beg Chg *** 04/04/16 **************************************
     C************************     and carcfl = 'C'
     C                             and (carcfl='C' or carcfl ='M')
     C***** End Chg *** 04/04/16 **************************************
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
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   exsr      pmodld
     C*** END ADD *** 10/17/02 *********************************************
     C                   endif
    C*
     C                   movel     'P'           catype
    C*
     C***** Beg Add *** 04/04/16 **************************************
     C                   if        cafdat >= 1120101
     C*
     C                   if        CAsrc = 'D' or CAsrc = 'A'
     C*
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 1.00
     C                   eval      camd80 = 1.00
     C                   eval      camd81 = 1.00

     C                   exsr      medicarePhy

     C                   else

     C                   exsr      proc

     C                   endif
     C*
     C                   else
     C***** End Add *** 04/04/16 **************************************
    C*
     C                   exsr      proc
    C*
     C***** Beg Add *** 04/04/16 **************************************
     C                   endif
     C***** End Add *** 04/04/16 **************************************
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
    C*
     C     duky          chain     pdu01                              60
    C*
     C                   if        *in60 = *off
     C                   move      durvcd        casvcd
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
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*** BEG ADD *** 01/04/02 ****************************************
    C*
    C*  Default to use service date for effective date for rules
    C*
     C                   move      cafdat        kkefdt
    C*** END ADD *** 01/04/02 ****************************************
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
    C*** BEG DEL *** 01/04/02 ****************************************
    C*
    C*  Default to use service date for effective date for rules
    C******************** MOVE CAFDAT    KKEFDT
    C*** END DEL *** 01/04/02 ****************************************
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*** BEG DEL *** 01/04/02 ****************************************
    C*  Setup correct effective date for rules
    C******************** Z-ADDPREFDT    KKEFDT
    C*** END DEL *** 01/04/02 ****************************************
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
     C*** BEG CHG *** 02/15/05 *********************************************
     C****************** eval      catech = pr92n2
     C                   eval      catech = pr92n1
     C*** END CHG *** 02/15/05 *********************************************
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C*** BEG CHG *** 03/03/03 *********************************************
     C****************** move      pr1a3         canct
     C                   move      pr1a5         canct
     C*** END CHG *** 03/03/03 *********************************************
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
     C                   eval      xxefdt = rxawpd
    C*
     C                   eval      caunts = dpunts
    C*
    C* Use previous average wholesale price(AWP)
     C                   if        cafdat < xxefdt
    C*
      *** Begin Change *** 05/02/18 *********************
     C****               eval      xxawp = rx1awp / 1000
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
     C*** BEG ADD **** 08/25/08 ******************************************
     C                   eval      xxdme = ' '
     C***** Beg Add *** 04/04/16 **************************************
     C                   if        CAfDat < 1120101
     C***** End Add *** 04/04/16 **************************************
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
     C                   when      mod(1) <> ' '
     C                   eval      xxmod1 = mod(1)
     C                   when      mod(2) <> ' '
     C                   eval      xxmod1 = mod(2)
     C                   when      mod(3) <> ' '
     C                   eval      xxmod1 = mod(3)
     C                   when      mod(4) <> ' '
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
     C***** Beg Add *** 04/04/16 **************************************
     C                   endif
     C***** End Add *** 04/04/16 **************************************
     C*
     C                   if        xxdme <> 'Y'
     C*
     C*** END ADD **** 08/25/08 ******************************************
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
     C*** BEG ADD **** 08/25/08 ******************************************
     C                   endif
     C*** END ADD **** 08/25/08 ******************************************
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
    C*
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*
    C*  Default to service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
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
    C*** BEG DEL *** 01/04/02 ****************************************
    C*
    C*  Default to service date for effective date for rules
    C******************** Z-ADDCAFDAT    KKEFDT
    C*** END DEL *** 01/04/02 ****************************************
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*** BEG DEL *** 01/04/02 ****************************************
    C*
    C*  Setup correct effective date for rules
    C******************** Z-ADDPREFDT    KKEFDT
    C*** END DEL *** 01/04/02 ****************************************
    C*
    C*** BEG DEL *** 02/02/00 ****************************************
    C********** CAFOLU    IFGT 0
    C**********           EXSR FOLU
    C**********           ENDIF                           END CAFOLU
    C*** END DEL *** 02/02/00 ****************************************
    C*** BEG ADD *** 08/08/00 ****************************************
     C                   select
    C*
     C                   when      capmod = 'UE'
    C*** BEG ADD *** 01/22/02 ****************************************
     C                             or capmod = 'KN'
    C*** END ADD *** 01/22/02 ****************************************
     C                   eval      camra = pr92n2
     C                   eval      xcmra = pr92n2
    C*
    C*** BEG ADD *** 01/22/02 ****************************************
     C                   when      capmod = 'KM'
     C                   eval      camra = pranam
     C                   eval      xcmra = pranam
    C*** END ADD *** 01/22/02 ****************************************
    C*
     C                   other
     C                   eval      camra = prmra
     C                   eval      xcmra = prmra
    C*
     C                   endsl
    C*** END ADD *** 08/08/00 ****************************************
    C*
    C*** BEG DEL *** 08/08/00 ****************************************
    C******************** Z-ADDPRMRA     XCMRA
    C******************** Z-ADDPRMRA     CAMRA
    C*** END DEL *** 08/08/00 ****************************************
     C                   eval      caprof = prprof
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   eval      catech = pr92n1
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a5         canct
     C                   move      pr1a3         catcf
     C                   move      pr1a4         capcf
     C                   move      pr1a6         caamb
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
     C                   eval      camd50 = 0.50
     C                   eval      camd51 = 0.50
     C*** BEG ADD *** 02/27/03 *********************************************
     C                   eval      camd62 = 0.25
     C*** END ADD *** 02/27/03 *********************************************
     C                   eval      camd80 = 0.20
     C                   eval      camd81 = 0.10
    C*
     C                   move      dhpos         caplac
     C                   move      dhprcd        cacpt4
     C                   eval      caunts = dhunts
    C*
     C                   if        pr1a2 = 'Y'
    C*** BEG ADD *** 08/08/00 ****************************************
     C                             or pr1a2 = 'R'
    C*** END ADD *** 08/08/00 ****************************************
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
    C*
     C                   else
    C*** BEG ADD *** 01/04/02 ****************************************
     C                   move      rcfsr         cafsr
    C*** END ADD *** 01/04/02 ****************************************
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
     C*** BEG CHG *** 02/15/05 *********************************************
     C****************** eval      catech = rc92n2
     C                   eval      catech = rc92n1
     C*** END CHG *** 02/15/05 *********************************************
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
     C                   eval      camd50 = 0.5
     C                   eval      camd51 = 0.5
     C                   eval      camd54 = *zeros
     C                   eval      camd55 = *zeros
     C                   eval      camd56 = *zeros
     C*** BEG CHG *** 02/27/03 *********************************************
     C****************** eval      camd62 = *zeros
     C                   eval      camd62 = 0.25
     C*** END CHG *** 02/27/03 *********************************************
     C                   eval      camd66 = *zeros
     C*** BEG DEL *** 02/27/03 *********************************************
     C****************** eval      camd80 = *zeros
     C*** END DEL *** 02/27/03 *********************************************
     C*** BEG ADD *** 02/27/03 *********************************************
     C                   eval      camd80 = .20
     C                   eval      camd81 = .10
     C*** END ADD *** 02/27/03 *********************************************
    C*
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
     C*** BEG ADD *** 08/25/08 ****************************************
     C                   if        cafdat >= 1080101
     C*** BEG CHG *** 01/16/09 ****************************************
     C***                eval      svalow = svalow * xxcf
     C                   eval      svalow = svrelw * xxcf
     C*** END CHG *** 01/16/09 ****************************************
     C                   endif
     C*** END ADD *** 08/25/08 ****************************************
     C                   eval      camra = svalow
     C                   eval      xcmra = svalow
     C                   move      dupos         caplac
    C*
     C                   move      'Y'           cafsr
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtsv1
     C*** Beg Add *** 04/04/16 ********************************
     C* Pay each line at a percentaged of charged
     C*
     C                   if        (euicn = 'AS' or euicn = 'AO')
     C                             and eudcdt >= 1120101 and @@rtcd = '2'
    C*
     C                   eval      carpt = 'P'
     C                   move      dupos         caplac
     C                   move      'Y'           cafsr
    C*
     C                   endif
     C*** End Add *** 04/04/16 ********************************
     C*** Beg Add *** 09/20/16 ********************************
          //*** Beg Add *** 09/20/16 ********************************
          //* Pay Rad/Path/Lab/Medi at the Medical Fee Schedule Rate
      /Free
            if        (euicn = 'HO' or euicn = 'HS') and DUrate >=
                      '70000' and DUrate <= '99999';

              setgt prky  ppr01;
              readp ppr01;
              Dow not %EOF;
               if not %found(fmprl) or PRcpt4 <> xxCpt4;
                  cainvl = 'Y';
                else;
                  if prstat <> 'I';
                    CAfolu = PRfolu;
                    CAmra = PRmra;
                    xcMra = PRmra;
                    CAprof = PRprof;
                    CAanes = PR1a2;
                    CAanam = PRanam;
                    CAsurg = PRsurg;
                    CAsupp = PRsupp;
                    CAtech = PR92n1;
                    CAstat = PRstat;
                    CArpt = PRrpt;
                    CAnct = PR1a5;
                    CAtcf = PR1a3;
                    CApcf = PR1a4;
                    CAamb = PR1a6;
                    CAsepp = PR1ad;
                    CAfsr = PRfsr;
                    CAmd50 = 0.50;
                    CAmd51 = 0.50;
                    CAmd62 = 0.25;
                    CAmd80 = 0.20;
                    CAmd81 = 0.10;
                    CAplac = DHpos;
                    CAcpt4 = DUrate;
                    CAunts = DUunts;
                    CAfsr = 'Y';
                    Leave;
                  endif;
                endif;
                readp ppr01;
              enddo;
            endif;
      /End-Free
     C*** End Add *** 09/20/16 ********************************
    C*
     C     svcde         endsr
     C***** Beg Add *** 04/04/16 **************************************
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
     C                   movel     'N'           carpt
     C                   movel     'Y'           cafsr
     C                   move      dhpos         caplac
     C                   movel     dhprcd        cacpt4
     C                   movel     dharea        capart
     C                   move      dhunts        caunts
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
     C                   If        *in99 = *off
     C*
     C                   If        CAsrc = 'A'
     C*
     C                   select
     C*
     C                   when      DHprCd = 'A0425' or DHprCd = 'A0435'
     C                             or DHprCd = 'A0436'
     C*
     C* Emergency Ambulance Mileage
     C*
     C                   if        %trim(DHtos) = 'D'
     C                   eval(h)   ##alw1 = rtmul2 * ##alw1
     C*
     C* Non-Emergency Ambulance Mileage
     C*
     C                   else
     C                   eval(h)   ##alw1 = rtmul3 * ##alw1
     C                   endif
     C*
     C* Emergency ambulance Transport
     C*
     C                   when      DHprCd = 'A0427' or DHprCd = 'A0429'
     C                   eval(h)   ##alw1 = rtmul2 * ##alw1
     C*
     C* All other ambulance services pay non-emergency rate
     C*
     C                   other
     C                   eval(h)   ##alw1 = rtmul3 * ##alw1
     C                   endsl
     C*
     C                   endif
     C*
     C* Durable Medical Equipment pay 100% of DMEPOS Fee Schedule
     C*
     C                   if        CAsrc = 'D'
     C                   eval(h)   ##alw1 = rtmul1 * ##alw1
     C                   endif
     C*
     C                   endif

     C                   move      ##alw1        camra
     C                   move      ##alw1        xcmra
     C*
     C                   endsr
     C*---------------------------------------------------------------------
     C***** End Add *** 04/04/16 **************************************
    C* ==================================================================
     Opca01     e            rca01
