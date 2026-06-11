    H**********************************************************************
     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000RAL
    H* DESCRIPTION: ADJUDICATION WORKFILE BUILD FOR AL
    H**********************************************************************
    H*  CREATED: 07/21/99  C:79   PROJECT: AL035
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  02/01/11 C:251 Project: AL076                                     *
    H*                 Add processing for OPT/CLAB field                  *
    H*                                                                    *
     H*  02/04/10 C:071 Support: 71488                                     *
    H*                 Correct issue with reading FMP2PAL                 *
    H*                                                                    *
     H*  08/14/09 C:198 Project: AL071                                     *
    H*                 Add processing for modifier TC                     *
    H*                                                                    *
     H*  08/21/08 C:062 Project: AL065                                     *
    H*                 Add processing for modifier FA                     *
    H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
    H*                                                                    *
     H*  04/07/04 C:79  Project: AL051                                     *
    H*                 1. Type of serv is AO-AS (ASC) pay discount rate   *
    H*                                                                    *
     H*  10/30/03 C:79  Project: FS271                                     *
    H*                 Use RXAWPD instead of KKEFDT to select AWP         *
    H*                                                                    *
     H*  10/02/03 C:79  Project: FSMNT                                     *
    H*                 Move clear of PCAWK2 after update PCA01            *
    H*                                                                    *
     H*  10/17/02 C:79  Project: FS239                                     *
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
    H*  06/22/02 C:79  Project: AL046                                     *
    H*                 1. If PT allowance exists use it. If not use       *
    H*                    physician amount.                               *
    H*                 2. If POS=11 load In-office surgery amount.        *
    H*                                                                    *
    H*  02/27/02 C:62  Project: MC071 31393                               *
    H*                 load pranam if specialty code 'PSY'.               *
    H*                                                                    *
    H*  11/26/01 C:79  Project: MC071 29801                               *
    H*                 In Dental subroutine use the service date for      *
    H*                 rules effective date                               *
    H*                                                                    *
    H*  08/30/01 C:79  Project: AL044                                     *
    H*                 1. Map for modifiers NU,RR,UE,KM,KN                *
    H*                 2. Use service date to set the rules date          *
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
    H*****************************************************************
    H*  INDICATOR USAGE:
    H*****************************************************************
    F*
     Ffdcalh    uf   e           k disk
     Ffddul     if   e           k disk
    F*** BEGIN ADD *** 03/28/01 **************************************
     Fftuml     if   e           k disk
    F*** END   ADD *** 03/28/01 **************************************
     Ffddhl     if   e           k disk
     Ffdddl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffmrxl     if   e           k disk
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
     Ffdrul1    if   e           k disk
    F*
     Ffmsvl1    if   e           k disk
    F*
     Ffdfrl5    if   e           k disk
    F*
    F*** BEG ADD *** 04/07/04 ****************************************
     Ffmp2lal   if   e           k disk
     Ffaeul     if   e           k disk
    F*** END ADD *** 04/07/04 ****************************************
    D*
     D qqf             s              7    dim(50)
    D*
    D*** BEG DEL *** 02/02/00 ****************************************
    D******************* FUF       250  7 0             FOLLOW UP FRM DY
    D******************* FUT       250  7 0             FOLLOW UP TO DAY
    D*** END DEL *** 02/02/00 ****************************************
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
    D*
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
     D camd50          s              3  2
     D camd51          s              3  2
     D camd54          s              3  2
     D camd55          s              3  2
     D camd56          s              3  2
     D camd62          s              3  2
     D camd66          s              3  2
     D camd80          s              3  2
     D catype          s              1
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D*** BEG ADD *** 04/07/04 *********************************************
     D kkcmsv          s                   like(svcmsv)
     D kktype          s                   like(catype)
     D*** END ADD *** 04/07/04 *********************************************
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
      *** Begin Change *** 05/02/18 *********************
     D***xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 05/02/18 *********************
     D xxfdat          s                   like(cafdat)
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
    C*  Claim Detail File read by EOB#
    C*
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
    C*
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
    C*
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
    C*
     C     prky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    cafdat
    C*
    C* Reasonable & Customary File
    C*
     C     rcky          klist
     C                   kfld                    qqco#
     C                   kfld                    cazipa
     C                   kfld                    xxcpt4
    C*** BEGIN CHG *** 03/28/01 **************************************
    C**********           KFLD           KKYMD
     C                   kfld                    kkefdt
    C*** BEGIN CHG *** 03/28/01 **************************************
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
     C*** BEG CHG *** 04/07/04 *********************************************
     C****************** kfld                    catype
     C                   kfld                    kktype
     C*** BEG CHG *** 04/07/04 *********************************************
     C                   kfld                    cafdat
    C*
     C     ruky1         klist
     C                   kfld                    qqst
     C*** BEG CHG *** 04/07/04 *********************************************
     C****************** kfld                    catype
     C                   kfld                    kktype
     C*** BEG CHG *** 04/07/04 *********************************************
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
    C*** BEG ADD *** 04/07/04 ****************************************
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
     C*** BEG ADD *** 02/04/10 *********************************************
     C                   kfld                    kkcmsv
     C*** END ADD *** 02/04/10 *********************************************
    C*
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*** END ADD *** 04/07/04 ****************************************
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
    C*****************************************************************
    C*
     C                   exsr      convrt
    C*
    C*****************************************************************
    C*  END PROGRAM AND CLOSE ALL FILES
    C*
     C                   close     facap
     C     term          tag
    C*
     C                   eval      xcmra = 0
     C                   return
     C                   move      *on           *inlr
    C********************************************************************
    C*  CONVERT FIELDS INTO BASE FIELDS USED FOR DETERMINING PAYMENT
    C*  CREATES WORKFILE WITH MRA AND MODIFIER AMOUNTS AND PROF/TECH
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
     C                   eval      kkymd = *zero
    C*
    C* Main read of detail charge file
    C*
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
    C*
     C                   eval      kkdseq = cadseq
     C                   eval      xxfdat = cafdat
    C*
    C*****************************************************************
    C*   If line item is in the reasonable and customary file - call
    C*   subrouting RECU to process all possibilities that may occur
    C*   ie.  BY-RPT, individual consideration, determine MRA
    C*
    C*** BEG CHG *** 11/26/01 ****************************************
    C********** CARCFL    IFEQ 'Y'
     C                   if        eobtyp = 'N'
     C                             and carcfl = 'Y'
    C*** END CHG *** 11/26/01 ****************************************
     C     dhky          chain     pdh01                              60
    C*
     C                   if        *in60 = *off
     C                   move      dhprcd        cacpt4
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      dhprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C                   endif
    C*
    C*** BEG ADD *** 11/26/01 ****************************************
     C                   movel     'P'           catype
    C*** END ADD *** 11/26/01 ****************************************
    C*
     C                   exsr      recu
     C                   else
    C*
    C*****************************************************************
    C*   Else - treat line item as a procedure code from the CPT4 file
    C*
    C*** BEG CHG *** 11/26/01 ****************************************
    C********** CARCFL    IFEQ 'C'
     C                   if        eobtyp = 'N'
     C                             and carcfl = 'C'
    C*** END CHG *** 11/26/01 ****************************************
     C     dhky          chain     pdh01                              60
    C*
     C                   if        *in60 = *off
     C                   move      dhprcd        cacpt4
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      dhprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C                   endif
    C*
    C*** BEG ADD *** 11/26/01 ****************************************
     C                   movel     'P'           catype
    C*** END ADD *** 11/26/01 ****************************************
    C*
     C                   exsr      proc
     C                   else
    C*
    C*****************************************************************
    C*   Else - treat line item as a dental code
    C*
    C*** BEG CHG *** 11/26/01 ****************************************
    C**********         if        carcfl = 'Y'
     C                   if        eobtyp = 'D'
    C*** END CHG *** 11/26/01 ****************************************
    C*** BEG ADD *** 08/22/02 *********************************************
     C                             and carcfl = 'C'
    C*** END ADD *** 08/22/02 *********************************************
    C*
     C     ddky          chain     pdd01                              60
    C*
     C                   if        *in60 = *off
     C                   move      ddprcd        cacpt4
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      ddprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C                   endif
    C*
    C*** BEG ADD *** 11/26/01 ****************************************
     C                   movel     'D'           catype
    C*** END ADD *** 11/26/01 ****************************************
    C*
     C                   exsr      dent
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
    C*****************************************************************
    C*   Else - treat line item as a pharmacy code
    C*** BEG CHG *** 11/26/01 ****************************************
    C********** CARCFL    IFEQ 'X'
     C                   if        eobtyp = 'X'
    C*** END CHG *** 11/26/01 ****************************************
    C*
     C     dpky          chain     pdp01                              60
    C*
     C                   if        *in60 = *off
     C                   move      dpndc         candc
     C                   endif
    C*
    C*** BEG ADD *** 11/26/01 ****************************************
     C                   movel     'X'           catype
    C*** END ADD *** 11/26/01 ****************************************
    C*
     C                   exsr      phar
     C                   else
    C*
    C*****************************************************************
    C*   Else - treat line item as a service code
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
     C     euky          chain     peu01                              99
    C*
    C*** BEG ADD *** 11/26/01 ****************************************
     C                   movel     'H'           catype
    C*** END ADD *** 11/26/01 ****************************************
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
     C                   if        mod1 <> *blanks
    C*
     C                   exsr      pmod
    C*
     C                   endif
    C*****************************************************************
    C*   Add all workfile fields to workfile
    C*   First initilize the work field XXMRA to the actual codes MRA.
    C*   This will be changed if there is a modifier and then moved
    C*   into CAAMT (FACAP) which is read in highest MRA order.
    C*
    C* Initialize work fields
    C*
    C*   Add all workfile fields to workfile
    C*   Load MRA or Charge to CAAMT in highest to lowest MRA order
     C                   if        xxmra < cachrg
     C                   z-add     xxmra         caamt                92
     C                   else
     C                   eval      caamt = cachrg
     C                   endif
    C*** BEG DEL *** 11/26/01 ****************************************
    C*  If R&C or CPT4 it is Physician type. Else if it is 'D' then
    C*  its Dental Else if its 'X' then its Pharmacy Else Hospital.
    C**********
    C**********
    C********** CARCFL    IFEQ 'Y'
    C********** CARCFL    OREQ 'C'
    C**********
    C**********           MOVEL'P'       CATYPE  1
    C**********
    C**********           ELSE
    C**********
    C********** CARCFL    IFEQ 'D'
    C**********
    C**********           MOVEL'D'       CATYPE  1
    C**********
    C**********           ELSE
    C**********
    C********** CARCFL    IFEQ 'X'
    C**********
    C**********           MOVEL'X'       CATYPE  1
    C**********
    C**********           ELSE
    C**********           MOVEL'H'       CATYPE  1
    C**********
    C**********           ENDIF                           CARCFL=X
    C**********
    C**********           ENDIF                           CARCFL=D
    C**********
    C**********           ENDIF                           CARCFL=Y
    C*** END DEL *** 11/26/01 ****************************************
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
    C*   Get the initial effective date of F4000N for this state
    C*
     C*** BEG ADD *** 04/07/04 *********************************************
     C                   select
    C*
     C                   when      eostyp = 'ASC'
     C                   eval      kktype = 'A'
    C*
     C                   other
     C                   eval      kktype = catype
    C*
     C                   endsl
     C*** END ADD *** 04/07/04 *********************************************
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
    C*
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
    C*    Clear CAPAID in detail file to reprocess correctly
    C*    Had problems with multiple surgical rule if reprocess
    C*    and there was a value in CAPAID
    C*
     C                   if        cacscd = '1  '
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
    C*
     C                   endif
    C*
    C* If FDCAP record recon doesn't match current recon release recd
    C*
     C                   else
     C                   except    rca01
    C*                                                    CARCN=XXRCN
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
    C*
    C*** BEG ADD *** 11/26/01 ****************************************
    C* Use service date for rules effective date
    C*  Setup correct effective date for rules
     C                   eval      kkefdt = cafdat
    C*** END ADD *** 11/26/01 ****************************************
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     ddprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
    C*
     C     prky          setgt     ppr01
     C     nxtpr3        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
    C*
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*** BEG DEL *** 11/26/01 ****************************************
    C*
    C*  Default to use service date for effective date for rules
    C******************** MOVE CAFDAT    KKEFDT
    C*** END DEL *** 11/26/01 ****************************************
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*** BEG DEL *** 11/26/01 ****************************************
    C*  Setup correct effective date for rules
    C******************** Z-ADDPREFDT    KKEFDT
    C*** END DEL *** 11/26/01 ****************************************
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
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
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
     C                   eval      caunts = dpunts
     C                   eval      camra = rxawp
     C                   move      rxptyp        captyp
     C                   move      rxstat        castat
     C                   move      'Y'           cafsr
    C*
    C* Use previous average wholesale price(AWP)
    C*
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
    C*
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
     C     '0 '          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * .125
    C*
     C                   endif
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
    C*** BEG ADD *** 08/30/01 ****************************************
    C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = pr92n5
    C*
     C                   endif
    C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = pr92n5
    C*
     C                   endif
    C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      camra = pranam
    C*
     C                   endif
    C*** END ADD *** 08/30/01 ****************************************
    C*** BEG ADD *** 08/21/08 ****************************************
    C*
     C     'FA'          lookup    mod(1)                                 99
     C                   if        *in99 = *on
     C                   eval      camra = prmra
     C                   endif
    C*** END ADD *** 08/21/08 ****************************************
    C*** BEG ADD *** 08/14/09 ****************************************
    C*
     C     'TC'          lookup    mod(1)                                 99
     C                   if        *in99 = *on
     C                   eval      camra = pr92n3
     C                   endif
    C*** END ADD *** 08/14/09 ****************************************
    C*** BEG ADD *** 02/01/11 ****************************************
    C*
     C     '26'          lookup    mod(1)                                 99
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
    C*** END ADD *** 02/01/11 ****************************************
    C*
     C     pmode         endsr
    C*
    C********************************************************************
    C** PROCEDURE CODE MRA SEARCH                             P R O C   *
    C********************************************************************
     C     proc          begsr
    C*
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*** BEG ADD *** 09/12/01 ****************************************
    C*  Default to service date for effective date for rules
    C*
     C                   eval      kkefdt = cafdat
    C*** END ADD *** 09/12/01 ****************************************
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
    C*** BEG DEL *** 09/12/01 ****************************************
    C********************
    C***Default to service date for effective date for rules
    C******************** Z-ADDCAFDAT    KKEFDT
    C*** END DEL *** 09/12/01 ****************************************
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
    C*** BEG DEL *** 09/12/01 ****************************************
    C** Setup correct effective date for rules
    C******************** Z-ADDPREFDT    KKEFDT
    C*** END DEL *** 09/12/01 ****************************************
    C*
    C*** BEG DEL *** 02/02/00 ****************************************
    C********** CAFOLU    IFGT 0
    C**********           EXSR FOLU
    C**********           ENDIF                           END CAFOLU
    C*** END DEL *** 02/02/00 ****************************************
    C*
    C* Load CAMRA with appropriate field based on the prov specialty
    C*
     C                   select
    C*
    C* CHI specialty use PR92N1
    C*
     C                   when      eospcd = 'CHI'
     C                   eval      camra = pr92n1
     C                   eval      xcmra = pr92n1
    C*
    C* OD specialty use PR92N2
    C*
     C                   when      eospcd = 'OD '
     C                             or eospcd = 'OPT'
    C*
     C                   eval      camra = pr92n2
     C                   eval      xcmra = pr92n2
    C*
    C* PHT specialty or PHT code use PRPROF if not 0 else use MRA.
    C* Pg 69 Notes 1-2 of Fee Schedule
    C*
     C                   when      eospcd = 'PHT'
    C*** BEG ADD *** 06/22/02 ****************************************
     C                             or cacpt4 >= '97001'
     C                             and cacpt4 <= '97770'
    C*
     C                   if        prprof <> 0
    C*** END ADD *** 06/22/02 ****************************************
    C*
     C                   eval      xcmra = prprof
     C                   eval      camra = prprof
    C*
    C*** BEG ADD *** 06/22/02 ****************************************
     C                   else
     C                   eval      camra = prmra
     C                   eval      xcmra = prmra
    C*
     C                   endif
    C*** END ADD *** 06/22/02 ****************************************
    C*
    C* ASC specialty use PR92N3
    C*
     C                   when      eospcd = 'ASC'
    C*
     C                   eval      xcmra = pr92n3
     C                   eval      camra = pr92n3
    C*
    C* DIA specialty use PR92N5
    C*
     C                   when      eospcd = 'DIA'
    C*
     C                   eval      camra = pr92n5
     C                   eval      xcmra = pr92n5
    C*
    C* ISP specialty or place of service=11 use PR92N4
    C*
     C                   when      eospcd = 'ISP'
    C*** BEG ADD *** 06/22/02 ****************************************
     C                             or dhpos = ' 11'
     C                             and pr92n4 <> 0
    C*** END ADD *** 06/22/02 ****************************************
    C*
     C                   eval      camra = pr92n4
     C                   eval      xcmra = pr92n4
    C*** BEGIN ADD *** 02/27/02 **************************************
    C*
    C* PSY specialty use PRANAM
    C*
     C                   when      eospcd = 'PSY'
    C*
    C*** BEG ADD *** 06/22/02 ****************************************
     C                   if        cafdat < 1020519
    C*** END ADD *** 06/22/02 ****************************************
     C                   eval      camra = pranam
     C                   eval      xcmra = pranam
    C*
    C*** BEG ADD *** 06/22/02 ****************************************
     C                   else
     C                   eval      camra = pr92n3
     C                   eval      xcmra = pr92n3
    C*
     C                   endif
    C*** END ADD *** 06/22/02 ****************************************
    C*** END   ADD *** 02/27/02 **************************************
    C*
     C*** Beg Add *** 02/01/11 ****************************************
     C                   when      eospcd = 'LAB'
     C                   eval      camra = pr92n2
     C                   eval      xcmra = pr92n2
     C*** End Add *** 02/01/11 ****************************************
     C                   other
    C*
     C                   eval      camra = prmra
     C                   eval      xcmra = prmra
    C*
     C                   endsl
    C*
    C* If there was no MRA set up for the providers specialty use the
    C* base MRA amount PRMRA
    C*
     C                   if        camra = 0
     C                   eval      camra = prmra
     C                   endif
    C*
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n3
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1ae         canct
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
     C                   move      dhpos         caplac
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
    C*   Modifiers
    C*
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 0.50
     C                   eval      camd80 = 0.25
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
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     dhprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
    C*** BEGIN ADD *** 03/28/01 **************************************
     C                   eval      kkefdt = cafdat
    C*** END   ADD *** 03/28/01 **************************************
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
    C*  Default to use service date for effective date for rules
    C*
     C                   eval      kkefdt = cafdat
    C*
     C                   else
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
     C                   eval      caprof = rcprof
    C*** BEGIN ADD *** 03/28/01 **************************************
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
    C*** END   ADD *** 03/28/01 **************************************
     C                   eval      caanam = rcanam
     C                   move      rcsurg        casurg
     C                   move      rcsupp        casupp
     C                   move      rcstat        castat
     C                   move      rcrpt         carpt
     C                   move      rcfsr         cafsr
     C*** BEG ADD *** 08/22/02 *********************************************
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C*
     C                   eval      caunts = dhunts
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
     C*
     C*** END ADD *** 08/22/02 *********************************************
     C                   move      dhpos         caplac
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
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 1.00
     C                   eval      camd54 = *zeros
     C                   eval      camd55 = *zeros
     C                   eval      camd56 = *zeros
     C                   eval      camd62 = *zeros
     C                   eval      camd66 = *zeros
     C                   eval      camd80 = *zeros
    C*
    C* REASONABLE AND CUSTOMARY DO NOT CONTAIN A VALID EFFECTIVE
    C* DATE FOR THE PROCEDURE CODE
    C*
    C*** BEGIN DEL *** 03/28/01 **************************************
    C********** RUKY      SETGTPRU01
    C********** RUKY1     REDPEPRU01                    56
    C*
    C********** *IN56     IFEQ '0'
    C**********           Z-ADDRUEFDT    KKEFDT
    C*
    C**********           ELSE
    C**********           MOVE *LOVAL    KKEFDT
    C*
    C**********           ENDIF
    C*** END   DEL *** 03/28/01 **************************************
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
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     durate        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   move      '000'         xxunit
    C*
    C*  Default to use service date for effective date for rules
    C*
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
    C*
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtsv1
     C*** BEG DEL *** 04/07/04 *********************************************
     C*
     C* Load percent for ASC's
     C*
     C                   if        euicn >= 'AO' and euicn <= 'AS'
     C*
     C                   eval      kkcmsv = '000OUTPT000'
     C*
     C     p2ky          setgt     pp201
     C     p2ky1         readpe    pp201                                  99
     C*
     C                   if        *in99 = *off
     C*** BEG ADD *** 02/04/10 *********************************************
     C                             and cafdat >= p2efdt
     C*** END ADD *** 02/04/10 *********************************************
     C*
     C                   eval      camra = (p2am01 * cachrg) / 100
     C                   move      *blank        cainvl
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
     C                   eval      cahtos = euicn
     C*** BEG ADD *** 02/04/10 *********************************************
     C                   else
     C                   move      'Y'           cainvl
     C*** END ADD *** 02/04/10 *********************************************
     C*
     C                   endif
     C*
     C                   endif                                                  euicn >= AO
     C*** END DEL *** 04/07/04 *********************************************
    C*
     C     svcde         endsr
     Opca01     e            rca01
