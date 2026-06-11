     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NPA                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR PA                           *
    H**********************************************************************
    H*  CREATED: 12/08/00  C:79   PROJECT: PA008                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  05/09/18 C:079 Support: 114 70740                                 *
    H*                 If DRG line was previously delete dont create.     *
    H*                                                                    *
     H*  04/27/18 C:180 Project: C114MJS                                   *
    H*                 Increase xxWAP size.                               *
    H*                                                                    *
     H*   04/05/18 C:079 PROJECT PA085                                     *
     H*                  Change DRG processing so that it hits rules       *
    H*                                                                    *
     H*   04/05/18 C:180 PROJECT PA085                                     *
     H*                  Change DRG processing                             *
     H*                                                                    *
     H*  01/22/18 C:079 PhaseWare: 114/63771                               *
    H*                 Add 19 and 20 to site of service array             *
    H*                                                                    *
     H*  01/26/16 C:079 PhaseWare: 114/39404                               *
    H*                 Dont set U&C flag to Y and dont load the rate      *
    H*                                                                    *
     H*  05/08/15 C:079 PhaseWare: 34226                                   *
    H*                 Pay site of service rate for 21,22,23,24,26,31,34, *
    H*                 41,51,53,56,61                                     *
    H*                                                                    *
     H*  10/29/13 C:274 Project: PA067                                     *
    H*                 New revenue codes.                                 *
    H*                                                                    *
     H*  11/19/10 C:079 Project: 114MXN                                    *
    H*                 Lookup the U&C allowance (85th percentile) for     *
    H*                                                                    *
     H*  11/16/10 C:257 Project: 114MXN                                    *
    H*                 Lookup the U&C allowance (85th percentile) for     *
    H*                 codes that do not have a FS allowance.             *
    H*                                                                    *
     H*  05/28/10 C:227 Support: 72552                                     *
    H*                 Change in SCVD to pay ASC bills for multiple proce-*
    H*                 dures based on highest FS allowance rather than    *
    H*                 highest amount billed.                             *
    H*                                                                    *
     H*  02/05/10 C:198 Support: 71626                                     *
    H*                 Allowed only active zip codes to be taken into     *
    H*                 consideration.                                     *
    H*                                                                    *
     H*  05/15/09 C:227 Support: 68178                                     *
    H*                 System allowance different from Bureau's           *
     H*                 calculation                                        *
    H*                                                                    *
     H*  05/14/09 C:198 Project: PA048                                     *
    H*                 Added processing for modifiers AU/AW/AV/9O         *
    H*                                                                    *
     H*  02/27/08 C:198 Support: 61294                                     *
    H*                 fixed decimal data error due to movement of        *
     H*                 durate to kkdrg#                                   *
    H*                                                                    *
     H*  09/12/07 C:47  Support: 58831
    H*                 Added end date to Support# 45207                   *
    H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  06/22/05 C:79  Support: 46923                                     *
    H*                 Change how we pay inpatient skilled nurse          *
    H*                                                                    *
     H*  04/08/05 C:79  Support: 46019                                     *
    H*                 Change how we find CCR's for rev codes             *
    H*                                                                    *
     H*  12/27/04 C:79  Support: 45207                                     *
    H*                 Per 167.1 Payment for anesthesia services mult conv*
    H*                 factors by 1.632                                   *
    H*                                                                    *
     H*  12/21/04 C:79  Support: 44157                                     *
    H*                 Be conservative on the Inpatient Cost Outlier      *
    H*                                                                    *
     H*  11/11/04 C:62  Project: PA027                                     *
    H*                 Check for DHPOS = '22'                             *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
    H*                                                                    *
     H*  03/24/04 C:79  Support: MC114 41888                               *
    H*                 Correct rcd format for reading FACAL1              *
    H*                                                                    *
     H*  03/05/04 C:79  Support: MC114 41575                               *
    H*                 Dont load CAFACF if Chain fails                    *
    H*                                                                    *
     H*  03/05/04 C:79  Project: PA024                                     *
    H*                 Update for 1/1/2004 Fee Schedule                   *
    H*                                                                    *
     H*  10/02/03 C:79  Project: FSMNT                                     *
    H*                 Move clear of PCAWK2 after update PCA01            *
    H*                                                                    *
    H*  08/27/03 C:79  Project: MC114 38781                               *
    H*                 SNF Inpatient pay 80% if rev code not in list      *
    H*                                                                    *
    H*  04/07/03 C:79  Project: PA020                                     *
    H*                 Pay exempt CORFs by Physical Therapy per visit rate*
    H*                                                                    *
    H*  02/12/03 C:79  Project: PA019                                     *
    H*                 Pay inpatient CAH using the Charge Master          *
    H*                                                                    *
    H*  11/18/02 C:79  Project: FSMNT                                     *
    H*                 On Inpatient bill include bill type 110            *
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
    H*  07/12/02 C:79  Support: MC114 33132                               *
    H*                 Remove Home Health bill types from Inp selection   *
    H*                                                                    *
    H*  05/21/02 C:79  Support: MC114 32491                               *
    H*                 Pay inpatient rehab using the Charge Master        *
    H*                                                                    *
    H*  03/28/02 C:79  Support: TXMNT 31825                               *
    H*                 Divide UCR mod 26,27 values by 100 to pop in 3.2   *
    H*                 field.                                             *
    H*                                                                    *
    H*  03/10/02 C:79  Project: PA014                                     *
    H*                 1. Load site of service amount if spec is LAB or PSY
    H*                    per Fee Schedule                                *
    H*                 2. Load separate procedure flag.                   *
    H*                                                                    *
    H*  01/16/02 C:79  Project: 114MAO                                    *
    H*                 If FS allowance=0 load 80% charged to CAAMT so that*
    H*                 the highest allowance proc is paid 1st.            *
    H*                                                                    *
    H*  07/13/01 C:79  Project: 114MCG                                    *
    H*                 Add files to load amounts for PA Hospital bills    *
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
     F***** Beg Add ***** 04/05/18 ************************************
     F                                     infds(dbfds)
     F***** End Add ***** 04/05/18 ************************************
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
     Ffmp1l     if   e           k disk
     F*** BEGIN DEL *** 05/30/01 *******************************************
     F**********fmgzl     if   e           k disk
     F*** END   DEL *** 05/30/01 *******************************************
     Ffmzrlpa   if   e           k disk
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
     F*** BEGIN ADD *** 03/28/01 **************************************
     Fftuml     if   e           k disk
     F*** END   ADD *** 03/28/01 **************************************
     F*** BEG ADD *** 07/13/01 ****************************************
     Fftfelpa   if   e           k disk
     Ffaeul     if   e           k disk
     Ffapolpa   if   e           k disk
     Ffaaplpa   if   e           k disk
     Ffasnlpa   if   e           k disk
     Ffahhlpa   if   e           k disk
     Ffacglpa   if   e           k disk
     Ffacgl3pa  if   e           k disk    rename(pcg01:pcg02)
     Ffacgl1pa  if   e           k disk    rename(pcg01:pcg03)
     Ffacrl1pa  if   e           k disk
     Ffarrlpa   if   e           k disk
     Ffacal1    uf   e           k disk    Rename(pca01:pca02) usropn
     Ffaphlpa   if   e           k disk
     F*** END ADD *** 07/13/01 ****************************************
     F*** BEG ADD *** 04/07/03 ****************************************
     Ffaptlpa   if   e           k disk
     F*** END ADD *** 04/07/03 ****************************************
     F*** BEG ADD *** 11/16/10 *******************************************
     Ffmr1l     if   e           k disk
     F*** END ADD *** 11/16/10 *******************************************
     D****** Beg Add ***** 04/05/18 ****************************************
     D*--------------------------------------------------------------------
     D* Procedures
     D*--------------------------------------------------------------------
     D*--------------------------------------------------------------------
     D* Retrieve inpatient allowance.
     D*--------------------------------------------------------------------
      *
     D Rtv_inpat_Alw   pr                  extpgm('FA756RPA')
     D                                9  0
     D                                1
     D                                9  2
     D                                9  2
      *
     D*--------------------------------------------------------------------
     D* Data structures
     D*--------------------------------------------------------------------
     D*
     D dbfds           ds
     D dbrrn                 397    400b 0
      ****** End Add ***** 04/05/18 ****************************************
      *
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
     D*--------------------------------------------------------------------
     D* Stand Alone
     D*--------------------------------------------------------------------
     D*
     D qqf             s              7    dim(50)
     D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D*** Beg Add *** 05/08/15 *******************************************
     D strPosArray     s              3a   dim(20)
     D xxSiteOfServ    s              2  0
     D*** End Add *** 05/08/15 *******************************************
     D*** BED ADD *** 07/13/01 ****************************************
     D xx551           s              1    inz('N')
     D xxdseq          s                   like(cadseq) inz(0)
     D xxprev          s                   like(casvcd) inz(0)
     D*** END ADD *** 07/13/01 ****************************************
     D*** BED ADD *** 06/20/05 ****************************************
     D xxinp           s              1    inz('N')
     D*** END ADD *** 06/20/05 ****************************************
     D sum1            s             14  5
     D sum1a           s             14  5
     D sum1b           s             14  5
     D sum2            s             14  5
     D sum3            s             14  5
     D sum3a           s             14  5
     D sum4            s             14  5
     D sum5            s             14  5
     D sum6            s             14  5
     D sum6a           s             14  5
     D sum6b           s             14  5
     D sum7            s             14  5
     D sum8            s             14  5
     D sum9            s             14  5
     D sum10           s             14  5
     D sum11           s             14  5
     D sum12           s             14  5
     D sum12a          s             14  5
     D sum12b          s             14  5
     D sum13           s             14  5
     D sum14           s             14  5
     D sum15           s             14  5
     D sum16           s             14  5
     D sum17           s             14  5
     D sum18           s             14  5
     D*** BEG ADD *** 02/27/08 *********************************************
     D xxdigits        c                   '0123456789'
     D xxpos           s              1  0 inz(0)
     D*** END ADD *** 02/27/08 *********************************************
     D*
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
     C*** BEG ADD *** 07/13/01 *********************************************
     D kkmed#          s                   like(cgmed#)
     D k1med#          s                   like(cgmed#)
     D kkgrp           s                   like(apgrp)
     D kkrvcd          s                   like(cgrvcd)
     D kksvcd          s                   like(cgsvcd)
     D kkhcps          s                   like(cghcps)
     D kkdrg#          s                   like(fedrg#)
     C*** END ADD *** 07/13/01 *********************************************
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
     D xxdcs#          s              5  0
     D*** BEGIN ADD *** 03/28/01 **************************************
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D*** BEGIN ADD *** 03/28/01 **************************************
     D xxefdt          s                   like(prefdt)
     D*** BEGIN DEL *** 05/30/01 *******************************************
     D********** xxfrzp          s                   like(gzfrzp)
     D*** END   DEL *** 05/30/01 *******************************************
     D xxinvl          s              1
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D*** BEGIN DEL *** 05/30/01 *******************************************
     D********** xxzipa          s                   like(gzzipa)
     D*** END   DEL *** 05/30/01 *******************************************
     D xxzip           s                   like(zrfzcd)
     D xxrgn#          s                   like(zrrgn#)
     D****** Beg Add ***** 04/05/18 ****************************************
     D inrrn           s              9  0
     D outrtcd         s              1
     D outallow1       s              9  2
     D outallow2       s              9  2
     D****** End Add ***** 04/05/18 ****************************************
      *
     C     caky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*** BEGIN DEL *** 05/30/01 *******************************************
      *
      * Reasonable & Customary File
     C*****gzky*         klist
     C**********         kfld                    qqco#
     C**********         kfld                    xxfrzp
     C*** END   DEL *** 05/30/01 *******************************************
      *
      *  Procedure Code file
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
     C*** BEGIN CHG *** 03/28/01 **************************************
     C**********         kfld                    kkymd
     C                   kfld                    kkefdt
     C*** END   CHG *** 03/28/01 **************************************
     C*** BEGIN ADD *** 03/28/01 **************************************
     C*
     C*  Usual and Customary Modifier Percent File
     C*
     C     umky          klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C*** END   ADD *** 03/28/01 **************************************
    C*
    C*  Provider Master File
     C     p1key         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
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
     C*
     C*  State Zip Code/Region File
     C     zrky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxzip
     C                   kfld                    kkefdt
    C*
     C*** BEG ADD *** 07/13/01 ****************************************
    C*
    C*  Procedure Code file
    C*
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
    C*
     C*  Hospital Header File
     C*
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*
     C*  Federal Register File
     C     feky          KLIST
     C                   KFLD                    kkdrg#
     C*
     C*  Prospective Payment File
     C     poky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kkefdt
     C*
     C*  Prospective Payment File
     C     poky1         klist
     C                   kfld                    kkmed#
     C*
     C*  Prospective Payment File
     C     pokya         klist
     C                   kfld                    k1med#
     C                   kfld                    kkefdt
     C*
     C*  Prospective Payment File
     C     poky1a        klist
     C                   kfld                    k1med#
     C*
     C*  ASC by region/group/efdt
     C     apky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kkgrp
     C                   kfld                    kkefdt
     C*
     C*  ASC by region/group/efdt
     C     apky1         klist
     C                   kfld                    kkmed#
     C                   kfld                    kkgrp
     C*
     C*  ASC by region/group/efdt
     C     apkya         klist
     C                   kfld                    k1med#
     C                   kfld                    kkgrp
     C                   kfld                    kkefdt
     C*
     C*  ASC by region/group/efdt
     C     apky1a        klist
     C                   kfld                    k1med#
     C                   kfld                    kkgrp
     C*
     C*  Skilled Nursing File
     C     snky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kkefdt
     C*
     C*  Skilled Nursing File
     C     snky1         klist
     C                   kfld                    kkmed#
     C*
     C*  Skilled Nursing File
     C     snkya         klist
     C                   kfld                    k1med#
     C                   kfld                    kkefdt
     C*
     C*  Skilled Nursing File
     C     snky1a        klist
     C                   kfld                    k1med#
     C*
     C*  Home Health File
     C     hhky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kkefdt
     C*
     C*  Home Health File
     C     hhky1         klist
     C                   kfld                    kkmed#
     C*
     C*  Home Health File
     C     hhkya         klist
     C                   kfld                    k1med#
     C                   kfld                    kkefdt
     C*
     C*  Home Health File
     C     hhky1a        klist
     C                   kfld                    k1med#
     C*
     C*  Charge Master File
     C     cgky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kksvcd
     C                   kfld                    kkhcps
     C                   kfld                    kkrvcd
     C                   kfld                    kkefdt
     C*
     C*  Charge Master File
     C     cgky1         klist
     C                   kfld                    kkmed#
     C                   kfld                    kksvcd
     C                   kfld                    kkhcps
     C                   kfld                    kkrvcd
     C*
     C*  Charge Master File
     C     cgky2         klist
     C                   kfld                    kkmed#
     C*
     C*  Charge Master File
     C     cgky3         klist
     C                   kfld                    kkmed#
     C                   kfld                    kksvcd
     C                   kfld                    kkrvcd
     C                   kfld                    kkefdt
     C*
     C*  Charge Master File
     C     cgky4         klist
     C                   kfld                    kkmed#
     C                   kfld                    kksvcd
     C                   kfld                    kkrvcd
     C*
     C*  Charge Master File
     C     cgky5         klist
     C                   kfld                    kkmed#
     C                   kfld                    kksvcd
     C                   kfld                    kkefdt
     C*
     C*  Charge Master File
     C     cgky6         klist
     C                   kfld                    kkmed#
     C                   kfld                    kksvcd
     C*
     C*  Charge Master File
     C     cgkya         klist
     C                   kfld                    k1med#
     C                   kfld                    kksvcd
     C                   kfld                    kkhcps
     C                   kfld                    kkrvcd
     C                   kfld                    kkefdt
     C*
     C*  Charge Master File
     C     cgky1a        klist
     C                   kfld                    k1med#
     C                   kfld                    kksvcd
     C                   kfld                    kkhcps
     C                   kfld                    kkrvcd
     C*
     C*  Charge Master File
     C     cgky2a        klist
     C                   kfld                    k1med#
     C*
     C*  Charge Master File
     C     cgky3a        klist
     C                   kfld                    k1med#
     C                   kfld                    kksvcd
     C                   kfld                    kkrvcd
     C                   kfld                    kkefdt
     C*
     C*  Charge Master File
     C     cgky4a        klist
     C                   kfld                    k1med#
     C                   kfld                    kksvcd
     C                   kfld                    kkrvcd
     C*
     C*  Charge Master File
     C     cgky5a        klist
     C                   kfld                    k1med#
     C                   kfld                    kksvcd
     C                   kfld                    kkefdt
     C*
     C*  Charge Master File
     C     cgky6a        klist
     C                   kfld                    k1med#
     C                   kfld                    kksvcd
     C*
     C*  Provider MSA and Region file
    C*
     C     rrky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kkrvcd
     C*** BEG ADD *** 04/08/05 *********************************************
    C*
     C     rrky1         klist
     C                   kfld                    kkmed#
     C*** END ADD *** 04/08/05 *********************************************
     C*
     C*  Pharmacy RCC file
    C*
     C     phky          klist
     C                   kfld                    kkmed#
     C*
     C*  Provider MSA and Region file
    C*
     C     rrkya         klist
     C                   kfld                    k1med#
     C                   kfld                    kkrvcd
     C*
     C*  Pharmacy RCC file
    C*
     C     phkya         klist
     C                   kfld                    k1med#
    C*
     C*  Detail charge workfile
     C*
     C     caky2         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    xxdseq
     C*
     C*  Detail charge file
     C*
     C     caky3         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    eorcn
     C                   kfld                    xxdseq
     C*** END ADD *** 07/13/01 ****************************************
     C*** BEG ADD *** 04/07/03 *********************************************
     C     ptky          klist
     C                   kfld                    kkmed#
     C                   kfld                    kkefdt
     C     ptky1         klist
     C                   kfld                    kkmed#
     C*** END ADD *** 04/07/03 *********************************************
     C*** BEG ADD *** 11/16/10 *********************************************
     C     r1ky1         klist
     C                   kfld                    rcco#
     C                   kfld                    rczipa
     C                   kfld                    rccpt4
     C                   kfld                    rcefdt
     C*** END ADD *** 11/16/10 *********************************************
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
     C*** BEG ADD *** 11/16/10 *********************************************
    C*
    C* For FA760R
    C*
     C     plist1        plist
     C                   parm                    caco#
     C                   parm                    casub#
     C                   parm                    cafnd
     C                   parm                    cafyr
     C                   parm                    cacase
     C                   parm                    caeob#
     C                   parm                    cadseq
     C                   parm                    xxprcd            5
     C                   parm                    cafdat
     C                   parm                    ##zipa            5 0
     C                   parm                    ##mod1            2
     C                   parm                    ##mod2            2
     C                   parm                    ##mod3            2
     C                   parm                    ##mod4            2
     C                   parm                    xxallw            9 2
    C*
    C* For SA001R
    C*
     C     plist2        plist
     C                   parm      *blanks       ##wcif            1            WC INST
     C                   parm      *blanks       ##mcif            1            MC INST
     C                   parm      *blanks       ##wcin            1            WC INTERFACE
     C                   parm      *blanks       ##ar              1            MC AR INST
     C                   parm      *blanks       ##ntwk            1            NETWORK INST
     C                   parm      *blanks       ##ur              1            PRECERT INST
     C                   parm      *blanks       ##med             1            MEDMGMT INST
     C                   parm      *blanks       ##edi             1            EDI INST
     C                   parm      *blanks       ##wcdk            1            WC DARTS KEY
     C                   parm      *blanks       ##rhab            1            REHAB INST
     C                   parm      *blanks       ##mss             1            MS SUPPORT
     C                   parm      *blanks       ##fsch            1            FEE SCHEDULE
     C                   parm      *blanks       ##prvb            1            PROVIDER BILL
     C                   parm      *blanks       ##npwc            1            NON-PDS WC  LS
     C                   parm      *blanks       ##os              1            OUTOME/SAVINGS
     C                   parm      *blanks       ##arls            1            Auto Release Flag
     C                   parm      *blanks       ##ccod            1            Correct Coding Flag
     C                   parm      *blanks       ##crvw            1            Code Review Flag
     C                   parm      *blanks       ##prom            1            ProMatch Flag
     C                   PARM      *blanks       ##mcrv            1            Medicare CR Flag
     C                   parm      *blanks       ##freq            1            Frequency Flag
     C                   parm      *blanks       ##crac            1            CR Auto Complete
     C                   parm      *blanks       ##preo            1            CR Auto Complete
     C                   parm      *blanks       ##parn            1            Par/Non-Par/Both
     C                   parm      *blanks       ##mgri            1            Multi Group Restr
     C                   parm      *blanks       ##kcif            1            Keep Claimant info
     C                   parm      *blanks       ##curs            1            PBILLS entry cursor
     C                   parm      *blanks       ##pcib            1            Prov Check in Batch
     C                   parm      *blanks       ##clt#            3            Client Number
     C                   parm      *blanks       ##ucpf            1            UCR Percentile
     C*** END ADD *** 11/16/10 *********************************************
    C*
     C                   move      qqdcs#        xxdcs#
    C*
     C                   open      facap
    C*
     C*** BEG ADD *** 11/16/10 *********************************************
     C                   EXSR      pgmstr
     C*** END ADD *** 11/16/10 *********************************************
     C                   exsr      convrt
     C*** BEG ADD *** 07/13/01 *********************************************
     C**********************************************************************
     C* Update the correct revenue code for inpatient skilled nursing
     C**********************************************************************
     C*
     C*** BEG ADD *** 08/27/03 *********************************************
     C                   if        eublty>='210' and eublty<='229'
     C                             or eublty>='240' and eublty<='299'
     C                   open      facal1
     C                   if        xxdseq <> 0
     C*** END ADD *** 08/27/03 *********************************************
     C*** BEG DEL *** 08/27/03 *********************************************
     C****************** if        eobtyp = 'H' and xxdseq <> 0
     C******************
     C****************** open      facal1
     C*** END DEL *** 08/27/03 *********************************************
     C*
     C     caky2         chain     pca02                              99
     C*
     C                   if        *in99 = *off
     C*
     C     snky          setgt     psn01
     C     snky1         readpe    psn01                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     snkya         setgt     psn01
     C     snky1a        readpe    psn01                                  99
     C*
     C                   endif
     C*
     C                   if        *in99 = *off
     C                   eval      casnfi = sninpt
     C                   endif
     C*
     C                   update    pca02
     C*
     C* Update CAMRAA for skilled nurse amount
     C*
     C     caky3         chain     pca01                              99
     C                   if        *in99 = *off
     C                   eval      camraa = sninpt
     C                   update    pca01
     C                   endif
     C*
     C                   endif
     C*
     C*** BEG ADD *** 08/27/03 *********************************************
     C*
     C* If revenue code was not in the list then set to pay 80% of charged
     C* or RCC
     C*
     C                   else
     C     caky1         chain     pca02                              99
     C                   dow       *in99 = *off
     C                   eval      cascma = 'N'
     C                   eval      caamt  = .80 * cachrg
     C                   update    pca02
     C*
     C* Update CAMRAA for skilled nurse amount
     C*
     C                   eval      xxdseq = cadseq
     C*
     C     caky3         chain     pca01                              99
     C                   if        *in99 = *off
     C                   eval      camraa = .80 * cachrg
     C                   update    pca01
     C                   endif
     C*
     C*** BEG CHG *** 03/24/04 *********************************************
     C**** caky1         reade     pca01                                  99
     C     caky1         reade     pca02                                  99
     C*** END CHG *** 03/24/04 *********************************************
     C*
     C                   enddo
     C*** END ADD *** 08/27/03 *********************************************
     C*
     C                   endif
     C*
     C*** END ADD *** 07/13/01 *********************************************
     C*** BEG ADD *** 08/27/03 *********************************************
     C*
     C                   close     facal1
     C*
     C                   endif                                                  if eoblty
     C*** END ADD *** 08/27/03 *********************************************
    C*
    C*  END PROGRAM AND CLOSE ALL FILES
     C                   close     facap
     C*** BEG ADD *** 07/13/01 *********************************************
     C*** BEG DEL *** 08/27/03 *********************************************
     C****************** close     facal1
     C*** END DEL *** 08/27/03 *********************************************
     C*** END ADD *** 07/13/01 *********************************************
    C*
     C     term          tag
    C*
     C                   move      *on           *inlr
    C*
     C                   eval      xcmra = 0
    C*
     C                   return
    C*
     C*** BEG ADD *** 11/16/10 *********************************************
    C********************************************************************
    C*  RETRIEVE SYSTEM VALUES
    C*****************************************************************
     C     pgmstr        begsr
     C*
    C                   call      'SA001R'      plist2
     C*** Beg Add *** 05/08/15 *********************************************
     C                   Clear                   strPosArray
     C**************     Eval      strPosArray(1) = ' 21'
     C**************     Eval      strPosArray(2) = ' 22'
     C**************     Eval      strPosArray(3) = ' 23'
     C**************     Eval      strPosArray(4) = ' 24'
     C**************     Eval      strPosArray(5) = ' 26'
     C**************     Eval      strPosArray(6) = ' 31'
     C**************     Eval      strPosArray(7) = ' 34'
     C**************     Eval      strPosArray(8) = ' 41'
     C**************     Eval      strPosArray(9) = ' 51'
     C**************     Eval      strPosArray(10) = ' 53'
     C**************     Eval      strPosArray(11) = ' 56'
     C**************     Eval      strPosArray(12) = ' 61'
     C                   Eval      strPosArray(1) = ' 19'
     C                   Eval      strPosArray(2) = ' 20'
     C                   Eval      strPosArray(3) = ' 21'
     C                   Eval      strPosArray(4) = ' 22'
     C                   Eval      strPosArray(5) = ' 23'
     C                   Eval      strPosArray(6) = ' 24'
     C                   Eval      strPosArray(7) = ' 26'
     C                   Eval      strPosArray(8) = ' 31'
     C                   Eval      strPosArray(9) = ' 34'
     C                   Eval      strPosArray(10) = ' 41'
     C                   Eval      strPosArray(11) = ' 51'
     C                   Eval      strPosArray(12) = ' 53'
     C                   Eval      strPosArray(13) = ' 56'
     C                   Eval      strPosArray(14) = ' 61'
     C*** End Add *** 01/22/18 *********************************************
     C*** End Add *** 05/08/15 *********************************************
     C*
     C     pgmstre       endsr
     C*** END ADD *** 11/16/10 *********************************************
    C********************************************************************
    C*  Convert fields into base fields used for determining payment
    C*  creates workfile with MRA and modifier amounts and prof/tech
    C*****************************************************************
     C     convrt        begsr
    C*
    C* Retrieve hospital header information
    C*
     C*** BEG ADD *** 07/13/01 *********************************************
     C     euky          chain     peu01                              99
     C     p1key         chain     pp101                              99
     C*** END ADD *** 07/13/01 *********************************************
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
     C***** Beg Add ***** 04/05/18 *****************************************
      /Free
       //*Calculate medicare inpatient bill
       //*** Beg Chg *** 05/09/18 ***************************************
       //*** if EORcn = 0 and EOBtyp = 'H';
       if EORcn = 0 and EOBtyp = 'H' and EOflg8 = ' ';
       //*** End Chg *** 05/09/18 ***************************************
          if  eublty>='110' and eublty<='129'
             or eublty>='150' and eublty<='209'
             or eublty>='300' and eublty<='309'
             or eublty>='340' and eublty<='429'
             or eublty>='440' and eublty<='529'
             or eublty>='540' and eublty<='629'
             or eublty>='760' and eublty<='829'
             or eublty>='850' and eublty<='929'
             or eublty>='940' and eublty<='999';
             if eospcd <> 'REH' and eospcd <> 'CAH';
                ExSr Medicare_inpt;
             endif;
          endif;
       endif;
      /End-Free
     C***** End Add ***** 04/05/18 *****************************************
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
     C*** BEG ADD *** 11/16/10 *********************************************
    C*
     C                   if        (eobtyp = 'D' or eobtyp = 'N')
    C                             and cafdat >= 1101101
    C*
    C* Populate the required parameters
    C*
     C*** BEG ADD *** 11/19/10 *********************************************
     C                   if        eobtyp = 'N'
    C*
    C* Chain out to the Physician Bill Detail file
    C*
     C     dhky          chain     pdh01                              60
    C*
     C                   if        *in60 = *off
     C*** END ADD *** 11/19/10 *********************************************
     C                   move      dhmod1        ##mod1
     C                   move      dhmod2        ##mod2
     C                   move      dhmod3        ##mod3
     C                   move      dhmod4        ##mod4
     C*** BEG ADD *** 11/19/10 *********************************************
     C                   endif
     C                   endif
     C*** END ADD *** 11/19/10 *********************************************
     C                   move      cacpt4        xxprcd
     C                   move      eoflic        ##zipa
    C*
    C* Retrieve PA Code Allowance
    C*
    C                   call      'FA760R'      plist1
    C*
     C*** Beg Add *** 01/26/16 *********************************************
    C****************** if        xxallw = 0
    C****************** move      'Y'           carcfl
    C****************** endif                                                  xxallw
     C*** End Add *** 01/26/16 *********************************************
     C*** BEG ADD *** 11/19/10 *********************************************
    C                   if        xxprcd >= '00100' and xxprcd <= '01999'
    C*
     C                   eval      xxcpt4 = xxprcd
    C*
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  99
    C*
     C*** Beg Add *** 01/26/16 *********************************************
     C****************   if        *in99 = *off and pranam = 0
    C****************   move      'Y'           carcfl
    C****************   endif                                                  xxallw
     C*** End Add *** 01/26/16 *********************************************
    C*
    C                   endif                                                  xxallw
     C*** END ADD *** 11/19/10 *********************************************
    C                   endif                                                  eobtyp
    C*
     C*** END ADD *** 11/16/10 *********************************************
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
    C* Load modifiers into table
     C                   exsr      loadmod
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
    C* Load modifiers into table
     C                   exsr      loadmod
    C*
     C                   exsr      proc
     C*
     C* Else treat line itme as a dental code and chain to dental detail
     C                   else
     C                   if        eobtyp = 'D'
    C*** BEG ADD *** 08/22/02 *********************************************
     C                             and carcfl = 'C'
    C*** END ADD *** 08/22/02 *********************************************
     C     ddky          chain     pdd01                              60
     C*
     C                   if        *in60 = *off
     C                   eval      cacpt4 = ddprcd
    C*** BEG ADD *** 08/22/02 *********************************************
     C                   move      ddprcd        xxcpt4
    C*** END ADD *** 08/22/02 *********************************************
     C                   endif
     C*
     C                   eval      catype = 'D'
     C*
     C                   exsr      dent
     C*
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
     C*** BEG DEL *** 05/28/10 *********************************************
     C*******            eval      xxmra = camra
     C*** END DEL *** 05/28/10 *********************************************
    C*
    C* If modifiers are present then adjust CAAMT accordingly.  CAAMT
    C* is the key field used to process FACAP in highest MRA order.
     C                   if        mod1 <> *blanks
    C*
     C                   exsr      pmod
    C*
     C                   endif
    C*
    C*   Add all workfile fields to workfile
    C*   Load MRA or Charge to CAAMT in highest to lowest MRA order
     C                   if        xxmra < cachrg
     C*** BEG ADD *** 01/16/02 *********************************************
     C                   if        xxmra <> 0
     C*** END ADD *** 01/16/02 *********************************************
     C                   z-add     xxmra         caamt                92
     C*** BEG ADD *** 01/16/02 *********************************************
     C                   else
     C                   eval      caamt = cachrg * .80
     C                   endif
     C*** END ADD *** 01/16/02 *********************************************
    C*
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
     C*
     C* Clear FACAP to prevent carryover of values to next record.
     C*
     C*** BEG ADD *** 08/13/02 ****************************************
     C*** BEG DEL *** 10/02/03 ****************************************
     C****************** clear                   pcawk2
     C*** END DEL *** 10/02/03 ****************************************
     C*** END ADD *** 08/13/02 ****************************************
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
     C                   EXCEPT    rca01
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
    C** DENTAL CODE MRA SEARCH                                D E N T   *
    C********************************************************************
     C     dent          begsr
    C*
    C*  Process dental codes
    C*
    C*  Default to service date for effective date for rules
     C                   eval      kkefdt = cafdat
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
     C* Load amounts based on service zip of the provider
     C*** begin chg *** 11/11/04 *******************************************
     C                   clear                   dhpos
     C*** end   chg *** 11/11/04 *******************************************
     C                   exsr      loadamt
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a3         canct
     C*** BEG CHG *** 04/03/02 *********************************************
     C****************** move      pr1ad         casepp
     C                   move      pr1a4         casepp
     C*** END CHG *** 04/03/02 *********************************************
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
     C                   move      ddpos         caplac
     C                   move      ddprcd        cacpt4
     C                   eval      caunts = ddunts
    C*
     C                   move      ddrcfl        carcfl
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr3
    C*
    C*
     C     dente         endsr
    C*
    C********************************************************************
    C** LOADAMT SUBROUTINE                             L O A D A M T    *
    C********************************************************************
     C     loadamt       begsr
     C*** Beg Add *** 05/08/15 *******************************************
     C                   eval      xxSiteOfServ = %lookup(dhpos:strPosArray)
     C*** End Add *** 05/08/15 *******************************************
     C*
     C                   if        eoflic <> *blanks
     C                   movel     eoflic        xxzip
     C*
     C                   else
     C                   movel     p1zip         xxzip
     C*
     C                   endif
     C*
     C                   eval      xxrgn# = 3
     C*
     C     zrky          setgt     pzr01
     C                   readp     pzr01                                  99
     C*
     C*** Beg Chg *** 02/05/10 **********************************************************
     C********           if        *in99 = *off and xxzip >= zrfzcd
     C********                     and xxzip <= zrtzcd
     C                   dow       *in99 = *off
     C*
     C                   if        xxzip >= zrfzcd
     C                             and xxzip <= zrtzcd
     C                             and zrstat = 'A'
     C*** End Chg *** 02/05/10 **********************************************************
     C*
     C                   eval      xxrgn# = zrrgn#
     C*
     C*** Beg Add *** 02/05/10 **********************************************************
     C                   leave
     C*** End Add *** 02/05/10 **********************************************************
     C*
     C                   endif
     C*
     C*** Beg Add *** 02/05/10 **********************************************************
     C                   readp     pzr01                                  99
     C*
     C                   enddo
     C*** End Add *** 02/05/10 **********************************************************
     C* Load amount fields based on region
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C*
     C*** BEG ADD *** 03/10/02 *********************************************
     C*
     C* Load the site of service amount for certain specialties
     C*
     C*** begin chg *** 11/11/04 *******************************************
     C**********         if        (eospcd = 'LAB' or eospcd = 'PSY')
     C*** Beg Chg *** 05/08/15 *******************************************
     C****************   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C****************             dhpos = ' 22')
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             xxSiteofServ <> 0)
     C*** End Chg *** 05/08/15 *******************************************
     C*** end   chg *** 11/11/04 *******************************************
     C                             and pr92nf <> 0
     C                   eval      camra = pr92nf
     C                   else
     C*** END ADD *** 03/10/02 *********************************************
     C                   eval      camra = prmra
     C*** BEG ADD *** 03/10/02 *********************************************
     C                   endif
     C*** END ADD *** 03/10/02 *********************************************
     C*
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n1
     C                   eval      caanam = pranam
     C*
     C                   when      xxrgn# = 2
     C*** BEG ADD *** 03/10/02 *********************************************
     C*
     C* Load the site of service amount for certain specialties
     C*
     C*** begin chg *** 11/11/04 *******************************************
     C**********         if        (eospcd = 'LAB' or eospcd = 'PSY')
     C*** Beg Chg *** 05/08/15 *******************************************
     C**************     if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C**************               dhpos = ' 22')
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             xxSiteofServ <> 0)
     C*** End Chg *** 05/08/15 *******************************************
     C*** end   chg *** 11/11/04 *******************************************
     C                             and pr92ng <> 0
     C                   eval      camra = pr92ng
     C                   else
     C*** END ADD *** 03/10/02 *********************************************
     C                   eval      camra = pr92n3
     C*** BEG ADD *** 03/10/02 *********************************************
     C                   endif
     C*** END ADD *** 03/10/02 *********************************************
     C                   eval      caprof = pr92n4
     C                   eval      catech = pr92n5
     C                   eval      caanam = pr92nj
     C*
     C                   when      xxrgn# = 3
     C*** BEG ADD *** 03/10/02 *********************************************
     C*
     C* Load the site of service amount for certain specialties
     C*
     C*** begin chg *** 11/11/04 *******************************************
     C**********         if        (eospcd = 'LAB' or eospcd = 'PSY')
     C*** Beg Chg *** 05/08/15 *******************************************
     C***************    if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C***************              dhpos = ' 22')
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             xxSiteofServ <> 0)
     C*** End Chg *** 05/08/15 *******************************************
     C*** end   chg *** 11/11/04 *******************************************
     C                             and pr92nh <> 0
     C                   eval      camra = pr92nh
     C                   else
     C*** END ADD *** 03/10/02 *********************************************
     C                   eval      camra = pr92n7
     C*** BEG ADD *** 03/10/02 *********************************************
     C                   endif
     C*** END ADD *** 03/10/02 *********************************************
     C*
     C                   eval      caprof = pr92n8
     C                   eval      catech = pr92n9
     C                   eval      caanam = pr92nk
     C*
     C                   when      xxrgn# = 4
     C*** BEG ADD *** 03/10/02 *********************************************
     C*
     C* Load the site of service amount for certain specialties
     C*
     C*** begin chg *** 11/11/04 *******************************************
     C**********         if        (eospcd = 'LAB' or eospcd = 'PSY')
     C*** Beg Chg *** 05/08/15 *******************************************
     C*****************  if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C*****************            dhpos = ' 22')
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             xxSiteofServ <> 0)
     C*** End Chg *** 05/08/15 *******************************************
     C*** end   chg *** 11/11/04 *******************************************
     C                             and pr92ni <> 0
     C                   eval      camra = pr92ni
     C                   else
     C*** END ADD *** 03/10/02 *********************************************
     C                   eval      camra = pr92nb
     C*** BEG ADD *** 03/10/02 *********************************************
     C                   endif
     C*** END ADD *** 03/10/02 *********************************************
     C*
     C                   eval      caprof = pr92nc
     C                   eval      catech = pr92nd
     C                   eval      caanam = pr92nl
     C*
     C                   endsl
     C*** BEG ADD *** 012/27/04*********************************************
     C*
     C* Per 167.2 Payment for Anesthesia services we mult the conv factor
     C* by 1.632 on or after Dec 4, 2004
     C*
     C                   if        cafdat >= 1041204
     C*** BEG ADD *** 09/12/07 ********************************************
     C                             and cafdat <= 1041231
     C*** END ADD *** 09/12/07 ********************************************
     C                             and dhprcd >= '00100'
     C                             and dhprcd <= '01999'
     C                   eval(H)   camra = camra * 1.632
     C                   endif
     C*** END ADD *** 012/27/04*********************************************
    C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      camra = pr92n2
     C*
     C                   when      xxrgn# = 2
     C                   eval      camra = pr92n6
     C*
     C                   when      xxrgn# = 3
     C                   eval      camra = pr92na
     C*
     C                   when      xxrgn# = 4
     C                   eval      camra = pr92ne
     C*
     C                   endsl
     C*
     C                   endif
     C*
     C     loadamte      endsr
    C********************************************************************
    C** LOADMOD SUBROUTINE                              L O A D M O D   *
    C********************************************************************
     C     loadmod       begsr
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
     C     loadmode      endsr
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
      *** Begin Change *** 04/27/18 **********************
     C***                eval      xxawp = rx1awp / 1000
     C                   eval      xxawp = rx1awp
      *** End  Change *** 04/27/18 **********************
    C*
    C* Use current average wholesale price(AWP)
     C                   else
      *** Begin Change *** 04/27/18 **********************
     C***                eval      xxawp = rxawp / 1000
     C                   eval      xxawp = rxawp
      *** End  Change *** 04/27/18 **********************
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
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      xxmra = pr92n2
     C*
     C                   when      xxrgn# = 2
     C                   eval      xxmra = pr92n6
     C*
     C                   when      xxrgn# = 3
     C                   eval      xxmra = pr92na
     C*
     C                   when      xxrgn# = 4
     C                   eval      xxmra = pr92ne
     C*
     C                   endsl
     C*
     C                   eval      xcmra = xxmra
     C*
     C                   endif
     C*** BEG ADD *** 05/14/09 *******************************************
     C*
     C     'AV'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      xxmra = pr92n2
     C*
     C                   when      xxrgn# = 2
     C                   eval      xxmra = pr92n6
     C*
     C                   when      xxrgn# = 3
     C                   eval      xxmra = pr92na
     C*
     C                   when      xxrgn# = 4
     C                   eval      xxmra = pr92ne
     C*
     C                   endsl
     C*
     C                   eval      xcmra = xxmra
     C                   eval      camra = xxmra
     C*
     C                   endif
     C*
     C     '9O'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      xxmra = prmra
     C*
     C                   when      xxrgn# = 2
     C                   eval      xxmra = pr92n3
     C*
     C                   when      xxrgn# = 3
     C                   eval      xxmra = pr92n7
     C*
     C                   when      xxrgn# = 4
     C                   eval      xxmra = pr92nb
     C*
     C                   endsl
     C*
     C                   eval      xcmra = xxmra
     C                   eval      camra = xxmra
     C*
     C                   endif
     C*
     C     'AW'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      xxmra = prmra
     C*
     C                   when      xxrgn# = 2
     C                   eval      xxmra = pr92n3
     C*
     C                   when      xxrgn# = 3
     C                   eval      xxmra = pr92n7
     C*
     C                   when      xxrgn# = 4
     C                   eval      xxmra = pr92nb
     C*
     C                   endsl
     C*
     C                   eval      xcmra = xxmra
     C                   eval      camra = xxmra
     C*
     C                   endif
     C*
     C     'AU'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      xxmra = prmra
     C*
     C                   when      xxrgn# = 2
     C                   eval      xxmra = pr92n3
     C*
     C                   when      xxrgn# = 3
     C                   eval      xxmra = pr92n7
     C*
     C                   when      xxrgn# = 4
     C                   eval      xxmra = pr92nb
     C*
     C                   endsl
    C*
     C                   eval      xcmra = xxmra
     C                   eval      camra = xxmra
     C*
     C                   endif
     C*
     C*** END ADD *** 05/14/09 *******************************************
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
     C                   movel     pr1a2         caanes
    C*
     C* Load amounts based on service zip of the provider and modifier
     C                   exsr      loadamt
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a3         canct
     C*** BEG CHG *** 04/03/02 *********************************************
     C****************** move      pr1ad         casepp
     C                   move      pr1a4         casepp
     C*** END CHG *** 04/03/02 *********************************************
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
    C*   MODIFIERS
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
     C*** BEGIN ADD *** 03/28/01 *******************************************
     C                   eval      kkefdt = cafdat
     C*** END   ADD *** 03/28/01 *******************************************
    C*
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     dhprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
    C*
     C*** BEGIN DEL *** 05/30/01 *******************************************
     C**********         clear                   xxfrzp
     C**********         clear                   xxzipa
    C*
     C*****p1key         chain     pp101                              60
     C**********         if        *in60 = *off
    C*
    C*
    C* Now that the Provider is established, get its Geographic Zip Area
    C* 1st - establish the first 3-bytes of the zip code
     C**********         movel     p1zip         xxfrzp
     C**********         endif
    C*
    C*
    C* 2nd - Get the corresponding Geographic Zip Area record:
     C*****gzky*         setgt     pgz01
     C**********         readp     pgz01                                  60
     C**********         if        *in60 = *off
     C**********                   and xxfrzp <= gztozp
     C**********         move      gzzipa        xxzipa
    C*
     C**********         else
    C*
    C* USE GEO-ZIP 430 IF ZIP CODE IS OUT OF STATE
     C**********         movel     '430'         xxzipa
    C*
     C**********         endif
     C*** END   DEL *** 05/30/01 *******************************************
    C*
     C     rcky          setgt     prc01
     C     nxrc1         tag
     C                   readp     prc01                                  60
    C*
     C                   if        *in60 = *on
     C                             or rcco# <> qqco#
     C*** BEGIN CHG *** 05/30/01 *******************************************
     C**********                   or rczipa <> xxzipa
     C                             or rczipa <> cazipa
     C*** END   CHG *** 05/30/01 *******************************************
     C                             or rccpt4 <> xxcpt4
    C*
     C                   movel     'Y'           xxinvl
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
     C                   else
     C*** BEG ADD *** 08/20/02 *********************************************
     C                   move      rcfsr         cafsr
     C*** END ADD *** 08/20/02 *********************************************
     C                   eval      cafolu = rcfolu
     C*** BEG ADD *** 04/28/04 *********************************************
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
     C*** END ADD *** 04/28/04 *********************************************
    C*
     C*** Beg Chg *** 01/26/16 *********************************************
     C****************   eval      xcmra = rcmra
     C                   eval      xcmra = 0
     C                   eval      rcmra = 0
     C*** End Chg *** 01/26/16 *********************************************
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C*** Beg Chg *** 01/26/16 *********************************************
     C*****************  eval      caprof = rcprof
     C*****************  eval      catech = rc92n2
     C                   eval      caprof = 0
     C                   eval      catech = 0
     C*** End Chg *** 01/26/16 *********************************************
     C*** BEG ADD *** 11/16/10 *********************************************
     C                   if        ##ucpf = 'Y'
     C     r1ky1         chain     pr101                              60
     C                   if        *in60 = *off
     C*** Beg Chg *** 01/26/16 *********************************************
     C***************    z-add     r1mr85        rcmra
     C                   z-add     0             rcmra
     C*** End Chg *** 01/26/16 *********************************************
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   clear                   rcprof
     C                   clear                   rc92n2
     C                   endif                                                  *in60
     C                   endif                                                  ##ucpf
     C*** END ADD *** 11/16/10 *********************************************
     C*** BEGIN ADD *** 03/28/01 **************************************
     C*
     C*  Load the professional and technical component amount based on
     C*  percentages in FTUMP.
     C*
     C     umky          setgt     pum01
     C                   readp     pum01                                  60
     C*
     C                   if        *in60 = *off and cacpt4 <= umtocp
     C*
     C*** BEG CHG *** 03/28/02 *********************************************
     C****************** eval      xxmd26 = umod26
     C****************** eval      xxmd27 = umod27
     C                   eval      xxmd26 = umod26/100
     C                   eval      xxmd27 = umod27/100
     C*** END CHG *** 03/28/02 *********************************************
     C*
     C*** Beg Del *** 01/26/16 *********************************************
     q****************** if        caprof = 0
     C****************** eval      caprof = rcmra * xxmd26
     C****************** endif                                                  caprof = 0
     C******************
     C****************** if        catech = 0
     C****************** eval      catech = rcmra * xxmd27
     C****************** endif                                                  catech = 0
     C*** End Del *** 01/26/16 *********************************************
     C*
     C                   endif                                                  end *in60
     C*** END   ADD *** 03/28/01 **************************************
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
    C* REASONABLE AND CUSTOMARY DO NOT CONTAIN A VALID EFFECTIVE
    C* DATE FOR THE PROCEDURE CODE
    C*
     C*** BEGIN DEL *** 03/28/01 *******************************************
     C*****ruky          setgt     pru01
     C*****ruky1         readpe    pru01                                  56
     C**********         if        *in56 = *off
     C**********         eval      kkefdt = ruefdt
     C**********         else
     C**********         move      *loval        kkefdt
     C**********         endif
     C*** END   DEL *** 03/28/01 *******************************************
    C*
     C                   endif
     C     rcstat        cabeq     'I'           nxrc1
     C*** BEG ADD *** 11/16/10 *********************************************
     C                   if        cafdat >= 1101101
    C                   move      'Y'           carcfl
    C                   endif
     C*** END ADD *** 11/16/10 *********************************************
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
     C*** BEG ADD *** 07/13/01 *********************************************
     C                   movel     eumcar        kkmed#
     C                   eval      k1med# = %subst(kkmed#:1:6)
     C                   movel     dupscd        kksvcd
     C                   move      durate        kkhcps
     C                   move      dupos         caplac
     C*** BEG ADD *** 02/27/08 *********************************************
     C                   testn                   durate               21
     C   21xxdigits      check     durate:5      xxpos
     C                   if        xxpos = 0  and *in21 = *on
     C*** END ADD *** 02/27/08 *********************************************
     C                   move      durate        kkdrg#
     C*** BEG ADD *** 02/27/08 *********************************************
     C                   EndIf
     C*** END ADD *** 02/27/08 *********************************************
     C                   move      durvcd        kkrvcd
     C                   eval      casnfo = 0
     C                   eval      casnfi = 0
     C                   eval      cadrg = 0
     C                   eval      cafacf = 0
     C                   eval      cascam = 0
     C                   eval      cacorf = 0
     C                   eval      caoccr = 0
     C                   eval      caiprc = 0
     C                   eval      caoprc = 0
     C                   eval      cafema = *blank
     C                   eval      capoma = *blank
      *** Beg Add ***  *** 04/05/18 ***************************************
      /Free
                         If DUrvCd = 100;
                            CAdrg = CApaid;
                         Endif;
      /End-Free
      *** End Add ***  *** 04/05/18 ***************************************
      /Free
      /End-Free
     C                   eval      camema = *blank
     C                   eval      cascma = *blank
     C                   eval      camrma = *blank
     C                   eval      cadrma = *blank
     C                   eval      casval = *blank
     C                   move      'Y'           cafsr
     C*** END ADD *** 07/13/01 *********************************************
     C*** BEG ADD *** 04/07/03 *********************************************
     C                   eval      captpv = 0
     C*** END ADD *** 04/07/03 *********************************************
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
     C     svky          setgt     psv01
     C     nxtsv1        tag
     C                   readp     psv01                                  60
    C*
     C*** BEG DEL *** 07/13/01 *********************************************
     C****************** if        *in60 = *on
     C****************** move      'Y'           cainvl
    C******************
     C****************** else
     C****************** eval      camra = svalow
    C******************
     C****************** endif
     C*** END DEL *** 07/13/01 *********************************************
     C*** BEG ADD *** 07/13/01 *********************************************
     C                   if        *in60 = *off
    C*
     C                   eval      camra = svalow
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = camra
     C*** END ADD *** 05/28/10 *********************************************
    C*
     C                   endif
     C*** BEG ADD *** 07/13/01 *********************************************
    C*
     C     prstat        cabeq     'I'           nxtsv1
     C*** BEG ADD *** 07/13/01 *********************************************
     C                   select
     C*
     C*** BEG CHG *** 11/18/02 *********************************************
     C****************** when      eublty>='111' and eublty<='129'
     C                   when      eublty>='110' and eublty<='129'
     C*** END CHG *** 11/18/02 *********************************************
     C                             or eublty>='150' and eublty<='209'
     C*** BEG CHG *** 07/12/02 *********************************************
     C**************************** or eublty>='300' and eublty<='329'
     C                             or eublty>='300' and eublty<='309'
     C*** END CHG *** 07/12/02 *********************************************
     C                             or eublty>='340' and eublty<='429'
     C                             or eublty>='440' and eublty<='529'
     C                             or eublty>='540' and eublty<='629'
     C                             or eublty>='760' and eublty<='829'
     C                             or eublty>='850' and eublty<='929'
     C                             or eublty>='940' and eublty<='999'
     C*
     C*** BEG ADD *** 05/21/02 *********************************************
     C                   if        eospcd = 'REH'
     C*** BEG ADD *** 02/12/03 *********************************************
     C                             or eospcd = 'CAH'
     C*** END ADD *** 02/12/03 *********************************************
     C*
    C**********************************************************************
    C* Inpat Rehab and Critical Acess Hospitals use Charge Master not DRG
    C**********************************************************************
     C*
     C* Set DRG fields for rules so this line wont hit DRG rules
     C*
     C                   eval      cafema = 'N'
     C                   eval      capoma = 'N'
      /Free
      /End-Free
     C*
     C                   select
     C*
     C* See if service code HCPCS and Rev code match
     C*
     C                   when      durate <> *blanks
     C*
     C     cgky          setgt     pcg02
     C     cgky1         readpe    pcg02                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     cgkya         setgt     pcg02
     C     cgky1a        readpe    pcg02                                  99
     C*
     C                   endif
     C*
     C* See if service code and rev code match
     C*
     C                   when      durate = *blanks
     C*
     C     cgky3         setgt     pcg01
     C     cgky4         readpe    pcg01                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     cgky3a        setgt     pcg01
     C     cgky4a        readpe    pcg01                                  99
     C*
     C* See if only service code matches
     C*
     C                   if        *in99 = *on
     C*
     C     cgky5         setgt     pcg03
     C     cgky6         readpe    pcg03                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     cgky5a        setgt     pcg03
     C     cgky6a        readpe    pcg03                                  99
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endsl
     C*
     C                   if        *in99 = *off
     C*
     C* Perform calculations
     C*
     C                   eval      cascam = cginpt
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cascam
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C* Verify value for service code
     C*
     C                   if        cascam = 0
     C                   eval      casval = 'N'
     C                   endif
     C*
     C* No match on service code
     C*
     C                   else
     C                   eval      cascma = 'N'
     C*
     C                   endif
     C*
     C* No match on service code file
     C*
     C*
     C* No match on medicare number in the charge master
     C*
     C     cgky2         chain     pcg01                              99
     C*
     C                   if        *in99 = *on
     C*
     C     cgky2a        chain     pcg01                              99
     C*
     C                   endif
     C*
     C*
     C                   if        *in99 = *on
     C                   eval      camema = 'N'
     C                   endif
     C*
     C* Load RCC amount
     C*
     C     rrky          chain     prr01                              99
     C*
     C                   if        *in99 = *on
     C*** BEG ADD *** 04/08/05 *********************************************
     C                   move      kkrvcd        xxrvcd            3
     C                   eval      xxrvcd = %subst(xxrvcd:1:2) + '0'
     C                   move      xxrvcd        kkrvcd
     C     rrky          chain     prr01                              99
     C                   if        *in99 = *on
     C*** END ADD *** 04/08/05 *********************************************
     C*
     C     rrkya         chain     prr01                              99
     C*
     C                   endif
     C*** BEG ADD *** 04/08/05 *********************************************
     C                   endif
     C*** END ADD *** 04/08/05 *********************************************
     C*
     C                   if        *in99 = *off
     C*** BEG ADD *** 04/08/05 *********************************************
     C                   eval      caoccr = rrrcc * cachrg * 1.13
     C*** END ADD *** 04/08/05 *********************************************
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   if        xxmra = 0
     C                   eval      xxmra = caoccr
     C                   endif                                                  in99 = *on
     C*** END ADD *** 05/28/10 *********************************************
     C*** BEG DEL *** 04/08/05 *********************************************
     C***************
     C***************    eval      sum1  = rrrcc * cachrg * 1.13
     C***************    eval      sum2 = sum1  + (sum1 * .032)
     C***************    eval      sum3 = sum2 + (sum2 * .035)
     C***************    eval      sum4 = sum3 + (sum3 * .028)
     C***************    eval      sum5 = sum4 + (sum4 * .035)
     C***************    eval      sum6 = sum5 + (sum5 * .048)
     C***************    eval      sum7 = sum6 + (sum6 * .040)
     C***************    eval      sum8 = sum7 + (sum7 * .053)
     C***************    eval      sum9 = sum8 + (sum8 * .028)
     C****BEG*ADD**** 02/12/03 *********************************************
     C***************    eval      sum10= sum9 + (sum9 * .020)
     C****END*ADD**** 02/12/03 *********************************************
     C****BEG*ADD**** 03/05/04 *********************************************
     C***************    eval      sum11= sum10 + (sum10 * .022)
     C****END*ADD**** 03/05/04 *********************************************
     C***************    select
     C***************
     C***************    when      cafdat >= 0950101 and
     C***************              cafdat <= 0951231
     C***************    eval      caoccr = sum2
     C***************
     C***************    when      cafdat >= 0960101 and
     C***************              cafdat <= 0961231
     C***************    eval      caoccr = sum3
     C***************
     C***************    when      cafdat >= 0970101 and
     C***************              cafdat <= 0971231
     C***************    eval      caoccr = sum4
     C***************
     C***************    when      cafdat >= 0980101 and
     C***************              cafdat <= 0981231
     C***************    eval      caoccr = sum5
     C***************
     C***************    when      cafdat >= 0990101 and
     C***************              cafdat <= 0991231
     C***************    eval      caoccr = sum6
     C***************
     C***************    when      cafdat >= 1000101 and
     C***************              cafdat <= 1001231
     C***************    eval      caoccr = sum7
     C***************
     C***************    when      cafdat >= 1010101 and
     C***************              cafdat <= 1011231
     C***************    eval      caoccr = sum8
     C***************
     C***************    when      cafdat >= 1020101
     C****BEG*ADD**** 02/12/03 *********************************************
     C***************              and cafdat <= 1021231
     C****END*ADD**** 02/12/03 *********************************************
     C***************    eval      caoccr = sum9
     C****BEG*ADD**** 02/12/03 *********************************************
     C***************
     C***************    when      cafdat >= 1030101
     C***************    eval      caoccr = sum10
     C****END*ADD**** 02/12/03 *********************************************
     C****BEG*ADD**** 03/05/04 *********************************************
     C***************
     C***************    when      cafdat >= 1040101
     C***************    eval      caoccr = sum11
     C****END*ADD**** 03/05/04 *********************************************
     C***************
     C***************    other
     C***************
     C***************    eval      caoccr = 0
     C***************
     C***************    endsl
     C*** END DEL *** 04/08/05 *********************************************
     C*
     C                   endif
     C*
     C                   else
     C*** END ADD *** 05/21/02 *********************************************
     C*
    C**********************************************************************
    C* DRG Payments
    C**********************************************************************
    C*
     C* Get Federal register amount
     C*
     C     feky          CHAIN     pfe01                                       99
     C                   if        *in99 = *on
     C                   eval      cafema = 'N'
     C                   endif
    C*
     C* Get Prospective Payment amount
     C*
     C     poky          setgt     ppo01
     C     poky1         readpe    ppo01                                  99
     C*
     C                   if        *in99 = *on
     C                   eval      capoma = 'N'
     C                   endif
      *** Begin Delete *** 04/05/18****************************************
     C*
     C*** 04/05/18                if        *in99 = *off and pomed# = kkmed#
     C*
     C* Perform calculations
     C*
     C*** 04/05/18                eval      sum1 = ferlwt * podrrt
     C*** 04/05/18                eval      sum2 = ferlwt * pocprt
     C*** 04/05/18                eval      sum3 = pohhrt
     C*** 04/05/18                eval      sum3a= (podrrt+pocprt)*ferlwt
     C*** 04/05/18                eval      sum4 = sum3a * poinad
     C*** 04/05/18                eval      sum5 = sum3a * podiad
     C*** 04/05/18                eval      sum6a = sum1 * 1.13
     C*** 04/05/18                eval      sum6 = sum6a * podsad
     C*** 04/05/18                eval      sum7 = sum3a * popmad
     C*** 04/05/18                eval      sum8 = pocprt
     C*** 04/05/18                eval      sum9 = 0
     C*** 04/05/18                eval      sum10 = sum3+sum4+sum5+sum6+sum6a
     C*** 04/05/18                        +sum7+sum2
     C*
     C* Calculate outlier payment
     C*
     C* Cost outlier
     C*
     C*** BEG ADD *** 12/21/04 *********************************************
     C*** BEG DEL *** 05/15/09 *********************************************
     C***                if        poarcc > 0.86
     C***                eval      poarcc = 0.86
     C***                endif
     C*** BEG DEL *** 05/15/09 *********************************************
     C*** END ADD *** 12/21/04 *********************************************
     C*** 04/05/18                   eval      sum11 = cachrg * poarcc
     C*
     C* Threshold 1 (T1)
     C*
     C*** 04/05/18                   eval      sum12a = ferlwt * podrrt
     C*** 04/05/18                   eval      sum12b = ferlwt * pocprt
     C*** 04/05/18                   eval      sum12 = (sum12a + sum12b) * 2
     C*
     C* Threshold 2 (T2) is 36,000 set by Medicare. Select Greater of T1,T2
     C*
     C*** 04/05/18                   if        sum12 > 36000
     C*** 04/05/18                   eval      sum13 = sum12
     C*** 04/05/18                   else
     C*** 04/05/18                   eval      sum13 = 36000
     C*** 04/05/18                   endif
     C*
     C*** 04/05/18                   eval      sum14 = sum11 - sum13
     C*** 04/05/18                   eval      sum15 = sum14 * .80
     C*
     C* Sum15 is total Cost outlier
     C*
     C*** 04/05/18                   if        sum15 <= 0
     C*** 04/05/18                   eval      sum15 = 0
     C*** 04/05/18                   endif
     C*
     C* Day outlier
     C*
     C*** 04/05/18                   if        caunts > feoutl
     C*
     C*** 04/05/18                   eval      sum16 = sum12a + sum12b
     C*
     C*** 04/05/18                   if        fealos > 0
     C*** 04/05/18                   eval      sum17 = sum16 / fealos
     C*** 04/05/18                   endif
     C*
     C*** 04/05/18                   eval      sum18 = (caunts-feoutl) * sum17
     C*
     C*** 04/05/18                   endif
     C*
     C* Sum18 is day outlier amount
     C*
     C*** 04/05/18                   if        caunts <= feoutl
     C*** 04/05/18                   eval      sum18 = 0
     C*** 04/05/18                   endif
     C*
     C* Calculate the final payment
     C*
     C*** 04/05/18                   if        sum15 = 0 or sum18 = 0
     C*** 04/05/18                   eval      cadrg = sum10 + sum15 + sum18
     C*** BEG ADD *** 05/28/10 *********************************************
     C*** 04/05/18                   eval      xxmra = cadrg
     C*** END ADD *** 05/28/10 *********************************************
     C*** 04/05/18                   endif
     C*
     C*** 04/05/18                   if        sum15 > 0 and sum18 > 0
     C*
     C*** 04/05/18                   if        sum15 <= sum18
     C*** 04/05/18                   eval      cadrg = sum10 + sum15
     C*** BEG ADD *** 05/28/10 *********************************************
     C*** 04/05/18                   eval      xxmra = cadrg
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C*** 04/05/18                   else
     C*** 04/05/18                   eval      cadrg = sum10 + sum18
     C*** BEG ADD *** 05/28/10 *********************************************
     C*** 04/05/18                   eval      xxmra = cadrg
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C*** 04/05/18                   endif
     C*
     C*** 04/05/18                   endif
     C*
     C                   Endif
     C*
     C*** BEG ADD *** 05/21/02 *********************************************
     C*** 04/05/18                   endif                                                  if eospc
     C*** END ADD *** 05/21/02 *********************************************
     C*
     C**********************************************************************
     C* Get ASC region/group amount
     C**********************************************************************
     C*
     C                   when      eublty>='830' and eublty<='839'
     C                             and eospcd <> 'HOS'
     C*
     C* Perform calculations
     C*
     C* Find group of CPT code
     C*
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  99
     C*
     C                   move      pr1a5         kkgrp
     C*
     C* Find allowance for the med#/group
     C*
     C     apky          setgt     pap01                              99
     C     apky1         readpe    pap01                                  99
     C*
     C*** BEG ADD *** 03/05/04 *********************************************
     C                   if        *in99 = *off
     C*** END ADD *** 03/05/04 *********************************************
     C                   eval      cafacf = apalow
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cafacf
     C*** END ADD *** 05/28/10 *********************************************
     C*** BEG ADD *** 03/05/04 *********************************************
     C                   endif
     C*** END ADD *** 03/05/04 *********************************************
    C*
    C**********************************************************************
     C* Skilled Nursing - Look for a room and board revenue code
     C* Inpatient
     C*
    C**********************************************************************
    C*
     C                   when      eublty>='210' and eublty<='229'
     C                             or eublty>='240' and eublty<='299'
    C*
    C* Check to see if Medicare# is in the table file
    C*
     C     snky          setgt     psn01
     C     snky1         readpe    psn01                                  99
     C*
     C                   if        *in99 = *on
     C     snkya         setgt     psn01
     C     snky1a        readpe    psn01                                  99
    C*
     C                   if        *in99 = *on
     C                   eval      camema = 'N'
     C                   endif
    C*
     C                   endif
    C*
     C*** BEG ADD *** 06/22/05 *********************************************
     C                   if        xxinp = 'N'
     C*
     C* Stop if we find room and board code
     C*
     C                   if        casvcd >= 100 and casvcd <= 169
     C*
     C                   eval      xxdseq = cadseq
     C                   eval      xxinp = 'Y'
     C*
     C                   endif
     C*
     C                   endif
     C*** END ADD *** 06/22/05 *********************************************
     C*** BEG DEL *** 06/22/05 *********************************************
     C****************
     C****************   if        xx551 = 'N'
     C****************
     C**Stop*if*we*find 551
     C****************
     C****************   if        casvcd = 551
     C****************
     C****************   eval      xxdseq = cadseq
     C****************   eval      xx551 = 'Y'
     C****************
     C****************   else
     C****************
     C**Else*check*to*see if it is between 550 and 559 then check it against
     C**the*previous*one.
     C****************
     C****************   if        casvcd >= 550 and casvcd <=559
     C****************
     C****************   if        xxprev <> 0 and casvcd < xxprev
     C****************   eval      xxdseq = cadseq
     C****************   eval      xxprev = casvcd
     C****************   endif                                                  xxprev <> 0
     C****************
     C****************   if        xxprev = 0
     C****************   eval      xxdseq = cadseq
     C****************   eval      xxprev = casvcd
     C****************   endif                                                  xxprev <> 0
     C****************
     C****************
     C****************   endif                                                  casvcd >= 550
     C****************
     C****************   endif                                                  casvcd = 551
     C****************
     C****************   endif                                                  xx551 = N
     C*** END DEL *** 06/22/05 *********************************************
     C*
     C* Skilled Nursing outpatient
     C*
    C*
     C                   when      eublty>='230' and eublty<='239'
    C*
     C     snky          setgt     psn01
     C     snky1         readpe    psn01                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     snkya         setgt     psn01
     C     snky1a        readpe    psn01                                  99
     C*
     C                   endif
     C*
     C                   if        *in99 = *off
     C                   eval      casnfo = snoupt
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = casnfo * cachrg
     C*** END ADD *** 05/28/10 *********************************************
    C*
     C                   else
     C                   eval      camema = 'N'
    C*
     C                   endif
     C
    C*
    C**********************************************************************
     C* Home Health Amount
    C**********************************************************************
     C*
     C                   when      eublty>='310' and eublty<='399'
    C*
     C     hhky          setgt     phh01
     C     hhky1         readpe    phh01                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     hhkya         setgt     phh01
     C     hhky1a        readpe    phh01                                  99
     C*
     C                   endif
     C*
     C                   if        *in99 = *off
     C*
     C* Load amount based on revenue code range
     C*
     C                   select
     C*
     C                   when      durvcd >= 420 and durvcd <= 429
     C*
     C                   eval      cahhli = hhptr
     C                   eval      cahhlo = hhptr
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cahhli
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C                   when      durvcd >= 430 and durvcd <= 439
     C*
     C                   eval      cahhli = hhoccr
     C                   eval      cahhlo = hhoccr
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cahhli
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C                   when      durvcd >= 440 and durvcd <= 449
     C*
     C                   eval      cahhli = hhsptr
     C                   eval      cahhlo = hhsptr
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cahhli
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C                   when      durvcd >= 550 and durvcd <= 559
     C*
     C                   eval      cahhli = hhsknr
     C                   eval      cahhlo = hhsknr
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cahhli
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C                   when      durvcd >= 560 and durvcd <= 569
     C*
     C                   eval      cahhli = hhssr
     C                   eval      cahhlo = hhssr
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cahhli
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C                   when      durvcd >= 570 and durvcd <= 579
     C*
     C                   eval      cahhli = hhhaa
     C                   eval      cahhlo = hhhaa
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cahhli
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C                   other
     C*
     C* If rev code not found pay 80% charged. Pass N to CAMEMA
     C*
     C                   eval      camema = 'N'
     C*
     C                   endsl
     C*
     C* No match on medicare number pay 80% of charged
     C*
     C                   else
     C                   eval      camema = 'N'
     C*
     C                   endif
    C*
    C**********************************************************************
     C* Charge Master Amount for outpatient including CORF's
    C**********************************************************************
     C*
     C                   when      eublty>='130' and eublty<='149'
     C                             or eublty>='740' and eublty<='759'
     C                             or eublty>='840' and eublty<='849'
     C                             or eublty>='830' and eublty<='839'
     C                             and eospcd = 'HOS'
     C*
     C                   select
     C*
     C* See if service code HCPCS and Rev code match
     C*
     C                   when      durate <> *blanks
     C*
     C     cgky          setgt     pcg02
     C     cgky1         readpe    pcg02                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     cgkya         setgt     pcg02
     C     cgky1a        readpe    pcg02                                  99
     C*
     C                   endif
     C*
     C* See if service code and rev code match
     C*
     C                   when      durate = *blanks
     C*
     C     cgky3         setgt     pcg01
     C     cgky4         readpe    pcg01                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     cgky3a        setgt     pcg01
     C     cgky4a        readpe    pcg01                                  99
     C*
     C* See if only service code matches
     C*
     C                   if        *in99 = *on
     C*
     C     cgky5         setgt     pcg03
     C     cgky6         readpe    pcg03                                  99
     C*
     C                   if        *in99 = *on
     C*
     C     cgky5a        setgt     pcg03
     C     cgky6a        readpe    pcg03                                  99
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endsl
     C*
     C                   if        *in99 = *off
     C*
     C* Perform calculations
     C*
     C                   eval      cascam = cgoutp
     C                   eval      cacorf = cgoutp
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = cacorf
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C* Verify value for service code
     C*
     C                   if        cascam = 0
     C                   eval      casval = 'N'
     C                   endif
     C*
     C* No match on service code
     C*
     C                   else
     C                   eval      cascma = 'N'
     C*
     C                   endif
     C*
     C*** BEG ADD *** 04/07/03 *********************************************
     C*
     C* See if this provider is in the PT per visit table
     C*
     C                   if        eublty >= '740' and eublty <= '759'
     C     ptky1         chain     ppt01                              99
     C                   if        *in99 = *off
     C                   eval      camema = 'Y'
     C                   endif
     C                   endif
     C*
     C*** END ADD *** 04/07/03 *********************************************
     C*
     C* No match on medicare number in the charge master
     C*
     C     cgky2         chain     pcg01                              99
     C*
     C                   if        *in99 = *on
     C*
     C     cgky2a        chain     pcg01                              99
     C*
     C                   endif
     C*
     C*
     C                   if        *in99 = *on
     C                   eval      camema = 'N'
     C                   endif
     C*
     C* Load RCC amount
     C*
     C     rrky          chain     prr01                              99
     C*
     C                   if        *in99 = *on
     C*** BEG ADD *** 04/08/05 *********************************************
     C                   move      kkrvcd        xxrvcd            3
     C                   eval      xxrvcd = %subst(xxrvcd:1:2) + '0'
     C                   move      xxrvcd        kkrvcd
     C     rrky          chain     prr01                              99
     C                   if        *in99 = *on
     C*** END ADD *** 04/08/05 *********************************************
     C*
     C     rrkya         chain     prr01                              99
     C*
     C                   endif
     C*** BEG ADD *** 04/08/05 *********************************************
     C                   endif
     C*** END ADD *** 04/08/05 *********************************************
     C*
     C                   if        *in99 = *off
     C*** BEG ADD *** 04/08/05 *********************************************
     C                   eval      caoccr = rrrcc * cachrg * 1.13
     C*** END ADD *** 04/08/05 *********************************************
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = caoccr
     C*** END ADD *** 05/28/10 *********************************************
     C*** BEG DEL *** 04/08/05 *********************************************
     C***************    eval      sum1  = rrrcc * cachrg * 1.13
     C***************    eval      sum2 = sum1  + (sum1 * .032)
     C***************    eval      sum3 = sum2 + (sum2 * .035)
     C***************    eval      sum4 = sum3 + (sum3 * .028)
     C***************    eval      sum5 = sum4 + (sum4 * .035)
     C***************    eval      sum6 = sum5 + (sum5 * .048)
     C***************    eval      sum7 = sum6 + (sum6 * .040)
     C***************    eval      sum8 = sum7 + (sum7 * .053)
     C***************    eval      sum9 = sum8 + (sum8 * .028)
     C****BEG*ADD**** 02/12/03 *********************************************
     C***************    eval      sum10= sum9 + (sum9 * .020)
     C****END*ADD**** 02/12/03 *********************************************
     C****BEG*ADD**** 03/05/04 *********************************************
     C***************    eval      sum11= sum10 + (sum10 * .022)
     C****END*ADD**** 03/05/04 *********************************************
     C***************    select
     C***************
     C***************    when      cafdat >= 0950101 and
     C***************              cafdat <= 0951231
     C***************    eval      caoccr = sum2
     C***************
     C***************    when      cafdat >= 0960101 and
     C***************              cafdat <= 0961231
     C***************    eval      caoccr = sum3
     C***************
     C***************    when      cafdat >= 0970101 and
     C***************              cafdat <= 0971231
     C***************    eval      caoccr = sum4
     C***************
     C***************    when      cafdat >= 0980101 and
     C***************              cafdat <= 0981231
     C***************    eval      caoccr = sum5
     C***************
     C***************    when      cafdat >= 0990101 and
     C***************              cafdat <= 0991231
     C***************    eval      caoccr = sum6
     C***************
     C***************    when      cafdat >= 1000101 and
     C***************              cafdat <= 1001231
     C***************    eval      caoccr = sum7
     C***************
     C***************    when      cafdat >= 1010101 and
     C***************              cafdat <= 1011231
     C***************    eval      caoccr = sum8
     C***************
     C***************    when      cafdat >= 1020101
     C****BEG*ADD**** 02/12/03 *********************************************
     C***************              and cafdat < 1030101
     C****END*ADD**** 02/12/03 *********************************************
     C***************    eval      caoccr = sum9
     C****BEG*ADD**** 02/12/03 *********************************************
     C***************
     C***************    when      cafdat >= 1030101
     C***************    eval      caoccr = sum10
     C****END*ADD**** 02/12/03 *********************************************
     C****BEG*ADD**** 03/05/04 *********************************************
     C***************
     C***************    when      cafdat >= 1040101
     C***************    eval      caoccr = sum11
     C****END*ADD**** 03/05/04 *********************************************
     C***************
     C***************    other
     C***************
     C***************    eval      caoccr = 0
     C***************
     C***************    endsl
     C***************
     C*** END DEL *** 04/08/05 *********************************************
     C*
     C                   endif
     C*
    C**********************************************************************
     C* Pharmacy RCC Outpatient and Inpatient Rates
    C**********************************************************************
     C*
     C     phky          chain     pph01                              99
     C*
     C                   if        *in99 = *on
     C*
     C     phkya         setgt     pph01                              99
     C*
     C                   endif
     C*
     C*
     C                   if        *in99 = *off
     C                   eval      caoprc = phoupt
     C                   eval      caiprc = phinpt
     C                   endif
     C*
    C**********************************************************************
     C* DME, Clinical Lab and Professional Services use Physician FS
    C**********************************************************************
     C*
     C                   if        durate <> *blanks
     C*
     C                   movel     durate        xxcpt4
    C*
     C     prky          setgt     ppr01
     C     nxtpr5        tag
     C     prky1         readpe    ppr01                                  99
     C*
     C                   if        *in99 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   if        prstat = 'I'
     C                   goto      nxtpr5
     C                   endif
    C*
     C*** begin chg *** 11/11/04 *******************************************
     C                   clear                   dhpos
     C*** end   chg *** 11/11/04 *******************************************
     C                   exsr      loadamt
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   if        xxmra = 0
     C                   eval      xxmra = camra
     C                   endif                                                  in99 = *on
     C*** END ADD *** 05/28/10 *********************************************
    C*
     C                   endif                                                  in99 = *on
    C*
     C                   endif                                                  durate <> blanks
     C*
     C*** BEG ADD *** 04/07/03 *********************************************
     C*
     C* Some CORFs are exempt from the Chargemaster
     C*
     C                   if        eublty >= '740' and eublty <= '759'
     C*
     C     ptky          setgt     ppt01
     C     ptky1         readpe    ppt01                                  99
     C                   if        kkefdt >= ptefdt and *in99 = *off
     C*
     C                   select
     C*
     C                   when      durvcd >= 420 and durvcd <= 429
     C                   eval      captpv = ptrvc1
     C*
     C                   when      durvcd >= 430 and durvcd <= 439
     C                   eval      captpv = ptrvc2
     C*
     C                   when      durvcd >= 440 and durvcd <= 449
     C                   eval      captpv = ptrvc3
     C*
     C                   when      durvcd >= 550 and durvcd <= 559
     C                   eval      captpv = ptrvc4
     C*
     C                   when      durvcd >= 560 and durvcd <= 569
     C                   eval      captpv = ptrvc5
     C*
     C                   when      durvcd >= 910 and durvcd <= 919
     C                   eval      captpv = ptrvc6
     C*
     C*** Beg Add *** 10/29/13 *********************************************
     C                   when      durvcd = 651
     C                   eval      captpv = ptrvc7
      *
     C                   when      durvcd = 652
     C                   eval      captpv = ptrvc8
      *
     C                   when      durvcd = 655
     C                   eval      captpv = ptrvc9
      *
     C                   when      durvcd = 656
     C                   eval      captpv = ptrvc0
     C*** End Add *** 10/29/13 *********************************************
      *
     C                   endsl
     C*** BEG ADD *** 05/28/10 *********************************************
     C                   eval      xxmra = captpv
     C*** END ADD *** 05/28/10 *********************************************
     C*
     C                   endif
     C*
     C                   endif
     C*** END ADD *** 04/07/03 *********************************************
    C*
     C                   endsl                                                  durate <> blanks
     C
     C*** END ADD *** 07/13/01 *********************************************
    C*
     C     svcde         endsr
      /Free
       //**** Begin Add *** 04/05/18 **************************************
       //*-------------------------------------------------------------------
       //*  Medicare Inpatient Subroutine
       //*-------------------------------------------------------------------
       Begsr medicare_inpt;
          inrrn = dbrrn;
          outrtcd = *blanks;
          outAllow1 = 0;
          outAllow2 = 0;
          Rtv_inpat_Alw(inrrn:outrtcd:outAllow1:outAllow2);
       EndSr;
       //**** End   Add *** 04/05/18 **************************************
      /End-Free
    C* ==================================================================
     Opca01     e            rca01
