    H**********************************************************************
     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F4000NFL
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR FL
    H**********************************************************************
    H*  CREATED: 10/21/97  C:59   PROJECT: FL011
    H**********************************************************************
    H*  REVISIONS:                                                        *
     H*
     H*  04/30/19 C:062 Project: 152WAB
     H*                 Apply Optacomp Mods.
    H*                                                                    *
     H*  04/27/18 C:180 Project: C114MJS                                   *
    H*                 Increase xxWAP size.                               *
    H*                                                                    *
     H*  08/18/17 C:079 Support: 114/48804                                 *
    H*                 Remove change for 6/13/17 that is causing zero pay *
    H*                                                                    *
     H*  06/13/17 C:180 Project: FL093                                     *
    H*                 Fix PARTB MRA amount for codes with Tech components*
     H*
     H*  06/02/17 C:180 Project: FL093                                     *
     H*                 Add processing for Paying MRA.                     *
     H*                                                                    *
     H*  07/13/16 C:079 Support: 42188                                     *
    H*                 Fix OPPS logic                                     *
    H*                                                                    *
     H*  06/16/16 C:307 Project: FL085                                     *
    H*                 Add logic in Medicare subroutine for DHPOS = 12 or *
    H*                 32                                                 *
     H*                                                                    *
     H*  06/10/16 C:079 Support: 114/41406                                 *
    H*                 Pay Rad/Path/Lab at phy FS                         *
    H*                                                                    *
     H*  05/19/16 C:079 Support: 114/41406                                 *
    H*                 Lookup non surgical codes in appendix B            *
    H*                                                                    *
     H*  03/08/16 C:079 Support: 114/39129                                 *
    H*                 Determine surgical stay based on type of service   *
    H*                                                                    *
     H*  02/22/16 C:079 Support: 114/39987                                 *
    H*                 OPPS if code is unlisted dont load MRA             *
    H*                                                                    *
     H*  02/10/16 C:079 Project: FL093                                     *
    H*                 Load allowance for 70000 series and Temp Codes     *
    H*                                                                    *
     H*  06/11/15 C:079 Support: 114WOT                                    *
    H*                 Check to see if code is in FL FS                   *
    H*                                                                    *
     H*  05/05/15 C:079 Project: Phaseware: 48/34405                       *
    H*                 Pay PT,OT,Rad,Path,Lab according to Physician MRA  *
    H*                                                                    *
     H*  01/15/15 C:296 Project: FL079                                     *
    H*                 Add subroutine "SROutpatient" for calculating      *
    H*                 scheduled and non-scheduled outpatient bills       *
    H*                                                                    *
     H*  08/27/13 C:079 Support: MC114 83289                               *
    H*                 1. Pay Non-Facility rate for POS 20                *
    H*                 2. Dont call Medicare for U&C codes                *
    H*                                                                    *
     H*  04/08/13 C:079 Project: 147MAT                                    *
    H*                 Look at 85th percentile UCR.                       *
    H*                                                                    *
     H*  04/08/13 C:284 Project: 147MAT                                    *
    H*                 If Fl allowance is 0 look at UCR.                  *
     H*                                                                    *
     H*  01/04/12 C:079 Project: FL082                                     *
    H*                 Program changes for bilateral procedures ASC       *
    H*                                                                    *
     H*  06/29/10 C:227 Project: FL083                                     *
    H*                 Program changes for Priss#35                       *
    H*                                                                    *
     H*  04/26/10 C:079 Project: FL083                                     *
    H*                 Correct ASC Processing                             *
    H*                                                                    *
     H*  03/11/10 C:227 Project: FL083                                     *
    H*                 Change to process ASC, Home health and Nursing Home*
    H*                 bills on UB4. Load all 4 modifier fields from      *
    H*                 hospital detail entry program.                     *
    H*                                                                    *
     H*  04/20/09 C:189 Support: 67495                                     *
    H*                 if the license number prefix is PA, CNS, or ARNP   *
    H*                 we take 85% of the MRA. But we need to also check  *
    H*                 for modifier 80 then we would take 75% instead.    *
     H*                                                                    *
     H*  03/25/09 C:079 Support: 67747                                     *
    H*                 Changed to only call medicare for Medical Bills    *
    c*                                                                    *
     H*  02/19/09 C:198 Project: FL080                                     *
    H*                 Changed to load out--of-state providers            *
    H*                                                                    *
     H*  12/19/07 C:47  Support: 61535                                     *
    H*                 FL066-Hospital xray/ lab issue is not paying       *
    H*                 correctly when MRA tech comp is more than the      *
    H*                 locality tech component.                           *
    H*                                                                    *
     H*  11/29/07 C:198 Project: FL072 (Part II)                           *
    H*                 Added logic to process supplies billed on a        *
    H*                 pharmacy form.                                     *
    H*                                                                    *
     H*  11/01/07 C:079 Project: FL068                                     *
    H*                 Add processing for NT license prefix               *
    H*                                                                    *
     H*  09/27/07 C:198 Support: 60185                                     *
    H*                 Changed the logic for codes in the range from      *
    H*                 70000 and 89999, if no allwance is paid acc. to    *
    H*                 locality, then payment would be acc. to TC.        *
    H*                                                                    *
     H*  09/05/07 C:47  Project: FL065                                     *
    H*                 Update code for new FL Fee Schedule                *
    H*                                                                    *
     H*  08/22/07 C:189 Project: FL066                                     *
    H*                 FL 2006 Hosp FS Update.Changes made to determine   *
    H*                 the scheduled ,unscheduled surgeries better with   *
    H*                 FAEUP/EUSCHD                                       *
    H*                                                                    *
     H*  08/09/07 C:194 Project: FL070                                     *
    H*                 Open Hospital Bill Location Zip Code               *
    H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  09/15/06 C:79  Support: MC010 54131                               *
    H*                 Dont reduce allowanc for PA license Number if Mod  *
    H*                 AS is present. Rule will handle this               *
    H*                                                                    *
     H*  03/20/06 C:79  Support: MC010 50307                               *
    H*                 Dont reduce allowanc for PA license Number if Spec *
    H*                 Code PA is present. Rule will handle this          *
    H*                                                                    *
     H*  10/24/05 C:79  Support: 45212                                     *
    H*                 Add rev code 481 as criteria for surgical stay     *
    H*                                                                    *
     H*  10/08/05 C:79  Project: FL057                                     *
    H*                 Add lic# prefix MSW to Medicare rate providers     *
    H*                                                                    *
     H*  07/06/05 C:79  Support: 47960                                     *
    H*                 Add lic# prefix MFC to Medicare rate providers     *
    H*                                                                    *
     H*  03/23/05 C:79  Project: FL050                                     *
    H*                 Change who gets Medicare rates as of new FS        *
    H*                                                                    *
     H*  12/02/04 C:79  Support: MC010 44193                               *
    H*                 Only physicians lic under 458,459 get new surg rate*
    H*                                                                    *
     H*  08/02/04 C:79  Support: MC051 43302                               *
    H*                 Set type of srv to HE for 99281-99285              *
    H*                                                                    *
     H*  06/28/04 C:79  Support: MC044 42921                               *
    H*                 Only set type of srv to HE for outpatient bills    *
    H*                                                                    *
     H*  04/30/04 C:79  Support: MC010 42344                               *
    H*                 If 760-769 compare type of service = HS            *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
    H*                                                                    *
     H*  02/27/04 C:79  Support: MC044 41458                               *
    H*                 Hos - If Rev Code 760-769  then surgical stay      *
    H*                                                                    *
     H*  01/21/04 C:79  Support: MC010 40913                               *
    H*                 Hos - If Rev Code 490 billed then surgical stay    *
    H*                                                                    *
     H*  12/12/03 C:79  Project: FL048                                     *
    H*                 1. Phys - Use new Medicare rates for certain prov  *
    H*                 2. Hos  - Use provider rates for certain bills     *
    H*                                                                    *
     H*  10/30/03 C:79  Project: FS271                                     *
    H*                 Use RXAWPD instead of KKEFDT to select AWP         *
    H*                                                                    *
     H*  10/21/03 C:79  Project: FL044                                     *
    H*                 Use sevice date to find rule date                  *
    H*                                                                    *
     H*  10/02/03 C:79  Project: FSMNT                                     *
    H*                 Move clear of PCAWK2 after update PCA01            *
    H*                                                                    *
    H*  10/17/02 C:79  Project: FS239                                     *
    H*                 Load Pharmacy allowance into CAAWP and CAMRA       *
    H*                                                                    *
    H*  10/10/02 C:79  Support: MC044 34328                               *
    H*                 Populate PR1A4 for excluding codes in counting     *
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
    H*  06/28/02 C:79  Project: MC010 33121                               *
    H*                 In PMOD subroutine process all modifiers           *
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
     Ffdcal13   uf   e           k disk
     Ffaeolg    if   e           k disk
     Ffacap     uf a e           k disk    usropn
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
    F*** BEGIN ADD *** 03/28/01 **************************************
     Fftuml     if   e           k disk
    F*** END   ADD *** 03/28/01 **************************************
    F*** BEG ADD *** 12/12/03 ****************************************
     Ffaehl     if   e           k disk
     Ffaeul     if   e           k disk
     Ffmzrlfl   if   e           k disk
    F*** END ADD *** 12/12/03 ****************************************
     F*** Beg Add *** 04/08/13 *******************************************
     Ffmr1l     if   e           k disk
     F*** End Add *** 04/08/13 *******************************************
     F*
     F*** Beg Add *** 04/08/13 *******************************************
     Ffmprlfl   if   e           k disk
     F                                     Rename(PPR01:PPR02)
     F*** End Add *** 04/08/13 *******************************************
     F*
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
     D xppmod          s                   like(capmod)
      *** Begin Change *** 04/27/18 *********************
     D*** xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 04/27/18 *********************
     D xxdcs#          s              5  0
     D xxinvl          s              1
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxmd62          s              5  3
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D*** BEG ADD *** 12/12/03 *********************************************
     D xxmramed        s                   like(camra)
     D xxmramed26      s                   like(camra)
     D xxmramedTC      s                   like(camra)
     D xxsstay         s              1
     D xxestay         s              1
     D xxhtos          s                   like(cahtos)
     D xxloc           s                   like(zrrgn#)
     D*** BEG ADD *** 03/23/05 *********************************************
     D xxperc          s              3  2
     D*** END ADD *** 03/23/05 *********************************************
     D qqzip           s                   like(zrfzcd)
     D qqefdt          s                   like(cafdat)
     D kkcpt4          s                   like(cacpt4)
     D*** END ADD *** 12/12/03 *********************************************
    D*** Beg Add *** 04/08/13 *********************************************
     D ##clt#          s              3
    D*** End Add *** 04/08/13 *********************************************
    D*** Beg Add *** 08/27/13 *********************************************
     D Xx1a7           s                   like(pr1a7)
    D*** End Add *** 08/27/13 *********************************************
    D*** Beg Add *** 07/13/16 *********************************************
     D xxSetCAamt      s              1
    D*** End Add *** 07/13/16 *********************************************
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
     C*** BEG ADD *** 12/12/03 ****************************************
    C*
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    kkcpt4
     C                   kfld                    cafdat
    C*
     C     prky2         klist
     C                   kfld                    qqco#
     C                   kfld                    kkcpt4
     C*** END ADD *** 12/12/03 ****************************************
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
    C*** BEGIN CHG *** 03/28/01 **************************************
    C**********           KFLD           KKYMD
     C                   kfld                    kkefdt
    C*** END   CHG *** 03/28/01 **************************************
     C*** Beg Add *** 04/08/13 *********************************************
     C     r1ky1         klist
     C                   kfld                    rcco#
     C                   kfld                    rczipa
     C                   kfld                    rccpt4
     C                   kfld                    rcefdt
     C*** End Add *** 04/08/13 *********************************************
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
     C*** BEG ADD *** 12/12/03 *********************************************
     C*
     C* Physician Header Info
     C*
     C     ehky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*
     C* Hospital Header Info
     C*
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*
     C* Florida Zip Code/Region
     C*
     C     zrky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqzip
     C                   kfld                    qqefdt
     C*
     C     zrky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqzip
     C*** END ADD *** 12/12/03 *********************************************
     C*
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
     C*** Beg Add *** 04/08/13 *********************************************
     C*
     C*** Beg Add *** 01/15/15 *********************************************
    C*  FMPRPFL procedure code file.
     C     keyFmprpFL    klist
     C                   kfld                    qqco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt

     C     keyFmprpFL1   klist
     C                   kfld                    qqco#
     C                   kfld                    kkcpt4
     C*** End Add *** 01/15/15 *********************************************
    C*
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
     C                   parm      *blanks       ##mcrv            1            Medicare CR Flag
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
     C*** End Add *** 04/08/13 *********************************************
    C*** BEGIN DEL *** 05/30/01 **************************************
    C********** *LIKE     DEFN GZFRZP    XXFRZP
    C********** *LIKE     DEFN GZZIPA    XXZIPA
    C*** END   DEL *** 05/30/01 **************************************
    C*
    C*
    C*
    C*
    C*** BEG DEL *** 02/02/00 ****************************************
    C******************** MOVE *ZERO     FUF
    C******************** MOVE *ZERO     FUT
    C******************** Z-ADD1         U       50
    C*** END DEL *** 02/02/00 ****************************************
     C                   move      qqdcs#        xxdcs#
    C*
     C                   open      facap
     C*** Beg Add *** 04/08/13 *********************************************
     C                   exsr      pgmstr
     C*** End Add *** 04/08/13 *********************************************
    C*
     C                   exsr      convrt
    C*
    C*  END PROGRAM AND CLOSE ALL FILES
     C                   close     facap
     C*** BEG ADD *** 12/12/03 *******************************************
    C*
    C* If this was an outpatient surgical or emergency room stay then
    C* cycle through each FACAP record and update CAHTOS.
    C*
     C                   if        xxsstay = 'Y' or xxestay = 'Y'
     C                             and euicn >= 'AO' and euicn <= 'HS'
     C                   exsr      pas2
     C                   endif
     C*** END ADD *** 12/12/03 *******************************************
    C*
     C     term          tag
    C*
     C                   eval      xcmra = 0
     C                   return
     C                   move      *on           *inlr
    C*
     C*** BEG ADD *** 03/11/10 *********************************************
    C********************************************************************
    C** SUBROUTINE ASC                                          A S C   *
    C********************************************************************
     C     asc           begsr
    C*
     C                   eval      kkefdt = cafdat
     C                   move      durvcd        casvcd
     C                   move      durate        cacpt4
     C                   move      durate        xxcpt4
     C                   move      dupos         caplac
     C                   move      duunts        caunts
     C*
    C* Load surgery ASC allowance from physician fee schedule
    C*
     C*** Beg Chg *** 02/10/16 *********************************
     C**************     if        durate >= '10000' and
     C**************               durate <= '69999'
     C                   if        durate >= 'AAAAA' and
     C                             durate <= '99999'
     C*** End Chg *** 02/10/16 *********************************
     C     prky          setgt     ppr01
     C     nxtpr6        tag
     C                   readp     ppr01                                  60
     C*
     C                   if        *in60 = *on
     C*
     C                   movel     'Y'           cainvl
     C                   else
     C     prstat        cabeq     'I'           nxtpr6
     C                   eval      camra = pr92n1
     C*** BEG ADD *** 04/26/10 *********************************************
     C                   eval      caasc = pr92n1
     C*** END ADD *** 04/26/10 *********************************************
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      prfsr         cafsr
     C*** BEG ADD *** 01/04/12 *********************************************
     C*
     C* Poplulate the Bilateral Procedure Flag
     C*
     C                   if        pr1a6 = 'Y'
     C                   move      '1'           ca50in
     C                   else
     C                   move      ' '           ca50in
     C                   endif
     C*** END ADD *** 01/04/12 *********************************************
     C*
     C                   endif
     C*
     C                   endif
     C*
     C*** BEG ADD *** 04/26/10 *********************************************
     C*
     C* Load Radiology allowance from physician fee schedule
     C*
     C                   if        durate >= '70000' and
     C                             durate <= '99999'
     C*** Beg Add *** 02/10/16 *********************************************
     C                             and CAasc = 0
     C*** End Add *** 02/10/16 *********************************************
     C     prky          setgt     ppr01
     C     nxtpr7        tag
     C                   readp     ppr01                                  60
     C*
     C                   if        *in60 = *on
     C*
     C                   movel     'Y'           cainvl
     C                   else
     C     prstat        cabeq     'I'           nxtpr7
     C*
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      prfsr         cafsr
     C*** BEG ADD *** 01/04/12 *********************************************
     C*
     C* Poplulate the Bilateral Procedure Flag
     C*
     C                   if        pr1a6 = 'Y'
     C                   move      '1'           ca50in
     C                   else
     C                   move      ' '           ca50in
     C                   endif
     C*** END ADD *** 01/04/12 *********************************************
     C                   Exsr      HospMedicare
     C*
     C                   endif
     C*
     C                   endif
     C*** END ADD *** 04/26/10 *********************************************
     C*** BEG ADD *** 06/29/10 *********************************************
     C                   if        camra = 0
     C                   eval      camra = cachrg
     C                   endif
     C*** END ADD *** 06/29/10 *********************************************
     C*
     C     asce          endsr
     C*** END ADD *** 03/11/10 *********************************************
    C********************************************************************
    C*  Convert fields into base fields used for determining payment
    C*  creates workfile with MRA and modifier amounts and prof/tech
    C*****************************************************************
     C     convrt        begsr
    C*
     C                   eval      x = 0
     C                   clear                   date
     C*** BEG ADD *** 12/12/03 ****************************************
     C                   eval      xxsstay = *blank
     C                   eval      xxestay = *blank
     C*** END ADD *** 12/12/03 ****************************************
    C*** BEG DEL *** 01/28/00 ****************************************
    C******************** MOVELQQSPCD    CASPCD
    C*** END DEL *** 01/28/00 ****************************************
    C*
    C* Find the recon number.  If this is a recon.
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      xxrcn = eorcn
     C*** BEG ADD *** 12/12/03 ********************************************
     C                   if        eobtyp = 'H'
     C     euky          chain(N)  peu01                              99
     C                   endif
     C*** END ADD *** 12/12/03 ********************************************
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
    C*** BEG ADD *** 01/28/00 ****************************************
     C                   movel     qqspcd        caspcd
    C*** END ADD *** 01/28/00 ****************************************
     C                   clear                   pdd01
     C                   clear                   pdh01
     C                   clear                   pdd01
     C                   clear                   pdu01
     C*** BEG ADD *** 10/17/02 *********************************************
     C                   clear                   mod
     C                   clear                   mod1
     C*** END ADD *** 10/17/02 *********************************************
     C*** Beg Add *** 08/27/13 *********************************************
     C                   clear                   Xx1a7
     C*** End Add *** 08/27/13 *********************************************
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
     C                   exsr      proc
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
     C*** Beg Add *** 11/29/07 *********************************************
     C                   move      dphcpc        cacpt4
     C*** End Add *** 11/29/07 *********************************************
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
    C*** BEG ADD *** 03/11/10 *********************************************
     C                   exsr      pmodld
    C*** END ADD *** 03/11/10 *********************************************
     C                   endif
    C*
     C                   movel     'H'           catype
    C*
    C*** BEG ADD *** 03/11/10 *********************************************
    C*** BEG DEL *** 04/26/10 *********************************************
     C****************   if        cafdat >= 1100708
    C*** END DEL *** 04/26/10 *********************************************
     C                   if        eublty >= '831' and eublty <= '839'
     C                   exsr      asc
     C                   else
     C                   exsr      svcd
     C                   endif                                                  eublty - 831-839
    C*** BEG DEL *** 04/26/10 *********************************************
     C**************     else
     C**************     if        eospcd = 'ASC' or
     C**************               %subst(euflic:1:3) = 'ASC'
     C**************     exsr      asc
     C**************     else
    C*** END DEL *** 04/26/10 *********************************************
    C*** END ADD *** 03/11/10 *********************************************
    C*** BEG DEL *** 04/26/10 *********************************************
     C****************** exsr      svcd
    C*** END DEL *** 04/26/10 *********************************************
    C*** BEG ADD *** 03/11/10 *********************************************
    C*** BEG DEL *** 04/26/10 *********************************************
     C*************      endif                                                  eospcd = ASC
     C*************      endif                                                  cafdat >=1100708
    C*** END DEL *** 04/26/10 *********************************************
    C*** END ADD *** 03/11/10 *********************************************
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
     C*** BEG ADD *** 12/12/03 *********************************************
     C*
     C* On or after 1/1/04 use greater of Medicare based rates vs. FL FS
     C*
     C                   if        cafdat >= 1040101 and eobtyp = 'N'
     C*** BEG ADD *** 03/23/05 *********************************************
     C                             and dhprcd >= '10000'
     C*** END ADD *** 03/23/05 *********************************************
     C*** BEG ADD *** 02/19/09 *********************************************
     C*** Beg Chg *** 08/27/13 *********************************************
     C***************************  or  pr1a7 = 'Y'
     C                             or  xx1a7 = 'Y'
     C*** End Chg *** 08/27/13 *********************************************
     C*** BEG ADD *** 03/25/09 *********************************************
     C                             and eobtyp = 'N'
     C*** END ADD *** 03/25/09 *********************************************
     C*** END ADD *** 02/19/09 *********************************************
     C*** Beg Add *** 08/27/13 *********************************************
     C                   if        carcfl <> 'Y'
     C*** End Add *** 08/27/13 *********************************************
     C                   exsr      medicare
     C*** Beg Add *** 08/27/13 *********************************************
     C                   endif
     C*** End Add *** 08/27/13 *********************************************
     C                   end                                                    if cafdat >=
     C*** END ADD *** 12/12/03 *********************************************
     C*** BEG ADD *** 03/23/05 *********************************************
     C                   if        cafdat >= 1050509 and eobtyp = 'N'
     C                             and dhprcd >= '10000'
     C                   exsr      lodlicense
     C                   end                                                    if cafdat >=
     C*** END ADD *** 03/23/05 *********************************************
    C*** Beg Add *** 04/08/13 *************************************************
     C                   call      'RTVCLTID'
     C                   parm      *blanks       ##clt#
      /Free
       //*** Begin Chg *** 04/30/19 ****************************************
       //*** If ##clt# = '114';
       If ##clt# = '114' Or ##clt# = '152';
       //*** End   Chg *** 04/30/19 ****************************************
          If (Catype = 'P' or Catype = 'D') and Carcfl <> 'Y';
             If Camra = 0 and Caprof = 0 and Catech = 0 and Caanam = 0
       // Beg Chg ***** 06/11/15 *******************************************
       //***  and Caasc = 0;
              and Caasc = 0 and CAinvl = 'Y';
       // End Chg ***** 06/11/15 *******************************************
                Exsr Recu;
             Endif;
          Endif;
       Endif;
      /End-Free
    C*** End Add *** 04/08/13 *************************************************
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
    C*** Beg Add *** 07/13/16 *******************************************
     C* Load CAamt as charged for Some OPPS services
     C                   if        xxSetCAamt = 'Y'
     C                   eval      CAamt = CAchrg
     C                   endif
    C*** End Add *** 07/13/16 *******************************************
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
      **** Begin Add *** 06/02/17 **********************************************************
      /Free
       If (EObtyp = 'N' or EOBtyp ='D') and CAfdat >= 1170701;
          CApayMRA = 'Y';
       Endif;
      /End-Free
      **** End   Add *** 06/02/17 **********************************************************
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
    C*** BEG ADD *** 02/18/00 ****************************************
     C                   clear                   pcawk2
    C*** END ADD *** 02/18/00 ****************************************
    C*
     C     caky1         reade     pca01                                  54
    C*
     C                   enddo
    C*
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
    C*
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     ddprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
    C*
    C*  Default to use service date for effective date for rules
    C*
     C*** BEG ADD *** 10/21/03 *********************************************
     C                   eval      kkefdt = cafdat
     C*** END ADD *** 10/21/03 *********************************************
    C*
     C     prky          setgt     ppr01
     C     nxtpr3        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
     C*** BEG DEL *** 10/21/03 *********************************************
     C****************** eval      kkefdt = prefdt
     C*** END DEL *** 10/21/03 *********************************************
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
     C*** BEG DEL *** 10/21/03 *********************************************
     C****************** eval      kkefdt = prefdt
     C*** END DEL *** 10/21/03 *********************************************
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
     C                   move      pr1a3         canct
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
     C                   eval      camd50 = *zero
     C                   eval      camd51 = *zero
     C                   eval      camd80 = *zero
     C                   eval      camd81 = *zero
     C                   clear                   caplac
     C*** Beg Add *** 08/27/13 *********************************************
     C                   eval      Xx1a7 = pr1a7
     C*** End Add *** 08/27/13 *********************************************
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
     C*** BEG ADD *** 03/23/05 *********************************************
    C*
    C********************************************************************
    C** LODLICENSE SUBROUTINE                        L O D L I C E N S E*
    C********************************************************************
     C     lodlicense    begsr
    C*
     C                   eval      xxperc = 0
     C*** BEG ADD *** 09/15/06 *********************************************
     C     'AS'          lookup    mod(1)                                 99
    C*
     C*** END ADD *** 09/15/06 *********************************************
    C*
    C* If license prefix = PA pay 85% unless specialty code is PA or mod
    C* AS is billed
    C*
     C*** BEG CHG *** 03/20/06 *********************************************
     C****************   if        %subst(ehflic:1:2) = 'PA' or
     C*** BEG CHG *** 09/15/06 *********************************************
     C****************   if        (%subst(ehflic:1:2) = 'PA' and
     C****************              eospcd <> 'PA') or
     C                   if        (%subst(ehflic:1:2) = 'PA' and
     C                              eospcd <> 'PA' and *in99 = *off) or
     C*** END CHG *** 09/15/06 *********************************************
     C*** END CHG *** 03/20/06 *********************************************
     C                             %subst(ehflic:1:3) = 'CNS' or
     C                             %subst(ehflic:1:4) = 'ARNP'
     C                   eval      xxperc = .85
     C*** BEG ADD *** 04/20/09 *********************************************
     C                   if        dhmod1 = '80'or dhmod2 = '80' or dhmod3= '80'
     C                             or dhmod4 = '80'
     C                   eval      xxperc = .75
     C                   endif
     C*** END ADD *** 04/20/09 *********************************************
     C                   endif
    C*
     C                   if        %subst(ehflic:1:2) = 'SW'
     C*** BEG ADD *** 10/08/05 *********************************************
     C                             or %subst(ehflic:1:3) = 'MSW'
     C                             or eospcd = 'SOC'
     C*** END ADD *** 10/08/05 *********************************************
     C                   eval      xxperc = .75
     C                   endif
     C*** BEG ADD *** 10/08/05 *********************************************
     C                   if        eospcd = 'NUT'
     C*** BEG ADD *** 11/01/07 *********************************************
     C                             or %subst(ehflic:1:2) = 'NT'
     C*** END ADD *** 11/01/07 *********************************************
     C                   eval      xxperc = .85
     C                   endif
     C                   if        eospcd = 'IL'
     C                             or %subst(ehflic:1:2) = 'IL'
     C                   eval      camra = catech
     C                   endif
     C*** END ADD *** 10/08/05 *********************************************
    C*
     C                   if        xxperc > 0
     C                   eval(H)   camra  = camra * xxperc
     C                   eval(H)   caprof = caprof * xxperc
     C                   eval(H)   catech = catech * xxperc
     C                   endif
    C*
     C                   endsr
     C*** END ADD *** 03/23/05 *********************************************
    C*
    C********************************************************************
    C** MEDICARE SUBROUTINE                           M E D I C A R E   *
    C********************************************************************
     C     medicare      begsr
     C*
     C* Only Physicians, Osteopaths and surgeries get Medicare rates
     C*
     C     ehky          chain     peh01                              99
     C*** BEG ADD *** 03/23/05 *********************************************
     C                   if        cafdat < 1050509
     C                             and (%subst(ehflic:1:2) = 'ME'
     C                             or %subst(ehflic:1:2) = 'OS')
     C*** BEG ADD *** 03/23/05 *********************************************
     C                             or %subst(ehflic:1:3) = 'MFC'
     C*** END ADD *** 03/23/05 *********************************************
     C
     C                             or cafdat >= 1050509 and
     C                             %subst(ehflic:1:2) <> 'ZZ' and
     C                             %subst(ehflic:1:2) <> 'XX'
     C*** END ADD *** 03/23/05 *********************************************
     C*
     C*** BEG DEL *** 03/23/05 *********************************************
     C************
     C************       if        %subst(ehflic:1:2) = 'ME' and
     C************                 dhprcd >= '10000' or
     C************                 %subst(ehflic:1:2) = 'OS' and
     C****BEG*CHG**** 12/02/04 *********************************************
     C**************************** dhprcd >= '10000' or
     C**************************** dhprcd >= '10000' and dhprcd <='69999'
     C************                 dhprcd >= '10000'
     C****END*CHG**** 12/02/04 *********************************************
     C*** END DEL *** 03/23/05 *********************************************
     C*
     C* Find the locality
     C*
     C                   movel     eoflic        zzzip             5
     C                   move      zzzip         qqzip
     C                   eval      qqefdt = cafdat
     C     zrky          setgt     pzr01
     C     zrky1         readpe    pzr01                                  99
     C*
     C                   if        *in99 = *off
     C                   eval      xxloc = zrrgn#
     C                   else
     C                   eval      xxloc = 99
     C                   endif
     C*
     C* Load the correct rate
     C*
     C                   eval      xxmramed = 0
     C*
     C                   select
     C*
     C* Locality 99 (01/02)
     C*
     C                   when      xxloc = 99
     C*
     C                   eval      xxmramed26 = pr92n5
     C                   eval      xxmramedTC = pr92n6
     C*
     C                   if        dhpos = ' 11'
     C*** Beg Add *** 08/27/13 *********************************************
     C                             or dhpos = ' 20'
     C*** End Add *** 08/27/13 *********************************************
     C*** Beg Add *** 06/16/16 *********************************************
     C                             or dhpos = ' 12'
     C                             or dhpos = ' 32'
     C*** End Add *** 06/16/16 *********************************************
     C                   eval      xxmramed = pr92n3
     C                   else
     C                   eval      xxmramed = pr92n4
     C                   endif
     C*
     C* Locality 03
     C*
     C                   when      xxloc = 03
     C*
     C                   eval      xxmramed26 = pr92n9
     C                   eval      xxmramedTC = pr92na
     C*
     C                   if        dhpos = ' 11'
     C*** Beg Add *** 08/27/13 *********************************************
     C                             or dhpos = ' 20'
     C*** End Add *** 08/27/13 *********************************************
     C                   eval      xxmramed = pr92n7
     C                   else
     C                   eval      xxmramed = pr92n8
     C                   endif
     C*
     C* Locality 04
     C*
     C                   when      xxloc = 04
     C*
     C                   eval      xxmramed26 = pr92nd
     C                   eval      xxmramedTC = pr92ne
     C*
     C                   if        dhpos = ' 11'
     C*** Beg Add *** 08/27/13 *********************************************
     C                             or dhpos = ' 20'
     C*** End Add *** 08/27/13 *********************************************
     C                   eval      xxmramed = pr92nb
     C                   else
     C                   eval      xxmramed = pr92nc
     C                   endif
     C*
     C                   endsl
     C*
     C                   if        xxmramed > camra
     C                   eval      camra = xxmramed
     C                   eval      xxmra = xxmramed
     C                   endif
     C*
     C                   if        xxmramed26 > caprof
     C                   eval      caprof = xxmramed26
     C                   endif
     C*
     C                   if        xxmramedTC > catech
     C                   eval      catech = xxmramedTC
     C                   endif
     C*
     C                   endif                                                  if xxprcd <> blank
     C*
     C*** BEG ADD *** 02/19/09 *********************************************
     C     ehky          chain     peh01                              99
     C*
     C                   if        cafdat >= 1090209
     C                             and (%subst(ehflic:1:2) = 'ZZ'
     C                             or %subst(ehflic:1:2) = 'XX')
     C*
     C                   if        dhpos = ' 11'
     C*** Beg Add *** 08/27/13 *********************************************
     C                             or dhpos = ' 20'
     C*** End Add *** 08/27/13 *********************************************
     C                   eval      camra = pr92nf
     C                   eval      xxmra = pr92nf
     C                   else
     C                   eval      camra = pr92ng
     C                   eval      xxmra = pr92ng
     C                   endif
     C*
     C     '26'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      caprof = pr92nh
    C*
     C                   endif
    C*
     C     'TC'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      catech = pr92ni
    C*
     C                   endif
     C*
     C                   endif
     C*
     C*** END ADD *** 02/19/09 *********************************************
     C                   endsr
     C*
    C*
    C********************************************************************
    C** PAS2 SUBROUTINE                                    P A S 2      *
    C********************************************************************
     C     pas2          begsr
     C*
     C                   if        xxsstay = 'Y' and xxestay <> 'Y'
     C                   eval      xxhtos = 'HSN'
     C                   endif
     C*
     C                   if        xxestay = 'Y' and xxsstay <> 'Y'
     C                   eval      xxhtos = 'HNE'
     C                   endif
     C*
     C                   if        xxestay = 'Y' and xxsstay = 'Y'
     C                   eval      xxhtos = 'HSE'
     C                   endif
     C*
     C                   open      facap
     C                   read      pcawk2                                 99
     C*
     C                   dow       *in99 = *off
       // Beg Add ***** 05/19/16 *******************************************
     C                   exsr      setCAhtos
       // End Add ***** 05/19/16 *******************************************
     C                   eval      cahtos = xxhtos
     C                   update    pcawk2
     C                   read      pcawk2                                 99
     C                   enddo
     C*
     C                   close     facap
     C*
     C                   endsr
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
     C*** Beg Add *** 11/29/07 *********************************************
     C                   Select
    C*
     C                   when      dprxpt <> 'S'
     C*** End Add *** 11/29/07 *********************************************
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
    C*
     C*** BEG CHG *** 10/30/03 *********************************************
     C****************** if        cafdat < kkefdt
     C                   if        cafdat < rxawpd
     C*** END CHG *** 10/30/03 *********************************************
    C*
      *** Begin Change *** 04/27/18 **********************
     C**                 eval      xxawp = rx1awp / 1000
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
    C*
     C                   endif
    C*
     C     rxstat        cabeq     'I'           nxtpr4
    C*
     C*** Beg Add *** 11/29/07 *********************************************
     C                   when      dprxpt = 'S'
     C*
     C                   move      dphcpc        kkcpt4
     C     prky1         setgt     ppr01
     C     nxtpr5        tag
     C     prky2         readpe    ppr01                                  60
     C*
     C*  A hard error will cause the program to terminate
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
     C                   else
     C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caamt = prmra
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n2
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      prfsr         cafsr
     C                   move      dphcpc        cacpt4
     C                   eval      caunts = dpunts
     C                   move      'C'           carcfl
     C*** Beg Add *** 08/27/13 *********************************************
     C                   eval      Xx1a7 = pr1a7
     C*** End Add *** 08/27/13 *********************************************
     C*
     C                   endif
     C*
     C     prstat        cabeq     'I'           nxtpr5
     C*
     C                   endsl
     C*** End Add *** 11/29/07 *********************************************
     C     phare         endsr
    C********************************************************************
    C** PROCEDURE CODE MODIFIER CALCULATIONS                  P M O D   *
    C********************************************************************
     C     pmod          begsr
    C* Adjust MRA depending on MOD
    C*** BEG ADD *** 06/28/02 ****************************************
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
     C     '54'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd54
    C*
     C                   endif
    C*
     C     '55'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd55
    C*
     C                   endif
    C*
     C     '56'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd56
    C*
     C                   endif
    C*
     C     '62'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
    C*** BEG CHG *** 06/28/02 ****************************************
    C********** CAMRA     MULT CAMD62    XXMRA
     C                   eval      xxmra = camra * xxmd62
    C*** BEG CHG *** 06/28/02 ****************************************
    C*
     C                   endif
    C*
     C     '66'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd66
    C*
     C                   endif
    C*** END ADD *** 06/28/02 ****************************************
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
     C*** BEG ADD *** 03/11/10 *********************************************
     C                   if        eobtyp = 'N'
     C*** END ADD *** 03/11/10 *********************************************
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
     C*** BEG ADD *** 03/11/10 *********************************************
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
     C*** END ADD *** 03/11/10 *********************************************
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
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     dhprcd        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
    C*
    C*  Default to use service date for effective date for rules
    C*
     C*** BEG ADD *** 10/21/03 *********************************************
     C                   eval      kkefdt = cafdat
     C*** END ADD *** 10/21/03 *********************************************
    C*
     C     prky          setgt     ppr01
     C     nxtpr2        tag
     C                   readp     ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C*** BEG DEL *** 10/21/03 *********************************************
     C****************** eval      kkefdt = cafdat
     C*** END DEL *** 10/21/03 *********************************************
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
     C*** BEG DEL *** 10/21/03 *********************************************
     C****************** eval      kkefdt = cafdat
     C*** END DEL *** 10/21/03 *********************************************
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
      *** BEGIN CHANGE *** 06/13/17 ****************************************
     C****               eval      catech = pr92n2
      *** Beg Chg *** 08/18/17 *********************************************
     C****************   eval      catech = pr92n6
     C                   eval      catech = pr92n2
      *** End Chg *** 08/18/17 *********************************************
      *** END   CHANGE *** 06/13/17 ****************************************
     C                   movel     pr1a2         caanes
     C                   eval      caanam = pranam
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   eval      caasc = pr92n1
     C*** BEG ADD *** 01/04/12 *********************************************
     C*
     C* Poplulate the Bilateral Procedure Flag
     C*
     C                   if        pr1a6 = 'Y'
     C                   move      '1'           ca50in
     C                   else
     C                   move      ' '           ca50in
     C                   endif
     C*** END ADD *** 01/04/12 *********************************************
    C*
     C*** BEG ADD *** 09/05/07 *********************************************
     C*** BEG ADD *** 03/11/10 *********************************************
     C                   if        cafdat >= 1100708
     C                   if        eublty >= '831' and eublty <= '839'
     C                             and dhprcd >= '10000'
     C                             and dhprcd <= '69999'
     C                             and pr92n1 >  0
     C                   eval      camra = pr92n1
     C                   endif
     C                   else
     C*** END ADD *** 03/11/10 *********************************************
     C                   if        eospcd = 'ASC'
     C                             and dhprcd >= '10000'
     C                             and dhprcd <= '69999'
     C                             and pr92n1 >  0
     C                   eval      camra = pr92n1
     C                   endif
     C*** BEG ADD *** 03/11/10 *********************************************
     C                   endif
     C*** END ADD *** 03/11/10 *********************************************
     C*** END ADD *** 09/05/07 *********************************************
    C*
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a3         canct
     C                   move      pr1ad         casepp
     C*** BEG CHG *** 10/10/02 *********************************************
     C****************** move      pr1a4         capcf
     C                   move      pr1a4         cabmri
     C*** END CHG *** 10/10/02 *********************************************
     C*** Beg Add *** 08/27/13 *********************************************
     C                   eval      Xx1a7 = pr1a7
     C*** End Add *** 08/27/13 *********************************************
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
     C                   if        pr1a9 = '1'
     C                   eval      camd50 = 0.50
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
    C*** BEG ADD *** 06/28/02 ****************************************
     C                   eval      xxmd62 = 0.625
    C*** END ADD *** 06/28/02 ****************************************
     C                   endif
     C                   if        pr1ac = '2'
     C                   eval      camd66 = 1.00
     C                   endif
     C                   if        pr1aa = '2'
     C                   eval      camd80 = 0.25
     C                   eval      camd81 = 0.25
     C                   eval      camd82 = 0.25
     C                   endif
    C*** BEG ADD *** 06/28/02 ****************************************
    C*
    C* On or after 9/30/2001 we don't have the mod ind table. Default
    C* modifiers to their percentages.
    C*
     C                   if        cafdat >= 1010930
    C*
     C                   eval      camd50 = 0.50
     C                   eval      camd51 = 0.50
     C                   eval      camd54 = 1.00
     C                   eval      camd55 = 1.00
     C                   eval      camd56 = 1.00
     C                   eval      camd62 = 6.25
     C                   eval      xxmd62 = 0.625
     C                   eval      camd66 = 1.00
     C                   eval      camd80 = 0.25
     C                   eval      camd81 = 0.25
     C                   eval      camd82 = 0.25
    C*
     C                   endif
    C*** END ADD *** 06/28/02 ****************************************
    C*
    C*
     C                   endif
     C     prstat        cabeq     'I'           nxtpr2
    C*
     C     proce         endsr
    C*** Beg Add *** 04/08/13 *********************************************
    C********************************************************************
    C*  RETRIEVE SYSTEM VALUES
    C*****************************************************************
     C     pgmstr        begsr
     C*
    C                   call      'SA001R'      plist2
     C*
     C     pgmstre       endsr
    C*** End Add *** 04/08/13 *********************************************
    C********************************************************************
    C** REASONABLE AND CUSTOMARY FILE SEARCH                  R E C U   *
    C********************************************************************
     C     recu          begsr
    C*
    C*** BEGIN ADD *** 03/28/01 **************************************
     C                   eval      kkefdt = cafdat
    C*** END   ADD *** 03/28/01 **************************************
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
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
     C                   else
    C*** BEG ADD *** 08/20/02 ****************************************
     C                   move      rcfsr         cafsr
    C*** END ADD *** 08/20/02 ****************************************
    C*** Beg Add *** 04/08/13 *********************************************
     C*** Begin Chg *** 04/30/19 *******************************************
     C**********         If        ##clt# = '114'
     C                   If        ##clt# = '114' Or ##clt# = '152'
     C*** End   Chg *** 04/30/19 *******************************************
     C                   move      'Y'           Carcfl
     C                   Endif
    C*** End Add *** 04/08/13 *********************************************
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
    D*** Beg Add *** 04/08/13 *********************************************
     C*** Begin Chg *** 04/30/19 *******************************************
     C**********         if        ##ucpf = 'Y' and ##clt# = '114'
     C                   if        ##ucpf = 'Y' and (##clt# = '114' Or
     C                             ##clt# = '152')
     C*** End   Chg *** 04/30/19 *******************************************
     C     r1ky1         chain     pr101                              60
     C                   if        *in60 = *off
     C                   z-add     r1mr85        rcmra
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   clear                   rcprof
     C                   clear                   rc92n2
     C                   endif                                                  *in60
     C                   endif                                                  ##ucpf
    D*** End Add *** 04/08/13 *********************************************
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
     C                   move      dhpos         caplac
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** move      dhprcd        cacpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   eval      caunts = dhunts
     C                   move      dharea        capart
     C*** Beg Add *** 04/08/13 *********************************************
     C                   if        carcfl <> 'Y'
     C*** End Add *** 04/08/13 *********************************************
     C                   move      dhrcfl        carcfl
     C*** Beg Add *** 04/08/13 *********************************************
     C                   endif
     C*** End Add *** 04/08/13 *********************************************
     C*** BEG ADD *** 08/22/02 *********************************************
     C*
     C                   when      eobtyp = 'D'
    C*
     C                   move      ddpos         caplac
     C                   eval      caunts = ddunts
     C                   move      ddpart        capart
     C*** Beg Add *** 04/08/13 *********************************************
     C                   if        carcfl <> 'Y'
     C*** End Add *** 04/08/13 *********************************************
     C                   move      ddrcfl        carcfl
     C*** Beg Add *** 04/08/13 *********************************************
     C                   endif
     C*** End Add *** 04/08/13 *********************************************
    C*
     C                   endsl
     C*** END ADD *** 08/22/02 *********************************************
    C*
    C* REASONABLE AND CUSTOMARY DO NOT CONTAIN A VALID EFFECTIVE
    C* DATE FOR THE PROCEDURE CODE
    C*
    C*** BEGIN DEL *** 03/28/01 **************************************
    C********** RUKY      SETGTPRU01
    C********** RUKY1     REDPEPRU01                    56
    C********** *IN56     IFEQ '0'
    C**********           Z-ADDRUEFDT    KKEFDT
    C**********           ELSE
    C**********           MOVE *LOVAL    KKEFDT
    C**********           ENDIF
    C*** END   DEL *** 03/28/01 **************************************
    C*
     C                   endif
     C     rcstat        cabeq     'I'           nxrc1
    C*
     C     recue         endsr
       // Beg Add ***** 05/19/16 *******************************************
      //*****************************************************************
      //  Set the hospital type of service
      //*****************************************************************
      /Free
       begsr setCAHtos;
         If CAfdat >= 1150101 and EUicn >= 'HO' and EUicn <= 'HS';
            if EUSchd = 'S' and EUicn = 'HO';
              xxHtos = 'HNN';
            EndIf;
            if EUSchd = 'U' and EUicn = 'HO';
              xxHtos = 'HNE';
            EndIf;
            if EUSchd = 'S' and EUicn = 'HS';
              xxHtos = 'HSN';
            EndIf;
            if EUSchd = 'U' and EUicn = 'HS';
              xxHtos = 'HSE';
            EndIf;
         EndIf;
       Endsr;
      /End-Free
       // End Add ***** 05/19/16 *******************************************
    C********************************************************************
    C** SERVICE CODE SEARCH                                   S V C D   *
    C********************************************************************
     C     svcd          begsr
    C*
    C*  Default to use service date for effective date for rules
    C*
     C*** BEG ADD *** 10/21/03 *********************************************
     C                   eval      kkefdt = cafdat
     C*** END ADD *** 10/21/03 *********************************************
     C*** BEG ADD *** 12/12/03 *********************************************
     C                   if        euicn >= 'AS' and euicn <= 'HS'
     C                   eval      cahtos = 'HO'
     C                   else
     C                   eval      cahtos = euicn
     C                   endif
     C*** END ADD *** 12/12/03 *********************************************
    C*
     C                   move      durvcd        xxsvcd
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     durate        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   move      '000'         xxunit
    C*** BEG ADD *** 01/28/00 ****************************************
     C                   move(p)   durvcd        casvcd
     C                   move      durate        cacpt4
    C*** END ADD *** 01/28/00 ****************************************
    C*
     C     svky          setgt     psv01
     C     nxtsv1        tag
     C                   readp     psv01                                  60
    C*
     C*** BEG DEL *** 10/21/03 *********************************************
     C****************** eval      kkefdt = cafdat
     C*** END DEL *** 10/21/03 *********************************************
    C*
     C                   if        *in60 = *on
     C                   move      'Y'           cainvl
    C*
     C                   else
     C                   eval      camra = svalow
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
     C                   move      duunts        caunts
     C*** BEG ADD *** 12/12/03 *********************************************
     C*
     C* Check for surgical stay 'HS' or emergency room stay 'HE'
     C*
     C*** BEG CHG *** 04/30/04 *********************************************
     C****************** if        euicn >= 'AS' and euicn <= 'HS'
     C                   if        euicn = 'AS' or euicn = 'HS'
     C*** END CHG *** 04/30/04 *********************************************
     C*
     C                   if        durvcd >= 360 and durvcd <= 369
     C*** BEG ADD *** 02/27/04 *********************************************
     C                             or durvcd >= 490 and durvcd <= 499
     C                             or durvcd >= 760 and durvcd <= 769
     C*** END ADD *** 02/27/04 *********************************************
     C*** BEG ADD *** 12/12/03 *********************************************
     C*** BEG DEL *** 02/27/04 *********************************************
     C**************************** or durvcd = 490
     C*** END DEL *** 02/27/04 *********************************************
     C*** END ADD *** 12/12/03 *********************************************
     C*** BEG ADD *** 10/24/05 *********************************************
     C                             or durvcd = 481
     C*** END DEL *** 10/24/05 *********************************************
     C                   eval      cahtos = 'HS'
     C                   eval      xxsstay = 'Y'
     C                   endif
     C*** BEG DEL *** 04/30/04 *********************************************
     C****************** if        durvcd >= 450 and durvcd <= 459
     C****************** eval      cahtos = 'HE'
     C****************** eval      xxestay = 'Y'
     C****************** endif
     C*** END DEL *** 04/30/04 *********************************************
     C*
     C                   endif
     C*** BEG ADD *** 08/22/07 *********************************************
     C                   if        euschd <> *Blanks
     C*
     C                   if        euschd = 'S'
     C                   Eval      xxestay = 'N'
     C                   endif
     C*
     C                   if        euschd = 'U'
     C                   Eval      xxestay = 'Y'
     C                   endif
     C*
     C                   Else                                                   euschd <> *Blanks
     C*** END ADD *** 08/22/07 *********************************************
     C*** BEG ADD *** 04/30/04 *********************************************
     C                   if        durvcd >= 450 and durvcd <= 459
     C*** BEG ADD *** 06/28/04 *********************************************
     C                             and euicn <> 'IM' and euicn <> 'IP' and
     C                             euicn <> 'IR' and euicn <> 'IS'
     C*** END ADD *** 06/28/04 *********************************************
     C*** BEG ADD *** 08/02/04 *********************************************
     C                             or durate >= '99281' and durate <= '99285'
     C                             and euicn <> 'IM' and euicn <> 'IP' and
     C                             euicn <> 'IR' and euicn <> 'IS'
     C*** END ADD *** 08/02/04 *********************************************
     C                   eval      cahtos = 'HE'
     C                   eval      xxestay = 'Y'
     C                   endif
     C*** END ADD *** 04/30/04 *********************************************
     C*** BEG ADD *** 08/22/07 *********************************************
     C                   endif
     C*** END ADD *** 08/22/07 *********************************************
     C*
     C                   if        durate <> *blanks
     C                   eval      kkcpt4 = durate
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off
     C*** BEG CHG *** 08/09/07 *********************************************
     C**********         eval      camra = prmra
     C**********         if        durate >= '70000' and durate <= '89999'
     C**********         eval      camra = pr92n2
     C*** Beg Add *** 08/27/13 *********************************************
     C                   eval      Xx1a7 = pr1a7
     C*** End Add *** 08/27/13 *********************************************
     C*
     C* Find The Locality............
     C*
     C                   Exsr      HospMedicare
     C*** END CHG *** 08/09/07 *********************************************
     C*** BEG DEL *** 08/09/07 *********************************************
     C****************** endif
     C*** END DEL *** 08/09/07 *********************************************
     C                   endif
     C*
     C                   endif
     C*** END ADD *** 12/12/03 *********************************************
    C*
     C                   endif
    C*
     C*** BEG CHG *** 12/12/03 *********************************************
     C**** prstat        cabeq     'I'           nxtsv1
     C     svstat        cabeq     'I'           nxtsv1
     C*** END CHG *** 12/12/03 *********************************************
      /Free
       // Beg Add ***** 01/15/15 *******************************************
       If CAfdat >= 1150101 and EUicn >= 'HO' and EUicn <= 'HS';
         Exsr SROutpatient;
       Endif;
       // End Add ***** 01/15/15 *******************************************
      /End-free
    C*
     C     svcde         endsr
     C*** BEG ADD *** 08/09/07 *********************************************
     C     hospmedicare  begsr
     C*
     C* Find the locality
     C*
     C                   movel     eoflic        zzzip             5
     C                   move      zzzip         qqzip
     C                   eval      qqefdt = cafdat
     C     zrky          setgt     pzr01
     C     zrky1         readpe    pzr01                                  99
     C*
     C                   if        *in99 = *off
     C                   eval      xxloc = zrrgn#
     C                   else
     C                   eval      xxloc = 99
     C                   endif
     C*
     C                   SELECT
     C*
     C* Locality 99 (01/02)
     C*
     C                   when      xxloc = 99
     C*
     C                   eval      xxmramedTC = pr92n6
     C                   eval      xxmramed = pr92n4
     C*
     C* Locality 03
     C*
     C                   when      xxloc = 03
     C*
     C                   eval      xxmramedTC = pr92na
     C                   eval      xxmramed = pr92n8
     C*
     C* Locality 04
     C*
     C                   when      xxloc = 04
     C*
     C                   eval      xxmramedTC = pr92ne
     C                   eval      xxmramed = pr92nc
     C*
     C                   ENDSL
     C*
     C                   if        durate >= '70000' and durate <= '89999'
     C                   eval      camra = xxmramedTC
     C*** BEG ADD *** 09/27/07 *********************************************
     C*** BEG CHG *** 12/19/07 *********************************************
     C*
     C* Use the greater amount between the Tech Comp MRA (xxmramedTC)
     C* and the Locality Technical comp amount (pr92n2).
     C*
     C***                if        camra = 0
     C                   if        pr92n2 > xxmramedTC
     C*** END CHG *** 12/19/07 *********************************************
     C                   eval      camra = pr92n2
     C                   endif
     C*** END ADD *** 09/27/07 *********************************************
     C                   else
     C                   eval      camra = xxmramed
     C*** BEG ADD *** 08/09/07 *********************************************
     C*
     C* If there was no Medicare allowance then load the MRA
     C*
     C                   if        camra = 0
     C                   eval      camra = prmra
     C                   endif
     C*** END ADD *** 08/09/07 *********************************************
     C                   endif
     C*** Beg Add *** 07/13/16 *********************************************
     C                   if        xxMraMedTC <> 0
     C                   eval      CAtech = xxMraMedTC
     C                   else
     C                   eval      CAtech = PR92N2
     C                   endif
     C*** End Add *** 07/13/16 *********************************************
     C*
     C                   ENDSR                                                  HospMedicare
     C*** END ADD *** 08/09/07 *********************************************
      /Free
       // Beg Add ***** 01/15/15 *******************************************

       Begsr SROutpatient;
       //*** Beg Add *** 02/22/16 ****************************************
       //* Clear MRA if not PT/OT/Speech
       if (DUrvCd >= 420 and DUrvCd <= 429)
           or (DUrvCd >= 430 and DUrvCd <= 439)
           or (DUrvCd >= 440 and DUrvCd <= 449)
           or (DUrvCd >= 930 and DUrvCd <= 932);
       //*** Beg Chg *** 03/08/16 ****************************************
       //**or (DUrate >= '70010' and DUrate <= '89999' and xxSstay = 'Y'
       // Beg Chg ***** 05/19/16 *******************************************
       //****   or (DUrate >= '70010' and DUrate <= '89999' and EUicn = 'HS'
       // Beg Del ***** 07/13/16 *******************************************
       //*****  or (DUrate >= '70010' and DUrate <= '89999'
       // End Del ***** 07/13/16 *******************************************
       // End Chg ***** 05/19/16 *******************************************
       //*** End Chg *** 03/08/16 ****************************************
       // Beg Del ***** 07/13/16 *******************************************
       //***and EUschd = 'S');
       // End Del ***** 07/13/16 *******************************************
       Else;
         CAmra = 0;
       EndIf;
       //*** End Add *** 02/22/16 *****************DUrvCd <=**************
       //*** Beg Add *** 02/20/15 ****************************************
       // Beg Chg ***** 05/19/16 *******************************************
       //*** if EUSchd = 'S';
        if EUSchd = 'S' and EUicn = 'HO';
       // End Chg ***** 05/19/16 *******************************************
          CAHtos = 'HNN';
        EndIf;
       // Beg Chg ***** 05/19/16 *******************************************
       //*** if EUSchd = 'U';
        if EUSchd = 'U' and EUicn = 'HO';
       // End Chg ***** 05/19/16 *******************************************
          CAHtos = 'HNE';
        EndIf;
       //*** End Add *** 02/20/15 ****************************************
       // Beg Add ***** 05/19/16 *******************************************
        if EUSchd = 'S' and EUicn = 'HS';
          CAHtos = 'HSN';
        EndIf;
        if EUSchd = 'U' and EUicn = 'HS';
          CAHtos = 'HSE';
        EndIf;
       // End Add ***** 05/19/16 *******************************************
        kkcpt4 = DUrate;
        kkefdt = CAfDat;

        Setgt keyFmprpFL Ppr02;
        Readpe keyFmprpFL1 Ppr02;
          If not %Eof(fmprlfl);

       // Beg Chg ***** 05/19/16 *******************************************
       //***If EUSCHD = 'U';
            If EUicn = 'HO';
       // End Chg ***** 05/19/16 *******************************************
               CAsvRate = PR92nJ;
               CAsrRate = *Zeros;
            Else;
               CAsrRate = PR92nK;
               CAsvRate = *Zeros;
            EndIf;

       // Beg Add ***** 05/05/15 *******************************************
       //** Only load MRA if this code is in Appendix A or B with a rate
       //** and its not rad/path/lab/PT/OT
       // Beg Chg ***** 06/10/16 *******************************************
       //******If  PR92nj <> 0 or PR92nk <> 0;
           If  (PR92nj <> 0 or PR92nk <> 0) and (camra = 0);
       // End Chg ***** 06/10/16 *******************************************
       // End Add ***** 05/05/15 *******************************************
            If CAsvRate <> *Zero;
              CAmra = CAsvRate * EOctyMod;
            Else;
              CAmra = CAsrRate * EOctyMod;
            Endif;
       // Beg Add ***** 05/05/15 *******************************************
           Endif;
       // End Add ***** 05/05/15 *******************************************

          Endif;
       //*** Beg Add *** 07/13/16 ***************************************
          xxSetCAamt = 'N';
          if CAmra = 0;
             xxSetCAamt = 'Y';
          Endif;
       //*** End Add *** 07/13/16 ***************************************

       Endsr;

       // End Add ***** 01/15/15 *******************************************
      /End-Free
    C* ==================================================================
     Opca01     e            rca01
