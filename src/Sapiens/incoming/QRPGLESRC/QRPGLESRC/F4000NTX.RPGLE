     H option(*nodebugio:*srcstmt)                                                                 *
     H dftactgrp(*no) actgrp(*caller)                                                              *
    H**********************************************************************
     H*Copyright('(c) 2023 Sapiens. All Rights Reserved.')
    H**********************************************************************
    H* PROGRAM: F4000NTX
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR TX
    H**********************************************************************
    H*  CREATED: 02/11/00  C:79   SUPPORT: MC048/#21799
    H**********************************************************************
    H*  REVISIONS:
    H*                                                                    *
     H*  07/18/23 C:342 Project: NY060                                     *
    H*                 NY Compliance: CARC/RARC Update                    *
    H*                                                                    *
     H*  06/21/18 C:079 Project: TX101                                     *
    H*                 Call FA763R to get allowances, desc, indicators,etc*
    H*                 directly from the Medicare files                   *
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
     H*
     H*  02/16/17  C:079  45755/025
     H*                   Pay Status ind. A, Q4, and Y per OPPS guideline
     H*
     H*  06/10/16  C:079  Project: FS497
     H*                   Pay Status indicator Q4 at CL Lab FS
     H*
     H*  05/30/16  C:309  Project: FS497
     H*                   Swap out APSI for APCSI
     H*
     H*  04/06/16 C:309 Support 40307
     H*                 Lab fee calculation without L1 modifier
     H*                 calculate MRA using outlier, add outlier
     H*                 (xxOutl) only if APrel > 0
     H*
     H*  10/08/12 C:284 Support 80621
     H*                 Changed condition to handle non device intensive procedure
     H*                 when implant were used as 'P' as well.
     H*
     H*  09/18/12 C:284 Project TX079
     H*                 Modifying program to handle processing zip codes using
     H*                 the Zip5 file and the new zip + 4 file.
     H*
     H*  02/25/12 C:251 Project FS404
     H*                 Medicare 2012 Q1 Update . Use straight multipliers instead
     H*                 of dividing by 100
     H*
     H*  09/02/11 C:274 Project FS392
     H*                 Add processing for Medicare Ambulance.
     H*
     H*  03/08/11 C:251 Support: 76074  MC144
     H*                 FA756R param value changed for correct drg 100
     H*                 payment for inpatient bills.
     H*
     H*  02/18/11 C:227 Support: 74272  MC144
     H*                 Reimbursement for Outpatient Medical Rehab.
     H*                 CARF and Non-CARF accredited.
     H*
     H*  12/07/10 C:079 Project: 114MWS
     H*                 Add parms when calling FA756R
     H*
     H*  11/18/10 C:227 Project: FS366
     H*                 Use MC to get conversion factor
     H*
     H*  11/11/10 C:220 Support: 74853                                                     *
     H*                 Move TIME before DSDATE                             file FTOPP.    *
     H*                                                                                    *
     H*  09/06/10 C:220 Project: FS365                                                     *
     H*                 Don't use OPWIND for the wage index for discharge   file FTOPP.    *
     H*                 date on or after 10/1/2010                                         *
     H*                                                                                    *
     H*  02/16/10 C:220 Project FS363
     H*                 Need to load the reduced conversion factor - RTAPC1
     H*
     H*  01/19/10 C:079 Project FS362
     H*                 If rate from FMDMPTX = 0 then load from FMPRPTX     o
     H*
     H*  11/23/09 C:198 Support 70629
     H*                 CAFSR is not being set to Y when prfsr is loaded into
     H*                 cafsr.
     H*
     H*  10/26/09 C:227 Support 70331
     H*                 ASC Billing issue
     H*
     H*  10/06/09 C:198 Support 70084
     H*                 Endoscopic rules are not being applied with place of service
     H*                 = 24 because the rules have been set up to exclude ASC bills.
     H*
     H*  08/12/09 C:079 Project FS348
     H*                 Handle payment indicator = Y same as A
     H*
     H*  08/12/09 C:227 Project FS348
     H*                 Mid-Year CMS DMEPOS Code/Rate Update
     H*
     H*  06/30/09 C:227 Project TX063
     H*                 Changes for ASC code
     H*
     H*  07/09/09 C:079 Support: 68838
    H*                 Load rule date correctly
     H*
     H*  06/03/09 C:227 Support: 68667
    H*                 Procedure Code 64640 not paying.
     H*
     H*  05/20/09 C:198 Project: FS343
     H*                 Re-compiled only as there is change in
     H*                 structure of FTOPP.
     H*
     H*  04/08/09 C:198 Project: TX043 P# 36
    H*                 incorporated functionality where charged amount <
     H*                 ASC amount, system should pay ASC_MRA.             *
     H*                                                                    *
     H*  03/18/09 C:198 Project: TX051
    H*                 Include processing in RECU                         *
     H*                                                                    *
     H*  03/16/09 C:198 Project: TX043 P# 33                               *
    H*                 Include processing for no match in FTWGP           *
     H*                                                                    *
     H*  03/11/09 C:062 Project: 67251                                     *
    H*                 Prevent overflow of amounts                        *
     H*                                                                    *
     H*  02/16/09 C:079 Project: TX055                                     *
    H*                 Include proceddsing for DMEPOS modifier RR         *
     H*                                                                    *
     H*  02/16/09 C:079 Project: TX057                                     *
     H*                 Load ASC Payment Indicator Correctly               *
     H*                                                                    *
     H*  02/06/09 C:079 Project: TX043                                     *
     H*                 Added processing for type of service               *
     H*                                                                    *
     H*  02/03/09 C:212 Project: TX055                                     *
    H*                 Include proceddsing for DMEPOS modifiers.          *
     H*                                                                    *
     H*  01/15/09 C:189 Proj : FS329                                       *
     H*                 CMS IPPS/OPPS Update                               *
    H*                                                                    *
     H*  12/19/08 C:198 SUPPORT: 66216                                     *
     H*                 use apalow in apc payment when wage index is zero. *
     H*                                                                    *
     H*  11/14/08 C:212 Project: TX043                                     *
     H*                 Added processing for specialty code ASC            *
    H*                                                                    *
     H*  10/14/08 C:079 SUPPORT: 65280                                     *
     H*                 Load Modifiers for Hospital Bills                  *
     H*                 Load FACAP correctly when paying > charged         *
     H*                 Fix outlier calculations                           *
    H*                                                                    *
     H*  10/02/08 C:079 PROJECT: TX039                                     *
     H*                 When implant status = I multiply by 200%           *
    H*                                                                    *
     H*  09/15/08 C:198 Support: 64232                                     *
     H*                 Corrected SG modifier Processing.                  *
    H*                                                                    *
     H*  08/12/08 C:198 PROJECT: TX039                                     *
     H*                 Replaced FTOIP with FTOPP.                         *
    H*                                                                    *
     H*  08/06/08 C:198 PROJECT: TX039                                     *
     H*                 Change processing where apsi = 'A' or code is not  *
     H*                 in APC file.                                       *
    H*                                                                    *
     H*  07/31/08 C:198 PROJECT: TX039                                     *
     H*                 Change outlier Processing                          *
    H*                                                                    *
     H*  07/11/08 C:079 PROJECT: TX039                                     *
     H*                 Load Hospital Type of Service                     *
    H*                                                                    *
     H*  07/04/08 C:189 SUPPORT                                            *
     H*                 Program going in endless loop                     *
    H*                                                                    *
     H*  06/10/08 C:198 PROJECT TX040                                      *
     H*                 Added Home health code file on pressing F10 at     *
     H*                 HCPC code.                                         *
     H*                                                                    *
     H*   04/16/08 C:198 Project: TX040                                    *
     H*                  to have underserved area payment.                 *
     H*                                                                    *
     H*   04/07/08 C:189 Project: TX039                                    *
     H*                  Add IPPS Processing.                              *
    H*                                                                    *
     H*   04/04/08 C:062 Project: TX040                                    *
     H*                  Add Home Health Processing.                       *
    H*                                                                    *
     H*   04/03/08 C:198 Project: TX039                                    *
     H*                  Add OPPS Processing.                              *
    H*                                                                    *
     H*   04/02/08 C:198 Project: TX040                                    *
     H*                  to have underserved area payment.                 *
     H*                                                                    *
     H*   11/06/07 C:198 Project: FS308                                    *
     H*                  Loaded lowest quartile/51+ miles (CALQ51)         *
     H*                                                                    *
     H*  05/03/07 C:079 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
    H*                                                                    *
     H*  03/01/07 C:079 Support: MC025 57405                               *
    H*                 Prevent infinite looping when reading through the  *
    H*                 Drug code file                                     *
    H*                                                                    *
     H*  01/25/07 C:118 Support: MC099 56856                               *
    H*                 TX pharmacy bills not recognizing the difference   *
     H*                 between generic and brand name drugs.              *
    H*                                                                    *
     H*  04/06/06 C:79  Support: MC025 52057                               *
    H*                 Load RTW Allowances based on modifier billed       *
    H*                                                                    *
     H*  03/29/06 C:79  Support: MC025 51991                               *
    H*                 Add mod AQ for HPSA Incentive                      *
    H*                                                                    *
     H*  03/21/06 C:79  Support: MC025 51959                               *
    H*                 99455 and 99456 are HPSA Exempt                    *
    H*                                                                    *
     H*  07/19/05 C:79  Support: 48114                                     *
    H*                 If PCTC indicator is 1 but mod TC is billed then   *
    H*                 its not HPSA eligible                              *
    H*                                                                    *
     H*  06/09/05 C:79  Support: 47464                                     *
    H*                 If POS is on the CMS list of Non-fac then pay that *
    H*                                                                    *
     H*  05/21/05 C:79  Support: 46226                                     *
    H*                 If zip code is in a HPSA area provider is eligible *
    H*                                                                    *
     H*  09/28/04 C:79  Project: TX026                                     *
    H*                 Load ASC rate when mod SG is billed                *
    H*                                                                    *
     H*  09/25/04 C:79  Project: TX027                                     *
    H*                 Set HPSA flag for certain license numbers          *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
    H*
    H*  09/25/03 C:79  Project: TX020
    H*                 1. Populate rural/urban flag for ambulance mileage
    H*                 2. Populate modifier indicators
    H*
    H*  06/27/03 C:79  Project: TX020
    H*                 1. Implementing new Fee Schedule
    H*                 2. For Dental Codes load CATECH with PR92N1
    H*
    H*  10/17/02 C:79  Project: FS239
    H*                 Load Pharmacy allowance into CAAWP and CAMRA
    H*
    H*  10/02/02 C:79  Support: MC010 33679
    H*                 Populate specialty code at the detail level not
     H*
     H*  08/23/02 C:79  Project: TX014
     H*                 Do not implement new FS. Judge set a temp injunct.
     H*                 against it.  Set eff date to 9999999.
     H*
     H*  08/22/02 C:79  Support: MC025 33965
     H*                 Process UCR for Dental Bills.
     H*
     H*  08/20/02 C:79  Support: MC025 33914
     H*                 Set FACAP(CAFSR) for UCR codes in RECU subroutine
     H*
     H*  08/13/02 C:79  Support: MC010 33679
     H*                 Clear rcd fmt PCAWK2 after writing
     H*
     H*  08/05/02 C:79  Project: TX014
     H*                 Add locality logic for DOS >= 9/1/2002.
     H*                 Use PR1A6 for separate Procedure flag
     H*
     H*  05/30/02 C:79  Support: MC025 32721
     H*                 Use 9.5 holding field for AWP/1000 calc.
     H*
     H*  03/28/02 C:79  Support: MC048 31825
     H*                 Divide UCR mod 26,27 values by 100 to pop in 3.2
     H*                 field.
     H*
     H*  02/28/02 C:79  Project: TX016
     H*                 Use correct calcs for Pharmacy
     H*
     H*  05/30/01 C:62  Project: FS228
     H*                 GET ZIPA FROM CAZIPA NOT FROM PROVIDER FILE.
     H*
     H*  03/28/01 C:62  Project: UC002
     H*                 1. Add logic to populate Prof/Tech components
    H*                                                               *
    H*  11/01/00 C:79 PROJECT:TXMNT/#24805                           *
    H*                1. Load tech component from PR92N1 not PR92N2  *
    H*                2. Populate CABUND from PR1A3                  *
    H*                3. Load no-cut flag from PR1AF                 *
    H*                                                               *
    H*  05/10/00 C:79 PROJECT:TX009                                  *
    H*                1. Load MRA,Prof/Tech components based on      *
    H*                   modifiers 22,52,WP                          *
    H*                                                               *
    H*****************************************************************
    H*  INDICATOR USAGE:
    H*****************************************************************
    F*** Beg Chg *** 09/02/11 ****************************************
     F*****fdcal13   uf   e           k disk
    Ffdcal13   uf   e           k disk    infds(cafds)
    F*** End Chg *** 09/02/11 ****************************************
     F*** BEG CHG *** 09/06/10 *********************************************
     F*** faeolg    if   e           k disk
     Ffaeolg    uf   e           k disk    usropn
     F*** END CHG *** 09/06/10 *********************************************
     F*** BEG ADD *** 04/07/08 *********************************************
     F                                     infds(dbfds)
     F*** END ADD *** 04/07/08 *********************************************
     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
     F*** BEGIN DEL *** 05/30/01 *******************************************
     F**********fmp1l     if   e           k disk
     F**********fmgzl     if   e           k disk
     F*** END   DEL *** 05/30/01 *******************************************
    F*
     F*** BEG CHG *** 08/05/02 *********************************************
     F***** fmprl     if   e           k disk
     Ffmprltx   if   e           k disk
     F*** END CHG *** 08/05/02 *********************************************
    F*** BEG ADD *** 02/03/09 ****************************************
     Ffmdmltx   if   e           k disk
    F*** END ADD *** 02/03/09 ****************************************
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
     F*** BEG ADD *** 08/05/02 *********************************************
     Ffmzrltx   if   e           k disk
     Ffmp1l     if   e           k disk
     F*** END ADD *** 08/05/02 *********************************************
     F*** BEG ADD *** 09/25/04 *********************************************
     Ffaehl     if   e           k disk
     F*** END ADD *** 09/25/04 *********************************************
     F*** BEG ADD *** 09/28/04 *********************************************
     Ffaagltx   if   e           k disk
     Fftccl1    if   e           k disk
     F*** END ADD *** 09/28/04 *********************************************
     F*** BEG ADD *** 05/21/05 *********************************************
     Ffthel1tx  if   e           k disk
     F*** END ADD *** 05/21/05 *********************************************
     F*** BEG ADD *** 05/12/08 *********************************************
     Ffthel2tx  if   e           k disk    rename(phe01:phe02)
     F*** END ADD *** 05/12/08 *********************************************
     F*** BEG ADD *** 04/02/08 *********************************************
     Fftualtx   if   e           k disk
     F*** END ADD *** 04/02/08 *********************************************
     F*** BEG ADD *** 04/03/08 *********************************************
     F*** BEG CHG *** 08/12/08 *********************************************
     F*******ftoil     if   e           k disk
     Fftopl     if   e           k disk
     F*** END CHG *** 08/12/08 *********************************************
     Fftapl     if   e           k disk
     F*** END ADD *** 04/03/08 *********************************************
     F*** BEG ADD *** 04/04/08 *********************************************
     Ffaeul     if   e           k disk
     F*** BEG ADD *** 04/07/08 *********************************************
     F*** BEG ADD *** 11/14/08 *********************************************
     Ffmasl1    if   e           k disk
     Fftwgl1    if   e           k disk
     F*** END ADD *** 11/14/08 *********************************************
     F*** BEG ADD *** 03/16/09 *********************************************
     Fftwgl2    if   e           k disk    rename(pwg01:pwg02)
     Fststl1    if   e           k disk
     F*** END ADD *** 03/16/09 *********************************************
      *** BEG ADD ***** 01/15/09 ******************************************
     Fftrtl2    IF   E           K DISK
      *** END ADD ***** 01/15/09 ******************************************
     F*** BEG ADD ***** 09/06/10 ******************************************
     F* Add wage table files
     FFTO2L1    IF   E           K DISK
     FFTOAL1    IF   E           K DISK
     FFTOBL1    IF   E           K DISK
     F*** END ADD ***** 09/06/10 ******************************************
     F*** Beg Add *** 09/18/12 *************************************************
     FFTZPL1    IF   E           K DISK
     F*** End Add *** 09/18/12 *************************************************
     d*** Beg Add *** 06/21/18 ********************************************
     d*********************************************************************
     d* Proto-types
     d*********************************************************************
     d* Texas Medicare Physician API
     d TxMedPhyApi     pr                  ExtPgm('FA763RTX')
     d   ##Rrn                        9  0
     d   ##Desc                      25
     d   ##PrCd                       5
     d   ##Mod1                       2
     d   ##EfDt                       7  0
     d   ##Loc9NonF                   9  2
     d   ##Loc9Facil                  9  2
     d   ##Loc9Prof                   9  2
     d   ##Loc9Tech                   9  2
     d   ##Loc9Anes                   9  2
     d   ##Loc11NonF                  9  2
     d   ##Loc11Facil                 9  2
     d   ##Loc11Prof                  9  2
     d   ##Loc11Tech                  9  2
     d   ##Loc11Anes                  9  2
     d   ##Loc15NonF                  9  2
     d   ##Loc15Facil                 9  2
     d   ##Loc15Prof                  9  2
     d   ##Loc15Tech                  9  2
     d   ##Loc15Anes                  9  2
     d   ##Loc18NonF                  9  2
     d   ##Loc18Facil                 9  2
     d   ##Loc18Prof                  9  2
     d   ##Loc18Tech                  9  2
     d   ##Loc18Anes                  9  2
     d   ##Loc20NonF                  9  2
     d   ##Loc20Facil                 9  2
     d   ##Loc20Prof                  9  2
     d   ##Loc20Tech                  9  2
     d   ##Loc20Anes                  9  2
     d   ##Loc28NonF                  9  2
     d   ##Loc28Facil                 9  2
     d   ##Loc28Prof                  9  2
     d   ##Loc28Tech                  9  2
     d   ##Loc28Anes                  9  2
     d   ##Loc31NonF                  9  2
     d   ##Loc31Facil                 9  2
     d   ##Loc31Prof                  9  2
     d   ##Loc31Tech                  9  2
     d   ##Loc31Anes                  9  2
     d   ##Loc99NonF                  9  2
     d   ##Loc99Facil                 9  2
     d   ##Loc99Prof                  9  2
     d   ##Loc99Tech                  9  2
     d   ##Loc99Anes                  9  2
     d   ##Loc9Used                   9  2
     d   ##Loc11Used                  9  2
     d   ##Loc15Used                  9  2
     d   ##Loc18Used                  9  2
     d   ##Loc20Used                  9  2
     d   ##Loc28Used                  9  2
     d   ##Loc31Used                  9  2
     d   ##Loc99Used                  9  2
     d   ##PcTc                       1
     d   ##Glob                       3
     d   ##PreOp                      5  2
     d   ##IntraOp                    5  2
     d   ##PostOp                     5  2
     d   ##MultSurg                   1
     d   ##Bilat                      1
     d   ##AsstSurg                   1
     d   ##CoSurg                     1
     d   ##TeamSurg                   1
     d   ##EndoBase                   5
     d   ##StatCd                     1
     d   ##Rrn         s              9  0
     d   ##Desc        s             25
     d   ##PrCd        s              5
     d   ##Mod1        s              2
     d   ##EfDt        s              7  0
     d   ##Loc9NonF    s              9  2
     d   ##Loc9Facil   s              9  2
     d   ##Loc9Prof    s              9  2
     d   ##Loc9Tech    s              9  2
     d   ##Loc9Anes    s              9  2
     d   ##Loc11NonF   s              9  2
     d   ##Loc11Facil  s              9  2
     d   ##Loc11Prof   s              9  2
     d   ##Loc11Tech   s              9  2
     d   ##Loc11Anes   s              9  2
     d   ##Loc15NonF   s              9  2
     d   ##Loc15Facil  s              9  2
     d   ##Loc15Prof   s              9  2
     d   ##Loc15Tech   s              9  2
     d   ##Loc15Anes   s              9  2
     d   ##Loc18NonF   s              9  2
     d   ##Loc18Facil  s              9  2
     d   ##Loc18Prof   s              9  2
     d   ##Loc18Tech   s              9  2
     d   ##Loc18Anes   s              9  2
     d   ##Loc20NonF   s              9  2
     d   ##Loc20Facil  s              9  2
     d   ##Loc20Prof   s              9  2
     d   ##Loc20Tech   s              9  2
     d   ##Loc20Anes   s              9  2
     d   ##Loc28NonF   s              9  2
     d   ##Loc28Facil  s              9  2
     d   ##Loc28Prof   s              9  2
     d   ##Loc28Tech   s              9  2
     d   ##Loc28Anes   s              9  2
     d   ##Loc31NonF   s              9  2
     d   ##Loc31Facil  s              9  2
     d   ##Loc31Prof   s              9  2
     d   ##Loc31Tech   s              9  2
     d   ##Loc31Anes   s              9  2
     d   ##Loc99NonF   s              9  2
     d   ##Loc99Facil  s              9  2
     d   ##Loc99Prof   s              9  2
     d   ##Loc99Tech   s              9  2
     d   ##Loc99Anes   s              9  2
     d   ##Loc9Used    s              9  2
     d   ##Loc11Used   s              9  2
     d   ##Loc15Used   s              9  2
     d   ##Loc18Used   s              9  2
     d   ##Loc20Used   s              9  2
     d   ##Loc28Used   s              9  2
     d   ##Loc31Used   s              9  2
     d   ##Loc99Used   s              9  2
     d   ##PcTc        s              1
     d   ##Glob        s              3
     d   ##PreOp       s              5  2
     d   ##IntraOp     s              5  2
     d   ##PostOp      s              5  2
     d   ##MultSurg    s              1
     d   ##Bilat       s              1
     d   ##AsstSurg    s              1
     d   ##CoSurg      s              1
     d   ##TeamSurg    s              1
     d   ##EndoBase    s              5
     d   ##StatCd      s              1
     d*** End Add *** 06/21/18 ********************************************
     D dbfds           ds
     D dbrrn                 397    400b 0
     D*** END ADD *** 04/07/08 *********************************************
     D*** END ADD *** 04/04/08 *********************************************
    D*** Beg Add *** 09/02/11 ****************************************
    Dcafds            ds
    Dcarrn                  397    400b 0
    D*** End ADD *** 09/02/11 ****************************************
    F*
    D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D*** BEG ADD *** 08/05/02 *********************************************
     D xxzip           s                   like(zrfzcd)
     D xxloc#          s                   like(zrrgn#)
     D*** END ADD *** 08/05/02 *********************************************
    D*** BEG ADD *** 02/03/09 ****************************************
     D i               s              1  0
     D j               s              1  0
     D k               s              1  0
     D xxdme           s              1    inz('N')
    D*** END ADD *** 02/03/09 ****************************************
     d*** Beg Add *** 06/21/18 ****************************************
     d xxRgn#          s                   Like(ZRrgn#)
     d xxIndex         s              1  0
     d*** End Add *** 06/21/18 ****************************************
     d*** Beg Add *** 02/18/20 ****************************************
     d isMC433         s              1n   Inz(*off)
     d*** End Add *** 02/18/20 ****************************************
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
     D*** BEG ADD *** 09/25/03 *********************************************
     D xxmd62          s              4  3
     D*** END ADD *** 09/25/03 *********************************************
     D*** BEG DEL *** 08/05/02 *********************************************
     D********* camd81          s              3  2
     D********* camd82          s              3  2
     D*** END DEL *** 08/05/02 *********************************************
     D*** BEG ADD *** 08/05/02 *********************************************
     D camd85          s              3  2
     D camd65          s              3  2
     D*** END ADD *** 08/05/02 *********************************************
     D catype          s              1
     D hlddat          s              7  0
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D*** BEG ADD *** 05/12/08 *********************************************
     D k1efdt          s                   like(frefdt)
     D*** END ADD *** 05/12/08 *********************************************
     D*** BEG ADD *** 04/07/08 *********************************************
     D kkefdt#         s                   like(frefdt)
     D*** END ADD *** 04/07/08 *********************************************
     D kkymd           s                   like(frefdt)
     D*** BEG ADD *** 09/28/04 *********************************************
     D kkrgn           s                   like(agrgn)
     D kkgrp           s                   like(aggrp)
     D kkkey           s                   like(cckey)
     D kkco#           s                   like(ccco#)
     D kkcnty          s              3
     D kkfein          s                   like(eofein)
     D kkmod           s                   like(eomod)
     D*** END ADD *** 09/28/04 *********************************************
     D*** BEG ADD *** 04/03/08 *********************************************
     D*** BEG DEL *** 08/12/08 *********************************************
     D*kkprov****      s                   like(oiprov)
     D*** END DEL *** 08/12/08 *********************************************
     D kkhcpc          s                   like(aphcpc)
     D*** END ADD *** 04/03/08 *********************************************
     D*** BEG ADD *** 05/21/05 *********************************************
     D kkzip           s              5  0
     D kktype          s              1
     D*** END ADD *** 05/21/05 *********************************************
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
     D xxdcs#          s              5  0
     D*** BEGIN ADD *** 03/28/01 **************************************
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D*** BEGIN ADD *** 03/28/01 **************************************
     D*** BEGIN DEL *** 05/30/01 *******************************************
     D********** xxfrzp          s                   like(gzfrzp)
     D*** END   DEL *** 05/30/01 *******************************************
     D xxinvl          s              1
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D*** BEG ADD *** 05/30/02 *********************************************
      *** Begin Change *** 05/02/18 *********************
     D****xxawp           s              9  5
     D xxawp           s                   like(CAawp)
      *** End   Change *** 05/02/18 *********************
     D*** END ADD *** 05/30/02 *********************************************
     D*** BEGIN DEL *** 05/30/01 *******************************************
     D********** xxzipa          s                   like(gzzipa)
     D*** END   DEL *** 05/30/01 *******************************************
     D*** BEG ADD *** 09/25/04 *********************************************
     D xxpref          s              2
     D xxpref3         s              3
     D*** END ADD *** 09/25/04 *********************************************
     D*** BEG ADD *** 06/09/05 *********************************************
     D xxfac           s              1
     D*** END ADD *** 06/09/05 *********************************************
     D*** BEGIN ADD *** 04/04/08 *******************************************
     D homeHealthHit   s               n
     D*** END   ADD *** 04/04/08 *******************************************
     D*** BEG ADD *** 04/03/08 *******************************************
     D*** BEG CHG *** 01/15/09 *******************************************
     D************xxcf             S              5  3 inz(63.694)
     Dxxcf             S              5  3
     D*** END CHG *** 01/15/09 *******************************************
     D xxoutl          s              9  2
     D*** END ADD *** 04/03/08 *******************************************
     D*** Beg Add *** 09/18/12 *************************************************
     DKkst             s                   like(Zpst)
     DKkpls4           s                   like(Zppls4)
     D*** End Add *** 09/18/12 *************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    d*** Beg Add ****** 06/21/18 **************************
      /Copy sPrModule
    d*** End Add ****** 06/21/18 **************************
    C*
     C     caky1         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*** BEG ADD *** 04/04/08 *********************************************
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*** END ADD *** 04/04/08 *********************************************
     C*** BEGIN DEL *** 05/30/01 *******************************************
    C*
    C* Reasonable & Customary File
     C*****gzky*         klist
     C**********         kfld                    qqco#
     C**********         kfld                    xxfrzp
     C*** END   DEL *** 05/30/01 *******************************************
    C*
    C*  Procedure Code file
     C     prky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    cafdat
    C*
     C*** BEG ADD *** 08/06/08 *******************************************
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C*** END ADD *** 08/06/08 *******************************************
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
     C*** BEGIN CHG *** 03/28/01 *******************************************
     C**********         kfld                    kkymd
     C                   kfld                    kkefdt
     C*** END   CHG *** 03/28/01 *******************************************
     C*** BEGIN ADD *** 03/28/01 **************************************
     C*
     C*  Usual and Customary Modifier Percent File
     C*
     C     umky          klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C                   kfld                    kkefdt
     C*** END   ADD *** 03/28/01 **************************************
     C*** BEGIN DEL *** 05/30/01 *******************************************
    C*
    C*  Provider Master File
     C*****p1key         klist
     C**********         kfld                    qqfein
     C**********         kfld                    qqmod
     C*** END   DEL *** 05/30/01 *******************************************
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
     C*** BEG ADD *** 08/05/02 *********************************************
     C*
     C*  State Zip Code/Region File
     C*
     C     zrky          klist
     C                   kfld                    qqco#
     C                   kfld                    xxzip
     C                   kfld                    kkefdt
     C*** END ADD *** 08/05/02 *********************************************
     C*** BEG ADD *** 09/25/04 *********************************************
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
     C*** END ADD *** 09/25/04 *********************************************
     C*** BEG ADD *** 09/28/04 *********************************************
     C*
     C* ASC rates by loc/group
     C*
     C     agky          klist
     C                   kfld                    kkgrp
     C                   kfld                    kkrgn
     C                   kfld                    kkefdt
     C*
     C     agky1         klist
     C                   kfld                    kkgrp
     C*
     C     ccky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     C*
     C     p1ky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C*** END ADD *** 09/28/04 *********************************************
     C*** BEG ADD *** 05/21/05 *********************************************
     C*
     C     heky          klist
     C                   kfld                    kkzip
     C                   kfld                    kktype
     C*** BEG CHG *** 05/12/08 *********************************************
     C**********         kfld                    kkefdt
     C                   kfld                    k1efdt
     C*** END CHG *** 05/12/08 *********************************************
     C*
     C     heky1         klist
     C                   kfld                    kkzip
     C                   kfld                    kktype
     C*** END ADD *** 05/21/05 *********************************************
     C*** BEG ADD *** 05/12/08 *********************************************
     C     heky2         klist
     C                   kfld                    kkefdt
     C*** END ADD *** 05/12/08 *********************************************
     C*** BEG ADD *** 04/02/08 *********************************************
     C     uaky          klist
     C                   kfld                    kkzip
     C*** END ADD *** 04/02/08 *********************************************
     C*** BEG ADD *** 04/03/08 *********************************************
     C*** BEG CHG *** 08/12/08 *********************************************
     C*****oiky1         klist
     C*****              kfld                    kkprov
     C*****              kfld                    kkefdt
     C*****oiky2         klist
     C*****              kfld                    kkprov
     C     opky1         klist
     C                   kfld                    kpprov            6
     C                   kfld                    kpefdt            8 0
     C     opky2         klist
     C                   kfld                    kpprov
     C*** END CHG *** 08/12/08 *********************************************
     C     apky1         klist
     C                   kfld                    kkhcpc
     C                   kfld                    kkefdt
     C     apky2         klist
     C                   kfld                    kkhcpc
     C*** END ADD *** 04/03/08 *********************************************
     D*** BEG ADD *** 04/07/08 *********************************************
     C     frky#         klist
     C                   kfld                    qqst
     C                   kfld                    catype
     C                   kfld                    kkefdt#
     D*** END ADD *** 04/07/08 *********************************************
     C*** BEG ADD *** 11/14/08 *********************************************
     C*
     C     asky          klist
     C                   kfld                    ##prcd            5
     C                   kfld                    ##efdt            7 0
     C*
     C     asky1         klist
     C                   kfld                    ##prcd
     C*
     C*    ehky          klist
     C*                  kfld                    ##CO#
     C*                  kfld                    ##SUB#
     C*                  kfld                    ##FND
     C*                  kfld                    ##FYR
     C*                  kfld                    ##CASE
     C*                  kfld                    ##EOB#
     C*
     C     wgky          klist
     C                   kfld                    ##cbsa            5
     C                   kfld                    kkefdt
     C*
     C     wgky1         klist
     C                   kfld                    ##cbsa
     C*
     C*** END ADD *** 11/14/08 *********************************************
     C*** BEG ADD *** 03/16/09 *********************************************
     C     wgky2         klist
     C                   kfld                    ##nam            40
     C                   kfld                    kkefdt
     C*
     C     wgky21        klist
     C                   kfld                    ##nam
     C*
     C     stky          klist
     C                   kfld                    ##key            10
     C*
     C*** END ADD *** 03/16/09 *********************************************
      *** BEG ADD ***** 01/15/09 ******************************************
     C*
     C     RTKY          KLIST
     C                   KFLD                    @@st              2
     C                   KFLD                    @@type            1
     C                   KFLD                    @@bgdt
     C*
     C*
     C     RTKY1         KLIST
     C                   KFLD                    @@st              2
     C                   KFLD                    @@type            1
     C*
      *** END ADD ***** 01/15/09 ******************************************
    C*** BEG ADD *** 02/03/09 ****************************************
     C     dmky2         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
     C                   kfld                    xxefdt            7 0
     C                   kfld                    xxmod1            2
     C                   kfld                    xxmod2            2
    C*** END ADD *** 02/03/09 ****************************************
     C*** BEG ADD ***** 09/06/10 ******************************************
     C*
     C     l2ky          klist
     C                   kfld                    xxprov
     C                   kfld                    @@efdt
     C*
     C     l2ky1         klist
     C                   kfld                    xxprov
     C*
     C     l4ky          klist
     C                   kfld                    opacbs
     C                   kfld                    @@efdt            7 0
     C*
     C     l4ky1         klist
     C                   kfld                    opacbs
      *
      *** END ADD ***** 09/06/10 ******************************************
     C*** Beg Add *** 09/18/12 *************************************************
     C     keyFtzpl1     klist
     C                   kfld                    Kkst
     C                   kfld                    Kkzip
     C                   Kfld                    Kkpls4
     C                   Kfld                    Kkefdt
     C*** End Add *** 09/18/12 *************************************************
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
     C*** BEG ADD *** 08/12/08 *************************************
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C*** END ADD *** 08/12/08 *************************************
     C*** BEG ADD *** 09/06/10 *************************************
     C     dtcymd2       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C
     C*** END ADD *** 09/06/10 *************************************
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
    C*
     C*** BEG ADD *** 04/07/08 *********************************************
     C                   if        eorcn = 0
     C                   if        eobtyp = 'H' and euaddt >=  1080301
    C                   if        euicn = 'IM' or euicn = 'IP' or euicn = 'IR'
    C                             or euicn = 'IS'
    C*
     C** BEG ADD *** 09/06/10 **********************************************
     C                   close     faeolg
     C** END ADD *** 09/06/10 **********************************************
     C                   Movel     dbrrn         @@rrn             9 0
    C*
     C                   Call      'FA756R'
     C                   parm                    @@rrn             9 0           RRN of FAEOP
     C                   parm                    @@rtcd            1             RETURN CODE
     C                   parm                    @@allow           9 2           ALLOWNECE
     C*** BEG ADD *** 12/07/10 *********************************************
     C*** BEG CHG *** 03/08/11 *********************************************
     C********           parm      'F'           @@call            1
     C                   parm      ' '           @@call            1
     C*** END CHG *** 03/08/11 *********************************************
     C**** Beg Chg *** 07/18/23 **************************************
     C****               parm      '   '         @@eob             3
     C                   parm      '   '         @@eob             6
     C**** End Chg *** 07/18/23 **************************************
     C                   parm      ' '           @@msg             7
     C                   PARM      ' '           @@code            3
     C*** END ADD *** 12/07/10 *********************************************
    C*
     C                   endif
     C                   endif
     C                   endif
     C*** END ADD *** 04/07/08 *********************************************
     C*
     C*** BEG ADD *** 09/06/10 *********************************************
     C* Upadte the FAEOP file to store the EOWIND values
     C*
     C** BEG ADD *** 11/11/10 **********************************************
     C                   TIME                    DSDTTM
     C** END ADD *** 11/11/10 **********************************************
     C                   Movel     q1user        eouser
     C                   Movel     q1pgm         eopgm
     C                   Z-add     dstime        eotime
     C                   Z-add     dsdate        srdti
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         eodt
     C*
     C                   if        %open(faeolg)
     C*
     C                   update    peo01
     C                   close     faeolg
     C
     C                   endif
     C** END ADD *** 09/06/10 **********************************************
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
    C*** BEG ADD *** 09/28/04 *********************************************
     C                   move      qqco#         kkco#
    C*
    C* Get provider auxiliary
    C*
     C                   move      qqfein        kkfein
     C                   move      qqmod         kkmod
     C     p1ky          chain     pp101                              99
    C*** END ADD *** 09/28/04 *********************************************
     C                   eval      x = 0
     C                   clear                   date
     C*** BEG DEL *** 10/02/02 *********************************************
     C****************** movel     qqspcd        caspcd
     C*** END DEL *** 10/02/02 *********************************************
    C*
    C* Find the recon number.  If this is a recon.
     C*** BEG ADD *** 09/06/10 *********************************************
     C                   open      faeolg
     C*** END ADD *** 09/06/10 *********************************************
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      xxrcn = eorcn
    C*
     C                   else
     C                   eval      xxrcn = *zero
    C*
     C                   endif
     C*
     C*** BEG ADD *** 09/25/04 *********************************************
     C                   if        eobtyp = 'N'
     C     ehky          chain     peh01                              99
     C                   endif
     C*** END ADD *** 09/25/04 *********************************************
     C*
     C*** BEG ADD *** 04/07/08 *********************************************
     C     ehky          chain     peu01                              99
     C*** END ADD *** 04/07/08 **********************************************
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
       //*** Beg Chg *** 06/21/18 *************************************
     C***********        if        eobtyp = 'N'
     C***********                  and carcfl = 'C'
      /Free
                         If EObTyp = 'N' or
                                (EObTyp = 'N' and CArcFl = 'M');
      /End-free
       //*** End Chg *** 06/21/18 *************************************
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
    C*** BEG ADD *** 09/02/11 *********************************************
       //*** Beg Add *** 06/21/18 *************************************
      /Free
                         Select;
                         When CAsrc = 'A' and CAfDat >= 1110701;
                            Exsr MedAmb;
       //*** Beg Chg *** 02/18/20 *************************************
       //********        When CAsrc = 'R' and CArcFL = 'M';
                         When CAsrc = 'R' and CArcFL = 'M' and isMC433;
       //*** End Chg *** 02/18/20 *************************************
                            Exsr MedPhy;
       //*** Beg Chg *** 02/18/20 *************************************
       //********        When CAsrc = 'F' and CArcFL = 'M' and isMC433;
                         When CAsrc = 'F' and CArcFL = 'M';
       //*** End Chg *** 02/18/20 *************************************
                            Exsr MedPhy;
       //*** Beg Chg *** 02/18/20 *************************************
       //*********       When CAsrc = 'L' and CArcFL = 'M';
                         When CAsrc = 'L' and CArcFL = 'M' and isMC433;
       //*** End Chg *** 02/18/20 *************************************
                            Exsr MedPhy;
       //*** Beg Chg *** 02/18/20 *************************************
       //*********       When CAsrc = 'D' and CArcFL = 'M';
                         When CAsrc = 'D' and CArcFL = 'M' and isMC433;
       //*** End Chg *** 02/18/20 *************************************
                            Exsr MedPhy;
                         Other;
                            Exsr Proc;
                         Endsl;
      /End-Free
       //*** End Add *** 06/21/18 *************************************
     C*** Beg Del *** 06/21/18 ****************************************
     C**************
     C**************     if        casrc='A' and cafdat >= 1110701
     C**************     exsr      MedAmb
     C**************     else
     C*** END ADD *** 09/02/11 *********************************************
     C**************     exsr      proc
     C*** BEG ADD *** 09/02/11 *********************************************
     C**************     endif
     C*** End Del *** 06/21/18 ****************************************
     C*** END ADD *** 09/02/11 *********************************************
     C                   else
    C*
    C* Else treat line item as a dental code
    C* Chain out to the Dental Bill Detail file
       //*** Beg Del *** 06/21/18 *************************************
     C*************      if        eobtyp = 'D'
    C*** BEG ADD *** 08/22/02 *********************************************
     C*************                and carcfl = 'C'
       //*** End Del *** 06/21/18 *************************************
       //*** Beg Add *** 06/21/18 *************************************
      /Free
                         If EObTyp = 'D' or
                                (EObTyp = 'D' and CArcFl = 'M');
      /End-free
       //*** End Add *** 06/21/18 *************************************
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
       //*** Beg Add *** 06/21/18 *************************************
      /Free
                         Select;
                         When CAsrc = 'T';
                            Exsr MedDent;
                         Other;
      /End-Free
       //*** End Add *** 06/21/18 *************************************
    C*
     C                   exsr      dent
       //*** Beg Add *** 06/21/18 *************************************
      /Free
                         Endsl;
      /End-Free
       //*** End Add *** 06/21/18 *************************************
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
     C*** BEG ADD *** 10/14/08 *********************************************
     C                   exsr      pmodldhos
     C*** END ADD *** 10/14/08 *********************************************
     C                   endif
    C*
     C                   movel     'H'           catype
    C*
    C*** BEG ADD *** 03/18/09 *********************************************
     C                   if        carcfl = 'Y'
     C                   exsr      recu
     C                   else
    C*** END ADD *** 03/18/09 *********************************************
     C                   exsr      svcd
    C*** BEG ADD *** 03/18/09 *********************************************
     C                   endif
    C*** END ADD *** 03/18/09 *********************************************
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
       //*** Beg Add *** 06/21/18 ***************************************
      /Free
                         If CAsrc <> 'D';
      /End-Free
       //*** End Add *** 06/21/18 ***************************************
    C*
     C                   exsr      pmod
       //*** Beg Add *** 06/21/18 ***************************************
      /Free
                         Endif;
      /End-Free
       //*** End Add *** 06/21/18 ***************************************
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
     C*** BEG ADD *** 10/14/08 *********************************************
    C*
    C*   If the situation allows payment > charged then load from xxmra
    C*
     C*** BEG ADD *** 04/07/08 *********************************************
     C                   if        euicn >= 'AO' and euicn <= 'HS'
     C                             and cafdat >= 1080301
     C*** BEG ADD *** 04/08/09 *********************************************
     C                             or eospcd = 'ASC' and
     C                              cafdat >= 1080901
     C*** END ADD *** 04/08/09 *********************************************
     C                   z-add     xxmra         caamt                92
     C                   endif
     C*** END ADD *** 10/14/08 *********************************************
     C*** BEG ADD *** 06/27/03 *********************************************
     C*
     C* Load base units for anesthesia codes
     C*
     C                   if        eobtyp = 'N' and dhprcd >= '00100'
     C                             and dhprcd <= '01999' and xxmra =0
     C                   eval      caamt = caanam
     C                   eval      xxmra = caanam
     C                   endif
     C*** END ADD *** 06/27/03 *********************************************
     C*** BEG ADD *** 04/07/09 *********************************************
     C                   if        eobtyp = 'H' and
     C                             (euicn ='IM' or euicn ='IS' or
     C                              euicn ='IR' or euicn ='IP')
     C*** END ADD *** 04/07/09 *********************************************
     C*** BEG ADD *** 04/07/08 *********************************************
     C*** BEG ADD *** 04/07/09 *********************************************
     C*****************  if        euaddt >= 1080301
     C                             and euaddt >= 1080301
     C*** END ADD *** 04/07/09 *********************************************
     C                   Z-Add     euaddt        kkefdt#
     C     frky#         setgt     pfr01
     C                   else
     C*** END ADD *** 04/07/08 *********************************************
    C*
    C*  Setup correct effective date for rules
     C     frky          setgt     pfr01
     C*** BEG ADD *** 04/07/08 *********************************************
     C                   endif
     C*** END ADD *** 04/07/08 *********************************************
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
    C*   Get the effective date of F4000N for this state
    C*
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
    C* Service date must be >= rule date to write the line item
    C*
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
     C*** BEG DEL *** 06/27/03 ****************************************
     C****************** clear                   pcawk2
     C*** END DEL *** 06/27/03 ****************************************
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
    C*
     C                   endif
    C*
     C*** BEG ADD *** 06/27/03 ****************************************
     C                   clear                   pcawk2
     C*** END ADD *** 06/27/03 ****************************************
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
     C*
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
     C****************** movel     ddprcd       00100t4
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
     C*** BEG ADD *** 08/05/02 *********************************************
     C* Load amounts based on service zip of the provider
    C*
     C*** BEG CHG *** 08/23/02 *********************************************
     C****************** if        cafdat >= 1020901
     C*** BEG CHG *** 06/27/03 *********************************************
     C****************** if        cafdat >= 9999999
     C                   if        cafdat >= 1030801
     C*** END CHG *** 06/27/03 *********************************************
     C*** END CHG *** 08/23/02 *********************************************
    C*
     C                   exsr      loadamt
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
     C                   else
     C*** END ADD *** 08/05/02 *********************************************
    C*
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caprof = prprof
     C*** BEG CHG *** 06/27/03 *********************************************
     C****************** eval      catech = pr92n2
     C                   eval      catech = pr92n1
     C*** END CHG *** 06/27/03 *********************************************
     C                   eval      caanam = pranam
    C*
     C*** BEG ADD *** 08/05/02 *********************************************
     C                   endif                                                  cafdat >= 1020901
     C*** END ADD *** 08/05/02 *********************************************
    C*
     C                   eval      cafolu = prfolu
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C*** BEG DEL *** 11/01/00 *********************************************
     C****************** move      pr1a3         canct
     C*** END DEL *** 11/01/00 *********************************************
     C*** BEG ADD *** 11/01/00 *********************************************
     C                   move      pr1af         canct
     C                   move      pr1a3         cabund
     C*** END ADD *** 11/01/00 *********************************************
     C*** BEG CHG *** 08/05/02 *********************************************
     C****************** move      pr1ad         casepp
     C                   move      pr1a6         casepp
     C*** END CHG *** 08/05/02 *********************************************
     C                   move      prfsr         cafsr
     C*** BEG DEL *** 08/05/02 *********************************************
     C****************** eval      camd50 = *zero
     C****************** eval      camd51 = *zero
     C****************** eval      camd80 = *zero
     C****************** eval      camd81 = *zero
     C*** END DEL *** 08/05/02 *********************************************
     C*** BEG ADD *** 08/05/02 *********************************************
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 0.50
     C                   eval      camd80 = 0.20
     C*** END ADD *** 08/05/02 *********************************************
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
    C*
    C*
     C     folue         endsr
    C*
     C*** BEG ADD *** 09/25/04 *********************************************
    C********************************************************************
    C** HPSA SUBROUTINE                                H P S A          *
    C********************************************************************
     C     hpsa          begsr
    C*
     C     'QB'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *off
     C     'QU'          lookup    mod(1)                                 99
     C*** BEG ADD *** 03/29/06 *********************************************
     C                   if        *in99 = *off
     C     'AQ'          lookup    mod(1)                                 99
     C                   endif
     C*** END ADD *** 03/29/06 *********************************************
     C                   endif
    C*
     C*** BEG ADD *** 05/21/05 *********************************************
     C                   eval      cabfhe = 'N'
    C*
     C                   if        *in99 = *off
    C*
     C                   movel     xxzip         kkzip
    C*
     C                   eval      xxpref3 = %subst(ehflic:1:3)
     C                   if        xxpref3 = 'PSY'
     C                   move      'M'           kktype
     C                   else
     C                   move      'P'           kktype
     C                   endif
     C*** BEG ADD *** 05/12/08 *********************************************
     C     heky2         setgt     phe02
     C                   readp     phe02                                  99
     C                   if        not *in99
     C                   eval      k1efdt=heefdt
     C*** END ADD *** 05/12/08 *********************************************
    C*
     C*** BEG CHG *** 05/12/08 *********************************************
     C*****heky*         setgt     phe01
     C*****heky1         readpe    phe01                                  99
     C     heky          chain     phe01                              99
     C*** END CHG *** 05/12/08 *********************************************
    C*
     C                   if        *in99 = *off and hestat = 'A'
     C                   eval      *in99 = *on
     C                   else
     C                   eval      *in99 = *off
     C                   endif
     C*** BEG ADD *** 05/12/08 *********************************************
     C                   else
     C                   eval      *in99 = *off
     C                   endif
     C*** END ADD *** 05/12/08 *********************************************
    C*
     C                   endif
     C*** END ADD *** 05/21/05 *********************************************
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxpref = %subst(ehflic:1:2)
     C                   if        xxpref = 'DC' or xxpref = 'DO' or
     C                             xxpref = 'DP' or xxpref = 'OD' or
     C                             xxpref = 'MD' or xxpref3 = 'PSY'
     C*** BEG ADD *** 05/21/05 *********************************************
     C*
     C* Procedure codes - check the PC/TC indicator
     C*
     C                   if        dhprcd >= '10000' and dhprcd <= '99999'
     C*** BEG ADD *** 03/21/06 *********************************************
     C                             and dhprcd <> '99455' and dhprcd <> '99456'
     C*** END ADD *** 03/21/06 *********************************************
     C                             or dhprcd >= 'G0001' and dhprcd <= 'G9999'
     C*** BEG ADD *** 03/21/06 *********************************************
     C                             and dhprcd <> '99455' and dhprcd <> '99456'
     C*** END ADD *** 03/21/06 *********************************************
     C*** Beg Add *** 06/21/18 *********************************************
      /Free
                         if CAfdat >= 1180401 and EObTyp = 'N';
                            PR1ab = CApcIn;
                         Endif;
      /End-free
     C*** End Add *** 06/21/18 *********************************************
     C*
     C                   select
     C*
     C                   when      pr1ab = '0'
     C                   eval      cabfhe = 'Y'
     C*
     C                   when      pr1ab = '1'
     C*** BEG CHG *** 07/19/05 *********************************************
     C**** '26'          lookup    mod(1)                                 99
     C****               if        *in99 = *on
     C     'TC'          lookup    mod(1)                                 99
     C                   if        *in99 = *off
     C     '27'          lookup    mod(1)                                 99
     C                   if        *in99 = *off
     C*** END CHG *** 07/19/05 *********************************************
     C                   eval      cabfhe = 'Y'
     C                   endif
     C*** BEG ADD *** 07/19/05 *********************************************
     C                   endif
     C*** END ADD *** 07/19/05 *********************************************
     C*
     C                   when      pr1ab = '2'
     C                   eval      cabfhe = 'Y'
     C*
     C                   when      pr1ab = '6'
     C                   eval      cabfhe = 'Y'
     C*
     C                   when      pr1ab = '8'
     C                   eval      cabfhe = 'Y'
     C
     C                   endsl
     C*
     C                   endif
     C*
     C* Anesthesia qualifies if performed by a physician
     C*
     C                   if        dhprcd >= '00100' and dhprcd <= '01999'
     C                             or dhprcd >= '96100' and dhprcd <= '96140'
     C*
     C     'QS'          lookup    mod(1)                                 99
     C                   if        *in99 = *off
     C     'QX'          lookup    mod(1)                                 99
     C                   if        *in99 = *off
     C     'QZ'          lookup    mod(1)                                 99
     C                   if        *in99 = *off
     C     '46'          lookup    mod(1)                                 99
     C*
     C                   if        *in99 = *off
     C                   eval      cabfhe = 'Y'
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endif                                                  if dhprcd >=
     C*** END ADD *** 05/21/05 *********************************************
     C*** BEG DEL *** 05/21/05 *********************************************
     C****************** eval      cabfhe = 'Y'
     C*** END DEL *** 05/21/05 *********************************************
     C                   endif
    C*
     C                   endif
    C*
     C                   endsr
     C*** END ADD *** 09/25/04 *********************************************
     C*** BEG ADD *** 04/02/08 *********************************************
    C********************************************************************
    C** UNDER-SERVED SUBROUTINE                                           *
    C********************************************************************
     C     under_served  begsr
    C*
     C                   eval      cabmri = ' '
    C*
     C                   movel     xxzip         kkzip
    C*
     C     uaky          setgt     pua01
     C     uaky          readpe    pua01                                  99
     C*** BEG ADD *** 04/16/08 *********************************************
    C
    C                   if        *in99 = *off and uastat = 'A'
     C*
     C                   call      'FA757R'
     C                   parm      cafein        ##fein            9 0
     C                   parm      camod         ##mod             3 0
     C                   parm      eontwk        ##nwcd            5
     C                   parm      cafdat        ##fdat            7 0
     C                   parm      dhprcd        ##prcd            5
     C                   parm      dupmod        ##pmod            2
     C                   parm      *blanks       ##rtcd            1
     C*
     C                   if        ##rtcd = ' '
     C                   eval      cabmri = 'T'
    C                   endif
     C*
    C                   endif
     C*** END ADD *** 04/16/08 *********************************************
    C*
     C*** BEG DEL *** 04/16/08 *********************************************
    C******
    C******             if        *in99 = *off and uastat = 'A'
    C******             eval      *in99 = *on
    C******             else
    C******             eval      *in99 = *off
    C******             endif
    C******
     C******             if        *in99 = *on
     C******
    C******
     C**Procedure codes - check the PC/TC indicator
     C******
     C******             if        dhprcd >= '10000' and dhprcd <= '99999'
     C******                       and dhprcd <> '99455' and dhprcd <> '99456'
     C******                       or dhprcd >= 'G0001' and dhprcd <= 'G9999'
     C******                       and dhprcd <> '99455' and dhprcd <> '99456'
     C******
     C******             select
     C******
     C******             when      pr1ab = '0'
     C******             eval      cabmri = 'T'
     C******
     C******             when      pr1ab = '1'
     C*****'TC'          lookup    mod(1)                                 99
     C******             if        *in99 = *off
     C*****'27'          lookup    mod(1)                                 99
     C******             if        *in99 = *off
     C******             eval      cabmri = 'T'
     C******             endif
     C******             endif
     C******
     C******             when      pr1ab = '2'
     C******             eval      cabmri = 'T'
     C******
     C******             when      pr1ab = '6'
     C******             eval      cabmri = 'T'
     C******
     C******             when      pr1ab = '8'
     C******             eval      cabmri = 'T'
     C******
     C******             endsl
     C******
     C******             endif                                                  dhprcd =
     C******
     C**Anesthesia qualifies if performed by a physician
     C******
     C******             if        dhprcd >= '00100' and dhprcd <= '01999'
     C******                       or dhprcd >= '96100' and dhprcd <= '96140'
     C******
     C*****'QS'          lookup    mod(1)                                 99
     C******             if        *in99 = *off
     C*****'QX'          lookup    mod(1)                                 99
     C******             if        *in99 = *off
     C*****'QZ'          lookup    mod(1)                                 99
     C******             if        *in99 = *off
     C*****'46'          lookup    mod(1)                                 99
     C******
     C******             if        *in99 = *off
     C******             eval      cabmri = 'T'
     C******             endif
     C******
     C******             endif
     C******
     C******             endif
     C******
     C******             endif
     C******
     C******             endif                                                  if dhprcd >=
     C******
     C******             endif                                                  *in99 = *on
     C*** END DEL *** 04/16/08 *********************************************
    C*
     C                   endsr
     C*** END ADD *** 04/02/08 *********************************************
     C*
    C********************************************************************
    C** LOADAMT SUBROUTINE                             L O A D A M T    *
    C********************************************************************
     C     loadamt       begsr
     C*
     C                   if        eoflic <> *blanks
     C                   movel     eoflic        xxzip
     C*
     C                   else
     C                   movel     p1zip         xxzip
     C*
     C                   endif
     C*
     C* Default to locality 99 and Urban area
     C*
     C                   eval      xxloc# = 99
     C*** BEG ADD *** 09/25/03 *********************************************
     C                   move      'U'           caarea
     C*** END ADD *** 09/25/03 *********************************************
     C*
     C     zrky          setgt     pzr01
     C                   readp     pzr01                                  99
     C*
     C                   if        *in99 = *off and xxzip >= zrfzcd
     C                             and xxzip <= zrtzcd
     C*
     C                   eval      xxloc# = zrrgn#
     C*** BEG ADD *** 09/25/03 *********************************************
     C                   move      zrarea        caarea
     C*** END ADD *** 09/25/03 *********************************************
     C*
     C                   endif
     C*** BEG ADD *** 06/09/05 *********************************************
     C                   eval      xxfac = 'N'
     C                   if        dhpos <> ' 03' and dhpos <> ' 04' and
     C                             dhpos <> ' 11' and dhpos <> ' 12' and
     C                             dhpos <> ' 15' and dhpos <> ' 20' and
     C                             dhpos <> ' 25' and dhpos <> ' 32' and
     C                             dhpos <> ' 33' and dhpos <> ' 50' and
     C                             dhpos <> ' 54' and dhpos <> ' 55' and
     C                             dhpos <> ' 60' and dhpos <> ' 62' and
     C                             dhpos <> ' 71' and dhpos <> ' 72' and
     C                             dhpos <> ' 81'
     C                   eval      xxfac = 'Y'
     C                   endif
     C*** END ADD *** 06/09/05 *********************************************
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       If %subst(Eoflic:6:4) <> *blanks;
          Kkst = Eoast;
          Kkzip = %dec(%subst(Eoflic:1:5):5:0);
          Kkpls4 = %dec(%subst(Eoflic:6:4):4:0);
          Kkefdt = Cafdat;
          Setgt keyFtzpl1 Pzp01;
          Readp Pzp01;
          If %found(Ftzpl1) and (Zpst = Kkst) and (Zpzip = Kkzip)
           and (Zppls4 = Kkpls4);
             Xxloc# = Zploc;
          Endif;
       Endif;
      /End-Free
     C*** End Add *** 09/18/12 *************************************************
     C*
     C* Load amount fields based on region
     C*
     C                   select
     C*
     C                   when      xxloc# = 9
     C*
     C                   eval      camra = prmra
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92of
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92n6
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pranam
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92n7
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92n7
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n1
     C                   eval      caanam = pranam
     C*
     C                   when      xxloc# = 11
     C*
     C                   eval      camra = pr92n8
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92og
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92n9
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92na
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nd
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nd
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nb
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92nb
     C                   eval      catech = pr92nc
     C                   eval      caanam = pr92na
     C*
     C                   when      xxloc# = 15
     C*
     C                   eval      camra = pr92ne
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92oh
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92nf
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92ng
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nj
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nj
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nh
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92nh
     C                   eval      catech = pr92ni
     C                   eval      caanam = pr92ng
     C*
     C                   when      xxloc# = 18
     C*
     C                   eval      camra = pr92nk
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92oi
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92nl
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nm
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92np
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92np
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nn
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92nn
     C                   eval      catech = pr92no
     C                   eval      caanam = pr92nm
     C*
     C                   when      xxloc# = 20
     C*
     C                   eval      camra = pr92nq
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92oj
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92nr
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92ns
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nv
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nv
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nt
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92nt
     C                   eval      catech = pr92nu
     C                   eval      caanam = pr92ns
     C*
     C                   when      xxloc# = 28
     C*
     C                   eval      camra = pr92nw
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92ok
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92nx
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92ny
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92o2
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92o2
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92nz
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92nz
     C                   eval      catech = pr92o1
     C                   eval      caanam = pr92ny
     C*
     C                   when      xxloc# = 31
     C*
     C                   eval      camra = pr92o3
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92ol
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92o4
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92o5
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92o8
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92o8
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92o6
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92o6
     C                   eval      catech = pr92o7
     C                   eval      caanam = pr92o5
     C*
     C                   when      xxloc# = 99
     C*
     C                   eval      camra = pr92o9
      *** Beg Add *** 11/06/07 ****************************************
     C                   eval      calq51 = pr92om
      *** End Add *** 11/06/07 ****************************************
     C*
     C* Facility allowance
     C*
     C*** BEG CHG *** 06/09/05 *********************************************
     C****************   if        dhpos <> ' 11'
     C                   if        xxfac = 'Y'
     C*** END DEL *** 06/09/05 *********************************************
     C                   eval      camra = pr92oa
     C                   endif
     C*
     C* Modifiers
     C*
     C     'KM'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92ob
     C                   endif
     C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92oe
     C                   endif
     C*
     C     'KN'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92oe
     C                   endif
     C*** BEG ADD *** 04/06/06 *********************************************
     C*
     C     'WH'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = pr92oc
     C                   endif
     C*
     C     'CP'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
     C                   eval      camra = prprof
     C                   endif
     C*** END ADD *** 04/06/06 *********************************************
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92oc
     C                   eval      catech = pr92od
     C                   eval      caanam = pr92ob
     C*
     C                   endsl
     C*** BEG ADD *** 09/28/04 *********************************************
    C*
    C* Load the ASC allowance if applicable
    C*
     C     'SG'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on and cafdat >= 1040901
     C                             and dhprcd >= '10000' and dhprcd <='69999'
     C*** BEG ADD *** 06/03/09 *********************************************
     C                             and cafdat < 1080901
     C*** END ADD *** 06/03/09 *********************************************
     C*** BEG ADD ************ 09/15/08 *******************************************
     C                             or  dhpos  = ' 24'  and cafdat >= 1040901
     C                             and dhprcd >= '10000' and dhprcd <='69999'
     C                              and dhtos = 'F'
     C*** BEG ADD *** 06/03/09 *********************************************
     C                             and cafdat < 1080901
     C*** END ADD *** 06/03/09 *********************************************
     C*** END ADD ************ 09/15/08 *******************************************
    C*
     C                   eval      camra = 0
    C*
     C                   eval      kkkey = 'TX' + %subst(p1cnty:6:3)
     C     ccky          chain     pcc01                              99
     C                   if        *in99 = *off
     C                   movel(P)  cc10a1        kkrgn
     C                   move      pr1ai         kkgrp
     C     agky          setgt     pag01
     C     agky1         readpe    pag01                                  99
     C                   if        *in99 = *off and agstat = 'A'
     C                   eval      camra = agalow
     C                   endif
     C                   endif
    C*
     C                   endif
     C*** END ADD *** 09/28/04 *********************************************
     C*
     C     loadamte      endsr
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
     C*** BEG ADD *** 01/25/07 *********************************************
    C                   eval      captyp = rxptyp
     C*** END ADD *** 01/25/07 *********************************************
    C*
    C* Brand-name Pharmaceuticals
     C                   if        rxptyp = '01'
     C                             or rxptyp = '07'
    C*
    C* Determine if fill date is less than AWP effective date if so
    C* use previous AWP unit price else use current AWP unit price
     C                   if        cafdat < rxawpd
    C*
     C*** BEG CHG *** 05/30/02 *********************************************
     C****************** eval(h)   camra = rx1awp
      *** Begin Change *** 05/02/18 *********************
     C****               eval(h)   xxawp = rx1awp/1000
     C                   eval(h)   xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 05/30/02 *********************************************
    C*
     C                   else
     C*** BEG CHG *** 05/30/02 *********************************************
     C****************** eval(h)   camra = rxawp
      *** Begin Change *** 05/02/18 *********************
     C****               eval(h)   xxawp = rxawp/1000
     C                   eval(h)   xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 05/30/02 *********************************************
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
     C*** BEG CHG *** 05/30/02 *********************************************
     C****************** eval(h)   camra = rx1awp
      *** Begin Change *** 05/02/18 *********************
     C***                eval(h)   xxawp = rx1awp/1000
     C                   eval(h)   xxawp = rx1awp
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 05/30/02 *********************************************
    C*
     C                   else
     C*** BEG CHG *** 05/30/02 *********************************************
     C****************** eval(h)   camra = rxawp
      *** Begin Change *** 05/02/18 *********************
     C****               eval(h)   xxawp = rxawp/1000
     C                   eval(h)   xxawp = rxawp
      *** End   Change *** 05/02/18 *********************
     C*** END CHG *** 05/30/02 *********************************************
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      caunts = dpunts
     C                   move      rxstat        castat
     C                   move      'Y'           cafsr
     C*** BEG ADD *** 05/30/02 *********************************************
     C                   eval      camra = xxawp * caunts
     C*** END ADD *** 05/30/02 *********************************************
     C*** BEG ADD *** 10/17/02 *********************************************
     C*** BEG CHG *** 05/03/07 *********************************************
     C****************** eval(h)   caawp = caunts * xxawp
     C                   eval      caawp = xxawp
     C*** END CHG *** 05/03/07 *********************************************
     C*** END ADD *** 10/17/02 *********************************************
     C*** BEG ADD *** 03/01/07 *********************************************
     C     rxstat        cabeq     'I'           nxtpr4
     C*** END ADD *** 03/01/07 *********************************************
    C*
     C                   endif
    C*
     C*** BEG DEL *** 03/01/07 *********************************************
     C*****rxstat        cabeq     'I'           nxtpr4
     C*** END DEL *** 03/01/07 *********************************************
    C*
     C     phare         endsr
    C********************************************************************
    C** PROCEDURE CODE MODIFIER CALCULATIONS                  P M O D   *
    C********************************************************************
     C     pmod          begsr
     C*** BEG ADD **** 02/03/09 ******************************************
     C                   eval      xxdme = ' '
     C     prky          setgt     ppr01                              99
     C     prky1         readpe    ppr01                                  99
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
     C*** BEG ADD **** 01/19/10 ******************************************
    C*
    C* If no amount was loaded from FMDMPTX then load from FMPRPTX
    C*
     C                   if        camra = 0
    C*
    C* Default to load from MRA
    C*
     C                   eval      xxmra = prmra
     C                   eval      camra = prmra
    C*
    C* Load based on modifiers billed
    C*
     C     'UE'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = pr92n7
     C                   eval      camra = pr92n7
    C*
     C                   endif
    C*
     C     'RR'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = pranam
     C                   eval      camra = pranam
    C*
     C                   endif
    C*
     C                   endif                                                  if xxdme <> 'Y'
     C*** END ADD **** 01/19/10 ******************************************
     C                   if        xxdme <> 'Y'
     C*
     C*** END ADD **** 02/03/09 ******************************************
    C*
    C* Adjust MRA depending on MOD
    C*
     C*** BEG ADD *** 08/05/02 *********************************************
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
     C                   eval      xxmra = camra * camd62
    C*
     C                   endif
    C*
     C     '65'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd65
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
     C*** END ADD *** 08/05/02 *********************************************
    C*
     C     '80'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd80
    C*
     C                   endif
    C*** BEG ADD *** 05/10/00 ****************************************
    C*
    C* Reload FMPRPTX fields to adjust per modifier
    C*
     C     prky          setgt     ppr01
     C                   readp     ppr01                                  99
    C*
     C                   if        *in99 = *off
     C                             and prcpt4 = dhprcd
    C*
    C* MRI with Extended Slices modifier
     C     '22'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      caprof = pr92n4
     C                   eval      catech = pr92n5
     C                   eval      camra = caprof + catech
     C                   eval      xcmra = camra
    C*
     C                   endif
    C*
    C* MRI with Limited Slices modifier
     C     '52'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      caprof = pr92n2
     C                   eval      catech = pr92n3
     C                   eval      camra = caprof + catech
     C                   eval      xcmra = camra
    C*
     C                   endif
    C*
     C                   endif
    C*** END ADD *** 05/10/00 ****************************************
    C*
     C*** BEG ADD **** 02/03/09 ******************************************
     C                   endif
     C*** END ADD **** 02/03/09 ******************************************
    C*** BEG ADD *** 02/18/11 ****************************************
    C*
    C* CARF and Non-CARF
    C*
     C                   if        dhprcd = '97799'
     C     'MR'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = pranam
     C                   eval      camra = pranam
    C*
     C                   endif
     C                   endif
    C*** END ADD *** 02/18/11 ****************************************
    C*** BEG ADD *** 02/16/09 ****************************************
     C     'RR'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      caanam = camra
    C*
     C                   endif
    C*** END ADD *** 02/16/09 ****************************************
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
     C*** Beg Add *** 06/21/18 *********************************************
      /Free
                         SortA Mod;
      /End-Free
     C*** End Add *** 06/21/18 *********************************************
    C*
     C                   movea     mod           mod1
    C*
     C                   endsr
     C*** END ADD *** 10/17/02 *********************************************
     C*** BEG ADD *** 10/14/08 *********************************************
    C********************************************************************
    C** MODIFIER LOADING                           P M O D L D H O S    *
    C********************************************************************
     C     pmodldhos     begsr
    C*
     C                   move      dupmod        camod1
     C*** BEG ADD *** 08/12/09 *********************************************
     C                   move      dumod2        camod2
     C*** END ADD *** 08/12/09 *********************************************
    C*
     C                   move      dupmod        mod(1)
     C*** BEG ADD *** 08/12/09 *********************************************
     C                   move      dumod2        mod(2)
     C*** END ADD *** 08/12/09 *********************************************
    C*
     C                   movea     mod           mod1
    C*
     C                   endsr
     C*** END ADD *** 10/14/08 *********************************************
    C*
    C********************************************************************
    C** PROCEDURE CODE MRA SEARCH                             P R O C   *
    C********************************************************************
     C     proc          begsr
    C*
    C*  Default to service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
    C* PROCESS THE FEE SCHEDULED CODES HERE
    C*
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
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
    C*
     C*** BEG ADD *** 08/05/02 *********************************************
     C* Load amounts based on service zip of the provider
    C*
     C*** BEG CHG *** 08/23/02 *********************************************
     C****************** if        cafdat >= 1020901
     C*** BEG CHG *** 06/27/03 *********************************************
     C****************** if        cafdat >= 9999999
     C                   if        cafdat >= 1030801
     C*** END CHG *** 06/27/03 *********************************************
     C*** END CHG *** 08/23/02 *********************************************
    C*
     C                   exsr      loadamt
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
     C                   else
     C*** END ADD *** 08/05/02 *********************************************
     C                   eval      xcmra = prmra
     C                   eval      camra = prmra
     C                   eval      caamt = prmra
    C*
    C*
     C                   eval      caprof = prprof
    C*** BEG CHG *** 11/01/00 ****************************************
    C******************** Z-ADDPR92N2    CATECH
     C                   eval      catech = pr92n1
    C*** END CHG *** 11/01/00 ****************************************
     C                   eval      caanam = pranam
     C*** BEG ADD *** 08/05/02 *********************************************
     C                   endif
     C*** END ADD *** 08/05/02 *********************************************
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   eval      caasc = pr92n1
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C*** BEG DEL *** 11/01/00 *********************************************
     C****************** move      pr1a3         canct
     C*** END DEL *** 11/01/00 *********************************************
     C*** BEG ADD *** 11/01/00 *********************************************
     C                   move      pr1af         canct
     C                   move      pr1a3         cabund
     C*** END ADD *** 11/01/00 *********************************************
     C*** BEG CHG *** 08/05/02 *********************************************
     C****************** move      pr1ad         casepp
     C                   move      pr1a6         casepp
     C*** END CHG *** 08/05/02 *********************************************
     C                   move      pr1a4         capcf
     C                   move      prfsr         cafsr
     C*** BEG DEL *** 08/05/02 *********************************************
     C******************
     C****************** eval      camd50 = *zero
     C****************** eval      camd51 = *zero
     C****************** eval      camd54 = *zero
     C****************** eval      camd55 = *zero
     C****************** eval      camd56 = *zero
     C****************** eval      camd62 = *zero
     C****************** eval      camd66 = *zero
     C****************** eval      camd80 = *zero
     C****************** eval      camd81 = *zero
     C****************** eval      camd82 = *zero
     C*** END DEL *** 08/05/02 *********************************************
     C*** BEG ADD *** 08/05/02 *********************************************
     C*** BEG ADD *** 09/25/03 *********************************************
    C*
    C*   MODIFIERS/CMS VALUES
     C*
     C                   if        cafdat >= 1030801
    C*
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
     C                   eval      capcin = pr1ab
     C                   eval      ca50in = pr1ad
     C                   eval      ca51in = pr1ac
     C                   eval      ca62in = pr1a9
     C                   eval      ca66in = pr1aa
     C                   eval      ca80in = pr1a8
     C*
     C                   if        pr1ad = '1'
     C                   eval      camd50 = 1.50
     C                   endif
    C*
     C                   if        pr1ac = '1'
     C                   eval      camd51 = 0.25
     C                   endif
    C*
     C                   if        pr1ac = '2'
     C                   eval      camd51 = 0.50
     C                   endif
    C*
     C                   if        pr1ac = '3'
     C                   eval      camd51 = 0.50
     C                   endif
    C*
     C                   eval      camd54 = pr52n2/100
     C                   eval      camd55 = pr52n3/100
     C                   eval      camd56 = pr52n1/100
    C*
     C                   if        pr1a9 = '1' or pr1a9 = '2'
     C                   eval      camd62 = 6.25
     C                   eval      xxmd62 = 0.625
     C                   endif
    C*
     C                   if        pr1aa = '1' or pr1aa = '2'
     C                   eval      camd66 = 1.00
     C                   endif
    C*
     C                   if        pr1aa = '2'
     C                   eval      camd80 = 0.16
     C                   eval      camd81 = 0.25
     C                   eval      camd82 = 0.13
     C                   endif
     C*
     C* Endoscopic base code (used in multi-surg calcs)
     C*
     C                   eval      caenba = pr5a2
    C*
     C                   else                                                   cafdat < 1030801
     C*** END ADD *** 09/25/03 *********************************************
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 0.50
     C                   eval      camd54 = 0.70
     C                   eval      camd55 = 0.30
     C                   eval      camd56 = 0.10
     C                   eval      camd62 = 1.00
     C                   eval      camd65 = 0.75
     C                   eval      camd66 = 1.00
     C                   eval      camd80 = 0.20
     C                   eval      camd85 = 0.10
     C*** BEG ADD *** 09/25/03 *********************************************
     C*
     C                   endif
     C*** END ADD *** 09/25/03 *********************************************
     C*** END ADD *** 08/05/02 *********************************************
     C*** BEG ADD *** 02/06/09 *********************************************
     C                   movel     dhtos         catos
     C*** END ADD *** 02/06/09 *********************************************
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
     C*** BEG ADD *** 09/25/04 *********************************************
     C                   exsr      hpsa
     C*** END ADD *** 09/25/04 *********************************************
     C*** BEG ADD *** 04/02/08 *********************************************
     C                   if        cabfhe <> 'Y'
     C*** BEG ADD *** 04/16/08 *********************************************
     C                             and dhprcd <> '99455' and
     C                             dhprcd <> '99456' and
     C                             dhprcd <> '97750' and
     C                             dhprcd <> '97545' and
     C                             dhprcd <> '97546' and
     C                             dhprcd <> '97799'
     C*** END ADD *** 04/16/08 *********************************************
     C                   exsr      under_served
     C                   endif
     C*** END ADD *** 04/02/08 *********************************************
     C*** BEG ADD *** 11/14/08 *********************************************
     C                   IF        eospcd = 'ASC' and CAFDAT >= 1080901
     C*
     C                   movel     dhprcd        ##prcd
     C                   move      cafdat        ##efdt
     C*** BEG ADD *** 11/23/09 *********************************************
     C                   move      'Y'           cafsr
     C*** END ADD *** 11/23/09 *********************************************
     C*
     C     asky          setgt     pas01
     C     asky1         readpe    pas01                                  99
     C                   If        *in99 = *off
     C*
     C*** BEG ADD *** 02/16/2009 *******************************************
     C                   movel     aspi          caaspi
     C*** END ADD *** 02/16/2009 *******************************************
     C*
     C                   if        aspi = 'A2' or aspi = 'G2' or aspi = 'H8'
     C                             or aspi = 'J8' or aspi = 'K2' or
     C                             aspi = 'P2' or aspi = 'R2' or aspi = 'Z2'
     C                   exsr      ASC_pymt
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
     C                   endif
     C*
     C                   if        aspi = 'P3' or aspi = 'Z3'
     C                   movel     dhpos         xxpos             3
     C                   exsr      loadamt
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
     C                   movel     xxpos         dhpos
     C                   endif
     C*
     C*** BEG ADD *** 10/26/09 *********************************************
     C                   else
     C                   eval      caaspi = '00'
     C                   eval      xcmra = 0
     C                   eval      caamt = 0
     C*** END ADD *** 10/26/09 *********************************************
     C                   endIf
     C*
     C                   endIF
     C*** END ADD *** 11/14/08 *********************************************
     C*** Beg Add *** 10/06/09 *********************************************
     C                   if        dhpos  = ' 24' and dhtos <> 'F  ' and
     C                             eospcd <> 'ASC'
     C                   eval      caplac = ' 11'
     C                   endif                                                  dhpos = ' 24'
     C*** End Add *** 10/06/09 *********************************************
     C*** BEG DEL *** 08/05/02 *********************************************
    C******************
    C****MODIFIERS*****
    C******************
    C****************** if        pr1a9 = '1'
    C****************** eval      camd50 = 1.00
    C****************** endif
    C****************** if        pr1a8 = '2'
    C****************** eval      camd51 = 0.50
    C****************** endif
    C****************** if        pr1a8 = '3'
    C****************** eval      camd51 = 0.50
    C****************** endif
    C****************** eval      camd54 = pr52n2
    C****************** eval      camd55 = pr52n3
    C****************** eval      camd56 = pr52n1
    C****************** if        pr1ab = '2'
    C****************** eval      camd62 = 6.25
    C****************** endif
    C****************** if        pr1ac = '2'
    C****************** eval      camd66 = 1.00
    C****************** endif
    C****************** if        pr1aa = '2'
    C****************** eval      camd80 = 1.00
    C****************** eval      camd81 = 1.00
    C****************** eval      camd82 = 1.00
    C****************** endif
    C******************
     C*** END DEL *** 08/05/02 *********************************************
    C                   endif
    C*
     C     prstat        cabeq     'I'           nxtpr2
    C*
     C     proce         endsr
    C*** BEG ADD *** 09/02/11 *********************************************
    C********************************************************************
    C** Medicare Ambulance                                M e d A m b   *
    C********************************************************************
     C     MedAmb        begsr
     C
     C                   move      carrn         ##rrn
     C                   eval      ##alw1 = 0
     C                   eval      ##alw2 = 0
     C                   eval      ##alw3 = 0
     C                   eval      ##rtcd = *blank
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
     * Get the allowance percentage from the FTRTP
     * (LF=FTRTL2) Fee Schedule Rate Table
     C
    C* Get Fee Schedule Rate for TX
     C                   Movel     'TX'          @@st
     C                   Movel     'M'           @@type
     C                   movel     kkefdt        @@bgdt
     C     rtky          setgt     prt01
     C     rtky1         readpe    prt01                                  99
    C*
     C                   if        *in99 = *off
     C                   eval      camra = ##alw1 * RTMUL3
     C                   eval      xcmra = ##alw1 * RTMUL3
     C                   else
    C*   No Fee Schedule Rate record for TX found
     C                   eval      camra = ##alw1
     C                   eval      xcmra = ##alw1
     C                   endif
     C
     C                   movel     'N'           carpt
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
     C     MedAmbe       endsr
       //*** Beg Add *** 06/21/18 ****************************************
       //*****************************************************************
       // Medicare Physician                               M e d P h y   *
       //*****************************************************************
      /Free
           Begsr MedPhy;
              ##Rrn = CArrn;
              kkEfDt = CAfDat;
              ##PrCd = DHprCd;
       //
       // Call API to calculate Texas Medicare Physicain Amt
       //
              callp TxMedPhyApi(##Rrn:##Desc:##PrCd:##Mod1:##EfDt:
               ##Loc9NonF:##Loc9Facil:##Loc9Prof:##Loc9Tech:##Loc9Anes:
               ##Loc11NonF:##Loc11Facil:##Loc11Prof:##Loc11Tech:##Loc11Anes:
               ##Loc15NonF:##Loc15Facil:##Loc15Prof:##Loc15Tech:##Loc15Anes:
               ##Loc18NonF:##Loc18Facil:##Loc18Prof:##Loc18Tech:##Loc18Anes:
               ##Loc20NonF:##Loc20Facil:##Loc20Prof:##Loc20Tech:##Loc20Anes:
               ##Loc28NonF:##Loc28Facil:##Loc28Prof:##Loc28Tech:##Loc28Anes:
               ##Loc31NonF:##Loc31Facil:##Loc31Prof:##Loc31Tech:##Loc31Anes:
               ##Loc99NonF:##Loc99Facil:##Loc99Prof:##Loc99Tech:##Loc99Anes:
               ##Loc9Used:##Loc11Used:##Loc15Used:##Loc18Used:##Loc20Used:
               ##Loc28Used:##Loc31Used:##Loc99Used:
               ##PcTc:##Glob:##PreOp:##IntraOp:##PostOp:##MultSurg:
               ##Bilat:##AsstSurg:##CoSurg:##TeamSurg:##EndoBase:
               ##StatCd);

              ##Key = EOast;
              Chain ##Key pst01;
              If not %Found(ststl1);
                 STzip4 = 'N';
              Endif;
       //
       // Zip 5.  Find the Texas locality
       //
                 xxZip = %Dec(%Subst(EOflic:1:5):5:0);
                 Setgt zrky pzr01;
                 Readp pzr01;
                 If not %Eof(fmzrltx) and xxZip >= ZRfZcd and xxZip <=
                      ZRtzCd;
                    xxRgn# = ZRrgn#;
                 Else;
                    xxRgn# = 99;
                 Endif;
       //
       // Zip 9.  Find the Texas locality
       //
              If (%Subst(EOflic:6:4) <> *blanks or %Subst(EOflic:6:4) <>
                     *zeros) and STzip4 = 'Y';
                 kkZip = %Dec(%Subst(EOflic:1:5):5:0);
                 kkPls4 = %dec(%subst(EOflic:6:4):4:0);
                 Setgt (EOast:kkZip:kkPls4:CAfDat) pzp01;
                 Readp Pzp01;
                 If not %Eof(ftzpl1) and (ZPst = EOst) and (ZPzip = kkZip)
                      and (ZPpls4 = kkPls4);
                    xxRgn# = ZPloc;
                 Endif;
              Endif;
       //
       // Select the allowance based on locality
       //
                 Select;
                 When xxRgn# = 9;
                    If DHpos = ' 11';
                       CAmra = ##Loc9Nonf;
                    Else;
                       CAmra = ##Loc9Facil;
                    Endif;
                    CaProf = ##Loc9Prof;
                    CaTech = ##Loc9Tech;
                    CaAnam = ##Loc9Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc9Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc9Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc9Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc9Prof;
                    Endif;
                 When xxRgn# = 11;
                    If DHpos = ' 11';
                       CAmra = ##Loc11Nonf;
                    Else;
                       CAmra = ##Loc11Facil;
                    Endif;
                    CaProf = ##Loc11Prof;
                    CaTech = ##Loc11Tech;
                    CaAnam = ##Loc11Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc11Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc11Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc11Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc11Prof;
                    Endif;
                 When xxRgn# = 15;
                    If DHpos = ' 11';
                       CAmra = ##Loc15Nonf;
                    Else;
                       CAmra = ##Loc15Facil;
                    Endif;
                    CaProf = ##Loc15Prof;
                    CaTech = ##Loc15Tech;
                    CaAnam = ##Loc15Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc15Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc15Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc15Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc15Prof;
                    Endif;
                 When xxRgn# = 18;
                    If DHpos = ' 11';
                       CAmra = ##Loc18Nonf;
                    Else;
                       CAmra = ##Loc18Facil;
                    Endif;
                    CaProf = ##Loc18Prof;
                    CaTech = ##Loc18Tech;
                    CaAnam = ##Loc18Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc18Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc18Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc18Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc18Prof;
                    Endif;
                 When xxRgn# = 20;
                    If DHpos = ' 11';
                       CAmra = ##Loc20Nonf;
                    Else;
                       CAmra = ##Loc20Facil;
                    Endif;
                    CaProf = ##Loc20Prof;
                    CaTech = ##Loc20Tech;
                    CaAnam = ##Loc20Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc20Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc20Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc20Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc20Prof;
                    Endif;
                 When xxRgn# = 28;
                    If DHpos = ' 11';
                       CAmra = ##Loc28Nonf;
                    Else;
                       CAmra = ##Loc28Facil;
                    Endif;
                    CaProf = ##Loc28Prof;
                    CaTech = ##Loc28Tech;
                    CaAnam = ##Loc28Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc28Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc28Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc28Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc28Prof;
                    Endif;
                 When xxRgn# = 31;
                    If DHpos = ' 11';
                       CAmra = ##Loc31Nonf;
                    Else;
                       CAmra = ##Loc31Facil;
                    Endif;
                    CaProf = ##Loc31Prof;
                    CaTech = ##Loc31Tech;
                    CaAnam = ##Loc31Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc31Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc31Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc31Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc31Prof;
                    Endif;
                 When xxRgn# = 99;
                    If DHpos = ' 11';
                       CAmra = ##Loc99Nonf;
                    Else;
                       CAmra = ##Loc99Facil;
                    Endif;
                    CaProf = ##Loc99Prof;
                    CaTech = ##Loc99Tech;
                    CaAnam = ##Loc99Anes;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc99Used;
                    Endif;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc99Anes;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc99Prof;
                    Endif;
                    If %Lookup('CP':Mod) > 0;
                       caMra = ##Loc99Prof;
                    Endif;
                 other;
                    If DHpos = ' 11';
                       CAmra = ##Loc99Nonf;
                    Else;
                       CAmra = ##Loc99Facil;
                    Endif;
                    CaProf = ##Loc99Prof;
                    CaAnam = ##Loc99Anes;
                    If %Lookup('KM':Mod) > 0;
                       caMra = ##Loc99Anes;
                    Endif;
                    If %Lookup('UE':Mod) > 0;
                       caMra = ##Loc99Used;
                    Endif;
                    If %Lookup('WH':Mod) > 0;
                       caMra = ##Loc99Prof;
                    Endif;
                    If %Lookup('CP':Mod:1:4) > 0;
                       caMra = ##Loc99Prof;
                    Endif;
                 Endsl;
              xcMra = CAmra;
       //
       // Load the modifiers indicators, etc.
       //
              CApos  = DHpos;
              CApcIn = ##PcTc;
       //
       // Allow PC/TC for commission assigned code and set anes flag
       //
              if DHprCd = '97545' or prcpt4 = '97546' or
                    prcpt4 = '97750' or prcpt4 = '97799' or
                    prcpt4 = '99455' or prcpt4 = '99456';
                 CAanes = 'U';
                 if ##PcTc = ' ' or ##PcTc = '0' or ##PcTc = '2'
                       or ##PcTc = '3' or ##PcTc = '4' or
                       ##PcTc ='5' or  ##PcTc = '6' or ##PcTc = '9';
                    ##PcTc = '1';
                 Endif;
              Endif;
              CA50in = ##Bilat;
              CA51in = ##MultSurg;
              CA62in = ##CoSurg;
              CA66in = ##TeamSurg;
              CA80in = ##AsstSurg;
              if CA50in = '1';
                 camd50 = 1.50;
              endif;
              if CA51in = '1';
                 camd51 = 0.25;
              endif;
              if CA51in = '2';
                 camd51 = 0.50;
              endif;
              if CA51in = '3';
                 camd51 = 0.50;
              endif;
              camd54 = ##IntraOp/100;
              camd55 = ##PostOp/100;
              camd56 = ##PreOp/100;
              if CA62in = '1' or CA62in = '2';
                 camd62 = 6.25;
                 xxmd62 = 0.625;
              endif;
              if CA66in = '1' or CA62in = '2';
                 camd66 = 1.00;
              endif;
              if CA80in = '2';
                 camd80 = 0.16;
                 camd81 = 0.25;
                 camd82 = 0.13;
              endif;
       //
       // Endoscopic base code (used in multi-surg calcs)
       //
              caenba = ##EndoBase;
       //
       // Check for HPSA eligibility
       //
              exsr Hpsa;
       //
       // Check to see if servicer performed in Under Served area.
       //
              if cabfhe <> 'Y' and dhprcd <> '99455' and
                     dhprcd <> '99456' and dhprcd <> '97750' and
                     dhprcd <> '97545' and dhprcd <> '97546' and
                     dhprcd <> '97799';
                 exsr Under_Served;
              endif;
       //
       // Set the By Report flag
       //
              If CAmra = 0 and CAprof = 0 and CATech = 0 and CAanam = 0;
                 CArpt = 'Y';
              Else;
                 CArpt = 'N';
              Endif;

              CAfsR = 'Y';
              CAplac = DHpos;
              CAcpt4 = DHprCd;
              CApart = DHarea;
              CAunts = DHunts;
              If DHprCd >= 'A0000' and DHprCd <= 'Z9999';
                 CAanes = 'U';
              Endif;

              If DHprCd >= '00100' and DHprCd <= '01999';
                 CAanMn = DHunts;
                 CAanes = 'Y';
              Endif;

           Endsr;
      /End-Free
       //*** End Add *** 06/21/18 ****************************************
       //*****************************************************************
       // Medicaid Dental                             M e d D e n t      *
       //*****************************************************************
      /Free
           Begsr MedDent;
              ##Rrn = CArrn;
              kkEfDt = CAfDat;
              ##PrCd = DDprCd;
       //
       // Call API to calculate Texas Medicare Physicain Amt
       //
              callp TxMedPhyApi(##Rrn:##Desc:##PrCd:##Mod1:##EfDt:
               ##Loc9NonF:##Loc9Facil:##Loc9Prof:##Loc9Tech:##Loc9Anes:
               ##Loc11NonF:##Loc11Facil:##Loc11Prof:##Loc11Tech:##Loc11Anes:
               ##Loc15NonF:##Loc15Facil:##Loc15Prof:##Loc15Tech:##Loc15Anes:
               ##Loc18NonF:##Loc18Facil:##Loc18Prof:##Loc18Tech:##Loc18Anes:
               ##Loc20NonF:##Loc20Facil:##Loc20Prof:##Loc20Tech:##Loc20Anes:
               ##Loc28NonF:##Loc28Facil:##Loc28Prof:##Loc28Tech:##Loc28Anes:
               ##Loc31NonF:##Loc31Facil:##Loc31Prof:##Loc31Tech:##Loc31Anes:
               ##Loc99NonF:##Loc99Facil:##Loc99Prof:##Loc99Tech:##Loc99Anes:
               ##Loc9Used:##Loc11Used:##Loc15Used:##Loc18Used:##Loc20Used:
               ##Loc28Used:##Loc31Used:##Loc99Used:
               ##PcTc:##Glob:##PreOp:##IntraOp:##PostOp:##MultSurg:
               ##Bilat:##AsstSurg:##CoSurg:##TeamSurg:##EndoBase:
               ##StatCd);

              ##Key = EOast;
              Chain ##Key pst01;
              If not %Found(ststl1);
                 STzip4 = 'N';
              Endif;
       //
       // Zip 5.  Find the Texas locality
       //
                 xxZip = %Dec(%Subst(EOflic:1:5):5:0);
                 Setgt zrky pzr01;
                 Readp pzr01;
                 If not %Eof(fmzrltx) and xxZip >= ZRfZcd and xxZip <=
                      ZRtzCd;
                    xxRgn# = ZRrgn#;
                 Else;
                    xxRgn# = 99;
                 Endif;
       //
       // Zip 9.  Find the Texas locality
       //
              If (%Subst(EOflic:6:4) <> *blanks or %Subst(EOflic:6:4) <>
                     *zeros) and STzip4 = 'Y';
                 kkZip = %Dec(%Subst(EOflic:1:5):5:0);
                 kkPls4 = %dec(%subst(EOflic:6:4):4:0);
                 Setgt (EOast:kkZip:kkPls4:CAfDat) pzp01;
                 Readp Pzp01;
                 If not %Eof(ftzpl1) and (ZPst = EOst) and (ZPzip = kkZip)
                      and (ZPpls4 = kkPls4);
                    xxRgn# = ZPloc;
                 Endif;
              Endif;
       //
       // Select the allowance based on locality
       //
                 Select;
                 When xxRgn# = 9;
                    If DHpos = ' 11';
                       CAmra = ##Loc9Nonf;
                    Else;
                       CAmra = ##Loc9Facil;
                    Endif;
                 When xxRgn# = 11;
                    If DHpos = ' 11';
                       CAmra = ##Loc11Nonf;
                    Else;
                       CAmra = ##Loc11Facil;
                    Endif;
                 When xxRgn# = 15;
                    If DHpos = ' 11';
                       CAmra = ##Loc15Nonf;
                    Else;
                       CAmra = ##Loc15Facil;
                    Endif;
                 When xxRgn# = 18;
                    If DHpos = ' 11';
                       CAmra = ##Loc18Nonf;
                    Else;
                       CAmra = ##Loc18Facil;
                    Endif;
                 When xxRgn# = 20;
                    If DHpos = ' 11';
                       CAmra = ##Loc20Nonf;
                    Else;
                       CAmra = ##Loc20Facil;
                    Endif;
                 When xxRgn# = 28;
                    If DHpos = ' 11';
                       CAmra = ##Loc28Nonf;
                    Else;
                       CAmra = ##Loc28Facil;
                    Endif;
                 When xxRgn# = 31;
                    If DHpos = ' 11';
                       CAmra = ##Loc31Nonf;
                    Else;
                       CAmra = ##Loc31Facil;
                    Endif;
                 When xxRgn# = 99;
                    If DHpos = ' 11';
                       CAmra = ##Loc99Nonf;
                    Else;
                       CAmra = ##Loc99Facil;
                    Endif;
                 other;
                    If DHpos = ' 11';
                       CAmra = ##Loc99Nonf;
                    Else;
                       CAmra = ##Loc99Facil;
                    Endif;
                    CaProf = ##Loc99Prof;
                 Endsl;
              xcMra = CAmra;
       //
       // Load the modifiers indicators, etc.
       //
              CApos  = DDpos;
              CApcIn = ##PcTc;
              CA50in = ##Bilat;
              CA51in = ##MultSurg;
              CA62in = ##CoSurg;
              CA66in = ##TeamSurg;
              CA80in = ##AsstSurg;
              if CA50in = '1';
                 camd50 = 1.50;
              endif;
              if CA51in = '1';
                 camd51 = 0.25;
              endif;
              if CA51in = '2';
                 camd51 = 0.50;
              endif;
              if CA51in = '3';
                 camd51 = 0.50;
              endif;
              camd54 = ##IntraOp/100;
              camd55 = ##PostOp/100;
              camd56 = ##PreOp/100;
              if CA62in = '1' or CA62in = '2';
                 camd62 = 6.25;
                 xxmd62 = 0.625;
              endif;
              if CA66in = '1' or CA62in = '2';
                 camd66 = 1.00;
              endif;
              if CA80in = '2';
                 camd80 = 0.16;
                 camd81 = 0.25;
                 camd82 = 0.13;
              endif;
       //
       // Endoscopic base code (used in multi-surg calcs)
       //
              caenba = ##EndoBase;
       //
       // Set the By Report flag
       //
              If CAmra = 0 and CAprof = 0 and CATech = 0 and CAanam = 0;
                 CArpt = 'Y';
              Else;
                 CArpt = 'N';
              Endif;

              CAfsR = 'Y';
              CAplac = DDpos;
              CAcpt4 = DDprCd;
              CAunts = DDunts;
              CAanes = 'U';

           Endsr;
      /End-Free
       //*** End Add *** 06/21/18 ****************************************
    C*** END ADD *** 09/02/11 *********************************************
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
     C                   else
     C                   eval      cafolu = rcfolu
    C*
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   eval      caprof = rcprof
     C*** BEG ADD *** 04/28/04 *********************************************
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
     C*** END ADD *** 04/28/04 *********************************************
    C*** BEG CHG *** 11/01/00 ****************************************
    C******************** Z-ADDRC92N2    CATECH
     C                   eval      catech = rc92n1
    C*** END CHG *** 11/01/00 ****************************************
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
     C                   if        caprof = 0
     C                   eval      caprof = rcmra * xxmd26
     C                   endif                                                  caprof = 0
     C*
     C                   if        catech = 0
     C                   eval      catech = rcmra * xxmd27
     C                   endif                                                  catech = 0
     C*
     C                   endif                                                  end *in60
     C*** END   ADD *** 03/28/01 **************************************
     C                   eval      caanam = rcanam
     C                   move      rcsurg        casurg
     C                   move      rcsupp        casupp
     C                   eval      caasc = rc92n1
     C                   move      rcstat        castat
     C                   move      rcrpt         carpt
     C*** BEG ADD *** 08/20/02 *********************************************
     C                   move      rcfsr         cafsr
     C*** END ADD *** 08/20/02 *********************************************
     C*** BEG ADD *** 09/25/04 *********************************************
     C                   exsr      hpsa
     C*** END ADD *** 09/25/04 *********************************************
     C*** BEG ADD *** 04/02/08 *********************************************
     C                   if        cabfhe <> 'Y'
     C*** BEG ADD *** 04/16/08 *********************************************
     C                             and dhprcd <> '99455' and
     C                             dhprcd <> '99456' and
     C                             dhprcd <> '97750' and
     C                             dhprcd <> '97545' and
     C                             dhprcd <> '97546' and
     C                             dhprcd <> '97799'
     C*** END ADD *** 04/16/08 *********************************************
     C                   exsr      under_served
     C                   endif
     C*** END ADD *** 04/02/08 *********************************************
    C*
     C*** BEG DEL *** 08/05/02 *********************************************
     C****************** eval      camd50 = *zeros
     C****************** eval      camd51 = *zeros
     C****************** eval      camd54 = *zeros
     C****************** eval      camd55 = *zeros
     C****************** eval      camd56 = *zeros
     C****************** eval      camd62 = *zeros
     C****************** eval      camd66 = *zeros
     C****************** eval      camd80 = *zeros
     C****************** eval      camd81 = *zeros
     C****************** eval      camd82 = *zeros
     C*** END DEL *** 08/05/02 *********************************************
     C*** BEG ADD *** 08/05/02 *********************************************
     C                   eval      camd50 = 1.00
     C                   eval      camd51 = 0.50
     C                   eval      camd54 = 0.70
     C                   eval      camd55 = 0.30
     C                   eval      camd56 = 0.10
     C                   eval      camd62 = 1.00
     C                   eval      camd65 = 0.75
     C                   eval      camd66 = 1.00
     C                   eval      camd80 = 0.20
     C                   eval      camd85 = 0.10
     C*** END ADD *** 08/05/02 *********************************************
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
     C*** BEG CHG *** 08/22/02 *********************************************
     C****************** move      dhrcfl        carcfl
     C                   move      'Y'           carcfl
     C*** END CHG *** 08/22/02 *********************************************
     C*** BEG ADD *** 08/22/02 *********************************************
     C*
     C                   when      eobtyp = 'D'
    C*
     C                   move      ddpos         caplac
     C                   eval      caunts = ddunts
     C                   move      ddpart        capart
     C*** BEG CHG *** 08/22/02 *********************************************
     C****************** move      ddrcfl        carcfl
     C                   move      'Y'           carcfl
     C*** END CHG *** 08/22/02 *********************************************
     C*** BEG ADD *** 03/18/09 *********************************************
    C*
     C                   when      eobtyp = 'H'
     C                   eval      caunts = duunts
     C                   move      dupos         caplac
     C                   eval      cahtos = euicn
     C                   move      durvcd        casvcd
     C
     C*** END ADD *** 03/18/09 *********************************************
    C*
     C                   endsl
     C*** END ADD *** 08/22/02 *********************************************
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
     C*** BEGIN ADD *** 04/04/08 *******************************************
    C*
    C*  Default to use service date for effective date for rules
     C                   eval      kkefdt = cafdat
    C*
    C* Set this bill as a non Home Health Bill
     C                   eval      homeHealthHit=*off
     C*
     C* Is this a Home Health Bill
     C     euky          chain     peu01                              99
     C*** Beg Add ***************** 04/03/08 *******************************
     C                   if        *in99 = *off
     C*** Beg Add ***************** 07/11/08 *******************************
     C                   eval      cahtos = euicn
     C*** End Add ***************** 07/11/08 *******************************
     C*
     C                   if        CAFDAT >= 1080301
     C                   if        EUICN = 'AO' or EUICN = 'AS' or
     C                             EUICN = 'HO' or EUICN = 'HS'
     C*** Beg Add ***************** 06/10/08 *******************************
     C                   if        eublty < '31A' or  eublty > '399'
     C*** End Add ***************** 06/10/08 *******************************
     C                   movel     euimst        caimst
     C                   exsr      OPPS_APC
     C                   move      'Y'           xxapc             1
     C*** Beg Add ***************** 06/10/08 *******************************
     C                   endif                                                  EUbltyp <'31A'
     C*** End Add ***************** 06/10/08 *******************************
     C                   endif                                                  EUICN = 'HO'
     C                   endif                                                  cafdat >= 1080301
     C*
     C                   endif                                                  *in99 = *off
     C                   if        xxapc <> 'Y'
     C*** End Add ***************** 04/03/08 *******************************
     C                   if        eublty>='31A' and eublty<='399' and
     C                             not *in99
     C*
     C                   call      'FA755R'
     C                   parm      eoco#         ##co#             3 0
     C                   parm      eoss#         ##ss#             9
     C                   parm      durate        ##prcd            5
     C                   parm      cafdat        ##efdt            7 0
     C                   parm      dupmod        ##mod1            2
     C                   parm      dumod2        ##mod2            2
     C                   parm      *zeros        ##alow            9 2
     C                   parm      'N'           ##rpt             1
     C                   parm      *zeros        ##rrn             9 0
     C                   parm      *blanks       ##rtcd            1
     C*
     C                   if        ##rtcd<>'F'
     C                   eval      homeHealthHit=*on
     C                   eval(h)   camra = ##alow * 1.25
     C                   eval      cafsr='Y'
     C                   move      dupos         caplac
     C                   move      ##rpt         carpt
     C                   endif
     C                   endif
     C
     C                   if        not homeHealthHit
     C*** END   ADD *** 04/04/08 *******************************************
     C                   move      durvcd        xxsvcd
     C*** BEG DEL *** 08/22/02 *********************************************
     C****************** movel     durate        xxcpt4
     C*** END DEL *** 08/22/02 *********************************************
     C                   move      '000'         xxunit
    C*
    C*  Default to use service date for effective date for rules
     C*** BEGIN DEL *** 04/04/08 *******************************************
     C**********         eval      kkefdt = cafdat
     C*** END   DEL *** 04/04/08 *******************************************
    C*
     C     svky          setgt     psv01
     C     nxtsv1        tag
     C                   readp     psv01                                  60
    C*
     C                   if        *in60 = *on
     C                   move      'Y'           cainvl
    C*
     C                   else
     C*** begin add *** 03/11/09 *******************************************
     C                   if        svalow>999999.99
     C                   eval      camra = svalow/10
     c                   else
     C*** end   add *** 03/11/09 *******************************************
     C                   eval      camra = svalow
     C*** end   add *** 03/11/09 *******************************************
     C                   endif
     C*** end   add *** 03/11/09 *******************************************
     C                   move      'Y'           cafsr
     C                   move      dupos         caplac
    C*
     C                   endif
    C*
     C*** BEGIN CHG *** 04/04/08 *******************************************
     C*****prstat        cabeq     'I'           nxtsv1
     C*** BEG   CHG *** 07/04/08 *******************************************
     C********     svstat        cabeq     'I'           nxtsv1
     C  N60svstat        cabeq     'I'           nxtsv1
     C*** END   CHG *** 07/04/08 *******************************************
     C                   endif
     C*** END   CHG *** 04/04/08 *******************************************
     C*** BEG ADD *** 04/03/08 *******************************************
     C                   endif
     C*** END ADD *** 04/03/08 *******************************************
    C*
     C     svcde         endsr
     C*** Beg Add *************** 04/03/08 *********************************
    C* ==================================================================
     C     OPPS_APC      begsr
     C*
     C*** BEG CHG *** 08/12/08 *********************************************
     C*****              movel     eumcar        kkprov
     C*****              movel     cafdat        kkefdt
     C                   movel     eumcar        kpprov
     C                   z-add     cafdat        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         kpefdt
     C*****oiky1         setgt     poi01                              99
     C*****oiky2         readpe    poi01                                  99
     C     opky1         setgt     pop01                              99
     C     opky2         readpe    pop01                                  99
     C*** END CHG *** 08/12/08 *********************************************
     C                   if        *in99 = *off
     C*
     C***** Beg add **** 09/06/10 *************************************
     C*
     C                   Movel     euaddt        @@efdt
     C*
     C                   if        eudcdt < 1091001
     C                   eval      eowind = opspwi
     C                   else
     C                   select
     C                   when      opsppy = 'Y' or opsppy = '2'
     C                   movel     opprov        xxprov           10
     C     xxprov        chain     PO201
     C*
     C     l2ky          setgt     PO201                              99
     C     l2ky1         readpe    PO201                                  99
     C  N99              eval      eowind = o2crwi
     C*
     C                   when      opsppy <> 'Y' and opsppy <> '2'
     C     l4ky          setgt     POA01                              99
     C     l4ky1         readpe    POA01                                  99
     C  N99              eval      eowind = oacrwi
     C*
     C                   if        *in99 = *on
     C     l4ky          setgt     POB01                              99
     C     l4ky1         readpe    POB01                                  99
     C  N99              eval      eowind = obcrwi
     C                   endif
     C                   endsl
     C                   endif
     C*
     C***** End Add **** 09/06/10 *************************************
      *** BEG ADD ***** 01/15/09 ******************************************
     C*Set the pointer for the FTRTP to get the rate values
     C*
     C*** BEG ADD *** 11/18/10 ****************************************
     C                   if        cafdat >= 1101001
     C                   movel     'MC'          @@st
     C                   Movel     'O'           @@type
     C                   movel     euaddt        @@bgdt            7 0
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe(n) prt01                                  99
     C                   eval      xxcf = rtapc
     C                   if        (cafdat >= 1090101) and (ophoqu = *blank)
     C                   eval      xxcf = rtapc1
     C                   endif
     C                   endif
     C*** END ADD *** 11/18/10 ****************************************
     C                   Movel     'TX'          @@st
     C                   Movel     'O'           @@type
     C                   movel     euaddt        @@bgdt            7 0
     C*
     C     rtky          setgt     Ftrtl2                             99
     C     rtky1         readpe    Ftrtl2                                 99
     C*** BEG ADD *** 11/18/10 ****************************************
     C                   if        cafdat < 1101001
     C*** END ADD *** 11/18/10 ****************************************
     C                   eval      xxcf = RTAPC
     C*** BEG ADD ***** 02/16/10 ******************************************
     C                   if        (cafdat >= 1090101) and (ophoqu = *blank)
     C                   eval      xxcf = rtapc1
     C                   endif
     C*** BEG ADD *** 11/18/10 ****************************************
     C                   endif
     C*** END ADD *** 11/18/10 ****************************************
     C*** END ADD ***** 02/16/10 ******************************************
      *** END ADD ***** 01/15/09 ******************************************
     C                   movel     durate        kkhcpc
     C                   move      cafdat        kkefdt
     C     apky1         setgt     pap01                              99
     C     apky2         readpe    pap01                                  99
     C*** BEG CHG ****** 08/06/08 **********************************************
     C********           if        *in99 = *off
     C*** BEG CHG ****** 08/12/09 **********************************************
     C*****************  if        *in99 = *off and apsi <> 'A'
    C***** Beg Chg *** 05/30/16 *********************************
     C*****              if        *in99 = *off and apsi <> 'A' and apsi
     C*****                        <> 'Y'
     C*
    C***** Beg Chg *** 06/10/16 *********************************
     C****************   if        *in99 = *off and apcsi <> 'A' and apcsi
     C****************             <> 'Y'
     C                   if        *in99 = *off and apcsi <> 'A' and apcsi
     C                             <> 'Y' and apcsi <> 'Q4'
    C***** End Chg *** 06/10/16 *********************************
     C*
    C***** End Chg *** 05/30/16 *********************************
     C*** END CHG ****** 08/12/09 **********************************************
     C*** END CHG ****** 08/06/08 **********************************************
    C***** Beg Chg *** 05/30/16 *********************************
     C*****              movel     apsi          caapsi
     C                   movel     apcsi         caapcsi
    C***** End Chg *** 05/30/16 *********************************
     C                   else
    C***** Beg Add *** 02/16/17 *********************************
     C* Effective 1/1/2016 load A and Y status indicators
     C                   if        EUaddt >= 1160101 and *in99 = *off
     C                             and (APcSi = 'A' or APcSi = 'Y' or
     C                             APcSi = 'Q4')
     C                   movel     apcsi         caapcsi
     C                   endif
    C***** End Add *** 02/16/17 *********************************
     C*** BEG CHG ****** 08/06/08 **********************************************
     C*******            if        *in99 = *on
     C*** BEG CHG ****** 08/12/09 **********************************************
     C*****************  if        *in99 = *on or (*in99 = *off and apsi = 'A')
    C***** Beg Chg *** 05/30/16 *********************************
     C*****              if        *in99 = *on or (*in99 = *off and (apsi = 'A'
     C*****                        or apsi = 'Y'))
     C*
    C***** Beg Chg *** 06/10/16 *********************************
     C***************    if        *in99 = *on or (*in99 = *off and (apcsi = 'A'
     C***************              or apcsi = 'Y'))
     C                   if        *in99 = *on or (*in99 = *off and (apcsi = 'A'
     C                             or apcsi = 'Y' or apcsi = 'Q4'))
    C***** End Chg *** 06/10/16 *********************************
     C*
    C***** End Chg *** 05/30/16 *********************************
     C*** END CHG ****** 08/12/09 **********************************************
     C*** END CHG ****** 08/06/08 **********************************************
     C                   movel     durate        xxcpt4
     C                   move      cafdat        kkefdt
     C*** BEG CHG ****** 08/06/08 **********************************************
     C*****prky          chain     ppr01                              99
     C     prky          setgt     ppr01                              99
     C     prky1         readpe    ppr01                                  99
     C                   if        *in99 = *off
     C                   exsr      loadamt
    C***** Beg Add *** 06/10/16 *********************************
     C                   if        apcsi = 'Q4'
     C                   eval      camra = catech
     C                   endif
    C***** End Add *** 06/10/16 *********************************
     C                   else
     C**** search in FMRCP file
     C                   exsr      recu
     C                   endif
     C*** END CHG ****** 08/06/08 **********************************************
     C                   goto      endtag
     C*** BEG ADD ****** 08/06/08 **********************************************
     C                   endif                                                  apsi = 'A'
     C*** END ADD ****** 08/06/08 **********************************************
     C                   endif                                                  *in99 = '0'
     C                   endif                                                  *in99 = '0'
     C*** BEG CHG ************ 08/12/08 *************************************
     C*****              eval      xxoutl = cachrg * oioccr
     C                   eval      xxoutl = cachrg * opoccr
     C*** END CHG ************ 08/12/08 *************************************
     C*** BEG ADD ************ 12/19/08 *************************************
     C*** BEG CHG ************ 09/06/10 *************************************
     C*****              if        opwind <> 0
     C                   if        eowind <> 0
     C*** END CHG ************ 09/06/10 *************************************
     C*** END ADD ************ 12/19/08 *************************************
     C                   if        xxoutl > (3.5*apalow)
     C*** BEG CHG ************ 10/14/08 *************************************
     C*****************  if        xxoutl > cachrg
     C*****************  eval      xxoutl = .5 * (xxoutl - cachrg)
     C                   if        xxoutl > apalow
     C                   eval      xxoutl = .5 * (xxoutl - apalow)
     C*** END CHG ************ 10/14/08 *************************************
     C                   else
     C*** BEG CHG ************ 10/14/08 *************************************
     C****************   eval      xxoutl = .5 * (cachrg - xxoutl)
     C                   eval      xxoutl = .5 * (apalow - xxoutl)
     C*** END CHG ************ 10/14/08 *************************************
     C                   endif
     C*** BEG ADD ************ 07/31/08 *************************************
     C                   else
     C                   eval      xxoutl = *zeros
     C*** END ADD ************ 07/31/08 *************************************
     C                   endif
     C*** BEG ADD ************ 08/12/08 *************************************
     C*** write to FDCAP/CAOUTL
     C                   eval      caoutl = xxoutl
     C*** END ADD ************ 08/12/08 *************************************
     C                   if        euimst = 'N' or euimst = 'P'
     C*** BEG ADD ****** 10/02/08 **********************************************
     C                             or euimst = 'I'
     C*** END ADD ****** 10/02/08 **********************************************
     C*** BEG CHG ************ 08/12/08 *************************************
     C                   If        optype = '16'
     C                             and %subst(opacbs : 1 : 3) = ' '
     C                             OR optype = '17'
     C                             and %subst(opacbs : 1 : 3) = ' '
     C******             If        oirsch = '1'
     C******             eval      camra = (((((oiprwi*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)*1.071) +
     C******                                + xxoutl) * 2
     C*** BEG CHG ************ 09/06/10 *************************************
     C**************     eval      camra = (((((opwind*.60*xxcf) +
     C**************                       (.40*xxcf))*aprel)*1.071) +
     C*** BEG CHG *** 01/15/09 *******************************************
     C**************                        + xxoutl) * 2
     C**************                        + xxoutl) * RTMUL1/100
     C*** END CHG *** 01/15/09 *******************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   if        rtefdt < 1120101
     C*** End Add *** 02/25/12 *******************************************
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = (((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)*1.071) +
     C                                      + xxoutl) * RTMUL1/100
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = (((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)*1.071) +
     C******                                + xxoutl) * RTMUL1/100
     C*** End Chg *** 04/06/16 ********************************************
     C*** END CHG ************ 09/06/10 *************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   else
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = (((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)*1.071) +
     C                                      + xxoutl) * RTMUL1
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = (((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)*1.071) +
     C******                                + xxoutl) * RTMUL1
     C*** End Chg *** 04/06/16 *******************************************
     C                   endif
     C*** End Add *** 02/25/12 *******************************************
     C*** write to FDCAP/CARSCH
     C*** BEG CHG *** 09/06/10 *******************************************
     C******             eval      carsch = (((opwind*.60*xxcf) +
     C******                                  (.40*xxcf))*aprel)*1.071
     C                   eval      carsch = (((eowind*.60*xxcf) +
     C                                        (.40*xxcf))*aprel)*1.071
     C*** END CHG *** 09/06/10 *******************************************
     C                   else
     C******             eval      camra = ((((oiprwi*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)        +
     C******                                + xxoutl) * 2
     C*** BEG CHG *** 09/06/10 *******************************************
     C***********        eval      camra = ((((opwind*.60*xxcf) +
     C***********                          (.40*xxcf))*aprel)        +
     C*** BEG CHG *** 01/15/09 *******************************************
     C***********                           + xxoutl) * 2
     C***********                           + xxoutl) * RTMUL1/100
     C*** END CHG *** 01/15/09 *******************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   if        rtefdt < 1120101
     C*** End Add *** 02/25/12 *******************************************
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = ((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)        +
     C                                      + xxoutl) * RTMUL1/100
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = ((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)        +
     C******                                + xxoutl) * RTMUL1/100
     C*** End Chg *** 04/06/16 *******************************************
     C*** END CHG *** 09/06/10 *******************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   else
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = ((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)        +
     C                                      + xxoutl) * RTMUL1
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = ((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)        +
     C******                                + xxoutl) * RTMUL1
     C*** End Chg *** 04/06/16 *******************************************
     C                   endif
     C*** End Add *** 02/25/12 *******************************************
     C                   endif
     C                   endif
     C*** BEG CHG ****** 10/02/08 **********************************************
     C*****************  if        euimst = 'I' or euimst = 'S'
     C                   if        euimst = 'S'
     C*** END CHG ****** 10/02/08 **********************************************
     C******             If        oirsch = '1'
     C                   If        optype = '16'
     C                             and %subst(opacbs : 1 : 3) = ' '
     C                             OR optype = '17'
     C                             and %subst(opacbs : 1 : 3) = ' '
     C******             eval      camra = (((((oiprwi*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)*1.071) +
     C******                                + xxoutl) * 1.3
     C*** BEG CHG *** 09/06/10 *******************************************
     C******             eval      camra = (((((opwind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)*1.071) +
     C*** BEG CHG *** 01/15/09 *******************************************
     C***************                       + xxoutl) * 1.3
     C******                                + xxoutl) * RTMUL2/100
     C*** END CHG *** 01/15/09 *******************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   if        rtefdt < 1120101
     C*** End Add *** 02/25/12 *******************************************
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = (((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)*1.071) +
     C                                      + xxoutl) * RTMUL2/100
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = (((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)*1.071) +
     C******                                + xxoutl) * RTMUL2/100
     C*** End Chg *** 04/06/16 *******************************************
     C*** END CHG *** 09/06/10 *******************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   else
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = (((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)*1.071) +
     C                                      + xxoutl) * RTMUL2
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = (((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)*1.071) +
     C******                                + xxoutl) * RTMUL2
     C*** End Chg *** 04/06/16 *******************************************
     C                   endif
     C*** End Add *** 02/25/12 *******************************************
     C*** write to FDCAP/CARSCH
     C*** BEG CHG *** 09/06/10 *******************************************
     C*******            eval      carsch = (((opwind*.60*xxcf) +
     C*******                              (.40*xxcf))*aprel)*1.071
     C                   eval      carsch = (((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)*1.071
     C*** END CHG *** 09/06/10 *******************************************
     C                   else
     C******             eval      camra = ((((oiprwi*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)        +
     C******                                + xxoutl) * 1.3
     C*** BEG CHG *** 09/06/10 *******************************************
     C*******            eval      camra = ((((opwind*.60*xxcf) +
     C*******                              (.40*xxcf))*aprel)        +
     C*** BEG CHG *** 01/15/09 *******************************************
     C************                          + xxoutl) * 1.3
     C*******                               + xxoutl) * RTMUL2/100
     C*** END CHG *** 01/15/09 *******************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   if        rtefdt < 1120101
     C*** End Add *** 02/25/12 *******************************************
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = ((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)        +
     C                                      + xxoutl) * RTMUL2/100
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = ((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)        +
     C******                                + xxoutl) * RTMUL2/100
     C*** End Chg *** 04/06/16 *******************************************
     C*** END CHG *** 09/06/10 *******************************************
     C*** Beg Add *** 02/25/12 *******************************************
     C                   else
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = ((((eowind*.60*xxcf) +
     C                                     (.40*xxcf))*aprel)        +
     C                                      + xxoutl) * RTMUL2
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C******             eval      camra = ((((eowind*.60*xxcf) +
     C******                               (.40*xxcf))*aprel)        +
     C******                                + xxoutl) * RTMUL2
     C*** End Chg *** 04/06/16 *******************************************
     C                   endif
     C*** End Add *** 02/25/12 *******************************************
     C*** END CHG ************ 08/12/08 *************************************
     C                   endif
     C                   endif
     C*** BEG ADD ************ 12/19/08 *************************************
     C                   else
     C                   if        euimst = 'N' or euimst = 'P'
     C                             or euimst = 'I'
     C******             If        optype = '16'
     C******                       and %subst(opacbs : 1 : 3) = ' '
     C******                       OR optype = '17'
     C******                       and %subst(opacbs : 1 : 3) = ' '
     C                   eval      camra = apalow * 2
     C*** write to FDCAP/CARSCH
     C****               eval      carsch = (((opwind*.60*xxcf) +
     C****                                 (.40*xxcf))*aprel)*1.071
     C******             else
     C                   eval      camra = apalow * 2
     C******             endif                                                  optype = '16'
     C                   endif                                                  euimst = 'N'/'P'/'I'
     C                   if        euimst = 'S'
     C******             If        optype = '16'
     C******                       and %subst(opacbs : 1 : 3) = ' '
     C******                       OR optype = '17'
     C******                       and %subst(opacbs : 1 : 3) = ' '
     C******             eval      camra = apalow * 1.3
     C*** write to FDCAP/CARSCH
     C****               eval      carsch = (((opwind*.60*xxcf) +
     C****                                 (.40*xxcf))*aprel)*1.071
     C******             else
     C                   eval      camra = apalow * 1.3
     C******             endif                                                  optype = '16'
     C                   endif                                                  euimst = 'S'
     C
     C                   endif                                                  opwind <> 0
     C*** END ADD ************ 12/19/08 *************************************
     C     endtag        tag
     C                   endsr
    C* ==================================================================
     C*** End Add *************** 04/03/08 *********************************
    C* ==================================================================
     C*
     C*** BEG ADD ****** 11/14/08 **********************************************
     C*
    C********************************************************************
    C** ASC Payment subroutine
    C********************************************************************
     C     ASC_pymt      begsr
     C*
     C                   move      *zeros        temp1             9 2
     C                   move      *zeros        temp2             6 4
     C                   move      *zeros        NatReimAmt        9 2
     C                   move      *zeros        GeoAdjReim        9 2
     C                   move      *zeros        TotReim           9 2
     C                   move      *zeros        Devamt            9 2
     C                   move      *zeros        Servamt           9 2
     C                   move      *zeros        TDIReimSrv        9 2
     C*
     C*** Determining the geographic adjustment
     C*
     C                   z-add     asalow        NatReimAmt
     C                   z-add     asdper        ##dper            5 2
     C                   movel     aspi          caaspi
     C*
     C     ehky          chain     peh01                              99
     C                   if        *in99 = *off
     C                   movel     ehcbsa        ##cbsa
     C                   endif
     C*
     C     wgky          setgt     pwg01
     C     wgky1         readpe    pwg01                                  99
     C                   if        *in99 = *off
     C                   z-add     wgwind        ##wind            5 4
     C*** BEG ADD *** 03/16/09 **************************************************
     C                   else
     C                   movel     ehrst         ##key
     C     stky          chain     pst01                              99
     C                   if        *in99 = *off
     C                   movel     stname        ##nam
     C     wgky2         setgt     pwg02                              99
     C     wgky21        readpe    pwg02                                  99
     C  N99              z-add     wgwind        ##wind
     C                   endif                                                  *in99 = *off
     C*** END ADD *** 03/16/09 **************************************************
     C                   endif
     C*
     C                   eval      temp1 = NatReimAmt * 0.5
     C                   eval      temp2 = ##wind + 1
     C                   eval      GeoAdjReim = temp1 * temp2
     C*
     C***  Calculate Reimbursements
     C*
     C***  Case#1: Non-device intensive procedure when no implants were used
     C***  in the procedure
     C*
     C                   if        ehimst = 'N' and asdev <> 'Y'
     C                   eval      TotReim = GeoAdjReim * 2.35
     C                   eval      camult = 2.35
     C                   endif
     C*
     C***  Case#2: Non-device intensive procedure when implants were used
     C***  in the procedure and seperate reimbursement for implants is not
     C***  requested.
     C*
     C                   if        ehimst = 'I' and asdev <> 'Y'
     C                   eval      TotReim = GeoAdjReim * 2.35
     C                   eval      camult = 2.35
     C                   endif
     C*
     C***  Case#3: Non-device intensive procedure when implants were used
     C***  in the procedure and seperate reimbursement for implants is
     C***  requested.
     C*
     C*** Beg Chg *** 10/08/12 ************************************************
     C********           if        ehimst = 'S' and asdev <> 'Y'
     C                   if        (ehimst = 'S' or ehimst = 'P')
     C                              and asdev <> 'Y'
     C*** End Chg *** 10/08/12 ************************************************
     C                   eval      TotReim = GeoAdjReim * 1.53
     C                   eval      camult = 1.53
     C                   endif
     C*
     C***  Case#4: Device intensive procedure when implants were used
     C***  in the procedure and seperate reimbursement for implants is not
     C***  requested.
     C*
     C                   if        ehimst = 'I' and asdev = 'Y'
     C*
     C     asky          setgt     pap01                              99
     C     asky1         readpe    pap01                                  99
     C                   if        *in99 = *off
     C                   eval      Devamt = apalow * (##dper/100)
     C                   endif
     C                   eval      Servamt = GeoAdjReim - Devamt
     C                   eval      TDIReimSrv = Servamt * 2.35
     C                   eval      TotReim = Devamt + TDIReimSrv
     C                   eval      camult = 2.35
     C                   endif
     C*
     C***  Case#5: Device intensive procedure when implants were used
     C***  in the procedure and seperate reimbursement for implants is
     C***  requested.
     C*
     C                   if        (ehimst = 'P' or ehimst = 'S')
     C                              and ASDEV = 'Y'
     C*
     C     asky          setgt     pap01                              99
     C     asky1         readpe    pap01                                  99
     C                   if        *in99 = *off
     C                   eval      Devamt = apalow * (##dper/100)
     C                   endif
     C                   eval      Servamt = GeoAdjReim - Devamt
     C                   eval      TDIReimSrv = Servamt * 2.35
     C                   eval      TotReim = TDIReimSrv
     C                   eval      camult = 2.35
     C                   endif
     C*
     C*** BEG DEL *** 06/30/09 *********************************************
     C*******            if        TotReim <> *zeros
     C*** END DEL *** 06/30/09 *********************************************
     C                   eval      camra = TotReim
     C*** BEG DEL *** 06/30/09 *********************************************
     C*******            endif
     C*** END DEL *** 06/30/09 *********************************************
     C*
     C                   endsr
     C*** END ADD ****** 11/14/08 **********************************************
     C
     Opca01     e            rca01
