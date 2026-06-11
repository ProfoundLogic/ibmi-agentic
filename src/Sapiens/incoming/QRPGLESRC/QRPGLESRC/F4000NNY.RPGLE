    H**********************************************************************
     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
    H**********************************************************************
    H* PROGRAM: F4000NNY                                                  *
    H* DESCRIPTION: ADJUDICATION PROGRAM FOR NY                           *
    H**********************************************************************
    H*  CREATED: 09/19/02  C:79   PROJECT: NY006                          *
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H** DO NOT INSTALL THIS PROGRAM WITHOUT INSTALLING F4000N.  A CHANGE **
    H** TO THE PHARM CALCS FOR SUPPORT 58250 REQUIRES THIS               **
    H*                                                                    *
     H*  08/11/21 C:295 Project: NY056                                     *
    H*                 Changes to add hospital name as key feild.         *
    H*                                                                    *
     H*  02/25/21 C:079 Project: NY053                                     *
    H*                 Changes how we accesss FTEMPNY for Detox           *
    H*                                                                    *
     H*  12/17/19 C:212 Project: NY048                                     *
    H*                 Changes to load for PT/OT and ACP                  *
    H*                                                                    *
     H*  11/11/19 C:212 Project: NY047                                     *
    H*                 for PHAR, change logic related to product type     *
    H*                 now being handled through rules                    *
    H*                                                                    *
     H*  06/03/19 C:212 Project: NY046                                     *
    H*                 Load AddOn code                                    *
    H*                                                                    *
     H*  04/15/19 C:079 Project: NY044                                     *
    H*                 Use new conversion factors file                    *
    H*                                                                    *
     H*  05/02/18 C:180 Project: MC420                                     *
    H*                 Increase xxWAP size, remove div by 1000.           *
    H*                                                                    *
     H*  05/11/17  C:309 Support: 47070                                    *
     H*                  Fix "Transfer" Calculation in loadAPRDRG subroutine*
    H*                                                                    *
     H*  04/05/17  C:309 NY041                                             *
     H*                  Adding FA761R call if service ='ID'               *
    H*                                                                    *
     H*  09/01/15 C:079 Support: 129/33851                                 *
     H*                 Lookup procedure code correctly                    *
    H*                                                                    *
     H*  09/11/13 C:296 Project: NY030                                     *
     H*                 Delete processing for MOD 51 in subroutine PMOD    *
    H*                                                                    *
     H*  06/26/13 C:274 Project: NY029                                     *
     H*                 Modify per diem for Detox types of service         *
    H*                                                                    *
     H*  08/31/11 C:251 Project: NY027                                     *
     H*                 Modify per diem for Detox types of service         *
    H*                                                                    *
     H*  02/07/11 C:079 Support: MC129 75657                               *
     H*                 Fix chiro calculations                             *
    H*                                                                    *
     H*  04/19/10 C:227 Project: NY020                                     *
     H*                 2010 NY Hospital Fee Schedule                      *
    H*                                                                    *
     H*  04/21/09 C:198 Project: NY017                                     *
     H*                 Corrected short stay to pay when low trim point is *
     H*                 1 and no of service units on screen is also 1.     *
    H*                                                                    *
     H*  10/28/08 C:079 Project: NY018                                     *
    H*                 DME allowance is not based on specialty code       *
    H*                                                                    *
     H*  05/09/07 C:188 Support: MC144 58250                               *
    H*                 Prevent result too large error                     *
     H*                                                                    *
     H*  07/22/06 C:79  Project: NY012                                     *
     H*                 Look up the region correctly                       *
     H*                                                                    *
     H*  03/23/05 C:79  Support: MC129 46193                               *
     H*                 Make sure PT evals are processed 1st               *
    H*                                                                    *
     H*  12/22/04 C:79  Support: 44873                                     *
    H*                 Load emergency room rate                           *
    H*                                                                    *
     H*  12/19/04 C:79  Support: 45094                                     *
    H*                 Load region = 4 for TUPP providers                 *
    H*                                                                    *
     H*  12/11/04 C:79  Support: 45010                                     *
    H*                 Load anes conv factor regardless of specialty      *
    H*                                                                    *
     H*  12/08/04 C:79  Support: 44945                                     *
    H*                 Only calc long stay if Avg LOS > 0                 *
    H*                                                                    *
     H*  12/07/04 C:79  Support: 44872                                     *
    H*                 1. Load CABMRI to exclude from rule count          *
    H*                 2. Load PT Conversion Factor                       *
    H*                 3. Pay Rad/Lab, etc                                *
    H*                                                                    *
     H*  09/07/04 C:79  Project: NY009                                     *
    H*                 Load ASC PAS rate to CAMRA                         *
    H*                 Use Phy FS for Outpatient CPT/HCPCS codes          *
    H*                 Load Inpatient rates                               *
    H*                                                                    *
     H*  04/28/04 C:79  Project: FS277                                     *
    H*                 For UCR Codes load RC1A2, RCFSR                    *
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
    H**********************************************************************
    H*  INDICATOR USAGE:                                                  *
    H**********************************************************************
     Ffdcal13   uf   e           k disk
     Ffaeolg    if   e           k disk
     F                                     infds(dbfds)

     Ffacap     o    e           k disk    usropn
     F                                     rename(pca01:pcawk2)
    F*
     Ffmprlny   if   e           k disk
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
     Fftuml     if   e           k disk
     F****** fmzrlny   if   e           k disk
     Ffmzrl1ny  if   e           k disk
     Ffmp1l     if   e           k disk
     Ffapglny   if   e           k disk
     Ffaeul     if   e           k disk
     Fftdglny   if   e           k disk
     Ffahrlny   if   e           k disk
     Ffmexlny   if   e           k disk
     Ffmp2lny   if   e           k disk
     Ffmp2l1ny  if   e           k disk
     F                                     rename(pp201:pp202)
     Fftadl1ny  if   e           k disk
     F***** 08/11/21 *************** Beg Chg ******
     F*faial1ny  if   e           k disk
     Ffaial2ny  if   e           k disk
     F*fteml1ny  if   e           k disk
     Ffteml2ny  if   e           k disk
     F***** 08/11/21 *************** End Chg ******
     Fftrtl1    if   e           k disk
     Ffmcdl     if   e           k disk
     Fftctl1ny  if   e           k disk
     Ffmcvl1ny  if   e           k disk
    F*
    D*
     D qqf             s              7    dim(50)
    D*
     D date            s              7  0 dim(999)
     D mod             s              2    dim(4) ascend
     D xxzip           s                   like(zrfzcd)
     D xxrgn#          s                   like(zrrgn#)
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
     D
     D dbfds           ds
     D dbrrn                 397    400b 0
     D
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
     D camd85          s              3  2
     D camd65          s              3  2
     D catype          s              1
     D hlddat          s              7  0
     D kkdseq          s                   like(cadseq)
     D kkefdt          s                   like(frefdt)
     D z1efdt          s                   like(zrefdt)
     D kkymd           s                   like(frefdt)
     D kkdrg           s                   like(dgdrg)
     D kkopct          s                   like(pgopct)
     D kkpsgp          s                   like(pgpsgp)
     D xxInlier        s                   like(camra)
     D xxperdiem       s                   like(camra)
     D xxpyschpmnt     s                   like(camra)
     D xxpyschpmnt1    s                   like(camra)
     D xxpyschpmnt2    s                   like(camra)
     D xxpyschpmnt3    s                   like(camra)
     D xxpyschpmnt4    s                   like(camra)
     D xxALC           s                   like(camra)
     D xxShortOutl     s                   like(camra)
     D xxLongOutl      s                   like(camra)
     D xxCostOutl      s                   like(camra)
     D xxTranfer       s                   like(camra)
     D xxDRGClass      s                   like(camra)
     D xxinldays       s                   like(duunts)
     D xxoutldays      s                   like(duunts)
     D xxacutedays     s                   like(duunts)
     D xxalcdays       s                   like(duunts)
     D xxline1         s                   like(camra)
     D xxline2         s                   like(camra)
     D xxline3         s                   like(camra)
     D xxline4         s              7  4
     D xxline5         s                   like(camra)
     D xxline6         s                   like(camra)
     D xxline7         s                   like(camra)
     D xxline8         s                   like(camra)
     D xxline9         s                   like(camra)
     D xxline10        s              5  2
     D xxlinea1        s                   like(camra)
     D xxlinea2        s                   like(camra)
     D xxlineL3        s              7  4
     D xxlineL4        s                   like(camra)
     D xxlineL5        s                   like(dgulos)
     D xxlineL6        s              7  4
     D xxline5L        s              7  4
     D xxlineL10       s                   like(camra)
     D xxlineS4        s                   like(camra)
     D xxlineS5        s              7  4
     D xxlineS7        s                   like(dgulos)
     D xxlineS9        s              5  2
     D xxlineS10       s                   like(camra)
     D xxline11        s                   like(camra)
     D xxline12        s                   like(camra)
     D xxline13        s                   like(camra)
     D xxline14        s              5  2
     D xxline15        s              5  2
     D xxlineP7        s                   like(duunts)
     D kkdcdt          s                   like(eudcdt)
     D kkdrg1          s                   like(durate)
     D xxsurcharge     s                   like(iac002)
     D x1inlier        s                   like(iac002)
     D x1line1         s                   like(iac002)
     D x1line2         s                   like(adsiw1)
     D x1line3         s             13  4
     D x1line4         s                   like(iac007)
     D x1line5         s                   like(iac008)
     D x1line6         s             13  4
     D yyline1         s                   like(emc001)
     D yyline2a        s                   like(duunts)
     D yyline2c        s                   like(duunts)
     D yyline3         s                   like(emc001)
     D yyperdiem       s                   like(emc001)
     D zzline1a        s                   like(duunts)
     D zzline1c        s                   like(duunts)
     D zzline10        s                   like(iac002)
     D zzline11        s                   like(iac002)
     D zzline12        s                   like(iac002)
     D zzline13        s                   like(iac002)
     D zzline14        s                   like(iac002)
     D zzline15a       s                   like(iac002)
     D zzline3         s                   like(iac002)
     D zzline4         s                   like(adsiw1)
     D zzline5         s             13  4
     D zzline6         s                   like(adalos)
     D zzline7         s                   like(iac002)
     D zzline8         s                   like(iac002)
     D zzline9         s                   like(iac002)
     D zztransfer      s                   like(iac002)
     D z1line1         s                   like(iac002)
     D z1line10        s                   like(iac002)
     D z1line2a        s                   like(iac002)
     D z1line2b        s                   like(iac002)
     D z1line2c        s                   like(iac002)
     D z1line2f        s                   like(iac002)
     D z1line3         s                   like(iac002)
     D z1line4         s                   like(iac005)
     D z1line5         s                   like(iac002)
     D z1line6a        s                   like(iac002)
     D z1line6b        s                   like(iac004)
     D z1line6c        s                   like(iac002)
     D z1line9         s                   like(iac002)
     D z1outlier       s                   like(iac002)
     D kkmcar          s                   like(iaopct)
     D kkast           s                   like(eoast)
     D kkcert          s                   like(emopct)
     D********* 08/11/21 *************** Beg Add **************
     D kkdesc          s                   like(emdesc)
     D kkhnam          s                   like(iadesc)
     D********* 08/11/21 *************** Beg Add **************
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
     D xxmd26          s              3  2
     D xxmd27          s              3  2
     D xxinvl          s              1
     D xxmra           s                   like(camra)
     D xxndc           s                   like(dpndc)
     D xxrcn           s                   like(carcn)
     D xxymd           s                   like(ruefdt)
     D xxawp           s                   like(CAawp)
     D kkrcn           s                   like(carcn)
     D DOBdate         S              8  0
     D DOB_ISO         S               D   Inz DATFMT(*ISO)
     D DOB_USA         S               D   Inz DATFMT(*USA)
     D Today_ISO       S               D   Inz DATFMT(*ISO)
     D Today_USA       S               D   Inz DATFMT(*USA)
     D Age             S              3  0
     D savDUunts       s                   like(duunts)
     D kkSrc           s                   like(CVsrc)
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
     C     caky2         klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C                   kfld                    kkrcn
     C                   kfld                    kkdseq
    C*
    C*  Procedure Code file
     C     prky1         klist
     C                   kfld                    qqco#
     C                   kfld                    xxcpt4
    C*
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
     C                   kfld                    kkefdt
     C*
     C*  Usual and Customary Modifier Percent File
     C*
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
     C*
     C*  State Zip Code/Region File
     C*
     C     zrky          klist
     C                   kfld                    qqco#
     C                   kfld                    kkefdt
     C*
     C*  State ASC PAS group File
     C*
     C     pgky          klist
     C                   kfld                    kkopct
     C                   kfld                    kkpsgp
     C                   kfld                    kkefdt
     C*
     C     dgky          klist
     C                   kfld                    kkdrg
     C                   kfld                    kkefdt
     C*
     C     dgky1         klist
     C                   kfld                    kkdrg
     C*
     C     hrky          klist
     C                   kfld                    kkopct
     C                   kfld                    kkefdt
     C*
     C     hrky1         klist
     C                   kfld                    kkopct
     C*
     C     exky          klist
     C                   kfld                    kkopct
     C                   kfld                    kkefdt
     C*
     C     exky1         klist
     C                   kfld                    kkopct
     C*
     C     pgky1         klist
     C                   kfld                    kkopct
     C                   kfld                    kkpsgp
     C*
     C*  Hospital header File
     C*
     C     euky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
     C*
     C*  Provider auxiliary file
     C*
     C     p1ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
    C*
     C     p2ky          klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C                   kfld                    xxcmsv
     C                   kfld                    cafdat
    C*
     C     p2ky1         klist
     C                   kfld                    qqfein
     C                   kfld                    qqmod
     C*
     C     iaky          klist
     C                   kfld                    kkmcar
     C***** 08/11/21 ********* Beg Add ************************
     C                   kfld                    kkhnam
     C***** 08/11/21 ********* Beg Add ************************
     C                   kfld                    kkdcdt
     C*
     C     iaky1         klist
     C                   kfld                    kkmcar
     C***** 08/11/21 ********* Beg Add ************************
     C                   kfld                    kkhnam
     C***** 08/11/21 ********* Beg Add ************************
     C*
     C     adky          klist
     C                   kfld                    kkdrg1
     C                   kfld                    kkdcdt
     C*
     C     adky1         klist
     C                   kfld                    kkdrg1
     C*
     C     rtky          klist
     C                   kfld                    kkast
     C                   kfld                    kkefdt
     C*
     C     rtky1         klist
     C                   kfld                    kkast
     C*
     C*
     C     emky          klist
     C                   kfld                    kkcert
     C                   kfld                    kkdcdt
     C*
     C     pcky          klist
     C                   kfld                    EOSS#
     C*
     C     moky          klist
     C                   kfld                    EUDIA1
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
    C*
    C* Find the recon number.  If this is a recon.
     C     eoky          chain     peo01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      xxrcn = eorcn
     C                   if        eobtyp = 'H'
     C     euky          chain(N)  peu01                              99
     C*
     C                   eval      kkopct = eumcar
     C                   eval      kkdcdt = eudcdt
     C*********** 08/11/21 ************* beg add ***************************
     C                   eval      kkhnam = euhnam
     C*********** 08/11/21 ************* beg add ***************************
     C*
     C                   if        EUicn  = 'ID'
     C                             and EUdcDt >= 1150601
     C*
     C                   movel     dbrrn         @@rrn             9 0
     C                   eval      @@desc = 'All incl room and board'
     C*
     C                   call      'FA761R'
     C                   parm                    @@rrn
     C                   parm      100           @@rvcd            3 0
     C                   parm      *blanks       @@hcpc            5
     C                   parm      *blanks       @@ndc             3
     C                   parm                    @@desc           25
     C                   parm      *blanks       @@rtcd            1
     C                   endif
     C*
     C                   endif
    C*
     C                   else
     C                   eval      xxrcn = *zero
    C*
     C                   endif
    C*
     C     p1ky          chain     pp101                              99
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
    C* Load the FDCAP fields for later use
    C*
     C                   eval      kkrcn = carcn
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
     C                   endif
    C*
     C                   movel     'H'           catype
    C*
     C                   exsr      svcd
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
     C                   if        dhprcd = '97001' or dhprcd = '97003'
     C                             and eobtyp = 'N'
     C                   eval      caamt = 500
     C                   endif
     C                   if        dhprcd = '97002' or dhprcd = '97004'
     C                             and eobtyp = 'N'
     C                   eval      caamt = 400
     C                   endif
     C                   if        dhprcd >= '99201' and dhprcd <= '99204'
     C                             and eobtyp = 'N' and eospcd='CHI'
     C                   eval      caamt = 500
     C                   endif
     C                   if        dhprcd = '99212'
     C                             and eobtyp = 'N' and eospcd='CHI'
     C                   eval      caamt = 500
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
     C*
     C* Clear FACAP to prevent carryover of values to next record.
     C*
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
    C* Release the record lock
    C*
     C                   except    rca01
    C*
     C     caky2         chain     pca01                              99
    C*
     C                   if        *in99 = *off
     C                   eval      camraa = xcmra
     C                   update    pca01
     C                   endif
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
    C*  Default to use service date for effective date for rules
    C*
     C                   move      cafdat        kkefdt
    C*
    C*  Process the fee scheduled codes here
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
     C     prky          setgt     ppr01
     C     nxtproc       tag
     C     prky1         readpe    ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
    C*
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
      /Free
                         Clear pcv01;
      /End-free
    C*
     C* Load amounts based on service zip of the provider
    C*
     C                   exsr      loadamt
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
     C                   eval      cafolu = prfolu
    C*
    C* Set flags for anes, assist surg, no cut, etc
    C*
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a5         canct
     C                   move      pr1a6         CAaddon
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
    C*
    C* Load modifier reductions for processing hierchy
    C*
     C                   exsr      modval
    C*
     C                   clear                   caplac
    C*
     C                   endif
    C*
     C     prstat        cabeq     'I'           nxtproc
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
    C********************************************************************
    C** LOADASC SUBROUTINE                             L O A D A S C    *
    C********************************************************************
     C     loadAsc       begsr
     C*
     C                   eval      kkpsgp = dupsgp
     C*
     C     pgky          setgt     ppg01
     C     prevrec       tag
     C     pgky1         readpe    ppg01                                  99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        pgstat <> 'I'
     C                   eval      camra = pgalow
     C*
     C                   else
     C                   goto      prevrec
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
    C********************************************************************
    C** LOADOUTP SUBROUTINE                             L O A D A M T H *
    C********************************************************************
     C     loadoutp      begsr
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
    C*
    C*
     C     prstat        cabeq     'I'           nxtpr3
    C*
     C                   endif
      /Free

                         Select;

                         When EOspCd = 'PHT' or EOspCd = 'OT'
                                   or EOspCd = 'PT';
                            kkSrc = 'MED';
                         When EOspCd = 'CHI';
                            kkSrc = 'CHI';
                         When EOspCd = 'POD' or EOspCd = 'PO1';
                            kkSrc = 'POD';
                         When EOspCd = 'PSY';
                            kkSrc = 'PSY';
                         Other;
                            kkSrc = 'MED';
                         Endsl;

                         Clear pcv01;

                         Setgt (DUrate:kkSrc:kkEfDt) pcv01;
                         Readpe (DUrate:kkSrc) pcv01;
      /End-free
     C*
     C                   movel     p1zip         xxzip
     C*
     C* Default to region 1 (lowest)
     C*
     C                   eval      xxrgn# = 1
     C*
     C     zrky          setgt     pzr01
     C                   readp     pzr01                                  99
     C                   eval      z1efdt = zrefdt
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        xxzip >= zrfzcd and xxzip <= zrtzcd
     C                             and zrefdt = z1efdt
     C                   eval      xxrgn# = zrrgn#
     C                   leave
     C                   endif
     C*
     C                   readp     pzr01                                  99
     C*
     C                   enddo
     C*
     C*
     C* Load amount fields based on region
     C*
     C                   if        xxrgn# = 1
     C                   eval      camra  = prmra
     C                   eval      catech = pr92n1
     C                   if        durate >= '70001' and durate <= '89999'
     C                   eval      camra  = pr92n1
     C                   endif
     C*
     C* MD conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 6.49;
                         When CAfDat >= 1190401;
                            CAconv = CVregI;
                         Endsl;
      /End-Free
     C*
     C                   endif
     C*
     C                   if        xxrgn# = 2
     C                   eval      camra  = pr92n9
     C                   eval      catech = pr92nf
     C                   if        durate >= '70001' and durate <= '89999'
     C                   eval      camra  = pr92nf
     C                   endif
     C*
     C* MD conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 6.80;
                         When CAfDat >= 1190401;
                            CAconv = CVregII;
                         Endsl;
      /End-Free
     C*
     C                   endif
     C*
     C                   if        xxrgn# = 3
     C                   eval      camra  = pr92nk
     C                   eval      catech = pr92nq
     C                   if        durate >= '70001' and durate <= '89999'
     C                   eval      camra  = pr92nq
     C                   endif
     C*
     C* MD conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 7.78;
                         When CAfDat >= 1190401;
                            CAconv = CVregIII;
                         Endsl;
      /End-Free
     C*
     C                   endif
     C*
     C                   if        xxrgn# = 4
     C                   eval      camra  = pr92nv
     C                   eval      catech = pr92o2
     C                   if        durate >= '70001' and durate <= '89999'
     C                   eval      camra  = pr92o2
     C                   endif
     C*
     C* MD conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 8.45;
                         When CAfDat >= 1190401;
                            CAconv = CVregIV;
                         Endsl;
      /End-Free
     C*
     C                   endif
    C*
     C                   move      pr1a4         cabmri
    C*
    C* Load relative value units for PT codes
    C*
     C                   if        durate >= '97001' and durate <= '97800'
     C                   eval      caanam = pr92ob
     C                   endif
     C                   if        durvcd >= 450 and durvcd <= 451
    C*
    C* Hard code service code so clients can enter rev codes but have
    C* the program use the EMERG cost to charge ratio set up for their
    C* providers.
     C                   movel     '000EMERG'    xxcmsv
     C                   move      '000'         xxcmsv
    C*
     C     p2ky          setgt     pp201
     C                   readp     pp201                                  60
    C*
     C                   if        *in60 = *off and cafein = p2fein
     C                             and camod = p2mod and xxcmsv = p2sv01
     C                             and cafdat >= p2efdt
     C
    C*
    C* Calculate payment on percentage
    C*
     C                   eval      cascam = p2am01
    C*
     C                   else
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
     C                   endsr
    C********************************************************************
    C** INITHOSP    SUBROUTINE                          I N I T H O S P *
    C********************************************************************
     C     initHosp      begsr
     C*
     C                   eval      cahtos = euicn
      /Free
           If DUrvcd <> 100  and EUdcdt >= 1150601 ;
              savDUunts  = DUunts;
           EndIf;
      /End-Free
     C                   eval      xxacutedays = duunts - xxalcdays
     C                   eval      xxinldays = 0
     C                   eval      xxlineL5 = 0
     C                   eval      xxlinea1 = 0
     C                   eval      xxlinea2 = 0
     C                   eval      xxalc  = 0
     C                   eval      xxalcdays = 0
     C                   eval      xxlongoutl = 0
     C                   eval      xxshortoutl = 0
     C*
     C                   eval      cainlr = 0
     C                   eval      catran = 0
     C                   eval      caoutl = 0
     C                   eval      x1inlier = 0
     C                   eval      zztransfer = 0
     C                   eval      z1outlier = 0
     C*
     C                   if        eudcdt < 1091201
     C     hrky          setgt     phr01
     C     hrky1         readpe    phr01                                  99
     C                   endif
     C*
     C                   endsr
    C********************************************************************
    C** LOADADJUSTMENT SUBROUTINE         L O A D A D J U S T M E N T S *
    C********************************************************************
     C     loadAdjustmentbegsr
     C*
     C                   select
     C*
     C                   when      casvcd = 964
     C                   eval      z1line2a = catotc
     C                   when      casvcd = 963
     C                   eval      z1line2b = catotc
     C                   when      casvcd >= 010 and casvcd <= 021
     C                   eval      z1line2c = catotc
     C*
     C                   endsl
     C*
     C                   endsr
    C**********************************************************************
    C** LOADAPRDRG     SUBROUTINE                   L O A D A P R D R G *
    C**********************************************************************
     C     loadAPRDRG    begsr
     C*
     C* DRG Payments
     C*
     C                   eval      kkmcar = eumcar
     C*********** 08/11/21 ************* beg add ***************************
     C                   eval      kkhnam = euhnam
     C*********** 08/11/21 ************* beg add ***************************
     C     iaky          setgt     pia01
     C     iaky1         readpe    pia01                                  99
     C*
     C                   if        *in99 = *off
     C**************
     C* Inlier cost
     C**************
     C*
     C*Discharge Case Payment Rate
     C*
     C                   eval      x1line1 = iac002
     C*
     C*Per Case Service Intensity Weight for DRG Classification
     C*
     C                   if        EUdcdt >= 1150601
     C                   Eval      kkdrg1 = %Editc(EUdrg#:'X')
     C                   else
     C                   movel(p)  durate        kkdrg1
     C                   EndIf
     C     adky          setgt     pad01
     C     adky1         readpe    pad01                                  98
     C                   if        *in98 = *off
     C                   if        EUdcdt >= 1150601

     C                   select
     C                   when      EUdgsv = '1'
     C                   eval      x1line2 = adsiw1
     C                   when      EUdgsv = '2'
     C                   eval      x1line2 = adsiw2
     C                   when      EUdgsv = '3'
     C                   eval      x1line2 = adsiw3
     C                   when      EUdgsv = '4'
     C                   eval      x1line2 = adsiw4
     C                   endsl

     C                   else
     C                   select
     C                   when      dudgsv = '1'
     C                   eval      x1line2 = adsiw1
     C                   when      dudgsv = '2'
     C                   eval      x1line2 = adsiw2
     C                   when      dudgsv = '3'
     C                   eval      x1line2 = adsiw3
     C                   when      dudgsv = '4'
     C                   eval      x1line2 = adsiw4
     C                   endsl
     C                   endif
     C                   endif
     C*
     C*Case Mix Adjusted Discharge Payment
     C*
     C                   eval      x1line3 = x1line1 * x1line2
     C*
     C*Direct Medical Education (DME) Add-On
     C*
     C                   eval      x1line4 = iac007
     C*
     C*Capital and Non-Comparable Add-Ons Cost Per Discharge
     C*
     C                   eval      x1line5 = iac008
     C*
     C*Inlier DRG Payment prior to Public Goods Pool Surcharge
     C*
     C                   eval      x1line6 = x1line3 + x1line4 + x1line5
     C*
     C*Public Goods Surcharge
     C*
     C                   movel     eoast         kkast
     C     rtky          setgt     prt01
     C     rtky1         readpe    prt01                                  98
     C                   if        *in98 = *off
     C                   eval      xxsurcharge = x1line6 * rtelsc
     C                   endif
     C*
     C*Payment to Hospital
     C*
     C                   eval      x1inlier = x1line6
     C**************
     C* Transfers
     C**************
     C                   if        eutrst = '01' or eutrst = '02'
     C*
     C*Load the total Number of Days in Stay
     C*
      /Free
           If DUrvcd =  100  and EUdcdt >= 1150601 ;
              zzline1a = savDUunts;
           Else;
      /End-Free

     C                   eval      zzline1a = duunts

     C                   EndIf
     C*
     C*Load the number of Transfer Days excluding ALC
     C*
     C                   eval      zzline1c = zzline1a
     C*
     C*Discharge Case Payment Rate
     C*
     C                   eval      zzline3 = iac002
     C*
     C*Per Case Service Intensity Weight for DRG Classification
     C*
     C                   if        EUdcdt >= 1150601
     C                   Eval      kkdrg1 = %Editc(EUdrg#:'X')
     C                   else
     C                   move      durate        kkdrg1
     C                   endif
     C     adky          setgt     pad01
     C     adky1         readpe    pad01                                  98
     C                   if        *in98 = *off
     C                   if        EUdcdt >= 1150601

     C                   select
     C                   when      EUdgsv = '1'
     C                   eval      zzline4 = adsiw1

     C                   when      EUdgsv = '2'
     C                   eval      zzline4 = adsiw2

     C                   when      EUdgsv = '3'
     C                   eval      zzline4 = adsiw3

     C                   when      EUdgsv = '4'
     C                   eval      zzline4 = adsiw4
     C                   endsl

     C                   else
     C                   select
     C                   when      dudgsv = '1'
     C                   eval      zzline4 = adsiw1
     C                   when      dudgsv = '2'
     C                   eval      zzline4 = adsiw2
     C                   when      dudgsv = '3'
     C                   eval      zzline4 = adsiw3
     C                   when      dudgsv = '4'
     C                   eval      zzline4 = adsiw4
     C                   endsl
     C                   endif
     C                   endif
     C*
     C*Case Mix Adjusted Discharge Payment
     C*
     C                   eval      zzline5 = zzline3 * zzline4
     C*
     C*Group Average Arithmetic Inlier Length of Stay for DRG
     C*
     C                   eval      zzline6 = adalos
     C*
     C*Average Inlier Cost Per Day
     C*
     C                   eval      zzline7 = zzline5/zzline6
     C*
     C*Transfer Adjustment Factor
     C*
     C                   if        zzline1c = 1 and zzline6 = 1
     C                   eval      zzline8 = 1
     C                   elseif    zzline1c >= 1 and zzline6 > 1
     C                   eval      zzline8 = 1.2
     C                   endif
     C*
     C*Transfer DRG Cost Per Day
     C*
     C                   eval      zzline9 = zzline7 * zzline8
     C*
     C*Case Payment Capital Per Diem
     C*
     C                   eval      zzline10 = iac009
     C*
     C*Total Transfer Cost Per Diem
     C*
     C                   eval      zzline11 = zzline9 + zzline10
     C*
     C*Transfer DRG Payment excluding DME
     C*
     C                   eval      zzline12 = zzline11 * zzline1c
     C*
     C*Direct Medical Education (DME) Add-on
     C*
     C                   eval      zzline13 = iac007
     C*
     C*Transfer Payment Amount before ALC
     C*
     C                   eval      zzline14 = zzline12 + zzline13
     C*
     C*Inlier DRG Before ALC
     C*
     C                   eval      zzline15a = x1inlier
     C*
     C*Total Transfer Payment Prior to Public Goods Pool Surcharge
     C*
     C                   if        zzline14 < zzline15a
     C                   eval      zztransfer = zzline14
     C                   else
     C                   eval      zztransfer = zzline15a
     C                   endif
     C*
     C*Public Goods Surcharge
     C*
     C                   eval      xxsurcharge = zztransfer * rtelsc
     C*
     C                   endif
     C****************
     C* Cost Outliers
     C****************
     C*
     C*Total Inpatient Gross Charges Per Patient Revenue Code 0001
     C*
     C                   eval      z1line1 = eochrg
     C*
     C*Total Inpatient Gross Charges Per Patient Revenue Code 0001
     C*
     C                   if        euicn = 'ID'
     C                   exsr      loadadjustment
     C                   endif
     C*
     C*Total Adjustments
     C*
     C                   eval      z1line2f = z1line2a + z1line2b + z1line2c
     C*
     C*Net Inpatient Gross Charges
     C*
     C                   eval      z1line3 = z1line1 - z1line2f
     C*
     C*High Cost Charge ConverterAcute_Col 5
     C*
     C                   eval      z1line4 = iac005
     C*
     C*Net Inpatient Gross Charges Converted to Costs
     C*
     C                   eval      z1line5 = z1line3 * z1line4
     C***********************
     C*Threshold Calculation
     C***********************
     C*
     C*APR-DRG Cost Outlier Threshold
     C*
     C                   eval      z1line6a = adoutl
     C*
     C*Institution-Specific Adjustment Factor
     C*
     C                   eval      z1line6b = iac004
     C*
     C*Adjusted Cost Outlier Threshold
     C*
     C                   eval      z1line6c = z1line6a * z1line6b
     C************************
     C*High Cost Payment Test
     C************************
     C                   if        (z1line5 > z1line6c) and
     C                             (eutrst <> '01' and eutrst <> '02')
     C                   eval      z1outlier = z1line5 - z1line6c
     C*
     C*Total Inlier
     C*
     C                   eval      z1line9 = x1inlier
     C*
     C*Total Payment to Provider at 100%
     C*
     C                   eval      z1line10 = z1outlier + z1line9
     C*
     C*Public Goods Surcharge
     C*
     C                   eval      xxsurcharge = z1line10 * rtelsc
     C*
     C                   endif
     C*
     C*Update fields of FDCAP
     C*
     C                   endif
     C*
     C                   eval      caoutl = z1outlier
     C*
     C                   if        eutrst = '01' or eutrst = '02'
     C                   eval      catran = zztransfer
     C                   eval      camra = catran + caoutl
     C                   else
     C                   eval      cainlr = x1inlier
     C                   eval      camra = cainlr + caoutl
     C                   endif
     C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
     C*
     C                   endsr
    C********************************************************************
    C** LOADDRG     SUBROUTINE                            L O A D D R G *
    C********************************************************************
     C     loadDRG       begsr
     C*
     C* DRG Payments
     C*
     C*
     C                   move      durate        kkdrg
     C     dgky          setgt     pdg01
     C     dgky1         readpe    pdg01                                  99
     C*
     C                   if        *in99 = *off
     C**************
     C* Inlier cost
     C**************
     C                   if        duunts >= dgltrm
     C*
     C                   if        duunts > dghtrm
     C                   eval      xxinldays = dghtrm
     C                   else
     C                   eval      xxinldays = duunts
     C                   endif
     C*
     C* Blended case mix neutral rate inc basic malp. exc other Pros or
     C* Top 20 DRG exc. oper. pros.
     C*
     C                   if        dgtp20 = 'Y'
     C                   eval      xxline1 = hrc003
     C                   else
     C                   eval      xxline1 = hrc002
     C                   endif
     C*
     C* Blended case mix neutral pros. adj.
     C*
     C                   eval      xxline2 = hrc012
     C*
     C* Blended case mix neutral rate or Top 20 DRG Payment
     C*
     C                   eval      xxline3 = xxline1 + xxline2
     C*
     C* Per Case Service intensity weight for DRG Classification
     C*
     C                   eval      xxline4 = dgsiwc
     C*
     C* Blended case mix neutral rate or Top 20 DRG Weighted oper comp.
     C*
     C                   eval      xxline5 = xxline3 * xxline4
     C*
     C* Capital cost per case less productivity and eff. plus HIV
     C*
     C                   eval      xxline6 = hrc004
     C*
     C* Capital cost per case prospective adjustment
     C*
     C                   eval      xxline7 = hrc014
     C*
     C* Inlier DRG subtotal
     C*
     C                   eval      xxline8 = xxline5 + xxline6 + xxline7
     C*
     C* Excess physicians malpractice prospective adj.
     C*
     C                   eval      xxline9 = hrc016
     C*
     C* SPARCS rate per case
     C*
     C                   eval      xxline10 = hrc009
     C*
     C* Inlier payment prior to Public Goods Surcharge
     C*
     C                   eval      xxinlier = xxline8 + xxline9 + xxline10
     C*
     C**********************************
     C* Alternate level of care payment
     C**********************************
     C*
     C                   if        xxalcdays > 0
     C*
     C* Alternate level of care operating per diem
     C*
     C                   eval      xxlinea1 = hrc011
     C*
     C* Alternate level of care days
     C*
     C                   eval      xxlinea2 = xxalcdays
     C*
     C* Alternate level of care payment prior to Public Goods Surcharge
     C*
     C                   eval      xxalc = xxlinea1 * xxlinea2
     C*
     C                   endif
     C*
     C                   endif
     C********************
     C* Long Stay Outlier
     C********************
     C                   if        duunts - xxalcdays >= dghtrm
     C                   if        dgulos > 0 or dgdlos > 0
     C*
     C* Long stay outlier days
     C*
     C                   eval      xxOutldays = duunts - xxalcdays - dghtrm
     C*
     C* Long stay group neutral cost per discharge
     C*
     C                   eval      xxline2 = hrc001
     C*
     C* Per case SIW for DRG
     C*
     C                   eval      xxlineL3 = dgsiwc
     C*
     C* Subtotal
     C*
     C                   eval      xxlineL4 = xxline2 * xxlineL3
     C*
     C* Group avg arith. inlier LOS for DRG (Upstate or Downstate)
     C* Default to downstate in case there is no indicator
     C*
     C                   if        dgdlos > 0
     C                   eval      xxlineL5 = dgdlos
     C                   else
     C                   eval      xxlineL5 = dgulos
     C                   endif
     C*
     C                   if        exgrp = 1 or exgrp = 2 or exgrp = 4
     C                   eval      xxlineL5 = dgulos
     C                   endif
     C*
     C                   if        exgrp = 5 or exgrp = 6
     C                   eval      xxlineL5 = dgdlos
     C                   endif
     C*
     C* Subtotal
     C*
     C                   eval      xxline6 = xxlineL4 / xxlinel5
     C*
     C* Subtotal
     C*
     C                   eval      xxline8 = xxline6 * .50
     C*
     C* Long stay outlier DRG cost per day
     C*
     C                   eval      xxlineL10 = xxline8 * .55
     C*
     C* Long stay outlier DRG prior to Public Goods Surcharge
     C*
     C                   eval      xxlongoutl = xxlineL10 * xxOutldays
     C*
     C                   endif
     C*
     C                   endif
     C*************
     C* Short Stay
     C*************
     C                   if        duunts < dgltrm or duunts = 1
     C*
     C* Blended case mix neutral rate inc basic malp. exc other Pros or
     C* Top 20 DRG exc. oper. pros.
     C*
     C                   if        dgtp20 = 'Y'
     C                   eval      xxline2 = hrc003
     C                   else
     C                   eval      xxline2 = hrc002
     C                   endif
     C*
     C* Blended case mix neutral pros. adj.
     C*
     C                   eval      xxline3 = hrc012
     C*
     C* Blended case mix neutral rate or Top 20 DRG Payment
     C*
     C                   eval      xxlines4 = xxline2 + xxline3
     C*
     C* Per Case Service intensity weight for DRG Classification
     C*
     C                   eval      xxlineS5 = dgsiwc
     C*
     C* Subtotal
     C*
     C                   eval      xxline6 = xxlines4 * xxlineS5
     C*
     C* Group avg arithmetic inlier LOS for DRG (upstate/downstate)
     C*
     C                   eval      xxlineS7 = dgdlos
     C                   if        exgrp = 1 or exgrp = 2 or exgrp = 4
     C                   eval      xxlineS7 = dgulos
     C                   endif
     C*
     C                   if        exgrp = 5 or exgrp = 6
     C                   eval      xxlineS7 = dgdlos
     C                   endif
     C*
     C* Avg inlier cost per day
     C*
     C                   eval      xxline8 = xxline6 / xxlineS7
     C*
     C* Inlier DRG subtotal
     C*
     C                   eval      xxlineS9 = 1.00
     C*
     C* Short stay oulier DRG codst per day
     C*
     C                   eval      xxlineS10 = xxline8 * xxlineS9
     C*
     C* Short stay and transfer capital per diem
     C*
     C                   eval      xxline11 = hrc007
     C*
     C* Short stay outlier cost per day
     C*
     C                   eval      xxline12 = xxlineS10 + xxline11
     C*
     C* Short stay outlier payment
     C*
     C                   eval      xxline13 = xxline12 * duunts
     C*
     C* Excess physicians malp. pros. adj.
     C*
     C                   eval      xxline14 = hrc016
     C*
     C* SPARCS rate per case
     C*
     C                   eval      xxline15 = hrc009
     C*
     C* Inlier payment prior to Public Goods Surcharge
     C*
     C                   eval      xxShortoutl = xxline13 + xxline14 + xxline15
     C                   endif
     C*
     C* Transfer payment
     C*
     C***** On hold for now. This is determined, it seems this should be
     C***** indicated on the bill
     C*
     C* High Cost
     C*
     C***** On hold for now. This is determined, it seems, when certain
     C***** Revenue codes are billed
     C                   if        duunts >= dgltrm and duunts <= dghtrm
     C                   eval      camra = xxinlier
     C                   endif
     C*
     C                   if        duunts < dgltrm
     C                             or (dgltrm = 1 and duunts = 1)
     C                   eval      camra = xxshortoutl
     C                   endif
     C*
     C                   if        duunts > dghtrm
     C                   eval      camra = xxinlier + xxlongoutl
     C                   endif
     C*
     C* Other services/supplies are included in the DRG payment
     C*
     C                   else
     C                   eval      camra = 0
     C*
     C                   endif
     C*
     C                   endsr
    C********************************************************************
    C** LOADAPRPERDIEM   SUBROUTINE         L O A D A P R P E R D I E M *
    C********************************************************************
     C     loadAPRPerDiembegsr
     C*
     C*Load Acute Per Diem
     C*
     C                   movel(p)  eumcar        kkmcar
     C     iaky          setgt     pem01
     C     iaky1         readpe    pem01                                  99
     C*
     C                   if        *in99 = *off
     C*
     C                   select
     C*
     C                   when      euicn = 'IS'
     C                   eval      yyline1 = emc001
     C*
     C                   when      euicn = 'IP'
     C                   eval      yyline1 = emc003
     C*
     C                   when      euicn = 'IC'
     C                   eval      yyline1 = emc005
     C*
     C                   when      euicn = 'IM'
     C                   eval      yyline1 = emc009
     C*
     C                   when      euicn = 'IW'
     C                   eval      yyline1 = emc011
     C*
     C                   when      euicn = 'IX'
     C                   eval      yyline1 = emc007
     C*
     C                   endsl
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      yyline2a = duunts
     C*
     C* Total acute care days excluding ALC
     C*
     C                   eval      yyline2c = yyline2a
     C*
     C* Total Exempt Unit/Hospital Acute Care Payment Before Public
     C* Goods Pool surcharge
     C*
     C                   eval      yyline3 = yyline1 * yyline2c
     C*
     C* Public Goods Surcharge
     C*
     C                   eval      xxsurcharge = yyline3 * rtelsc
     C*
     C* Payment to Hospital
     C*
     C                   eval      yyperdiem = yyline3
     C*
     C                   endif
     C*
     C                   eval      camra = yyperdiem
     C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
     C*
     C                   endsr
    C********************************************************************
    C** LOADPERDIEM   SUBROUTINE                  L O A D P E R D I E M *
    C********************************************************************
     C     loadPerdiem   begsr
     C*
     C* Exempt units get the per diem calcs based on the unit
     C*
     C                   if        durvcd >= 100 and durvcd <= 199
     C*
     C                   select
     C******************
     C* Exempt Hospital
     C******************
     C                   when      euicn = 'IH'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc022 + hrc023 + hrc027 + hrc029
     C                             + hrc030
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc021 + xxalcdays + hrc027 + hrc030
     C                   endif
     C********************
     C* Exempt Psych unit
     C********************
     C                   when      euicn = 'IP'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc032 + hrc033 + hrc037 + hrc039
     C                             + hrc040
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc031 + xxalcdays + hrc037 + hrc040
     C                   endif
     C*******************
     C* Exempt AIDS unit
     C*******************
     C                   when      euicn = 'IA'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc042 + hrc043 + hrc047 + hrc049
     C                             + hrc050
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc041 + xxalcdays + hrc047 + hrc050
     C                   endif
     C**********************
     C* Exempt Alcohol unit
     C**********************
     C                   when      euicn = 'IL'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc052 + hrc053 + hrc057 + hrc059
     C                             + hrc060
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc051 + xxalcdays + hrc057 + hrc060
     C                   endif
     C********************
     C* Exempt Rehab unit
     C********************
     C                   when      euicn = 'IR'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc062 + hrc063 + hrc067 + hrc069
     C                             + hrc070
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc061 + xxalcdays + hrc067 + hrc070
     C                   endif
     C*
     C*
     C***********************
     C* Exempt Epilepsy unit
     C***********************
     C                   when      euicn = 'IE'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc072 + hrc073 + hrc077 + hrc079
     C                             + hrc080
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc071 + xxalcdays + hrc077 + hrc080
     C                   endif
     C********************
     C* Exempt Other unit
     C********************
     C                   when      euicn = 'IO'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc082 + hrc083 + hrc087 + hrc089
     C                             + hrc090
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc081 + xxalcdays + hrc087 + hrc090
     C                   endif
     C****************************
     C* Exempt Medical Rehab unit
     C****************************
     C                   when      euicn = 'IM'
     C*
     C* Total exempt unit hospital acute care per diem
     C*
     C                   eval      xxline6 = hrc092 + hrc093 + hrc097 + hrc099
     C                             + hrc100
     C*
     C* Exempt unit/hosp stay days
     C*
     C                   eval      xxlineP7 = duunts - xxalcdays
     C*
     C* Total exempt unit/hosp acute care payment before surcharge
     C*
     C                   eval      xxperdiem = xxline6 * xxlineP7
     C*
     C* ALC Payment
     C*
     C                   if        xxalcdays > 0
     C                   eval      xxalc = hrc091 + xxalcdays + hrc097 + hrc100
     C                   endif
     C*
     C                   endsl
     C*
     C                   eval      camra = xxperdiem +xxalc
     C
     C                   else
     C                   eval      camra = 0
     C*
     C                   endif
     C*
     C                   endsr
     C
    C*
    C********************************************************************
    C** LOADPSYCHEXMPT  SUBROUTINE           L O A D P S Y C H E X M P T*
    C********************************************************************
     C     loadPsychExmptbegsr
     C*
     C* Total Acute Care Days
     C                   eval      xxline1 = duunts - xxalcdays
     C*
     C* SIW for Physch DRG
     C                   movel(p)  durate        kkdrg1
     C     adky          setgt     pad01
     C     adky1         readpe    pad01                                  98
     C                   if        *in98 = *off
     C                   Select
     C                   When      dudgsv = '1'
     C                   eval      xxlineL3 = ADSIW5
     C                   When      dudgsv = '2'
     C                   eval      xxlineL3 = ADSIW6
     C                   When      dudgsv = '3'
     C                   eval      xxlineL3 = ADSIW7
     C                   When      dudgsv = '4'
     C                   eval      xxlineL3 = ADSIW8
     C                   Endsl
     C                   Endif                                                  SIW for Physch DRG
     C*
     C* Age Adjustment Factor
     C     pcky          chain     pcd01
     C                   if        %found
     C                   eval      srdti = CDDTOB                               DOB
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         DOBdate
     C                   movel     DOBdate       DOB_USA
     C                   eval      DOB_ISO = DOB_USA
     C                   time                    dsdttm
     C                   movel     dsdate        Today_USA                      Today
     C                   eval      Today_ISO = Today_USA
     C                   eval      Age=%DIFF(Today_ISO:DOB_ISO:*YEARS)          Calculate age
     C*
     C                   If        Age <= 17
     C                   eval      xxline4 = 1.0872
     C                   Else
     C                   eval      xxline4 = 1.0000
     C                   Endif
     C                   Else                                                   FMCDP rec not found
     C                   eval      xxline4 = 1.0000
     C                   Endif
     C*
     C* Ment. Ret. Factor
     C                   eval      xxline5L = 1.0000
     C*
     C* Comorbidity Factor
     C     moky          chain     mor01
     C                   if        %found
     C                   eval      xxlineL6 = CTFACT
     C                   Endif
     C*
     C                   eval      kkcert = eumcar
     C                   eval      kkdcdt = eudcdt
     C********* Beg chg ******* 08/11/21 *****************************
     C                   eval      kkdesc = euhnam
     C********* End chg ******* 08/11/21 *****************************
     C*
       //*** Beg Chg *** 02/25/21 ******************************************
     C**** emky          chain     pem01
     C****               if        %found
        //********* Beg chg ******* 08/11/21 *****************************
          //***** Setgt (kkCert:KkDcDt) pem01;
          //******  Readpe (kkCert) pem01;
          //*******If Not %Eof(FTEML1NY);
          Setgt (kkCert:KKdesc:KkDcDt) pem01;
          Readpe (kkCert:kkdesc:kkDcDt) pem01;
          If Not %Eof(FTEML2NY);
        //********* End chg ******* 08/11/21 *****************************
       //*** End Chg *** 02/25/21 ******************************************
     C*
     C* Physch Oper Billing Rate
     C                   eval      xxline2 = EMC003
     C*
     C* Psych Non-Oper Billing Rate
     C                   eval      xxline8 = EMNOPER
     C*
     C* Psych ECT Payment
     C                   eval      xxline9 = EMECTP
     C                   endif
     C*
     C* Per diem
     C                   eval      xxperdiem =(xxline2 * xxlineL3 * xxline4*  +
     C                                         xxline5L * xxlineL6)
     C*
     C* Psych payment, part 1
     C                   eval      xxpyschpmnt1 = *zero
     C                   eval      xxpyschpmnt2 = *zero
     C                   eval      xxpyschpmnt3 = *zero
     C                   eval      xxpyschpmnt4 = *zero
     C                   Select
     C                   When      xxline1 >=1 and xxline1 <=4
     C                   eval      xxpyschpmnt1 = 1.2 * xxline1 *xxperdiem
     C                   When      xxline1 >=5 and xxline1 <=11
     C                   eval      xxpyschpmnt1 = 1.2 * 4 *xxperdiem
     C                   eval      xxpyschpmnt2 = 1.0 *(xxline1-4) *xxperdiem
     C                   When      xxline1 >=12 and xxline1 <=22
     C                   eval      xxpyschpmnt1 = 1.2 * 4 *xxperdiem
     C                   eval      xxpyschpmnt2 = 1.0 * 7 *xxperdiem
     C                   eval      xxpyschpmnt3 = .96 * (xxline1-11) *xxperdiem
     C                   When      xxline1 >22
     C                   eval      xxpyschpmnt1 = 1.2 * 4 *xxperdiem
     C                   eval      xxpyschpmnt2 = 1.0 * 7 *xxperdiem
     C                   eval      xxpyschpmnt3 = .96 * 11 *xxperdiem
     C                   eval      xxpyschpmnt4 = .92 * xxline1-11 *xxperdiem
     C                   Endsl                                                  Pysch payment
     C                   eval      xxpyschpmnt = xxpyschpmnt1 + xxpyschpmnt2
     C                                          + xxpyschpmnt3 + xxpyschpmnt4
     C                                          + (xxline8 * xxline1) + xxline9
     C*
     C* XXalc
     C                   if        xxalcdays > 0
     C                   eval      xxalc = xxalcdays * EMC004
     C                   endif
     C*
     C* Psych payment, part 2
     C                   eval      xxpyschpmnt = xxpyschpmnt + xxalc
     C*
     C* Psych payment, loaded
     C                   eval      camra = xxpyschpmnt
     C*
     C                   endsr
     C
     C
    C********************************************************************
    C** LOADDETOXPD   SUBROUTINE                  L O A D P E R D I E M *
    C********************************************************************
     C     loadDetoxPD   begsr
     C*
     C                   eval      kkcert = eumcar
     C                   eval      kkdcdt = eudcdt
     C                   eval      kkdesc = euhnam
     C*
       //*** Beg Chg *** 02/25/21 ******************************************
     C**** emky          chain     pem01
     C****               if        %found
        //********* Beg chg ******* 08/11/21 *****************************
          //***** Setgt (kkCert:KkDcDt) pem01;
          //***** Readpe (kkCert) pem01;
          //***** If Not %Eof(FTEML1NY);
          Setgt (kkCert:KKdesc:KkDcDt) pem01;
          Readpe (kkCert:kkdesc:kkDcDt) pem01;
          If Not %Eof(FTEML2NY);
        //********* End chg ******* 08/11/21 *****************************
       //*** End Chg *** 02/25/21 ******************************************
     C*
     C                   if        euicn = 'MD'
     C*
     C                   select
     C*
     C                   when      duunts >= 1 and duunts <= 5
     C                   eval      camra = (emc011 + emdtcc) * duunts
     C*
     C                   when      duunts >= 6 and duunts <= 10
     C                   eval      camra = (emc011 + emdtcc) * 0.5 * duunts
     C*
     C                   other
     C                   eval      camra = 0
     C                   endsl                                                  select 'MD' units
     C*
     C                   elseif    euicn = 'MS'
     C*
     C                   select
     C*
     C                   when      duunts >= 1 and duunts <= 5
     C                   eval      camra = (emc012 + emdtcc) * duunts
     C*
     C                   when      duunts >= 6 and duunts <= 10
     C                   eval      camra = (emc012 + emdtcc) * 0.5 * duunts
     C*
     C                   other
     C                   eval      camra = 0
     C                   endsl                                                  select 'MS' units
     C*
     C                   endif                                                  EUICN type
     C*
     C                   else
     C                   eval      camra = 0
     C                   endif                                                  %found fteml1ny
     C*
     C                   endsr
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
     C* Default to region 1 (lowest)
     C*
     C                   eval      xxrgn# = 1
     C*
     C     zrky          setgt     pzr01
     C                   readp     pzr01                                  99
     C*
     C                   eval      z1efdt = zrefdt
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        xxzip >= zrfzcd and xxzip <= zrtzcd
     C                             and zrefdt = z1efdt
     C                   eval      xxrgn# = zrrgn#
     C                   leave
     C                   endif
     C*
     C                   readp     pzr01                                  99
     C*
     C                   enddo
     C*
     C*
     C* Pay region 4 for TUPP participants
     C*
     C                   if        %subst(p1cnty:1:5) = 'TUPP '
     C                   eval      xxrgn# = 4
     C                   endif
     C*
     C* Load amount fields based on specialty/region
     C*
     C                   select
     C*
     C                   when      eospcd = 'CHI'
     C*
     C                   if        xxrgn# = 1
     C*
     C* Region 1 - Chiro
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92n2
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 4.44;
                         When CAfDat >= 1190401;
                            CAconv = CVregI;
                         Endsl;
      /End-Free
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92n5
     C                   eval      catech = pr92n6
     C                   eval      caanam = pranam
     C*
     C                   endif                                                  if xxrgn = 1
     C*
     C                   if        xxrgn# = 2
     C*
     C* Region 2 - Chiro
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92na
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 4.65;
                         When CAfDat >= 1190401;
                            CAconv = CVregII;
                         Endsl;
      /End-Free
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92ng
     C                   eval      catech = pr92nh
     C                   eval      caanam = pr92nd
     C*
     C                   endif                                                  if xxrgn = 2
     C*
     C                   if        xxrgn# = 3
     C*
     C* Region 3 - Chiro
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nl
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 5.32;
                         When CAfDat >= 1190401;
                            CAconv = CVregIII;
                         Endsl;
      /End-Free
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92nr
     C                   eval      catech = pr92ns
     C                   eval      caanam = pr92no
     C*
     C                   endif                                                  if xxrgn = 3
     C*
     C                   if        xxrgn# = 4
     C*
     C* Region 4 - Chiro
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nw
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 5.78;
                         When CAfDat >= 1190401;
                            CAconv = CVregIV;
                         Endsl;
      /End-Free
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92o3
     C                   eval      catech = pr92o4
     C                   eval      caanam = pr92nz
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C                   when      eospcd = 'POD' or eospcd = 'PO1'
     C*
     C                   if        xxrgn# = 1
     C*
     C* Region 1 - Podiatry
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92n3
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92n7
     C                   eval      catech = pr92n8
     C                   eval      caanam = pranam
     C*
     C                   endif                                                  if xxrgn = 1
     C*
     C                   if        xxrgn# = 2
     C*
     C* Region 2 - Podiatry
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nb
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92ni
     C                   eval      catech = pr92nj
     C                   eval      caanam = pr92nd
     C*
     C                   endif                                                  if xxrgn = 2
     C*
     C                   if        xxrgn# = 3
     C*
     C* Region 3 - Podiatry
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nm
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92nt
     C                   eval      catech = pr92nu
     C                   eval      caanam = pr92no
     C*
     C                   endif                                                  if xxrgn = 3
     C*
     C                   if        xxrgn# = 4
     C*
     C* Region 4 - Podiatry
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nx
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92o5
     C                   eval      catech = pr92o6
     C                   eval      caanam = pr92nz
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C                   when      eospcd = 'PSY'
     C*
     C                   if        xxrgn# = 1
     C*
     C* Region 1 - Psych
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92n4
     C*
     C* Anes
     C*
     C                   eval      caanam = pranam
     C*
     C                   endif                                                  if xxrgn = 1
     C*
     C                   if        xxrgn# = 2
     C*
     C* Region 2 - Psych
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nc
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92nd
     C*
     C                   endif                                                  if xxrgn = 2
     C*
     C                   if        xxrgn# = 3
     C*
     C* Region 3 - Psych
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nn
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92no
     C*
     C                   endif                                                  if xxrgn = 3
     C*
     C                   if        xxrgn# = 4
     C*
     C* Region 4 - Psych
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92ny
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92nz
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C*
     C                   when      eospcd = 'ACP'
     C*
     C                   if        xxrgn# = 1
     C*
     C* Region 1 - Acupuncture
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92oC
     C*
     C* Anes
     C*
     C                   eval      caanam = pranam
     C*
     C                   endif                                                  if xxrgn = 1
     C*
     C                   if        xxrgn# = 2
     C*
     C* Region 2 - Acupuncture
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92oD
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92nd
     C*
     C                   endif                                                  if xxrgn = 2
     C*
     C                   if        xxrgn# = 3
     C*
     C* Region 3 - Acupuncture
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92oE
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92no
     C*
     C                   endif                                                  if xxrgn = 3
     C*
     C                   if        xxrgn# = 4
     C*
     C* Region 4 - Acupuncture
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92oF
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92nz
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C                   when      eospcd = 'PHT' or eospcd = 'OT'
     C                             or eospcd = 'PT'
     C*
     C                   if        xxrgn# = 1
     C*
     C* Region 1 - Physical or Occupational Therapist
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92o7
     C*
     C* Anes
     C*
     C                   eval      caanam = pranam
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 5.91;
                         When CAfDat >= 1190401 and
                              CAfDat < 1200101;
                            If CVregIPT <> 0;
                               CAconv = CVregIPT;
                            Else;
                               CAconv = CVregI;
                            Endif;
                         When CAfDat >= 1200101;
                            CAconv = CVregI;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 1
     C*
     C                   if        xxrgn# = 2
     C*
     C* Region 2 - Physical or Occupational Therapist
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92o8
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92nd
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 6.20;
                         When CAfDat >= 1190401 and
                              CAfDat < 1200101;
                            If CVregIIPT <> 0;
                               CAconv = CVregIIPT;
                            Else;
                               CAconv = CVregII;
                            Endif;
                         When CAfDat >= 1200101;
                            CAconv = CVregII;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 2
     C*
     C                   if        xxrgn# = 3
     C*
     C* Region 3 - Physical or Occupational Therapist
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92o9
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92no
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 7.09;
                         When CAfDat >= 1190401 and
                              CAfDat < 1200101;
                            If CVregIIIPT <> 0;
                               CAconv = CVregIIIPT;
                            Else;
                               CAconv = CVregIII;
                            Endif;
                         When CAfDat >= 1200101;
                            CAconv = CVregIII;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 3
     C*
     C                   if        xxrgn# = 4
     C*
     C* Region 4 - Physical or Occupational Therapist
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92oa
     C*
     C* Anes
     C*
     C                   eval      caanam = pr92nz
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 7.70;
                         When CAfDat >= 1190401 and
                              CAfDat < 1200101;
                            If CVregIVPT <> 0;
                               CAconv = CVregIVPT;
                            Else;
                               CAconv = CVregIV;
                            Endif;
                         When CAfDat >= 1200101;
                            CAconv = CVregIV;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C                   other
     C*
     C                   if        xxrgn# = 1
     C*
     C* Region 1 - Other (MD, etc)
     C*
     C* Whole amount
     C*
     C                   eval      camra = prmra
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = prprof
     C                   eval      catech = pr92n1
     C                   eval      caanam = pranam
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 6.49;
                         When CAfDat >= 1190401;
                            CAconv = CVregI;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 1
     C*
     C                   if        xxrgn# = 2
     C*
     C* Region 2 - Other (MD, etc)
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92n9
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92ne
     C                   eval      catech = pr92nf
     C                   eval      caanam = pr92nd
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 6.80;
                         When CAfDat >= 1190401;
                            CAconv = CVregII;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 2
     C*
     C                   if        xxrgn# = 3
     C*
     C* Region 3 - Other (MD, etc)
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nk
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92np
     C                   eval      catech = pr92nq
     C                   eval      caanam = pr92no
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 7.78;
                         When CAfDat >= 1190401;
                            CAconv = CVregIII;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 3
     C*
     C                   if        xxrgn# = 4
     C*
     C* Region 4 - Other (MD, etc)
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nv
     C*
     C* Prof, tech and anes
     C*
     C                   eval      caprof = pr92o1
     C                   eval      catech = pr92o2
     C                   eval      caanam = pr92nz
     C*
     C* PT conversion factor
     C*
      /Free
                         Select;
                         When CAfDat < 1190401;
                            CAconv = 8.45;
                         When CAfDat >= 1190401;
                            CAconv = CVregIV;
                         Endsl;
      /End-Free
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C                   endsl
     C*
     C                   if        dhprcd >= '00100' and dhprcd <= '01999' or
     C                             dhprcd >= '99100' and dhprcd <= '99140'
     C*
     C                   if        xxrgn# = 1
     C*
     C* Region 1
     C*
     C* Whole amount
     C*
     C                   eval      camra = prmra
     C                   eval      caanam = pranam
     C*
     C                   endif                                                  if xxrgn = 1
     C*
     C                   if        xxrgn# = 2
     C*
     C* Region 2
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92n9
     C                   eval      caanam = pr92nd
     C*
     C                   endif                                                  if xxrgn = 2
     C*
     C                   if        xxrgn# = 3
     C*
     C* Region 3 - Other (MD, etc)
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nk
     C                   eval      caanam = pr92no
     C*
     C                   endif                                                  if xxrgn = 3
     C*
     C                   if        xxrgn# = 4
     C*
     C* Region 4
     C*
     C* Whole amount
     C*
     C                   eval      camra = pr92nv
     C                   eval      caanam = pr92nz
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C                   endif                                                  if xxrgn = 4
     C*
     C     loadamte      endsr
    C*
    C********************************************************************
    C** MODVAL SUBROUTINE                               M O D V A L     *
    C********************************************************************
    C*
     C     modval        begsr
    C*
     C                   eval      camd50 = 0.50
     C
     C                   if        prcpt4 >= '70000' and prcpt4 <= '79999'
     C                   eval      camd50 = 0.75
     C                   endif
     C
     C                   eval      camd51 = 0.50
     C                   eval      camd62 = 6.25
     C                   eval      camd80 = 0.16
     C                   eval      camd81 = 0.16
     C                   eval      camd82 = 0.16
     C*
     C     modvale       endsr
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
     C/free
       // load product type
         CAptyp = RXptyp;
      /end-free
    C*
     C*** remove the condition to check gereric/brand name, product type
     C*** is being handled through rules
     C***
    C** Brand-name Pharmaceuticals
     C*
    C* Determine if fill date is less than AWP effective date if so
    C* use previous AWP unit price else use current AWP unit price
     C                   if        cafdat < rxawpd
    C*
     C                   eval(h)   xxawp = rx1awp
    C*
     C                   else
     C                   eval(h)   xxawp = rxawp
    C*
     C                   endif
    C*
    C*
     C                   eval      caunts = dpunts
     C                   move      rxstat        castat
     C                   move      'Y'           cafsr
     C                   eval      camra = xxawp * caunts
     C                   eval      caawp = xxawp
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
     C     '50'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd50
    C*
     C                   endif
    C*
     C     '62'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = (camra * camd62)/10
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
     C     '82'          lookup    mod(1)                                 99
    C*
     C                   if        *in99 = *on
    C*
     C                   eval      xxmra = camra * camd82
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
    C** PROCEDURE CODE MRA SEARCH                             P R O C   *
    C********************************************************************
     C     proc          begsr
    C*
    C*  Default to service date for effective date for rules
    C*
     C                   eval      kkefdt = cafdat
    C*
    C*  Process the Fee Scheduled codes here
    C*
    C*
    C*  Test for Fee Schedule Reimbursement = 'Y'
    C*
    C*
     C     prky          setgt     ppr01
     C     nxtpr2        tag
     C     prky1         readpe    ppr01                                  60
    C*
    C*  A hard error will cause the program to terminate ASAP.
     C                   if        *in60 = *on
     C                   movel     'Y'           cainvl
    C*
     C                   else
    C*
     C                   eval      cafolu = prfolu
      /Free
                         Select;

                         When EOspCd = 'PHT' or EOspCd = 'OT'
                                   or EOspCd = 'PT';
                            kkSrc = 'MED';
                         When EOspCd = 'CHI';
                            kkSrc = 'CHI';
                         When EOspCd = 'POD' or EOspCd = 'PO1';
                            kkSrc = 'POD';
                         When EOspCd = 'PSY';
                            kkSrc = 'PSY';
                         Other;
                            kkSrc = 'MED';
                         Endsl;

                         Clear pcv01;

                         Setgt (DHprCd:kkSrc:kkEfDt) pcv01;
                         Readpe (DHprCd:kkSrc) pcv01;
      /End-free
    C*
     C* Load amounts based on service zip of the provider
    C*
     C                   exsr      loadamt
    C*
     C                   eval      xcmra = camra
     C                   eval      caamt = camra
    C*
    C* Load flags for anes, asst surg, no cut, etc.
    C*
     C                   movel     pr1a2         caanes
     C                   move      prsurg        casurg
     C                   move      prsupp        casupp
     C                   move      prstat        castat
     C                   move      prrpt         carpt
     C                   move      pr1a5         canct
     C                   move      pr1a6         CAaddon
     C                   move      pr1ad         casepp
     C                   move      prfsr         cafsr
    C*
    C* Load MRA for HCPCS
    C*
     C                   if        dhprcd >= 'A0001' and dhprcd <= 'Z9999'
     C                   eval      camra = prmra
     C                   eval      xcmra = prmra
     C                   endif
    C*
    C* Load modifier reductions for processing hierchy
    C*
     C                   exsr      modval
    C*
     C                   move      dhpos         caplac
     C                   move      dhprcd        cacpt4
     C                   eval      caunts = dhunts
     C                   move      pr1a4         cabmri
    C*
    C* Load relative value units for PT codes
    C*
     C                   if        dhprcd >= '97001' and dhprcd <= '97800'
     C                   eval      caanam = pr92ob
     C                   endif
    C*
     C                   if        pr1a2 = 'Y'
     C                             or dhprcd >= '00100'
     C                             and dhprcd <= '01999'
     C                   eval      caanmn = dhunts
     C                   endif
    C*
     C                   move      dharea        capart
     C                   move      dhrcfl        carcfl
    C                   endif
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
    C*
     C                   eval      kkefdt = cafdat
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
     C                   movel     'Y'           CAinvl
     C
    C*
     C                   else
     C                   eval      CAinvl = ' '
     C                   eval      cafolu = rcfolu
     C                   eval      caanes = rc1a2
     C                   eval      cafsr = rcfsr
    C*
     C                   eval      xcmra = rcmra
     C                   eval      camra = rcmra
     C                   eval      caamt = rcmra
     C                   eval      caprof = rcprof
     C                   eval      catech = rc92n1
     C*
     C*  Load the professional and technical component amount based on
     C*  percentages in FTUMP.
     C*
     C     umky          setgt     pum01
     C                   readp     pum01                                  60
     C*
     C                   if        *in60 = *off and cacpt4 <= umtocp
     C*
     C                   eval      xxmd26 = umod26/100
     C                   eval      xxmd27 = umod27/100
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
     C*
     C                   eval      caanam = rcanam
     C                   move      rcsurg        casurg
     C                   move      rcsupp        casupp
     C                   eval      caasc = rc92n1
     C                   move      rcstat        castat
     C                   move      rcrpt         carpt
     C                   move      rcfsr         cafsr
    C*
    C* Load modifier reductions for processing hierchy
    C*
     C                   exsr      modval
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
     C     svstat        cabeq     'I'           nxtsv1
     C*
     C                   exsr      inithosp
     C*
     C                   if        eudcdt < 1091201
     C     exky          setgt     pex01
     C     exky1         readpe    pex01                                  99
     C                   endif
     C*
     C                   select
     C*
     C* Load allowance from physician Fee Schedule for outpatient bills
     C*
     C                   when      euicn = 'HO' or euicn = 'HS' or
     C                             euicn = 'AO' and durate <> *blanks or
     C                             durate >= '97010' and durate <= '97899' or
     C                             durate >= '70001' and durate <= '89999'
     C*
     C                   exsr      loadoutp
     C*
     C* Load PAS group rate for ASC's
     C*
     C                   when      dupsgp <> 0 and euicn = 'AS' or
     C                             dupsgp <> 0 and eublty >= '830' and
     C                             eublty <= '839'
     C*
     C                   exsr      loadAsc
     C*
     C* For inpatient non-exempt units calculate DRG payment
     C*
     C                   when      euicn = 'ID'
     C*
     C*
     C                   if        (EUdcdt >= 1091201 and EUdcdt <  1150601) or
     C                             (DUrvcd  = 100     and EUdcdt >= 1150601)
     C*
     C                   exsr      loadAPRDRG
     C                   else
     C                   exsr      loadDRG
     C                   endif
     C*
     C* For inpatient exempt units calculate Per Diem payment
     C*
     C                   when      (euicn = 'IA' or euicn = 'IH' or
     C                             euicn = 'IP' or euicn = 'IL' or
     C                             euicn = 'IM' or euicn = 'IE' or
     C                             euicn = 'IO' or euicn = 'IR') and
     C                             eudcdt < 1091201
     C*
     C                   exsr      loadPerdiem
     C*
     C                   when      eudcdt >= 1091201 and
     C                             (euicn = 'IS' or euicn = 'IP' or
     C                             euicn = 'IC' or euicn = 'IM' or
     C                             euicn = 'IW' or euicn = 'IX')
     C*
     C                   exsr      loadAPRPerdiem
     C*
     C*
     C* For inpatient non-exempt units calculate DRG payment
     C*
     C                   when      (euicn = 'MD' or euicn = 'MS') and
     C                             eudcdt >= 1110101
     C*
     C                   exsr      loadDetoxPD
     C                   when      (euicn = 'IR' and  eudcdt >= 1120101)
     C*
     C                   exsr      loadPsychExmpt
     C*
     C                   other
     C                   eval      camra = 0
     C*
     C                   endsl
     C*
    C*
     C     svcde         endsr
    C* ==================================================================
     Opca01     e            rca01
