     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/Title W8371R - Policy Data Extract Program
     H*****************************************************************
     H*                                                               *
     H* THIS PROGRAM PULLS POLICY INFORMATION BY VENDOR AND WRITES TO *
     H* THE FLAT FILE WXPXP.                                          *
     H*                                                               *
     H* CREATED: 05/15/03    PROGRAMMER: 65                           *
     H* PROJECT: WC1115      PROGRAM:    W8371R                       *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*    W8371C                                                     *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H* 50 - GENERAL FILE READ                                        *
     H* 51 - FILE READ FOR PAYROLL DESCRIPTION EXTENSION              *
     H* 60 - VENDOR FILE                                              *
     H* 61 - VENDOR HEADER                                            *
     H* 70 - EMPLOYER FILE                                            *
     H* 71 - EMPLOYER HEADER                                          *
     H* 77 - POLICY IS ANNIVERSARY RATED                              *
     H* 78 - POLICY IS ANNIVERSARY SPLIT                              *
     H* 80 - EMPLOYER HISTORY FILE                                    *
     H* 81 - EMPLOYER HISTORY TRANSACTION                             *
     H* 90 - CANCELLATION HISTORY FILE                                *
     H* 91 - CANCELLATION HISTORY TRANSACTION                         *
     H*                                                               *
     H*****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      * 06/17/24 C:344 Project: WC2952                                *
      *                Increase rate field size to zone 8.3          *
      *                                                               *
      * 11/27/23 C:344 Support: PHW113984  (Sunz)                     *
      *                Prevent 'E' records from being generated for   *
      *                Sunz (Client = '150').
      *                                                               *
      * 09/15/23 C:65  Support: Phw#113963 (Sunz)                     *
      *                Reduce looping such as loop finding state rpt  *
      *                div # and not leaving loop.                    *
      *                                                               *
      * 02/22/22 C:62  Project: 150WFN                                *
      *                Use new GetNameAddress.                        *
      *                                                               *
      * 12/26/19 C:136 Support: 86072                                 *
      *                Correction for carrier number logic written    *
      *                in WRTNA. It is supposed to assign latest      *
      *                carrier number to additional names while       *
      *                transactions are assigned the carrier number   *
      *                from the year being processed.                 *
      *                                                               *
      * 04/08/19 C:65  Support: Phw#78586 (Sunz)                      *
      *                Within procedure for populating dates, do not  *
      *                manipulate the index, instead restore default  *
      *                value to array slot for 2/29 date and re-sort. *
      *                                                               *
      * 04/30/18 C:136 Project: 150WCW                                *
      *                Add Accidental Fund Policy Export for Sunz     *
      *                                                               *
      * 02/21/18 C:136 WC2588                                         *
      *                Condition export to include non billed         *
      *                divisions or exclude non reporting locations   *
      *                based on TPA setup                             *
      *                                                               *
      * 01/24/18 C:136 150WAT                                         *
      *                Add Dec Page name to the policy header         *
      *                                                               *
      * 01/05/18 C:136 Support: 63417 (150WAT)                        *
      *               -Group midterm endorsement flag should control  *
      *                write of midterm endorsement records           *
      *               -Pull deductible amount from table or large     *
      *                deductible setup instead of ELAMPO             *
      *               -If writing a header for each division write    *
      *                only the addresses for that division           *
      *               -Populate effective date of division or location*
      *                in address record                              *
      *                                                               *
      * 11/22/17 C:062 Project: 51180(150)                            *
      *                Exclude vendor with same day expiration        *
      *                                                               *
      * 11/10/17 C:136 Project: 150WAT                                *
      *                Add vendor and vendor name to policy           *
      *                header for Sunz                                *
      *                                                               *
      * 09/01/17 C:136 Project: 150WAT                                *
      *                Sunz scope changes                             *
      *                                                               *
      * 07/25/17 C:291 Project: 150WAT                                *
      *                Sunz modifications                             *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 07/15/13 C:282 Project: WC2211                                *
     H*                To pull the terrorism rates from the new       *
     H*                employer detail fields (ELTERR and ELDTRR)     *
     H*                if populated.                                  *
     H*                                                               *
     H*  11/21/12 C:65  Project: WC2116                               *
     H*                 Populate name and federal name field with no  *
     H*                 data manipulation using movel/move instead of *
     H*                 concatenation.                                *
     H*                                                               *
     H*  06/21/10 C:255 PROJECT: 114WVY                               *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATIO-*
     H*                NS WHEN DEVIATIONS ARE SPLIT. POPULATE INFORMA-*
     H*                TION IN FILES WDMPP AND WDELP OF 2ND PERIOD    *
     H*                DEVIATIONS AS IT IS FOR 1ST PERIOD DEVIATIONS. *
     H*                                                               *
     H*  01/12/10 C:136 PROJECT: WC1838                               *
     H*                 ADD PROCESSING FOR LARGE DEDUCTIBLES          *
     H*                                                               *
     H*  07/23/09 C:198 PROJECT WC1694-PRISS# 7                       *
     H*                 XXFDT AND TDT NEED TO BE RESET AT GROUP BREAK.*
     H*                                                               *
     H*  06/26/09 C:198 PROJECT WC1694                                *
     H*                 INCORPORATE NEW GROUP POLICY EXPORT PROCESS.  *
     H*                                                               *
     H*  06/19/09 C:198 PROJECT 139WLO                                *
     H*                 UPDATE THE POLICY EXPORT PROCESS SO THAT IT   *
     H*                 DESIGNATES WHICH DIVISION IS THE STATE        *
     H*                 REPORTING DIVISION AND EXPORTS A 'P' TYPE     *
     H*                 RECORD FOR EVERY DIVISION OF THE POLICY IF    *
     H*                 IF ANY DIVISION WITHIN THE POLICY HAS         *
     H*                 ACTIVITY. EXCLUDE DIVISIONS WITH INCEPTION    *
     H*                 DATES SUBSEQUENT TO THE ACTIVITY DATE RANGE   *
     H*                 FROM PROCESSING 'P' OR 'A' TYPE RECORDS.      *
     H*                                                               *
     H*  04/13/09 C:191 PROJECT 114WTU                                *
     H*                 LOAD ADDITIONAL ENTITY/LOCATION NAME FIELD    *
     H*                 (ALNAM2)WHEN CREATING POLICY DATA EXPORT      *
     H*                 RECORDS (WXPOP)                               *
     H*                                                               *
     H* 09/04/08 C:62  PROJECT WC1611                                 *
     H*                USE ELNWRW                                     *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H* 04/05/06 C:65  PROJECT SUPP# 51896                            *
     H*                ENSURE NEGATIVE VALUES ARE NOT MOVED INTO THE  *
     H*                NUMERIC FIELDS.  THE ONLY FIELDS WHICH SHOULD  *
     H*                CONTAIN A NEGATIVE VALUE ARE CERTAIN FACTORS   *
     H*                AND THOSE WILL HAVE THE NEGATIVE VALUE NEGATED *
     H*                AS WELL (STAT CODE WILL INTERPRET THE VALUE).  *
     H*                                                               *
     H* 01/24/06 C:65  PROJECT 114WHO                                 *
     H*                ADD RETROSPECTIVE PREMIUM TO POLICY HEADER     *
     H*                RECORD AND TRIGGER EXPORT ON EMPLR HISTORY.    *
     H*                                                               *
     H* 01/23/06 C:65  PROJECT 139WBT                                 *
     H*                ADD SIC AND NAICS CODES TO POLICY EXPORT       *
     H*                WITHIN THE NAME/ADDRESS RECORD.                *
     H*                                                               *
     H* 12/15/05 C:65  PROJECT 139WBT                                 *
     H*                ADD NEW PROCESSING FOR REPORTING NON-PREMIUM   *
     H*                BEARING ENDORSEMENTS AND ADDITIONAL LOCATIONS/ *
     H*                DIVISIONS.                                     *
     H*                                                               *
     H*  09/02/05 C:136 SUPPORT: WC1250                               *
     H*                 ADD DOMESTIC TERR CHARGE                      *
     H*                                                               *
     H*  06/11/04 C:136 SUPPORT: WA129 42820                          *
     H*                 CORRECT CONDITION SO THAT A POLICY EXPORT     *
     H*                 RECORD WILL BE CREATED IF THE EFFECTIVE DATE  *
     H*                 OF THE POLICY AND VENDOR HISTORY RECORD ARE   *
     H*                 EQUAL AND IN THE FUTURE.                      *
     H*                                                               *
     H*  05/11/04 C:65  PROJECT: SUPPORT                              *
     H*                 COPY RATE/NET RATE INTO BLENDED RATE FIELDS   *
     H*                 INSTEAD OF ZERO FOR NON-SPLIT POLICIES.       *
     H*                                                               *
     H*  01/09/04 C:136 PROJECT: WC1115                               *
     H*                 CHECK VENDOR HISTORY INSTEAD OF EMP DETAIL    *
     H*                 FOR VENDOR NUMBER.                            *
     H*                                                               *
     H*  08/29/03 C:65  PROJECT: SUPPORT                              *
     H*                 ADD BILLING SCHEDULE CODE INFO TO DATA FILE.  *
     H*                                                               *
     H*  07/23/03 C:65  PROJECT: SUPPORT                              *
     H*                 CALCULATE NET RATE USING UNPRORATED PREMIUM.  *
     H*                                                               *
     H*  07/10/03 C:65  PROJECT: SUPPORT                              *
     H*                 POPULATE THE EMPLOYER FEIN FIELD.             *
     H*                                                               *
     H*****************************************************************
     FWXPXP     O  A E             DISK
      *** beg add ***** 04/30/18 **************************
     fwdell1    if   e           k disk    Rename(pel01:pel0l1)
     fwmeal23   if   e           k disk    Rename(pea01:pea23)
     fwxpxpaf   o    e             disk    Rename(ppx01af:ppx01af) usropn
     fwxpxpafv  o    e             disk    Rename(ppx01afv:ppx01afv) usropn
     fwxpppaf   uf a e           k disk    Rename(ppp01af:ppp01af)
      *** end add ***** 04/30/18 **************************
     FWMVPL     IF   E           K DISK
     FWDEHL12   IF   E           K DISK
     FWDXHL15   IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWTLML1    IF   E           K DISK
     FWTDDL2    IF   E           K DISK
     FWDMPL     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWMEPL1    IF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     FWMPRL1    IF   E           K DISK
     FWMPXL1    IF   E           K DISK
     F                                     RENAME(PPX01:PPX02)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWDVHL1    IF   E           K DISK
     FWMMUL4    IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWDF1L1    IF   E           K DISK
     FWDELL2    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *** beg add ***** 04/30/18 **************************
     Fwdf1l     if   e           k disk    Rename(pf101:pf100)
     fwdell3    if   e           k disk
     f                                     rename(pel01:pel03) prefix(l3)
      *** end add ***** 04/30/18 **************************
     FWXGSL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      *** beg add ***** 09/01/17 ***************
     FWDF2L     IF   E           K DISK    Rename(pf201:pf200)
      *** end add ***** 09/01/17 ***************
      *** beg add ***** 02/21/18 ***************
     FWMTCL     IF   E           K DISK    Rename(ptc01:ptc00)
      *** end add ***** 02/21/18 ***************
     FWMPEL     IF   E           K DISK
     FWMLDL1    IF   E           K DISK
     FSWCPF     IF   E             DISK
      *** beg add ***** 07/25/17 ******************************************
     FWMEEL4    IF   E           K DISK    rename(pee01:pee04)

     D firstEE         s               n
     D xxLSN           s                   like(EELSN)
     D xxFSN           s                   like(EEFSN)
     D xxMID           s                   like(EEMID)
     D xxSS#           s                   like(EESS#)
     D xxTITL          s                   like(EETITL)
     D xxPOSN          s                   like(EEPOSN)
     D xxCLAS1         s                   like(EECLAS)
     D xxSPEC          s                   like(EESPEC)
     D xxEXT           s                   like(EEEXT)
     D xxEAPR          s                   like(EEEAPR)
     D xxSAPR          s                   like(EESAPR)
     d xxExcPrem       s              1a
     d xxExcPayR       s              1a
      *** end add ***** 07/25/17 ******************************************
      *** beg add ***** 11/10/17 ***************************
     d currDateCymd    s              7  0
      *** end add ***** 11/10/17 ***************************
      *** beg add ***** 01/05/18 ***************************
     d xxMidTerm       s              1a
     d POLNAWRTN       s               n
     d GetDedAmpo      pr                  like(LDamp1)
     d RtvSmDedInfo    pr                  like(LDamp1)
     d  code                               like(ELdcde)
     d  efdt                               like(ELdcef)
      *** end add ***** 01/05/18 ***************************
      *** beg add ***** 04/30/18 ***************************
     d asOfDate        s               d
     d cymdValDate     s              7  0
     d isVldtMode      s               n
     d max#OfStEfDts   c                   4000
     d maxPDays        c                   400
     d xxAddLoad       s              9  2
     d xxCxDate        s              7  0
     d pDaysAry        s               d   Dim(maxPDays) Inz(*Hival) Ascend
     d ePremArray      s             15  6 Dim(maxPDays) Inz(*Zeros)
     d expConArray     s             11  6 Dim(maxPDays) Inz(*Zeros)
     d sPremArray      s              9  2 Dim(maxPDays) Inz(*Zeros)
     d procEmpYr       s             12a   Dim(99999) Inz(*Blanks)
     d pgmLDA          ds                  Dtaara(*lda)
     d  afMode                       15a
     d dsLastEH        ds                  LikeRec(peh01)
     d dsWXPXPAF     e ds                  ExtName(WXPXPAF)
     d dsWXPXPAFV    e ds                  ExtName(WXPXPAFV)
     d polInfo         ds                  Qualified Dim(max#OfStEfDts)
     d  coid                               Like(PXCOID) Inz(*Blanks)
     d  pol#                               Like(PXPOL#) Inz(*Blanks)
     d  polEfDt                            Like(PXPOLEFFDT) Inz(*Zeros)
     d  rateState                          Like(PXPOLST) Inz(*Blanks)
     d  address                            Like(PXPOLADDR) Inz(*Blanks)
     d  city                               Like(PXPOLCITY) Inz(*Blanks)
     d  state                              Like(PXPOLSTATE) Inz(*Blanks)
     d  zip                                Like(PXPOLZIP) Inz(*Blanks)
     d  lrgDedFlg                          Like(PXLRGDEDFL) Inz('N')
     d  fClassFlg                          Like(PXFCLASSFL) Inz('N')
     d  occDisFlg                          Like(PXOCCDISFL) Inz('N')
     d  liabLimits                         Like(PXEMPLL) Inz(*Blanks)
     d  dedPlan                            Like(PXDEDPLAN) Inz(*Blanks)
     d  dedAggAmt                          Like(PXAGGAMT) Inz(*Zeros)
     d  perAccAmt                          Like(PXPERACCAM) Inz(*Zeros)
     d  earnPrem                           Like(PXEARNPREM) Inz(*Zeros)
     d  earnDeduct                         Like(PXEARNDED) Inz(*Zeros)
     d  earnMerit                          Like(PXEARNMRAT) Inz(*Zeros)
     d  earnSchCred                        Like(PXEARNSCCR) Inz(*Zeros)
     d  earnSchDebit                       Like(PXEARNSCDE) Inz(*Zeros)
     d  earnPrmDisc                        Like(PXEARNPRMD) Inz(*Zeros)
     d  earnEmpAsst                        Like(PXEARNEMAS) Inz(*Zeros)
     d  earnTerrAmt                        Like(PXEARNTERR) Inz(*Zeros)
     d  earnCatAmt                         Like(PXEARNCATA) Inz(*Zeros)
     d  earnSafeCrd                        Like(PXEARNSFCR) Inz(*Zeros)
     d  earnRetro                          Like(PXEARNRTRO) Inz(*Zeros)
     d  earnMCO                            Like(PXEARNMCO) Inz(*Zeros)
     d  earnDividend                       Like(PXEARNDIV) Inz(*Zeros)
     d  earnExpConst                       Like(PXEARNEXCN) Inz(*Zeros)
     d  earnMinPrem                        Like(PXEARNMNPR) Inz(*Zeros)
     d  wrtnPrem                           Like(PXWRITPREM) Inz(*Zeros)
     d  wrtnDeduct                         Like(PXWRITDED) Inz(*Zeros)
     d  wrtnMerit                          Like(PXWRITMRAT) Inz(*Zeros)
     d  wrtnSchCred                        Like(PXWRITSCCR) Inz(*Zeros)
     d  wrtnSchDebit                       Like(PXWRITSCDE) Inz(*Zeros)
     d  wrtnPrmDisc                        Like(PXWRITPRMD) Inz(*Zeros)
     d  wrtnEmpAsst                        Like(PXWRITEMAS) Inz(*Zeros)
     d  wrtnTerrAmt                        Like(PXWRITTERR) Inz(*Zeros)
     d  wrtnCatAmt                         Like(PXWRITCATA) Inz(*Zeros)
     d  wrtnSafeCrd                        Like(PXWRITSFCR) Inz(*Zeros)
     d  wrtnRetro                          Like(PXWRITRTRO) Inz(*Zeros)
     d  wrtnMCO                            Like(PXWRITMCO) Inz(*Zeros)
     d  wrtnDividend                       Like(PXWRITDIV) Inz(*Zeros)
     d  wrtnExpConst                       Like(PXWRITEXCN) Inz(*Zeros)
     d  wrtnMinPrem                        Like(PXWRITMNPR) Inz(*Zeros)
     d  indemDedRcvd                       Like(PXINDDEDRC) Inz(*Blanks)
     d  medDedRcvd                         Like(PXMEDDEDRC) Inz(*Blanks)

     d valInfo         ds                  Qualified Dim(max#OfStEfDts)
     d  coid                               Like(VXCOID) Inz(*Blanks)
     d  pol#                               Like(VXPOL#) Inz(*Blanks)
     d  polEfDt                            Like(VXPOLEFFDT) Inz(*Zeros)
     d  rateState                          Like(VXPOLST) Inz(*Blanks)
     d  address                            Like(VXPOLADDR) Inz(*Blanks)
     d  city                               Like(VXPOLCITY) Inz(*Blanks)
     d  state                              Like(VXPOLSTATE) Inz(*Blanks)
     d  zip                                Like(VXPOLZIP) Inz(*Blanks)
     d  lrgDedFlg                          Like(VXLRGDEDFL) Inz('N')
     d  fClassFlg                          Like(VXFCLASSFL) Inz('N')
     d  occDisFlg                          Like(VXOCCDISFL) Inz('N')
     d  liabLimits                         Like(VXEMPLL) Inz(*Blanks)
     d  dedPlan                            Like(VXDEDPLAN) Inz(*Blanks)
     d  dedAggAmt                          Like(VXAGGAMT) Inz(*Zeros)
     d  perAccAmt                          Like(VXPERACCAM) Inz(*Zeros)
     d  earnPrem                           Like(VXEARNPREM) Inz(*Zeros)
     d  earnDeduct                         Like(VXEARNDED) Inz(*Zeros)
     d  earnMerit                          Like(VXEARNMRAT) Inz(*Zeros)
     d  earnSchCred                        Like(VXEARNSCCR) Inz(*Zeros)
     d  earnSchDebit                       Like(VXEARNSCDE) Inz(*Zeros)
     d  earnPrmDisc                        Like(VXEARNPRMD) Inz(*Zeros)
     d  earnEmpAsst                        Like(VXEARNEMAS) Inz(*Zeros)
     d  earnTerrAmt                        Like(VXEARNTERR) Inz(*Zeros)
     d  earnCatAmt                         Like(VXEARNCATA) Inz(*Zeros)
     d  earnSafeCrd                        Like(VXEARNSFCR) Inz(*Zeros)
     d  earnRetro                          Like(VXEARNRTRO) Inz(*Zeros)
     d  earnMCO                            Like(VXEARNMCO) Inz(*Zeros)
     d  earnDividend                       Like(VXEARNDIV) Inz(*Zeros)
     d  earnExpConst                       Like(VXEARNEXCN) Inz(*Zeros)
     d  earnMinPrem                        Like(VXEARNMNPR) Inz(*Zeros)
     d  wrtnPrem                           Like(VXWRITPREM) Inz(*Zeros)
     d  wrtnDeduct                         Like(VXWRITDED) Inz(*Zeros)
     d  wrtnMerit                          Like(VXWRITMRAT) Inz(*Zeros)
     d  wrtnSchCred                        Like(VXWRITSCCR) Inz(*Zeros)
     d  wrtnSchDebit                       Like(VXWRITSCDE) Inz(*Zeros)
     d  wrtnPrmDisc                        Like(VXWRITPRMD) Inz(*Zeros)
     d  wrtnEmpAsst                        Like(VXWRITEMAS) Inz(*Zeros)
     d  wrtnTerrAmt                        Like(VXWRITTERR) Inz(*Zeros)
     d  wrtnCatAmt                         Like(VXWRITCATA) Inz(*Zeros)
     d  wrtnSafeCrd                        Like(VXWRITSFCR) Inz(*Zeros)
     d  wrtnRetro                          Like(VXWRITRTRO) Inz(*Zeros)
     d  wrtnMCO                            Like(VXWRITMCO) Inz(*Zeros)
     d  wrtnDividend                       Like(VXWRITDIV) Inz(*Zeros)
     d  wrtnExpConst                       Like(VXWRITEXCN) Inz(*Zeros)
     d  wrtnMinPrem                        Like(VXWRITMNPR) Inz(*Zeros)
     d  indemDedRcvd                       Like(VXINDDEDRC) Inz(*Blanks)
     d  medDedRcvd                         Like(VXMEDDEDRC) Inz(*Blanks)
     d  fund                          3    Inz(*Blanks)
     d  employer                      9    Inz(*Blanks)
     d  division                      5    Inz(*Blanks)
     d  year                          3    Inz(*Blanks)
     d  earnEPrmFact                  6    Inz(*Blanks)
     d  earnETimFact                  6    Inz(*Blanks)
     d  xrtnPrem                           Like(VXWRITPREM) Inz(*Zeros)
     d  xrtnDeduct                         Like(VXWRITDED) Inz(*Zeros)
     d  xrtnMerit                          Like(VXWRITMRAT) Inz(*Zeros)
     d  xrtnSchCred                        Like(VXWRITSCCR) Inz(*Zeros)
     d  xrtnSchDebit                       Like(VXWRITSCDE) Inz(*Zeros)
     d  xrtnPrmDisc                        Like(VXWRITPRMD) Inz(*Zeros)
     d  xrtnEmpAsst                        Like(VXWRITEMAS) Inz(*Zeros)
     d  xrtnTerrAmt                        Like(VXWRITTERR) Inz(*Zeros)
     d  xrtnCatAmt                         Like(VXWRITCATA) Inz(*Zeros)
     d  xrtnSafeCrd                        Like(VXWRITSFCR) Inz(*Zeros)
     d  xrtnRetro                          Like(VXWRITRTRO) Inz(*Zeros)
     d  xrtnMCO                            Like(VXWRITMCO) Inz(*Zeros)
     d  xrtnDividend                       Like(VXWRITDIV) Inz(*Zeros)
     d  xrtnExpConst                       Like(VXWRITEXCN) Inz(*Zeros)
     d  xrtnMinPrem                        Like(VXWRITMNPR) Inz(*Zeros)

     d GetAddress      pr
     d  pAddress                     81
     d  pCity                        25
     d  pState                        2
     d  pZip                         10
     d*** Begin Del *** 02/22/22 ******************************************
     d*GetCxDate       pr             7  0
     d*** End   Del *** 02/22/22 ******************************************
     d GetFactors      pr
     d  pEarnPrem                    11  2
      *** beg chg ***** 03/17/22 ******************
     d***pEarnPremFact                  5  4
     d pEarnPremFact                  7  4
      *** end chg ***** 03/17/22 ******************
     d  pEarnTimeFact                 5  4
     d  pAuditStatus                  1
     d  pWrtnPrem                    11  2
     d GetLastRetro    pr            11  2
     d GetLiabLimits   pr                  Like(PXEmpLL)
     d GetDedInfo      pr
     d  pFlgLrgDed                    1
     d  pDedPlan                     20
     d  pDedAggAmt                         like(LDamg1)
     d  pDedAccAmt                         like(LDamp1)
     d GetMeritPrem    pr            11  2
     d GetPol#         pr                  Like(RPcap#)
     d PopDateArray    pr
     d  begDate                       7  0 Options(*Nopass)
     d  endDate                       7  0 Options(*Nopass)
     d PopEPremArray   pr
     d PopSPremArray   pr
     d ProcessGroup    pr
     d RunAfExport     pr
     d WritePayInfo    pr
     d  idx                           5  0
     d  fClassFlag                    1a
     d  occDisFlag                    1a
     d  pAuditStatus                  1a
     d WritePolInfo    pr
     d WriteWXPXPAF    pr
      *** end add ***** 04/30/18 ***************************
      *** beg add ***** 02/21/18 ***************************
     d hasWMMUP        s               n
     d isExcNRLoc      s               n
     d isIncNBDiv      s               n
     d xxCmpDate       s                   like(ELidt)
     d GetGrpInfo      pr
     d  inFnd                              like(ELfnd)
      *** beg add ***** 02/21/18 ***************************
      *** beg add ***** 01/24/18 ***************************
     d GetDecPageInfo  pr
     d SrAddr          pr                  extpgm('SRADDR')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##odr                          1
     d ##nodr                         1
     d ##aodr                         1
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d ##adrl                         3  0
     d ##naml                         3  0
     d ##fedl                         3  0
     d ##peo1                        57
     d ##peo2                        57
     d ##pgfl                         1
     d ##conl                         3  0
     d ##amn1                        60
     d ##amn2                        60
     d ##amn3                        60
     d ##amn4                        60
     d ##amn5                        60
     d ##amn6                        60
     d ##amn7                        60
     d ##amn8                        60
     d ##amn9                        60
     d ##peol                         3  0
     d ##dbaf                         1
     d ##al2                          3  0
     d ##nl2                          3  0
     d ##fl2                          3  0
     d ##pl2                          3  0
     d ##cl2                          3  0
     d ##PeoExt                      10
     d ##Name1                      120
     d ##Name2                      120
     d ##Name3                      120
     d ##Name4                      120
     d ##Fein                         9  0

     d*** Begin Add *** 02/22/22 *******************************************
     d/copy wprEmplr
     d*** End   Add *** 02/22/22 *******************************************

     d ##co#           s              3
     d ##fnd           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##odr           s              1
     d ##nodr          s              1
     d ##aodr          s              1
     d ##emn1          s             50
     d ##emn2          s             50
     d ##emn3          s             50
     d ##emn4          s             50
     d ##emn5          s             50
     d ##emn6          s             50
     d ##emn7          s             50
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d ##peol          s              3  0
     d ##dbaf          s              1
     d ##al2           s              3  0
     d ##nl2           s              3  0
     d ##fl2           s              3  0
     d ##pl2           s              3  0
     d ##cl2           s              3  0
     d ##PeoExt        s             10
     d ##Name1         s            120
     d ##Name2         s            120
     d ##Name3         s            120
     d ##Name4         s            120
     d ##Fein          s              9  0
     d svEmp#          s                   like(ELemp#)
     d svFnd           s                   like(ELfnd)
     d xxDecNam1       s            120
     d xxDecNam2       s            120
     d xxDecNam3       s            120
     d xxDecNam4       s            120
     d xxDecAdd1       s             60
     d xxDecAdd2       s             60
     d xxDecAdd3       s             60
     d xxDecFein       s              9  0

     d                 ds
     d  amn                    1    540    dim(9)
     d  ##amn1               001    060
     d  ##amn2               061    120
     d  ##amn3               121    180
     d  ##amn4               181    240
     d  ##amn5               241    300
     d  ##amn6               301    360
     d  ##amn7               361    420
     d  ##amn8               421    480
     d  ##amn9               481    540
      *** end add ***** 01/24/18 ***************************

     D RDAT            S              7  0 DIM(4) ASCEND                        RATING DATES
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D* VENDOR HEADER DATA STRUCTURE
     D*
     D                 DS
     D  VHD                    1    232
     D  VHRTYP                 1      1
     D  VHVDR#                 2      6  0
     D  VHFTYP                 7      7
     D  VHDTYP                 8      8
     D  VHDFMT                 9     16
     D  VHNAME                17     56
     D  VHMAIL                57    116
     D  VHADR1               117    156
     D  VHADR2               157    196
     D  VHCITY               197    221
     D  VHST                 222    223
     D  VHZIP                224    232  0
     D*
     D* POLICY HEADER DATA STRUCTURE
     D*
      *** beg chg ***** 07/25/17 ******************************************
     D***PHD             DS           746
     D PHD             DS          2000
      *** end chg ***** 07/25/17 ******************************************
     D  PHRTYP                 1      1
     D  PHCAP#                 2     26
     D  PHFND                 27     29  0
     D  PHEMP#                30     38  0
     D  PHDIV                 39     43  0
     D  PHFYR                 44     46  0
     D  PHNAME                47     86
     D  PHFDNM                87    126
     D  PHMAD1               127    166
     D  PHMAD2               167    206
     D  PHMCTY               207    231
     D  PHMST                232    233
     D  PHMZIP               234    242  0
     D  PHPAD1               243    282
     D  PHPAD2               283    322
     D  PHPCTY               323    347
     D  PHPST                348    349
     D  PHPZIP               350    358  0
     D  PHPHN                359    368  0
     D  PHFAX                369    378  0
     D  PHPFDT               379    386  0
     D  PHPTDT               387    394  0
     D  PHCXDT               395    402  0
     D  PHANDT               403    410  0
     D  PHPRTF               411    421  2
     D  PHPROR               422    426  3
     D  PHPRTD               427    437  2
     D  PHJRST               438    439
     D  PHANPR               440    450  2
     D  PHAAPR               451    461  2
     D  PHAFLG               462    462
     D  PHFEIN               463    471  0
     D  PHCTYP               472    472
     D  PHMINP               473    473
     D  PHMAPR               474    484  2
     D  PHAMPR               485    495  2
     D  PHESBJ               496    506  2
     D  PHASBJ               507    517  2
     D  PHSTPR               518    528  2
     D  PHASPR               529    539  2
     D  PHEMPM               540    550  2
     D  PHAMPM               551    561  2
     D  PHENMP               562    572  2
     D  PHANMP               573    583  2
     D  PHTXDU               584    594  2
     D  PHLOSS               595    605  2
     D  PHTPOB               606    606
     D  PHBTYP               607    607
     D  PHEXM1               608    612  4
     D  PHEMD1               613    620  0
     D  PHEXM2               621    625  4
     D  PHEMD2               626    633  0
     D  PHEXM3               634    638  4
     D  PHEMD3               639    646  0
     D  PHEXP1               647    657  2
     D  PHEXP2               658    668  2
     D  PHEXP3               669    679  2
     D  PHAXP1               680    690  2
     D  PHAXP2               691    701  2
     D  PHAXP3               702    712  2
     D  PHANNP               713    723  2
     D  PHBFRQ               724    724
     D  PHBDAY               725    726
     D  PHBDAT               727    734  0
     D  PHSTAM               735    745  2
     D  PHSTRP               746    746
      *** beg add ***** 07/25/17 ******************************************
     D  PHLINK               747    761
     D  PHCUST               762    781
      *** end add ***** 07/25/17 ******************************************
      *** beg add ***** 09/01/17 ******************************************
     d  phCarCode            782    786
     d  phGrpName            787    846
     d  phPolFil#            847    866
     d  phDedAmPO            867    875  0
     d  phPercMan            876    880  2
      *** end add ***** 09/01/17 ******************************************
      *** beg add ***** 11/10/17 ******************************************
     d  phVdr#               881    885  0
     d  phVdrName            886    925
      *** end add ***** 11/10/17 ******************************************
      *** beg add ***** 01/24/18 ******************************************
     d  phDecNam1            926   1045
     d  phDecNam2           1046   1165
     d  phDecNam3           1166   1285
     d  phDecNam4           1286   1405
     d  phDecAdd1           1406   1465
     d  phDecAdd2           1466   1525
     d  phDecAdd3           1526   1585
     d  phDecFein           1586   1594
      *** end add ***** 01/24/18 ******************************************
     D*
     D* PREMIUM FACTORS DATA STRUCTURE
     D*
     D                 DS
     D  PFD                    1    108
     D  PFRTYP                 1      1
     D  PFCAP#                 2     26
     D  PFFND                 27     29  0
     D  PFEMP#                30     38  0
     D  PFDIV                 39     43  0
     D  PFCODE                44     48  0
     D  PFPRTE                49     54  3
     D  PFFRTE                55     59  4
     D  PFBLVL                60     60
     D  PFALVL                61     61
     D  PFEFDT                62     69  0
     D  PFEXDT                70     77  0
     D  PFAMT                 78     88  2
      *** beg add ***** 09/01/17 ******************************************
     d  pfPolFil#             89    108
      *** end add ***** 09/01/17 ******************************************
     D*
     D* PAYROLL DETAIL DATA STRUCTURE
     D*
     D                 DS
      *** beg chg ***** 06/17/24 ******************************************
     D*****PAD                    1    223
     D*****PAD                    1    259
     D  PAD                    1    261
      *** end chg ***** 06/17/24 ******************************************
     D  PARTYP                 1      1
     D  PACAP#                 2     26
     D  PAFND                 27     29  0
     D  PAEMP#                30     38  0
     D  PADIV                 39     43  0
     D  PAEAPR                44     54  2
     D  PACLAS                55     59  0
     D  PASPEC                60     60
     D  PAADLD                61     69  2
     D  PAEMPR                70     80  2
      **** Beg Chg **** 06/17/24 *******************************************
     D***PAARAT                81     87  2
     D***PAECNT                88     94  0
     D***PANTRT                95    101  2
     D***PABLRT               102    108  2
     D***PANBRT               109    115  2
     D***PACADT               116    123  0
     D***PADESC               124    153
     D***PAPPAY               154    164  2
     D***PAAAPR               165    175  2
     D***PASAFL               176    176
     D***PAPCF                177    177
     D***PAMNPR               178    188  2
     D***PANETF               189    193  4
     D***PAFPRM               194    204  2
     D***PAAMPR               205    215  2
     D***PAEFDT               216    223  0
     D***PAEXT                224    228
     D***PASAPR               229    239  0
     d***paPolFil#            240    259
     D  PAARAT                81     88  3
     D  PAECNT                89     95  0
     D  PANTRT                96    102  2
     D  PABLRT               103    110  3
     D  PANBRT               111    117  2
     D  PACADT               118    125  0
     D  PADESC               126    155
     D  PAPPAY               156    166  2
     D  PAAAPR               167    177  2
     D  PASAFL               178    178
     D  PAPCF                179    179
     D  PAMNPR               180    190  2
     D  PANETF               191    195  4
     D  PAFPRM               196    206  2
     D  PAAMPR               207    217  2
     D  PAEFDT               218    225  0
     D  PAEXT                226    230
     D  PASAPR               231    241  0
     d  paPolFil#            242    261
      **** End Chg **** 06/17/24 *******************************************
     D*
     D* EMPLOYEE DETAIL DATA STRUCTURE
     D*
     D                 DS
     D  PAE                    1    196
     D  PERTYP                 1      1
     D  PECAP#                 2     26
     D  PEFND                 27     29  0
     D  PEEMP#                30     38  0
     D  PEDIV                 39     43  0
     D  PECLAS                44     48  0
     D  PESPEC                49     49
     D  PEEXT                 50     54
     D  PELSN                 55     84
     D  PEFSN                 85     99
     D  PEMID                100    100
     D  PETITL               101    105
     D  PEPOSN               106    145
     D  PESS#                146    154
     D  PEEAPR               155    165  2
     D  PESAPR               166    176  2
      *** end add ***** 07/25/17 ******************************************
      *** beg add ***** 09/01/17 ******************************************
     d  pePolFil#            177    196
      *** end add ***** 09/01/17 ******************************************
     D*
     D* TRANSACTION DETAIL DATA STRUCTURE
     D*
     D                 DS
     D  TRD                    1     90
     D  TRRTYP                 1      1
     D  TRCAP#                 2     26
     D  TRFND                 27     29  0
     D  TREMP#                30     38  0
     D  TRDIV                 39     43  0
     D  TRTTYP                44     45
     D  TREFDT                46     53  0
     D  TRCRDT                54     61  0
     D  TRCTYP                62     62
     D  TRENDT                63     70  0
      *** beg add ***** 09/01/17 ******************************************
     d  trPolFil#             71     90
      *** end add ***** 09/01/17 ******************************************
     D*
     D* VENDOR TRAILER DATA STRUCTURE
     D*
     D                 DS
     D  VTD                    1     90
     D  VTRTYP                 1      1
     D  VTVDR#                 2      6  0
     D  VTRCNT                 7     19  0
     D  VTTPAY                20     36  2
     D  VTTADL                37     47  2
     D  VTTMAN                48     64  2
     D  VTECNT                65     73  0
     D  VTTPRM                74     90  2
     D*
     D* ADDITIONAL LOCATION/DIVISION DATA STRUCTURE.
     D*
     D ADD             DS           626
     D  ADRTYP                 1      1
     D  ADCAP#                 2     26
     D  ADFND                 27     29  0
     D  ADEMP#                30     38  0
     D  ADDIV                 39     43  0
     D  ADFYR                 44     46  0
     D  ADTYPE                47     47
     D  ADELID                48     57
     D  ADAGT#                58     62  0
     D  ADNAME                63    102
     D  ADFDNM               103    142
     D  ADMAD1               143    182
     D  ADMAD2               183    222
     D  ADMCTY               223    247
     D  ADMST                248    249
     D  ADMZIP               250    258  0
     D  ADPAD1               259    298
     D  ADPAD2               299    338
     D  ADPCTY               339    363
     D  ADPST                364    365
     D  ADPZIP               366    374  0
     D  ADPHN                375    384  0
     D  ADFAX                385    394  0
     D  ADCXDT               395    402  0
     D  ADSIC                403    408
     D  ADNAIC               409    418
     D  ADN100               419    518
     D  ADF100               519    618
     D  ADEFDT               619    626  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPEL02
     I              ELCO#                       E2CO#
     I              ELFND                       E2FND
     I              ELEMP#                      E2EMP#
     I              ELDIV                       E2DIV
     I              ELFYR                       E2FYR
     I              ELPFDT                      E2PFDT
     I              ELPTDT                      E2PTDT

     C*
     C* VENDOR FILE KEY
     C*
     C     VPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXVDR#
     C*
     C     VPKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C* EMPLOYER DETAIL KEY
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    X1FYR
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXTDT
     C*
     C     ELKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXTDT
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     GSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    GSFND
     C*
     C     PEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C*
     C*
     C*
     C* HISTORY FILE KEY
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXFDT
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C* ENDORSEMENT FILE KEY.
     C*
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEND#
     C*
     C     MUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  CARRIER/STATE REPORTING MASTER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    EMDIV
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  STAT RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXEFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXEFDT
     C*
     C     LMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXKEY
     C*
     C*  DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXDT              7 0
     C*
     C     DDKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXCODE
     C*
     C* EMPLOYER PREMIUM FILE
     C*
     C     MPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C                   KFLD                    XXEFDT
     C*
     C     EPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C*
     C*  PAYROLL CLASS TABLE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
      *
      *  VENDOR HISTORY FILE
      *
     C     VHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    KKFDT             7 0
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         X1FND
     C     *LIKE         DEFINE    GSFND         XXGRP
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELFYR         X1FYR
     C     *LIKE         DEFINE    ELFYR         XXLFYR
     C     *LIKE         DEFINE    ELFYR         XXLDYR
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    VPVDR#        XXVDR#
     C     *LIKE         DEFINE    VPVDR#        X1VDR#
     C     *LIKE         DEFINE    RPCAP#        XXCAP#
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    LMKEY         XXKEY
     C     *LIKE         DEFINE    DDCODE        XXCODE
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C     *LIKE         DEFINE    EPSPEC        XXSPEC
     C     *LIKE         DEFINE    MUEND#        XXEND#
      *** BEG ADD ***** 12/26/19 **************************
     C     *LIKE         DEFINE    RPCAP#        XXNACAP#
      *** END ADD ***** 12/26/19 **************************
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQVEND            5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQCALL            1
      *** beg add ***** 07/25/17 ******************************************
     C                   PARM                    QQEPRM            1
     C                   PARM                    QQEPYRL           1
      *** end add ***** 07/25/17 ******************************************
      *** beg add ***** 04/30/18 ******************************************
     C                   PARM                    QQVLDT            8
      *** end add ***** 04/30/18 ******************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* GET CLIENT ID
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C                   READ      SWCPF                                  60
     C*
     C* SETUP PROGRAM VARIABLES
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQVEND        XXVDR#
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C                   MOVE      QQTODT        XXTODT            8 0
     C     Q1PRMS        IFGT      4
     C                   MOVE      QQCALL        XXCALL            1
     C                   ELSE
     C                   MOVE      *BLANK        XXCALL
     C                   ENDIF
      *** beg add ***** 09/01/17 **************************
      /Free
       If %Parms > 5;
          xxExcPrem = qqEPrm;
          xxExcPayR = qqEPyRl;
       Else;
          xxExcPrem = *Blanks;
          xxExcPayR = *Blanks;
       Endif;
      /End-Free
      *** end add ***** 09/01/17 **************************
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT             7 0
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C*
     C                   SELECT
      *** beg add ***** 04/30/18 ***************************
      /Free
       When  xxCall = 'F';
          asOfDate = %Date(qqVlDt:*Usa0);
          cymdValDate = %Dec(asOfDate:*Cymd);
          In pgmLDA;
          If afMode = 'Validate';
             Open WXPXPAFV;
          Else;
             Open WXPXPAF;
          Endif;
          RunAfExport();
          If afMode = 'Validate';
             Close WXPXPAFV;
          Else;
             Close WXPXPAF;
          Endif;
      /End-Free
      *** end add ***** 04/30/18 ***************************
     C     XXCALL        WHENNE    'G'
     C*
     C* CHAIN TO VENDOR FILE
     C*
     C     XXVDR#        IFNE      *ZERO
     C     VPKY          CHAIN     PVP01                              60
     C                   ELSE
     C     VPKY1         CHAIN     PVP01                              60
     C                   ENDIF                                                  XXVDR# IFNE
     C*
     C* MAIN LOOP
     C*
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      *OFF          *IN61
     C                   Z-ADD     0             XXRCNT           13 0
     C                   Z-ADD     0             XXTPAY           17 2
     C                   Z-ADD     0             XXTADL           11 2
     C                   Z-ADD     0             XXTMAN           17 2
     C                   Z-ADD     0             XXECNT            9 0
     C                   Z-ADD     0             XXTPRM           17 2
      *** beg add ***** 01/05/18 ************************
      /Free
       xxPTyC    = VPptyc; // Write policy header for each division
       xxAddA    = VPexal; // Write additional locations
       xxMidTerm = VPexnp; // Include non-premium m/t endorsements
      /End-Free
      *** end add ***** 01/05/18 ************************
     C*
     C     ELKY          CHAIN     PEL01                              70
     C***                MOVE      'N'           XXADNA            1            DLT 01/05/18
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     *IN70         DOWEQ     *OFF
     C                   MOVE      *OFF          *IN71

      *** beg add ***** 02/21/18******************
      /Free
       If ELfnd <> xxFnd;
          GetGrpInfo(ELfnd);
       Endif;
      /End-Free
      *** end add ***** 02/21/18******************

     C*
     C     XXPTYC        IFEQ      'Y'                                          chg 01/05/18
     C*
     C* GET LATEST POLICY YEAR FOR FUND
     C     ELFND         IFNE      XXFND
     C                   EXSR      GTLFYR
     C                   ENDIF                                                  ELFND<>XXFND
     C*
     C* THIS IS NEW DIVISION, SET PRINT FLAG FOR DIVISION CHANGE
     C* AND FETCH THE LATEST DIVISION YEAR
     C     ELFND         IFNE      XXFND
     C     ELEMP#        ORNE      XXEMP#
     C     ELDIV         ORNE      XXDIV
     C                   MOVE      'Y'           XXWRDV            1
     C                   EXSR      GTLDYR
     C                   ENDIF                                                  ELDIV NE XXDIV
     C*
     C* CHECK ACTIVITY FOR POLICY
     C     ELFND         IFNE      XXFND
     C     ELEMP#        ORNE      XXEMP#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C                   EXSR      CHKACT
     C                   ENDIF                                                  ELEMP#>XXEMP#

     C                   ENDIF                                                  XXPTYC = 'Y'

      *** BEG ADD ***** 01/05/18 **********************
      * Indicate that Name and Address records haven't been written yet
      *  if not flagged to write policy headers on each division
     C     ELFND         IFNE      XXFND
     C     ELEMP#        ORNE      XXEMP#
     C                   EVAL      POLNAWRTN = *OFF
     C                   ENDIF                                                  ELEMP#>XXEMP#
      *** END ADD ***** 01/05/18 **********************

     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C     ELPFDT        IFGT      XXFDT
     C                   Z-ADD     ELPFDT        KKFDT
     C                   ELSE
     C                   Z-ADD     XXFDT         KKFDT
     C                   ENDIF
      *
     C     VHKY          SETGT     PVH01
     C     EHKY1         READPE    PVH01                                  62
     C     *IN62         IFEQ      *OFF
      *
     C                   SELECT
     C     VHEXDT        WHENEQ    *ZERO
     C                   Z-ADD     VHVDR#        X1VDR#
     C     VHEXDT        WHENNE    *ZERO
     C     VHEXDT        ANDGT     XXFDT
     C*** Begin Add *** 11/22/17 *******************************
     C     VHEXDT        ANDNE     VHEFDT
     C*** End   Add *** 11/22/17 *******************************
     C                   Z-ADD     VHVDR#        X1VDR#
     C                   OTHER
     C                   Z-ADD     *ZERO         X1VDR#
     C                   ENDSL
      *
     C                   ELSE
     C                   Z-ADD     *ZERO         X1VDR#
     C                   ENDIF
      *
     C     X1VDR#        IFEQ      VPVDR#
     C*
     C                   EXSR      EMPSR
     C*
     C*** WRITE HEADER RECORD
     C*** AND ACTIVITY FOUND FOR ANY DIVISION
     C*** AND CURRENT DIVISION HEADER NOT WRITTEN YET
     C*** AND POLICY FROM DATE IS WITHIN RUN TO DATE
     C     XXPTYC        IFEQ      'Y'                                          chg 01/05/18
     C     XXACTV        ANDEQ     'Y'
     C     XXWRDV        ANDEQ     'Y'
     C     ELPFDT        ANDLE     XXTDT
     C*
     C*** IF THIS IS LATEST FUND YEAR OR LATEST YEAR ON DIVISION
     C     XXFYR         IFEQ      XXLFYR
     C     XXFYR         OREQ      XXLDYR
     C*
     C* WRITE VENDOR HEADER IF NEEDED
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           *IN61
     C                   EXSR      WRTVH
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C     *IN71         IFEQ      *OFF
     C                   MOVE      *ON           *IN71
     C                   EXSR      WRTEH
     C                   ENDIF                                                  *IN71= '0'
     C*
     C                   ENDIF                                                  XXFYR = XXLFYR
     C*
     C                   ENDIF                                                  XXPTYC = 'Y'
     C*
     C                   ENDIF                                                  ELVDR# IFEQ
     C*
     C     ELKY          READE     PEL01                                  70
     C*
      *** BEG DEL ***** 01/05/18 ***************************
     C***  xxPTYC        IFEQ      'Y'
     C**CHECK POLICY BREAK AND PRINT ADDITIONAL ADDRESSES
     C***OR EOF REACHED
     C***  ELFND         IFNE      XXFND
     C***  ELEMP#        ORNE      XXEMP#
     C***  *IN70         OREQ      *ON
     C***
     C**PRINT ADDITIONAL ADDRESSES ON POLICY BREAK FOR 139
     C***  XXADDA        IFEQ      'Y'
     C***  XXADNA        ANDEQ     'Y'
     C***                EXSR      WRTNA
     C***                ENDIF
     C***
     C***                MOVE      'N'           XXADNA
     C***
     C***                ENDIF                                                  ELEMP# <> XXEMP#
     C***
     C***                ENDIF                                                  XXCLT# ='139'
      *** END DEL ***** 01/05/18 ***************************
     C*
     C                   ENDDO                                                  *IN70 DOWEQ
     C*
     C* WRITE VENDOR TRAILER RECORD IF NEEDED
     C*
     C     *IN61         IFEQ      *ON
     C                   EXSR      WRTVT
     C                   ENDIF
     C*
     C* PROCESS NEXT VENDOR IF RUNNING FOR ALL
     C*
     C     XXVDR#        IFNE      *ZERO
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C     VPKY1         READE     PVP01                                  60
     C                   ENDIF                                                  XXVDR# IFNE
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXCALL        WHENEQ    'G'                                          Group Select
     C*
     C* CHAIN TO GROUP FILE
     C*
     C     ELKY          CHAIN     PGS01                              60
     C                   Z-ADD     GSFND         XXGRP
     C     *IN60         DOWEQ     *OFF
     C     GSIEFL        IFEQ      'I'
      *** beg chg ***** 02/21/18 ****************
     C***     GSFND         IFNE      XXGRP
     C***                   Z-ADD     GSFND         XXGRP
     C***                   Z-ADD     XXFRDT        SRDTI
     C***                   CALL      'SRCVTDAT'    DTCYMD
     C***                   MOVEL     SRDTO         XXFDT             7 0
     C***                   Z-ADD     XXTODT        SRDTI
     C***                   CALL      'SRCVTDAT'    DTCYMD
     C***                   MOVEL     SRDTO         XXTDT             7 0
     C***                   ENDIF                                                  GSFND <> XXGRP
      /Free
       If GSfnd <> xxGrp;
          GetGrpInfo(GSfnd);
       Endif;
      /End-Free
      *** end chg ***** 02/21/18 ****************
     C                   MOVE      *OFF          *IN61
     C                   Z-ADD     0             XXRCNT           13 0
     C                   Z-ADD     0             XXTPAY           17 2
     C                   Z-ADD     0             XXTADL           11 2
     C                   Z-ADD     0             XXTMAN           17 2
     C                   Z-ADD     0             XXECNT            9 0
     C                   Z-ADD     0             XXTPRM           17 2
     C     GSKY          CHAIN     PEL01                              70
     C***                MOVE      'N'           XXADNA            1            DLT 01/05/18
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C*
     C     *IN70         DOWEQ     *OFF
     C                   MOVE      *OFF          *IN71
     C     PEKY          CHAIN     PPE01                              62
     C     *IN62         IFEQ      *OFF
      *** MODIFY ACTIVITY DATE RANGE ON THE BASIS OF EFFECTIVE
      *** AND EXPIRATION DATE
     C                   MOVE      PEPTYC        XXPTYC            1
     C                   MOVE      PEADDA        XXADDA            1
     C                   MOVE      PEENDO        XXMIDTERM                      ADD 01/05/18
      *
     C     PEEFDT        IFNE      *ZEROS
     C     PEEFDT        ANDGT     XXFDT
     C                   Z-ADD     PEEFDT        XXFDT
     C                   ENDIF
      *
     C     PEEXDT        IFNE      *ZEROS
     C     PEEXDT        ANDLT     XXTDT
     C                   Z-ADD     PEEXDT        XXTDT
     C                   ENDIF
      *
     C     PEEFDT        IFNE      *ZEROS
      *
     C     XXFDT         IFLT      PEEFDT
     C     XXTDT         ANDLT     PEEFDT
     C                   MOVE      *BLANK        XXPTYC
     C                   MOVE      *BLANK        XXADDA
     C                   ENDIF
     C                   ENDIF                                                  EFDT & EXDT<>0
      *
     C     PEEXDT        IFNE      *ZEROS
      *
     C     XXFDT         IFGT      PEEXDT
     C     XXTDT         ANDGT     PEEXDT
     C                   MOVE      *BLANK        XXPTYC
     C                   MOVE      *BLANK        XXADDA
     C                   ENDIF
      *
     C                   ENDIF                                                  EFDT & EXDT<>0
      *
     C     XXPTYC        IFEQ      'Y'
     C*
     C* GET LATEST POLICY YEAR FOR FUND
     C     ELFND         IFNE      XXFND
     C                   EXSR      GTLFYR
     C                   ENDIF                                                  ELFND<>XXFND
     C*
     C* THIS IS NEW DIVISION, SET PRINT FLAG FOR DIVISION CHANGE
     C* AND FETCH THE LATEST DIVISION YEAR
     C     ELFND         IFNE      XXFND
     C     ELEMP#        ORNE      XXEMP#
     C     ELDIV         ORNE      XXDIV
     C                   MOVE      'Y'           XXWRDV
     C                   EXSR      GTLDYR
     C                   ENDIF                                                  ELDIV NE XXDIV
     C*
     C* CHECK ACTIVITY FOR POLICY
     C     ELFND         IFNE      XXFND
     C     ELEMP#        ORNE      XXEMP#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C                   EXSR      VERACT
     C                   ENDIF                                                  ELEMP#>XXEMP#

     C                   ENDIF                                                  XXPTYC = 'Y'

      *** BEG ADD ***** 01/05/18 **********************
      * Indicate that Name and Address records haven't been written yet
      *  if not flagged to write policy headers on each division
     C     ELFND         IFNE      XXFND
     C     ELEMP#        ORNE      XXEMP#
     C                   EVAL      POLNAWRTN = *OFF
     C                   ENDIF                                                  ELEMP#>XXEMP#
      *** END ADD ***** 01/05/18 **********************

     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFND         X1FND
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C     PEEFDT        IFNE      *ZEROS
      *
     C     XXFDT         IFLT      PEEFDT
     C     XXTDT         ANDLT     PEEFDT
     C                   Z-ADD     *ZERO         X1FND
     C                   ENDIF
      *
     C                   ENDIF                                                  EFDT & EXDT<>0
      *
     C     PEEXDT        IFNE      *ZEROS
      *
     C     XXFDT         IFGT      PEEXDT
     C     XXTDT         ANDGT     PEEXDT
     C                   Z-ADD     *ZERO         X1FND
     C                   ENDIF
      *
     C                   ENDIF                                                  EFDT & EXDT<>0
      *
     C     X1FND         IFEQ      GSFND
      *
     C                   EXSR      EMPSR
     C*
     C* WRITE HEADER RECORD - IF PEPTYC = 'Y'
     C*** AND ACTIVITY FOUND FOR ANY DIVISION
     C*** AND CURRENT DIVISION HEADER NOT WRITTEN YET
     C*** AND POLICY FROM DATE IS WITHIN RUN TO DATE
     C     XXPTYC        IFEQ      'Y'
     C     XXACTV        ANDEQ     'Y'
     C     XXWRDV        ANDEQ     'Y'
     C     ELPFDT        ANDLE     XXTDT
     C*
     C*** IF THIS IS LATEST FUND YEAR OR LATEST YEAR ON DIVISION
     C     XXFYR         IFEQ      XXLFYR
     C     XXFYR         OREQ      XXLDYR
     C*
     C* WRITE GROUP HEADER IF NEEDED
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           *IN61
     C                   EXSR      WRTVH
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C     *IN71         IFEQ      *OFF
     C                   MOVE      *ON           *IN71
     C                   EXSR      WRTEH
     C                   ENDIF                                                  *IN71= '0'
     C*
     C                   ENDIF                                                  XXFYR = XXLFYR
     C                   ENDIF                                                  XXACTV= 'Y'
     C*
     C                   ENDIF                                                  GSFND = X1FND
     C*
     C                   ENDIF                                                  *IN62 = *OFF
     C*
     C     GSKY          READE     PEL01                                  70
     C*
      *** BEG DEL ***** 01/05/18 ***************************
     C**CHECK POLICY BREAK AND PRINT ADDITIONAL ADDRESSES
     C***OR EOF REACHED
     C***  XXPTYC        IFEQ      'Y'
     C***
     C***  ELFND         IFNE      XXFND
     C***  ELEMP#        ORNE      XXEMP#
     C***  *IN70         OREQ      *ON
     C***
     C**PRINT ADDITIONAL ADDRESSES ON POLICY BREAK FOR 139
     C***  XXADDA        IFEQ      'Y'
     C***  XXADNA        ANDEQ     'Y'
     C***                EXSR      WRTNA
     C***                ENDIF                                                  PEADDA
     C***
     C***                MOVE      'N'           XXADNA
     C***
     C***                ENDIF                                                  ELEMP# <> XXEMP#
     C***
     C***                ENDIF                                                  XXPTYC = ' '
      *** END DEL ***** 01/05/18 ***************************
     C*
     C                   ENDDO                                                  *IN70 DOWEQ
     C*
     C* WRITE GROUP TRAILER RECORD IF NEEDED
     C*
     C     *IN61         IFEQ      *ON
     C                   EXSR      WRTVT
     C                   ENDIF
     C*
     C                   ENDIF                                                  GSIEFL = 'I'
     C* PROCESS NEXT GROUP
     C*
     C     ELKY          READE     PGS01                                  60
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDSL
     C* END OF PROGRAM
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C*
     C* SUBROUTINE TO PROCESS A VENDOR'S EMPLOYER
     C*
     C     EMPSR         BEGSR
     C*
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C* START READING HISTORY FILES AT BEGINNING DATE
     C*
     C     EHKY          SETLL     PEH01
     C     EHKY          SETLL     PXH01
     C*
     C     EHKY1         READE     PEH01                                  80
     C     EHKY1         READE     PXH01                                  90
     C*
     C*
     C* PROCESS HISTORY TRANSACTIONS
     C*
     C     *IN80         DOWEQ     *OFF
     C     EHTRDT        ANDLE     XXTDT
     C     *IN90         OREQ      *OFF
     C     XHDT          ANDLE     XXTDT
     C*
     C                   MOVE      *OFF          *IN81
     C                   MOVE      *OFF          *IN91
      *** beg add ***** 02/21/18 **************************
     c                   Eval      hasWMMUP = *Off
      *** end add ***** 02/21/18 **************************
     C*
     C* IF WE HAVE TWO VALID TRANSACTIONS, PICK WHICH OCCURS FIRST
     C*
     C     *IN80         IFEQ      *OFF
     C     EHTRDT        ANDLE     XXTDT
     C     *IN90         ANDEQ     *OFF
     C     XHDT          ANDLE     XXTDT
     C*
     C                   MOVEL     EHTRDT        XXEHDT           13 0
     C                   MOVE      EHTIME        XXEHDT           13 0
     C                   MOVEL     XHDT          XXXHDT           13 0
     C                   MOVE      XHTIME        XXXHDT           13 0
     C*
     C     XXEHDT        IFLE      XXXHDT
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C                   MOVE      *ON           *IN91
     C                   ENDIF
     C*
     C                   ELSE                                                   *IN80 IFEQ
     C*
     C* PROCESS EMPLOYER HISTORY IF VALID
     C*
     C     *IN80         IFEQ      *OFF
     C     EHTRDT        ANDLE     XXTDT
     C                   MOVE      *ON           *IN81
     C                   ENDIF                                                  *IN80 IFEQ
     C*
     C* PROCESS CANCELLATION HISTORY IF VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     XHDT          ANDLE     XXTDT
     C                   MOVE      *ON           *IN91
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ENDIF                                                  *IN80 IFEQ
     C*
     C* PROCESS TRANSACTION AND READ NEXT RECORD
     C*
     C     *IN81         IFEQ      *ON
     C                   EXSR      ETRANS
     C     EHKY1         READE     PEH01                                  80
     C                   ENDIF                                                  *IN81 IFEQ
     C*
     C     *IN91         IFEQ      *ON
     C                   EXSR      XTRANS
     C     EHKY1         READE     PXH01                                  90
     C                   ENDIF                                                  *IN91 IFEQ
     C*
     C                   ENDDO                                                  *IN80 DOWEQ
     C*
     C* PROCESS ENDORSEMENT TRANSACTIONS.
     C*
      *** beg chg ***** 01/05/18 **********************
     C***  VPEXNP        IFEQ      'Y'
     C     XXMIDTERM     IFEQ      'Y'
      *** end chg ***** 01/05/18 **********************
     C*
     C                   Z-ADD     0             XXEND#
     C     MUKY          SETGT     PMU01
     C     MUKY1         READE     PMU01                                  95
     C*
     C     *IN95         DOWEQ     *OFF
     C     MUDT          ANDLT     XXFDT
     C     MUKY1         READE     PMU01                                  95
     C                   ENDDO
     C*
     C     *IN95         DOWEQ     *OFF
     C     MUPPGM        ANDEQ     'W0051R21'
     C     *IN95         OREQ      *OFF
     C     MUPPGM        ANDEQ     *BLANKS
     C     MUKY1         READE     PMU01                                  95
     C                   ENDDO                                                  *IN95 DOWEQ
     C*
     C     *IN95         DOWEQ     *OFF
     C     MUDT          ANDLE     XXTDT
     C*
     C                   EXSR      MTRANS
      *** beg add ***** 02/21/18 ************************
     c                   eval      hasWMMUP = *On
      *** end add ***** 02/21/18 ************************
     C*
     C     MUKY1         READE     PMU01                                  95
     C*
     C     *IN95         DOWEQ     *OFF
     C     MUPPGM        ANDEQ     'W0051R21'
     C     *IN95         OREQ      *OFF
     C     MUPPGM        ANDEQ     *BLANKS
     C     MUKY1         READE     PMU01                                  95
     C                   ENDDO                                                  *IN95 DOWEQ
     C*
     C                   ENDDO                                                  *IN95 DOWEQ
     C*
     C                   ENDIF                                                  XXMIDTERM = 'Y'
     C*
      *** beg add ***** 02/21/18 **************************
      /Free
       Select;
       When *In81 or *In91 or hasWMMUP or not(isIncNBDiv);
       Other;
          If ELqtfr <> 0;
             xxCmpDate = ELqtfr;
          Else;
             xxCmpDate = ELidt;
          Endif;
          If xxCmpDate >= xxFDt and ELidt <= xxTDt;
             If Not(*In61);
                *In61 = *On;
                Exsr WrtVh;
             Endif;
             *In71 = *On;
             Exsr wrtEH;
          Endif;
       Endsl;

      /End-Free
      *** end add ***** 02/21/18 **************************
     C                   ENDSR                                                  EMPSR
     C*****************************************************************
     C*
     C* SUBROUTINE TO PROCESS AN EMPLOYER HISTORY TRANSACTION
     C*
     C     ETRANS        BEGSR
     C*
     C* WRITE VENDOR HEADER IF NEEDED
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           *IN61
     C                   EXSR      WRTVH
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C* WRITE EMPLOYER HEADER IF NEEDED
     C*
     C     *IN71         IFEQ      *OFF
     C                   MOVE      *ON           *IN71
     C                   EXSR      WRTEH
     C                   ENDIF                                                  *IN71 IFEQ
     C*
     C* PROCESS TRANSACTION
     C*
     C                   CLEAR                   TRD
     C                   CLEAR                   DATA
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'X'           TRRTYP
     C                   MOVEL     XXCAP#        TRCAP#
     C                   MOVE      XXFND         TRFND
     C                   MOVE      XXEMP#        TREMP#
     C                   MOVE      XXDIV         TRDIV
     C*
     C     EHFAF         IFEQ      'X'
     C                   MOVE      'XB'          TRTTYP
     C                   ELSE
     C*
     C     EHFAF         IFEQ      'A'
     C                   MOVE      'AB'          TRTTYP
     C                   ELSE
     C*
     C     EHFAF         IFEQ      'R'
     C                   MOVE      'EB'          TRTTYP
     C                   ELSE
     C*
     C     ELNWRW        IFEQ      'R'
     C                   MOVE      'RB'          TRTTYP
     C                   ELSE
     C                   MOVE      'NB'          TRTTYP
     C                   ENDIF                                                  EHNWRW IFEQ
     C*
     C                   ENDIF                                                  EHFAF IFEQ
     C*
     C                   ENDIF                                                  EHFAF IFEQ
     C*
     C                   ENDIF                                                  EHFAF IFEQ
     C*
     C     19000000      ADD       EHEFDT        TREFDT
     C                   Z-ADD     0             TRCRDT
     C                   Z-ADD     0             TRENDT
     C                   MOVE      *BLANKS       TRCTYP
     C*
     C                   MOVEL     TRD           DATA

      *** Beg Chg ********* 9/01/17 *****************************
      *****              WRITE     PPX01
      *****              ADD       1             XXRCNT
      *** Only write transactions if Exclude premium flag = *blank
     C                   If        xxExcPrem = *Blank
     C                   Write     PPX01
     C                   Add       1             XXRCNT
     C                   Endif
      *** End Chg ********* 9/01/17 *****************************
     C*
     C                   ENDSR                                                  ETRANS
     C*****************************************************************
     C*
     C* SUBROUTINE TO PROCESS AN CANCELLATION HISTORY TRANSACTION
     C*
     C     XTRANS        BEGSR
     C*
     C* WRITE VENDOR HEADER IF NEEDED
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           *IN61
     C                   EXSR      WRTVH
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C* WRITE EMPLOYER HEADER IF NEEDED
     C*
     C     *IN71         IFEQ      *OFF
     C                   MOVE      *ON           *IN71
     C                   EXSR      WRTEH
     C                   ENDIF                                                  *IN71 IFEQ
     C*
     C* PROCESS TRANSACTION
     C*
     C                   CLEAR                   TRD
     C                   CLEAR                   DATA
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'X'           TRRTYP
     C                   MOVEL     XXCAP#        TRCAP#
     C                   MOVE      XXFND         TRFND
     C                   MOVE      XXEMP#        TREMP#
     C                   MOVE      XXDIV         TRDIV
     C*
     C     XHREDT        IFNE      *ZERO
     C                   MOVE      'RE'          TRTTYP
     C                   ELSE
     C                   MOVE      'CX'          TRTTYP
     C                   ENDIF
     C*
     C                   Z-ADD     0             TREFDT
     C                   Z-ADD     0             TRENDT
     C*
     C     XHREDT        IFNE      *ZERO
     C     19000000      ADD       XHREDT        TRCRDT
     C                   ELSE
     C     19000000      ADD       XHCXDT        TRCRDT
     C                   ENDIF
     C*
     C     XHCXDT        IFEQ      ELPFDT
     C                   MOVE      'F'           TRCTYP
     C                   ELSE
     C*
     C     XHCXDT        IFGT      ELPFDT
     C                   MOVE      ELSRPR        TRCTYP
     C                   ELSE
     C                   MOVE      *BLANKS       TRCTYP
     C                   ENDIF                                                  XHCXDT IFGT
     C*
     C                   ENDIF                                                  XHCXDT IFEQ
     C*
     C                   MOVEL     TRD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C*
     C                   ENDSR                                                  XTRANS
     C*****************************************************************
     C*
     C* SUBROUTINE TO PROCESS AN ENDORSEMENT TRANSACTION
     C*
     C     MTRANS        BEGSR
     C*
     C* WRITE VENDOR HEADER IF NEEDED
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           *IN61
     C                   EXSR      WRTVH
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C* WRITE EMPLOYER HEADER IF NEEDED
     C*
     C     *IN71         IFEQ      *OFF
     C                   MOVE      *ON           *IN71
     C                   EXSR      WRTEH
     C                   ENDIF                                                  *IN71 IFEQ
     C*
     C* PROCESS TRANSACTION
     C*
     C                   CLEAR                   TRD
     C                   CLEAR                   DATA
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'X'           TRRTYP
     C                   MOVEL     XXCAP#        TRCAP#
     C                   MOVE      XXFND         TRFND
     C                   MOVE      XXEMP#        TREMP#
     C                   MOVE      XXDIV         TRDIV
       //*** beg add ***** 09/01/17 **********************
      /Free
       trPolFil# = %EditC(ELfnd:'X') + %EditC(ELemp#:'X') + %EditC(ELfyr:'X');
      /End-Free
       //*** end add ***** 09/01/17 **********************
     C*
     C                   MOVE      'ME'          TRTTYP
     C*
     C                   Z-ADD     0             TREFDT
     C                   Z-ADD     0             TRCRDT
     C*
     C     19000000      ADD       MUEEDT        TRENDT
     C*
     C                   MOVEL     TRD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C*
     C                   ENDSR                                                  MTRANS
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE A VENDOR HEADER RECORD
     C*
     C     WRTVH         BEGSR
     C*
     C                   CLEAR                   VHD
     C                   CLEAR                   DATA
     C*
     C                   SELECT
     C     XXCALL        WHENEQ    ' '
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'H'           VHRTYP
     C                   Z-ADD     VPVDR#        VHVDR#
     C                   MOVE      VPFTYP        VHFTYP
     C                   MOVE      VPDTYP        VHDTYP
     C                   MOVE      VPDFMT        VHDFMT
     C                   MOVEL     VPNAME        VHNAME
     C                   MOVEL     VPEMAL        VHMAIL
     C                   MOVEL     VPADR1        VHADR1
     C                   MOVEL     VPADR2        VHADR2
     C                   MOVEL     VPCITY        VHCITY
     C                   MOVEL     VPST          VHST
     C                   Z-ADD     VPZIP         VHZIP
     C*
     C     XXCALL        WHENEQ    'G'
     C*** MOVE GROUP INFORMATION
      *** beg del ***** 02/21/18 ****************
     C***  GSKY          CHAIN     PFN01                              60
      *** beg add ***** 09/01/17 ****************
     C***  GSKY          CHAIN     PF200
     C***  *IN60         IFEQ      *OFF
      *** end del ***** 02/21/18 ****************
     C                   MOVE      'H'           VHRTYP
     C                   Z-ADD     GSFND         VHVDR#
     C                   MOVEL     FNNAME        VHNAME
     C                   MOVE      ' '           VHFTYP
     C                   MOVE      ' '           VHDTYP
     C                   MOVE      ' '           VHDFMT
     C                   MOVEL     VPEMAL        VHMAIL
     C                   MOVEL     FNMAD1        VHADR1
     C                   MOVEL     FNMAD2        VHADR2
     C                   MOVEL     FNMCTY        VHCITY
     C                   MOVEL     FNMST         VHST
     C                   Z-ADD     FNMZIP        VHZIP
      *** beg del ***** 02/21/18 ****************
     C***                ENDIF
      *** end del ***** 02/21/18 ****************
     C                   ENDSL
     C                   MOVEL     VHD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C*
     C                   ENDSR                                                  WRTVH
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE AN EMPLOYER HEADER RECORD
     C*
     C     WRTEH         BEGSR
     C*
     C                   CLEAR                   PHD
     C                   CLEAR                   DATA
     C*
     C* FIND MINIMUM PREMIUM FLAG FROM STATE RPT DIVISION
     C*
     C                   Z-ADD     0             XSDIV             5 0
     C                   Z-ADD     XXDIV         SVDIV             5 0
     C                   MOVE      *BLANK        XXMINP            1
     C     EMKY1         CHAIN     PEM01                              50
     C     *IN50         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C*** begin add ***  09/15/23  ******************************************
     C                   Leave
     C*** end add *****  09/15/23  ******************************************
     C                   ENDIF
     C     EMKY1         READE     PEM01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C     XSDIV         IFEQ      XXDIV
     C                   MOVE      'Y'           PHSTRP
     C                   ENDIF
     C                   Z-ADD     XSDIV         XXDIV
     C     ELKY1         CHAIN     PEL01                              50
     C     *IN50         IFEQ      *OFF
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVE      ELAMNP        XXMINP
     C                   ELSE
     C                   MOVE      ELEMNP        XXMINP
     C                   ENDIF                                                  ELFAF IFEQ

      *** beg add ***** 01/24/18 ***************************
      /Free
       GetDecPageInfo();
       phDecNam1 = xxDecNam1;
       phDecNam2 = xxDecNam2;
       phDecNam3 = xxDecNam3;
       phDecNam4 = xxDecNam4;
       phDecAdd1 = xxDecAdd1;
       phDecAdd2 = xxDecAdd2;
       phDecAdd3 = xxDecAdd3;
       phDecFein = %Editc(xxDecFein:'X');
      /End-Free
      *** end add ***** 01/24/18 ***************************

     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKY1         CHAIN     PEL01                              50
     C*
     C* FIND CANCELLATION DATE FROM EMPLOYER MASTER
     C*
     C     EMKY          CHAIN     PEM01                              50

      *** beg add ***** 07/25/17 ******************************************
     C                   Eval      PHLINK = EMLINK
     C                   Eval      PHCUST = EMCUST
      *** end add ***** 07/25/17 ******************************************
     C*
     C                   Z-ADD     0             XXCADT            7 0
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C     Y             DOWNE     0
     C     ELPTDT        COMP      ED(Y)                              50  50
     C  N50              SUB       1             Y
     C  N50              ENDDO
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C                   Z-ADD     CD(Y)         XXCADT
     C                   ENDIF
     C*
     C* FIND CARRIER POLICY #
     C*
     C                   MOVE      *BLANKS       XXCAP#
     C     RPKY          CHAIN     PRP01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVE      RPCAP#        XXCAP#
     C                   ENDIF
     C*
     C* FIND PREMIUM RATING STATE
     C*
     C                   MOVE      *BLANKS       XXJRST            2
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVE      RSJRST        XXJRST
     C                   ENDIF
     C*
     C* CHECK FOR AGENCY/DIRECT BILL
     C*
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     XXEMP#        @@EMP#
     C                   Z-ADD     XXDIV         @@DIV
     C                   Z-ADD     XXFYR         @@FYR
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@AGT#            5 0
     C                   PARM                    @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
     C*
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'P'           PHRTYP
     C                   MOVEL     XXCAP#        PHCAP#
     C                   Z-ADD     XXFND         PHFND
     C                   Z-ADD     XXEMP#        PHEMP#
     C                   Z-ADD     XXDIV         PHDIV
     C                   Z-ADD     XXFYR         PHFYR
     C                   MOVEL     EMNAME        PHNAME
     C                   MOVEL     EMFDNM        PHFDNM
     C                   MOVEL     EMMAD1        PHMAD1
     C                   MOVEL     EMMAD2        PHMAD2
     C                   MOVEL     EMMCTY        PHMCTY
     C                   MOVEL     EMMST         PHMST
     C                   Z-ADD     EMMZIP        PHMZIP
     C                   MOVEL     EMPAD1        PHPAD1
     C                   MOVEL     EMPAD2        PHPAD2
     C                   MOVEL     EMPCTY        PHPCTY
     C                   MOVEL     EMPST         PHPST
     C                   Z-ADD     EMPZIP        PHPZIP
     C                   MOVEL     EMAREA        PHPHN
     C                   MOVE      EMPHN#        PHPHN
     C                   MOVEL     EMFAXA        PHFAX
     C                   MOVE      EMFAX#        PHFAX
     C     19000000      ADD       ELPFDT        PHPFDT
     C     19000000      ADD       ELPTDT        PHPTDT
     C*
     C     XXCADT        IFNE      *ZERO
     C     19000000      ADD       XXCADT        PHCXDT
     C                   ENDIF
     C*
     C     ELANDT        IFNE      *ZERO
     C     19000000      ADD       ELANDT        PHANDT
     C                   ENDIF
     C*
     C                   Z-ADD     ELPRTF        PHPRTF
     C                   Z-ADD     ELPROR        PHPROR
     C                   Z-ADD     ELPRTD        PHPRTD
     C                   MOVE      XXJRST        PHJRST
     C                   Z-ADD     ELANPR        PHANPR
     C                   Z-ADD     ELAAPR        PHAAPR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVE      'Y'           PHAFLG
     C                   ENDIF
     C*
     C     XXCADT        IFNE      *ZERO
     C*
     C     XXCADT        IFEQ      ELPFDT
     C                   MOVE      'F'           PHCTYP
     C                   ELSE
     C                   MOVE      ELSRPR        PHCTYP
     C                   ENDIF                                                  XXCADT IFNE
     C*
     C                   ENDIF                                                  XXCADT IFNE
     C*
     C                   Z-ADD     EMFEIN        PHFEIN
     C                   MOVE      XXMINP        PHMINP
     C                   Z-ADD     ELMAPR        PHMAPR
     C                   Z-ADD     ELAMPR        PHAMPR
     C                   Z-ADD     ELESBJ        PHESBJ
     C                   Z-ADD     ELASBJ        PHASBJ
     C                   Z-ADD     ELSTPR        PHSTPR
     C                   Z-ADD     ELASPR        PHASPR
     C                   Z-ADD     ELEMPM        PHEMPM
     C                   Z-ADD     ELAMPM        PHAMPM
     C                   Z-ADD     ELENMP        PHENMP
     C                   Z-ADD     ELANMP        PHANMP
     C                   Z-ADD     ELTXDU        PHTXDU
     C                   Z-ADD     ELLOSS        PHLOSS
     C                   MOVE      EMTPOB        PHTPOB
     C*
     C     @@PLAN        IFEQ      'A'
     C                   MOVE      'A'           PHBTYP
     C                   ELSE
     C                   MOVE      'D'           PHBTYP
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C                   Z-ADD     ELEXM1        PHEXM1
     C*
     C     ELEMD1        IFNE      *ZERO
     C     19000000      ADD       ELEMD1        PHEMD1
     C                   ENDIF
     C*
     C                   Z-ADD     ELEXM2        PHEXM2
     C*
     C     ELEMD2        IFNE      *ZERO
     C     19000000      ADD       ELEMD2        PHEMD2
     C                   ENDIF
     C*
     C                   Z-ADD     ELEXM3        PHEXM3
     C*
     C     ELEMD3        IFNE      *ZERO
     C     19000000      ADD       ELEMD3        PHEMD3
     C                   ENDIF
     C*
     C                   Z-ADD     ELEXP1        PHEXP1
     C                   Z-ADD     ELEXP2        PHEXP2
     C                   Z-ADD     ELEXP3        PHEXP3
     C                   Z-ADD     ELAXP1        PHAXP1
     C                   Z-ADD     ELAXP2        PHAXP2
     C                   Z-ADD     ELAXP3        PHAXP3
     C                   Z-ADD     ELENMP        PHANNP
     C                   ADD       ELPRTF        PHANNP
     C                   ADD       ELETER        PHANNP
     C                   ADD       ELEDTR        PHANNP
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     BSFREQ        PHBFRQ
     C                   MOVEL     BSDOWK        PHBDAY
     C                   ENDIF
      *
      * CHECK IF POLICY IS SET UP FOR LARGE DEDUCTIBLE
      *
     C                   MOVE      *BLANK        LGDFLG            1
     C     *IN50         IFEQ      *OFF
     C     WCFL43        ANDEQ     'Y'
     C     BSLPGM        ANDEQ     'L'
     C     ELKY1         CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           LGDFLG
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     ELSTAM        PHSTAM
     C     PHPRTF        IFLT      0
     C                   MULT      -1            PHPRTF
     C                   ENDIF
     C     PHPRTD        IFLT      0
     C                   MULT      -1            PHPRTD
     C                   ENDIF
     C     PHANPR        IFLT      0
     C                   MULT      -1            PHANPR
     C                   ENDIF
     C     PHAAPR        IFLT      0
     C                   MULT      -1            PHAAPR
     C                   ENDIF
     C     PHMAPR        IFLT      0
     C                   MULT      -1            PHMAPR
     C                   ENDIF
     C     PHAMPR        IFLT      0
     C                   MULT      -1            PHAMPR
     C                   ENDIF
     C     PHESBJ        IFLT      0
     C                   MULT      -1            PHESBJ
     C                   ENDIF
     C     PHASBJ        IFLT      0
     C                   MULT      -1            PHASBJ
     C                   ENDIF
     C     PHSTPR        IFLT      0
     C                   MULT      -1            PHSTPR
     C                   ENDIF
     C     PHASPR        IFLT      0
     C                   MULT      -1            PHASPR
     C                   ENDIF
     C     PHEMPM        IFLT      0
     C                   MULT      -1            PHEMPM
     C                   ENDIF
     C     PHAMPM        IFLT      0
     C                   MULT      -1            PHAMPM
     C                   ENDIF
     C     PHENMP        IFLT      0
     C                   MULT      -1            PHENMP
     C                   ENDIF
     C     PHANMP        IFLT      0
     C                   MULT      -1            PHANMP
     C                   ENDIF
     C     PHTXDU        IFLT      0
     C                   MULT      -1            PHTXDU
     C                   ENDIF
     C     PHLOSS        IFLT      0
     C                   MULT      -1            PHLOSS
     C                   ENDIF
     C     PHEXP1        IFLT      0
     C                   MULT      -1            PHEXP1
     C                   ENDIF
     C     PHEXP2        IFLT      0
     C                   MULT      -1            PHEXP2
     C                   ENDIF
     C     PHEXP3        IFLT      0
     C                   MULT      -1            PHEXP3
     C                   ENDIF
     C     PHAXP1        IFLT      0
     C                   MULT      -1            PHAXP1
     C                   ENDIF
     C     PHAXP2        IFLT      0
     C                   MULT      -1            PHAXP2
     C                   ENDIF
     C     PHAXP3        IFLT      0
     C                   MULT      -1            PHAXP3
     C                   ENDIF
     C     PHANNP        IFLT      0
     C                   MULT      -1            PHANNP
     C                   ENDIF
     C     PHSTAM        IFLT      0
     C                   MULT      -1            PHSTAM
     C                   ENDIF

       //*** beg add ***** 09/01/17 ****************
      /Free
       phGrpName  = FNname;
       phCarCode = %EditC(F2Car#:'X');
       phPolFil# = %EditC(ELfnd:'X') + %EditC(ELemp#:'X') + %EditC(ELfyr:'X');
       //*** beg add ***** 01/05/18 ****************
       phDedAmPO = GetDedAmpo();
       //*** end add ***** 01/05/18 ****************
       phPercMan = ELmanp;
       //*** beg add ***** 11/10/17 ****************
       If xxCall = *Blank;
          phVdr# = VPvdr#;
          phVdrName = VPname;
       Else;
          currDateCymd = %Dec(%Char(%Date:*Cymd0):7:0);
          Setgt (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:currDateCymd) pvh01;
          Dou %Eof;
             Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pvh01;
             If %Eof;
                Leave;
             Endif;
             If VHefdt <= currDateCymd and
              //*** Begin Chg *** 11/22/17 ******************
              //*** (VHexdt = 0 or VHexdt > currDateCymd);
              (VHexdt = 0 or (VHexdt > currDateCymd and VHefDt <> VHexDt));
              //*** End   Chg *** 11/22/17 ******************
                Leave;
             Endif;
          Enddo;
          If Not(%Eof);
             Chain (VHco#:VHvdr#) pvp01;
             phVdr# = VPvdr#;
             phVdrName = VPname;
          Endif;
       Endif;
       //*** end add ***** 11/10/17 ****************
      /End-Free
       //*** end add ***** 09/01/17 ****************

      *** beg add ***** 07/25/17 ******************************************
      * These fields are suppressed from policy header if Exclude premium flag
      * is set to 'Y' in TPA maintenance.
     c                   If        xxExcPrem  = 'Y'
     c                   Clear                   PHPRTF
     c                   Clear                   PHPROR
     c                   Clear                   PHPRTD
     c                   Clear                   PHANPR
     c                   Clear                   PHAAPR
     c                   Clear                   PHMINP
     c                   Clear                   PHMAPR
     c                   Clear                   PHAMPR
     c                   Clear                   PHESBJ
     c                   Clear                   PHASBJ
     c                   Clear                   PHSTPR
     c                   Clear                   PHASPR
     c                   Clear                   PHEMPM
     c                   Clear                   PHAMPM
     c                   Clear                   PHENMP
     c                   Clear                   PHANMP
     c                   Clear                   PHTXDU
     c                   Clear                   PHLOSS
     c                   Clear                   PHBTYP
     c                   Clear                   PHEXM1
     c                   Clear                   PHEMD1
     c                   Clear                   PHEXM2
     c                   Clear                   PHEMD2
     c                   Clear                   PHEXM3
     c                   Clear                   PHEMD3
     c                   Clear                   PHEXP1
     c                   Clear                   PHEXP2
     c                   Clear                   PHEXP3
     c                   Clear                   PHAXP1
     c                   Clear                   PHAXP2
     c                   Clear                   PHAXP3
     c                   Clear                   PHANNP
     c                   Clear                   PHBFRQ
     c                   Clear                   PHBDAY
     c                   Clear                   PHBDAT
     c                   Clear                   PHSTAM
      *** beg add ***** 09/01/17 ******************************
     c                   Clear                   phDedAmPO
     c                   Clear                   phPercMan
      *** end add ***** 09/01/17 ******************************
     c                   Endif
      *** end add ***** 07/25/17 ******************************


     C                   MOVEL     PHD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C*
     C* CREATE PREMIUM FACTOR RECORDS
     C*
      *** beg chg ***** 07/25/17 ******************************************
      ***                EXSR      PREMF
     C                   If        xxExcPrem  = ' '
     C                   Exsr      PremF
     C                   Endif                                                  xxExcPrem = ' '
      *** end chg ***** 07/25/17 ******************************************
     C*
     C* CREATE PAYROLL DETAIL RECORDS
     C*
     C                   EXSR      PAYRD

     C* Create Employee Payroll Detail records
      *** Beg Del ***** 11/27/23 ****** PHW113984 *********
     C***                Exsr      WrtEE
      *** End Del ***** 11/27/23 ****** PHW113984 *********
      *** Beg Add ***** 11/27/23 ****** PHW113984 *********
        Select;
        When xxClt# = '150'; // Skip from Writing 'EE' records.
        Other;
           Exsr WrtEE;
        Endsl;
      *** End Add ***** 11/27/23 ****** PHW113984 *********
     C*
     C* CREATE ADDITIONAL NAME/ADDRESS RECORDS.
     C*
      *** BEG CHG ***** 01/05/18 **************************
     C***                SELECT
     C***  XXCALL        WHENNE    'G'
     C***  XXPTYC        IFNE      'Y'
     C***  XXADDA        IFEQ      'Y'
     C***                EXSR      WRTNA
     C***                ENDIF                                                  VPEXAL IFEQ
     C***                ELSE
     C**SET FLAG TO WRITE ADDITINAL ADD AT POLICY BREAK
     C***                MOVE      'Y'           XXADNA            1
     C**DIV ALREADY PRINTED - DO NOT PRINT AGAIN FOR DIFF FYR WITHOUT ACTIVITY
     C***                MOVE      'N'           XXWRDV                         WRT DIVISION
     C***                ENDIF
     C***  XXCALL        WHENEQ    'G'
     C***  XXPTYC        IFNE      'Y'
     C***  XXADDA        IFEQ      'Y'
     C***                EXSR      WRTNA
     C***                ENDIF                                                  VPEXAL IFEQ
     C***                ELSE
     C**SET FLAG TO WRITE ADDITINAL ADD AT POLICY BREAK
     C***                MOVE      'Y'           XXADNA            1
     C**DIV ALREADY PRINTED - DO NOT PRINT AGAIN FOR DIFF FYR WITHOUT ACTIVITY
     C***                MOVE      'N'           XXWRDV                         WRT DIVISION
     C***                ENDIF
     C***                ENDSL

     C                   MOVE      'N'           XXWRDV
     C     XXADDA        IFEQ      'Y'
     C                   EXSR      WRTNA
     C                   ENDIF
      *** END CHG ***** 01/05/18 **************************
     C*
     C                   ENDSR                                                  WRTEH
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE A PREMIUM FACTOR RECORDS
     C*
     C     PREMF         BEGSR
     C*
     C                   CLEAR                   PFD
     C                   CLEAR                   DATA
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELADCT        XXEDCT           11 2
     C                   Z-ADD     ELADD2        X2EDCT           11 2
     C                   Z-ADD     ELADCM        XXEDCM           11 2
     C                   Z-ADD     ELADCS        XXEDCS           11 2
     C                   Z-ADD     ELADC1        XXEDC1           11 2
     C                   Z-ADD     ELADC2        XXEDC2           11 2
     C                   Z-ADD     ELADC3        XXEDC3           11 2
     C                   Z-ADD     ELADV2        XXEDV2           11 2
     C                   Z-ADD     ELD1A2        XXD1E2           11 2
     C                   Z-ADD     ELD2A2        XXD2E2           11 2
     C                   Z-ADD     ELADC         XXEDC            11 2
     C                   Z-ADD     ELRPA1        XXRPE1           11 2
     C                   Z-ADD     ELRPA2        XXRPE2           11 2
     C                   Z-ADD     ELRPA3        XXRPE3           11 2
     C                   Z-ADD     ELATER        XXETER           11 2
     C                   Z-ADD     ELADTR        XXEDTR           11 2
     C                   ELSE
     C                   Z-ADD     ELEDCT        XXEDCT           11 2
     C                   Z-ADD     ELEDD2        X2EDCT           11 2
     C                   Z-ADD     ELEDCM        XXEDCM           11 2
     C                   Z-ADD     ELEDCS        XXEDCS           11 2
     C                   Z-ADD     ELEDC1        XXEDC1           11 2
     C                   Z-ADD     ELEDC2        XXEDC2           11 2
     C                   Z-ADD     ELEDC3        XXEDC3           11 2
     C                   Z-ADD     ELEDV2        XXEDV2           11 2
     C                   Z-ADD     ELD1E2        XXD1E2
     C                   Z-ADD     ELD2E2        XXD2E2
     C                   Z-ADD     ELEDC         XXEDC            11 2
     C                   Z-ADD     ELRPE1        XXRPE1           11 2
     C                   Z-ADD     ELRPE2        XXRPE2           11 2
     C                   Z-ADD     ELRPE3        XXRPE3           11 2
     C                   Z-ADD     ELETER        XXETER           11 2
     C                   Z-ADD     ELEDTR        XXEDTR           11 2
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C     MPKY          CHAIN     PMP01                              50
     C     *IN50         IFEQ      *OFF
     C     XXMINP        ANDEQ     'Y'
     C*
     C     ELFAF         IFNE      'A'
     C                   Z-ADD     MPEDCT        XXEDCT           11 2
     C                   Z-ADD     MPEDCM        XXEDCM           11 2
     C                   Z-ADD     MPEDCS        XXEDCS           11 2
     C                   Z-ADD     MPEDC1        XXEDC1           11 2
     C                   Z-ADD     MPEDC2        XXEDC2           11 2
     C                   Z-ADD     MPEDC3        XXEDC3           11 2
     C                   Z-ADD     MPEDV2        XXEDV2           11 2
     C                   Z-ADD     MPD1E2        XXD1E2
     C                   Z-ADD     MPD2E2        XXD2E2
     C                   Z-ADD     MPEDC         XXEDC            11 2
     C                   ELSE
     C                   Z-ADD     MPADCT        XXEDCT           11 2
     C                   Z-ADD     MPADCM        XXEDCM           11 2
     C                   Z-ADD     MPADCS        XXEDCS           11 2
     C                   Z-ADD     MPADC1        XXEDC1           11 2
     C                   Z-ADD     MPADC2        XXEDC2           11 2
     C                   Z-ADD     MPADC3        XXEDC3           11 2
     C                   Z-ADD     MPADV2        XXEDV2           11 2
     C                   Z-ADD     MPD1A2        XXD1E2
     C                   Z-ADD     MPD2A2        XXD2E2
     C                   Z-ADD     MPADC         XXEDC            11 2
     C                   ENDIF                                                  ELFAF IFNE
     C*
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C* FIND STATISTICAL RPT/RATING INFO
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  50
     C*
     C* FIND INCREASED LIMITS INFO
     C*
     C                   Z-ADD     0             XXILP1            5 3
     C                   Z-ADD     0             XXILC1            5 0
     C                   Z-ADD     0             XXILP2            5 3
     C                   Z-ADD     0             XXILC2            5 0
     C*
     C* INCR LIMITS #1
     C*
     C     ELILCD        IFNE      *BLANKS
     C*
     C                   MOVEL     ELILCD        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY1         READPE    PLM01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY1         READPE    PLM01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     LMPCT         XXILP1
     C                   Z-ADD     LMCLAS        XXILC1
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   ENDIF                                                  ELILCD IFNE
     C*
     C* INCR LIMITS #2
     C*
     C     ELINL2        IFNE      *BLANKS
     C*
     C                   MOVEL     ELINL2        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY1         READPE    PLM01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY1         READPE    PLM01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     LMPCT         XXILP2
     C                   Z-ADD     LMCLAS        XXILC2
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   ENDIF                                                  ELINL2 IFNE
     C*
     C* FIND DEDUCTIBLE DISCOUNT INFORMATION
     C*
     C                   Z-ADD     0             XXDDP1            5 2
     C                   Z-ADD     0             XXDDC1            5 0
     C*
     C     ELDCDE        IFNE      *BLANK
     C*
     C                   MOVE      ELDCDE        XXCODE
     C                   MOVE      ELDCEF        QQDATE
     C                   MOVE      ELFND         QQFND
     C                   MOVE      ELFYR         QQFYR
     C                   MOVE      ELEMP#        QQEMP#
     C                   MOVE      ELDIV         QQDIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQDATE            7
     C*
     C                   MOVE      QQDATE        XXDT
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C     DDSTAT        ANDEQ     'A'
     C                   Z-ADD     DDPCT         XXDDP1
     C                   Z-ADD     DDCLAS        XXDDC1
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   ENDIF                                                  ELDCDE IFNE
     C                   Z-ADD     0             XXDDP2            5 2
     C                   Z-ADD     0             XXDDC2            5 0
     C*
     C     ELDCD2        IFNE      *BLANK
     C*
     C                   MOVE      ELDCD2        XXCODE
     C                   MOVE      EL2DEF        QQDATE
     C                   MOVE      ELFND         QQFND
     C                   MOVE      ELFYR         QQFYR
     C                   MOVE      ELEMP#        QQEMP#
     C                   MOVE      ELDIV         QQDIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQDATE            7
     C*
     C                   MOVE      QQDATE        XXDT
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C     DDSTAT        ANDEQ     'A'
     C                   Z-ADD     DDPCT         XXDDP2
     C                   Z-ADD     DDCLAS        XXDDC2
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   ENDIF                                                  ELDCD2 IFNE
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'F'           PFRTYP
     C                   MOVEL     XXCAP#        PFCAP#
     C                   Z-ADD     XXFND         PFFND
     C                   Z-ADD     XXEMP#        PFEMP#
     C                   Z-ADD     XXDIV         PFDIV
       //*** beg add ***** 09/01/17 **********************
      /Free
       pfPolFil# = %EditC(ELfnd:'X') + %EditC(ELemp#:'X') + %EditC(ELfyr:'X');
      /End-Free
       //*** end add ***** 09/01/17 **********************
     C*
     C* REPORT INCR LIMITS #1
     C*
     C     ELILCD        IFNE      *BLANKS
     C*
     C                   Z-ADD     XXILC1        PFCODE
     C                   Z-ADD     XXILP1        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      'M'           PFBLVL
     C                   MOVE      'M'           PFALVL
     C*
     C     ELILEF        IFNE      *ZERO
     C     19000000      ADD       ELILEF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELILEX        IFNE      *ZERO
     C     19000000      ADD       ELILEX        PFEXDT
     C                   ELSE
     C*
     C     ELI2EF        IFNE      *ZERO
     C     19000000      ADD       ELI2EF        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF                                                  ELI2EF IFNE
     C*
     C                   ENDIF                                                  ELILEX IFNE
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAIN1        PFAMT
     C                   ELSE
     C                   Z-ADD     ELEIN1        PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELILCD IFNE
     C*
     C* REPORT INCR LIMITS #2
     C*
     C     ELINL2        IFNE      *BLANKS
     C*
     C                   Z-ADD     XXILC2        PFCODE
     C                   Z-ADD     XXILP2        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      'M'           PFBLVL
     C                   MOVE      'M'           PFALVL
     C     19000000      ADD       ELI2EF        PFEFDT
     C*
     C     ELI2EX        IFNE      *ZERO
     C     19000000      ADD       ELI2EX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAIN2        PFAMT
     C                   ELSE
     C                   Z-ADD     ELEIN2        PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELINL2 IFNE
     C*
     C* REPORT DEDUCTIBLE DISCOUNT
     C*
     C     ELDCDE        IFNE      *BLANKS
     C     LGDFLG        ANDNE     'Y'
     C*
     C                   Z-ADD     XXDDC1        PFCODE
     C                   Z-ADD     XXDDP1        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      RRRDPR        PFBLVL
     C                   MOVE      RRRDPR        PFALVL
     C*
     C     ELDCEF        IFNE      *ZERO
     C     19000000      ADD       ELDCEF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELDCEX        IFNE      *ZERO
     C     19000000      ADD       ELDCEX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDCT        PFAMT
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELDCDE IFNE
     C     ELDCD2        IFNE      *ZERO
     C     LGDFLG        ANDNE     'Y'
     C*
     C                   Z-ADD     XXDDC2        PFCODE
     C                   Z-ADD     XXDDP2        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      RRRDPR        PFBLVL
     C                   MOVE      RRRDPR        PFALVL
     C*
     C     EL2DEF        IFNE      *ZERO
     C     19000000      ADD       EL2DEF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     EL2DEX        IFNE      *ZERO
     C     19000000      ADD       EL2DEX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     X2EDCT        PFAMT
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  LDPCT2 IFNE
      *
      * APPLY LARGE DEDUCTIBLE FACTORS
      *
     C     LDPCT1        IFNE      *ZERO
     C     LGDFLG        ANDEQ     'Y'
     C*
     C                   Z-ADD     LDCLS1        PFCODE
     C                   Z-ADD     LDPCT1        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      LDPLV1        PFBLVL
     C                   MOVE      LDPLV1        PFALVL
     C*
     C     LDEFD1        IFNE      *ZERO
     C     19000000      ADD       LDEFD1        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     LDEXP1        IFNE      *ZERO
     C     19000000      ADD       LDEXP1        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDCT        PFAMT
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  LDPCT1 IFNE
      *
     C     LDPCT2        IFNE      *ZERO
     C     LGDFLG        ANDEQ     'Y'
     C*
     C                   Z-ADD     LDCLS2        PFCODE
     C                   Z-ADD     LDPCT2        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      LDPLV2        PFBLVL
     C                   MOVE      LDPLV2        PFALVL
     C*
     C     LDEFD2        IFNE      *ZERO
     C     19000000      ADD       LDEFD2        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     LDEXP2        IFNE      *ZERO
     C     19000000      ADD       LDEXP2        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     X2EDCT        PFAMT
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  LDPCT2 IFNE
     C*
     C* REPORT WAIVER SURCHARGE
     C*
     C     ELEWAV        IFNE      *ZERO
     C     ELFAF         ANDNE     'A'
     C     ELAWAV        ORNE      *ZERO
     C     ELFAF         ANDEQ     'A'
     C*
     C                   MOVE      RRWVAC        PFCODE
     C                   Z-ADD     ELWVPC        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      ELWVFL        PFBLVL
     C                   MOVE      ELWVFL        PFALVL
     C*
     C     19000000      ADD       ELPFDT        PFEFDT
     C     19000000      ADD       ELPTDT        PFEXDT
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAWAV        PFAMT
     C                   ELSE
     C                   Z-ADD     ELEWAV        PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELEWAV IFNE
     C*
     C* REPORT MANAGED CARE CREDIT
     C*
     C     ELASPM        IFNE      *ZERO
     C*
     C                   Z-ADD     RR1ACD        PFCODE
     C                   Z-ADD     ELASPM        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      'J'           PFBLVL
     C                   MOVE      'J'           PFALVL
     C*
     C     ELMCEF        IFNE      *ZERO
     C     19000000      ADD       ELMCEF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELMCEX        IFNE      *ZERO
     C     19000000      ADD       ELMCEX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDCM        PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELASPM IFNE
     C*
     C* REPORT SAFETY CREDIT
     C*
     C     ELASPS        IFNE      *ZERO
     C*
     C                   Z-ADD     RR1BCD        PFCODE
     C                   Z-ADD     ELASPS        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      'J'           PFBLVL
     C                   MOVE      'J'           PFALVL
     C*
     C     ELSCEF        IFNE      *ZERO
     C     19000000      ADD       ELSCEF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELSCEX        IFNE      *ZERO
     C     19000000      ADD       ELSCEX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDCS        PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELASPS IFNE
     C*
     C* REPORT DEVIATION #1
     C*
     C     ELASP2        IFNE      *ZERO
     C*
     C                   Z-ADD     RRDV1C        PFCODE
     C                   Z-ADD     ELASP2        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      RRDV1L        PFBLVL
     C                   MOVE      RRDV1L        PFALVL
     C*
     C     ELDVEF        IFNE      *ZERO
     C     19000000      ADD       ELDVEF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELDVEX        IFNE      *ZERO
     C     19000000      ADD       ELDVEX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDC2        PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELASP2 IFNE
     C*
     C* REPORT DEVIATION #1 2ND PERIOD
     C     ELD1S2        IFNE      *ZERO
     C                   Z-ADD     RRDV1C        PFCODE
     C                   Z-ADD     ELD1S2        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      RRDV1L        PFBLVL
     C                   MOVE      RRDV1L        PFALVL
     C*
     C     ELD1D2        IFNE      *ZERO
     C     19000000      ADD       ELD1D2        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELD1X2        IFNE      *ZERO
     C     19000000      ADD       ELD1X2        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXD1E2        PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELD1S2 IFNE
     C* REPORT DEVIATION #2
     C*
     C     ELDEV2        IFNE      *ZERO
     C*** Begin Add *** 04/07/17 ***************************
     C     ELDEV2F       ORNE      *ZERO
     C*** End   Add *** 04/07/17 ***************************
     C*
     C                   Z-ADD     RRDV2C        PFCODE
     C*** Begin Chg *** 04/07/17 ***************************
     C**********         Z-ADD     ELDEV2        PFPRTE
     C                   Select
     C                   When      ELdev2 <> 0
     C                   Eval      PFprtE = ELdev2
     C                   When      ELdev2F <> 0
     C                   Eval      PFprtE = ELdev2F
     C                   EndSl
     C*** End   Chg *** 04/07/17 ***************************
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      RRDV2L        PFBLVL
     C                   MOVE      RRDV2L        PFALVL
     C*
     C     ELD2EF        IFNE      *ZERO
     C     19000000      ADD       ELD2EF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELD2EX        IFNE      *ZERO
     C     19000000      ADD       ELD2EX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDV2        PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELDEV2 IFNE
     C*
     C* REPORT DEVIATION #2 2ND PERIOD
     C     ELD2S2        IFNE      *ZERO
     C                   Z-ADD     RRDV2C        PFCODE
     C                   Z-ADD     ELD2S2        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      RRDV2L        PFBLVL
     C                   MOVE      RRDV2L        PFALVL
     C*
     C     ELD2D2        IFNE      *ZERO
     C     19000000      ADD       ELD2D2        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELD2X2        IFNE      *ZERO
     C     19000000      ADD       ELD2X2        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXD2E2        PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELD2S2 IFNE
     C* REPORT CONTRACTORS CREDIT
     C*
     C     ELASP1        IFNE      *ZERO
     C*
     C                   Z-ADD     RRCCCD        PFCODE
     C                   Z-ADD     ELASP1        PFPRTE
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      'E'           PFBLVL
     C                   MOVE      'E'           PFALVL
     C*
     C     ELCREF        IFNE      *ZERO
     C     19000000      ADD       ELCREF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELCREX        IFNE      *ZERO
     C     19000000      ADD       ELCREX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDC1        PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELASP2 IFNE
     C*
     C* REPORT SCHEDULE DB/CR
     C*
     C     ELDBCR        IFNE      *ZERO
     C*
     C     XXEDC         IFGE      *ZERO
     C                   Z-ADD     RRDBCD        PFCODE
     C                   ELSE
     C                   Z-ADD     RRCRCD        PFCODE
     C                   ENDIF                                                  XXEDC IFGE
     C*
     C                   Z-ADD     0             PFPRTE
     C                   Z-ADD     ELDBCR        PFFRTE
     C                   MOVE      'E'           PFBLVL
     C                   MOVE      'E'           PFALVL
     C*
     C     ELSDEF        IFNE      *ZERO
     C     19000000      ADD       ELSDEF        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ENDIF
     C*
     C     ELSDEX        IFNE      *ZERO
     C     19000000      ADD       ELSDEX        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXEDC         PFAMT
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELDBCR IFNE
     C*
     C* REPORT STOCK DISCOUNT
     C*
     C     ELASP3        IFNE      *ZERO
     C     ELDISC        ORNE      *BLANKS
     C*
     C                   Z-ADD     0             PFCODE
     C*
     C     ELASP3        IFNE      *ZERO
     C                   Z-ADD     ELASP3        PFPRTE
     C                   ELSE
     C                   Z-ADD     ELDISP        PFPRTE
     C                   ENDIF                                                  ELASP3 IFNE
     C*
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      RRDSPR        PFBLVL
     C                   MOVE      RRDSPR        PFALVL
     C     19000000      ADD       ELPFDT        PFEFDT
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   Z-ADD     XXEDC3        PFAMT
     C*
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELASP3 IFNE
     C*
     C* REPORT ARAP #1
     C*
     C     ELARP1        IFNE      *ZERO
     C*
     C                   Z-ADD     277           PFCODE
     C                   Z-ADD     0             PFPRTE
     C                   Z-ADD     ELARP1        PFFRTE
     C                   MOVE      'S'           PFBLVL
     C                   MOVE      'V'           PFALVL
     C*
     C     ELEMD1        IFLT      ELPFDT
     C     19000000      ADD       ELPFDT        PFEFDT
     C                   ELSE
     C     19000000      ADD       ELEMD1        PFEFDT
     C                   ENDIF                                                  ELEMD1 IFLT
     C*
     C     ELEMD2        IFNE      *ZERO
     C     19000000      ADD       ELEMD2        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF                                                  ELEMD2 IFNE
     C*
     C                   Z-ADD     XXRPE1        PFAMT
     C*
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELARP1 IFNE
     C*
     C* REPORT ARAP #2
     C*
     C     ELARP2        IFNE      *ZERO
     C*
     C                   Z-ADD     277           PFCODE
     C                   Z-ADD     0             PFPRTE
     C                   Z-ADD     ELARP2        PFFRTE
     C                   MOVE      'S'           PFBLVL
     C                   MOVE      'V'           PFALVL
     C     19000000      ADD       ELEMD2        PFEFDT
     C*
     C     ELEMD3        IFNE      *ZERO
     C     19000000      ADD       ELEMD3        PFEXDT
     C                   ELSE
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   ENDIF                                                  ELEMD3 IFNE
     C*
     C                   Z-ADD     XXRPE2        PFAMT
     C*
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELARP2 IFNE
     C*
     C* REPORT ARAP #3
     C*
     C     ELARP3        IFNE      *ZERO
     C*
     C                   Z-ADD     277           PFCODE
     C                   Z-ADD     0             PFPRTE
     C                   Z-ADD     ELARP3        PFFRTE
     C                   MOVE      'S'           PFBLVL
     C                   MOVE      'V'           PFALVL
     C     19000000      ADD       ELEMD3        PFEFDT
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   Z-ADD     XXRPE3        PFAMT
     C*
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  ELARP3 IFNE
     C*
     C* REPORT TERRORISM CHARGE
     C*
     C     XXETER        IFNE      *ZERO
     C*
     C                   Z-ADD     RRTERC        PFCODE
     C*** BEG ADD ***  07/15/13  ************************************
     C                   IF        ELTERR <> 0
     C                   Z-ADD     ELTERR        PFPRTE
     C                   ELSE
     C*** END ADD ***  07/15/13  ************************************
     C                   Z-ADD     RRTERR        PFPRTE
     C*** BEG ADD ***  07/15/13  ************************************
     C                   ENDIF
     C*** END ADD ***  07/15/13  ************************************
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      'P'           PFBLVL
     C                   MOVE      'X'           PFALVL
     C     19000000      ADD       ELPFDT        PFEFDT
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   Z-ADD     XXETER        PFAMT
     C*
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  XXETER IFNE
     C*
     C* REPORT DOMESTIC TERRORISM CHARGE
     C*
     C     XXEDTR        IFNE      *ZERO
     C*
     C                   Z-ADD     RRDTRC        PFCODE
     C*** BEG ADD ***  07/15/13  ************************************
     C                   IF        ELDTRR <> 0
     C                   Z-ADD     ELDTRR        PFPRTE
     C                   ELSE
     C*** END ADD ***  07/15/13  ************************************
     C                   Z-ADD     RRDTRR        PFPRTE
     C*** BEG ADD ***  07/15/13  ************************************
     C                   ENDIF
     C*** END ADD ***  07/15/13  ************************************
     C                   Z-ADD     0             PFFRTE
     C                   MOVE      'P'           PFBLVL
     C                   MOVE      'X'           PFALVL
     C     19000000      ADD       ELPFDT        PFEFDT
     C     19000000      ADD       ELPTDT        PFEXDT
     C                   Z-ADD     XXEDTR        PFAMT
     C*
     C     PFAMT         IFLT      *ZERO
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C     PFCODE        IFLT      0
     C                   MULT      -1            PFCODE
     C                   ENDIF
     C     PFPRTE        IFLT      0
     C                   MULT      -1            PFPRTE
     C                   ENDIF
     C     PFFRTE        IFLT      0
     C                   MULT      -1            PFFRTE
     C                   ENDIF
     C     PFAMT         IFLT      0
     C                   MULT      -1            PFAMT
     C                   ENDIF
     C*
     C                   MOVEL     PFD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C                   ENDIF                                                  XXETER IFNE
     C*
     C                   ENDSR                                                  PREMF
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE A PAYROLL DETAIL RECORDS
     C*
     C     PAYRD         BEGSR
     C*
     C                   CLEAR                   PAD
     C                   CLEAR                   DATA
     C*
     C* CALCULATE FACTORS AND RATES
     C*
     C     ELFAF         IFEQ      'A'
     C     ELANMP        ADD       ELATER        XXANPR           11 2
     C                   ADD       ELADTR        XXANPR           11 2
     C                   Z-ADD     ELAMPR        XXMAPR           11 2
     C                   ELSE
     C     ELENMP        ADD       ELETER        XXANPR           11 2
     C                   ADD       ELEDTR        XXANPR           11 2
     C                   Z-ADD     ELMAPR        XXMAPR           11 2
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C     EPKY          CHAIN     PEP01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C                   Z-ADD     0             PXCODE
     C*
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST              2
     C     PXKY          CHAIN     PPX02                              50
     C   50RSKY          CHAIN     PRS01                              51
     C  N51
     CAN 50              MOVE      RSJRST        XXST
     C  N51
     CAN 50PXKY          CHAIN     PPX02                              50
     C                   ENDIF                                                  EPEXT IFNE
     C*
     C     PXCODE        IFEQ      *ZERO
     C                   ADD       EPADLD        XXMAPR
     C                   ENDIF                                                  PXCODE IFNE
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C     EPKY          READE     PEP01                                  50
     C                   ENDDO
     C*
     C     XXMINP        IFNE      'Y'
     C     XXMAPR        ANDNE     *ZERO
     C     XXANPR        DIV(H)    XXMAPR        XXFCT1            7 4
     C                   ELSE
     C                   Z-ADD     0             XXFCT1
     C                   ENDIF                                                  XXMINP IFNE
     C*
     C* LOAD THE ANNIVERSARY RATING DATE AND APPROPRIATE ARRAYS.
     C*
     C                   Z-ADD     0             RDAT
     C                   MOVE      '0'           *IN77
     C                   MOVE      '1'           *IN78
     C                   Z-ADD     ELPFDT        RDAT(1)
     C                   Z-ADD     0             XXANDT
     C                   Z-ADD     2             X                 2 0
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C     ELANDT        IFNE      0
     C     ELANDT        ANDNE     ELPFDT
     C*
     C                   MOVE      ELANDT        XXANDT
     C                   ADD       1             XXANYR
     C*
     C     XXANDT        IFGT      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C*
     C     XXANDT        IFGE      ELPTDT
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      *ON           *IN78
     C                   ELSE
     C                   MOVE      *OFF          *IN78
     C                   ENDIF
     C*
     C     *IN78         IFEQ      '0'
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                  *IN78='0'
     C*
     C                   ENDIF                                                  ELANDT IFNE
     C*
     C                   Z-ADD     1             PFACT1            7 4
     C                   Z-ADD     1             PFACT2            7 4
     C*
     C     *IN78         IFEQ      *OFF
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     ELPFDT        XXLPDT            7 0
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      ELPFDT
     C                   Z-ADD     ELPTDT        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   ENDIF
     C*
     C     XXLPDT        IFLT      ELPFDT
     C     XXLPDT        ORGT      ELPTDT
     C                   Z-ADD     *ZERO         XXLPDT
     C                   ENDIF
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   MOVE      *BLANK        SRERR             1
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     SRERR         IFEQ      '1'
     C     XXLPDT        OREQ      ELPFDT
     C                   Z-ADD     0             XXLPDT
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXLPDT IFNE
     C*
     C*  GET NUMBER OF EMPLOYER POLICY DAYS
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C*  SUBTRACT 1 FROM 1ST ANNIV PERIOD IF IT IS LEAP YEAR AND
     C*  FEBRUARY 29 FALLS WITHIN THE FIRST PERIOD.
     C*
     C     XXLPDT        IFNE      0
     C                   SUB       1             QQDAYS
     C                   END
     C*
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C*
     C*  GET NUMBER OF DAYS IN EACH ANNIV RATING PERIOD
     C*  START FROM ELPFDT BECAUSE ANNUAL PAYROLL AMOUNTS ARE USED
     C*
     C                   Z-ADD     0             PDAYS1            5 0
     C                   Z-ADD     0             PDAYS2            5 0
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XXANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C*  SUBTRACT 1 FROM 1ST ANNIV PERIOD IF IT IS LEAP YEAR AND
     C*  FEBRUARY 29 FALLS WITHIN THE FIRST PERIOD.
     C*
     C     XXLPDT        IFNE      0
     C     XXLPDT        ANDLT     XXANDT
     C                   SUB       1             QQDAYS
     C                   END
     C*
     C                   Z-ADD     QQDAYS        PDAYS1
     C     PDAYS         SUB       PDAYS1        PDAYS2
     C*
     C     PDAYS         IFNE      *ZERO
     C     PDAYS1        DIV(H)    PDAYS         PFACT1            7 4
     C     PDAYS2        DIV(H)    PDAYS         PFACT2            7 4
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN78 IFEQ
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'D'           PARTYP
     C                   MOVEL     XXCAP#        PACAP#
     C                   Z-ADD     XXFND         PAFND
     C                   Z-ADD     XXEMP#        PAEMP#
     C                   Z-ADD     XXDIV         PADIV
     C*
     C     EPKY          CHAIN     PEP01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C                   Z-ADD     EPEAPR        PAEAPR
     C                   Z-ADD     EPCLAS        PACLAS
     C                   MOVE      EPSPEC        PASPEC
     C                   Z-ADD     EPADLD        PAADLD
     C                   Z-ADD     EPEMPR        PAEMPR
     C                   Z-ADD     EPARAT        PAARAT
     C                   Z-ADD     EPECNT        PAECNT
      *** beg add ***** 07/25/17 ******************************************
     C                   Eval      PAEXT  = EPEXT
     C                   Eval      PASAPR = EPSAPR
      *** end add ***** 07/25/17 ******************************************
     C*
     C*  GET CORRECT DATE FROM RDAT ARRAY TO PULL RATE FROM RATE FILE
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    RDAT(X)                            50
     C     *IN50         IFEQ      '1'
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   Z-ADD     RDAT(X)       XXEFDT
     C                   ELSE
     C                   Z-ADD     1             X
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   ENDIF
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C*
     C*  GET PAYROLL CLASS FILE
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  50
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     PRMNPR        PAMNPR
     C                   MOVEL(P)  PRDESC        PADESC
     C                   MOVE      PRPCF         PAPCF
     C                   ELSE
     C                   Z-ADD     0             PAMNPR
     C                   MOVE      *BLANKS       PADESC
     C                   MOVE      *BLANKS       PAPCF
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   Z-ADD     0             PXCODE
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C     PXKY          CHAIN     PPX02                              50
     C  N50              MOVEL(P)  PXDESC        PADESC
     C   50RSKY          CHAIN     PRS01                              51
     C  N51
     CAN 50              MOVE      RSJRST        XXST
     C  N51
     CAN 50PXKY          CHAIN     PPX02                              50
     C  N51
     CANN50              MOVEL(P)  PXDESC        PADESC
     C                   ENDIF                                                  EPEXT IFNE
     C*
     C* CALCULATE NET RATE
     C*
     C     PRPCF         IFEQ      'O'
     C                   Z-ADD(H)  XXFCT1        PANTRT
     C                   ELSE
     C     XXFCT1        MULT(H)   EPARAT        PANTRT
     C                   ENDIF
     C*
     C     EPCLAS        IFEQ      RRDCCD
     C*
     C     ELFAF         IFEQ      'A'
     C     EPARAT        MULT      EPAAPR        XXWK10
     C                   ELSE
     C     EPARAT        MULT      EPEAPR        XXWK10
     C                   ENDIF
     C*
     C     XXWK10        IFGT      RRAMAX
     C     XXWK10        ANDNE     *ZERO
     C     RRAMAX        DIV(H)    XXWK10        XXFCT2            7 4
     C                   MULT(H)   XXFCT2        PANTRT
     C                   ENDIF                                                  XXWK10 IFGT
     C*
     C                   ENDIF                                                  EPCLAS IFEQ
     C*
     C     EPCADT        IFNE      *ZERO
     C     19000000      ADD       EPCADT        PACADT
     C                   ELSE
     C                   Z-ADD     0             PACADT
     C                   ENDIF
     C*
     C     19000000      ADD       EPEFDT        PAEFDT
     C                   MOVE      EPSAFL        PASAFL
     C*
     C     EPADLD        IFNE      *ZERO
     C     EPCLAS        ANDNE     RRDCCD
     C                   Z-ADD     XXFCT1        PANETF
     C                   ELSE
     C                   Z-ADD     0             PANETF
     C                   ENDIF
     C*
     C                   Z-ADD     EPAAPR        PAAAPR
     C                   Z-ADD     EPAMPR        PAAMPR
     C*
     C* NOTE, EMPLOYER PAYROLL FIELDS AFTER THIS POINT WILL BE CHANGED
     C* DUE TO CHAINS TO ANOTHER LOGICAL.
     C*
     C                   Z-ADD     EPEFDT        SVEFDT            7 0
     C*
     C* CALCULATE BLENDED RATE
     C*
     C     *IN78         IFEQ      *OFF
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     EPKY1         CHAIN     PEP02                              50
     C     *IN50         IFEQ      *OFF
     C     PFACT1        MULT(H)   EPARAT        XXRT1             7 4
     C                   ENDIF
     C*
     C                   Z-ADD     XXANDT        XXEFDT
     C     EPKY1         CHAIN     PEP02                              51
     C*
     C     *IN51         IFEQ      *OFF
     C     *IN50         ANDEQ     *OFF
     C     PFACT2        MULT(H)   EPARAT        XXRT2             7 4
     C     XXRT1         ADD(H)    XXRT2         PABLRT
     C                   ELSE
     C                   Z-ADD     EPARAT        PABLRT
     C                   ENDIF
     C*
     C* CALCULATE NET BLENDED RATE
     C*
     C     XXFCT1        MULT(H)   PABLRT        PANBRT
     C*
     C                   ELSE
     C                   Z-ADD     PAARAT        PABLRT
     C                   Z-ADD     PANTRT        PANBRT
     C                   ENDIF                                                  *IN78 IFEQ
     C*
     C* CALCULATE PRORATED PAYROLL AMOUNT IF SHORT TERM AND NO OTHER
     C* RECORDS FOR THIS PAYROLL CLASS.
     C*
     C                   Z-ADD     0             PAPPAY
     C     SVEFDT        IFEQ      ELPFDT
     C     ELPROR        ANDNE     1
     C*
     C                   Z-ADD     SVEFDT        XXEFDT
     C     EPKY1         CHAIN     PEP02                              50
     C     EPKY2         READE     PEP02                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C     EPSTAT        ANDEQ     'I'
     C     EPKY2         READE     PEP02                                  50
     C                   ENDDO
     C*
     C     *IN50         IFEQ      *ON
     C     ELPROR        MULT(H)   PAEAPR        XXWK10           11 0
     C                   Z-ADD     XXWK10        PAPPAY
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   ENDIF                                                  EPEFDT IFEQ
     C*
     C     ELFAF         IFEQ      'A'
     C     XXFCT1        MULT(H)   PAAMPR        XXWK10
     C                   ELSE
     C     XXFCT1        MULT(H)   PAEMPR        XXWK10
     C                   ENDIF                                                  ELFAF IFEQ
     C     XXFCT1        MULT(H)   PAADLD        XXTEMP           11 0
     C     XXWK10        ADD       XXTEMP        PAFPRM
     C     PAEAPR        IFLT      0
     C                   MULT      -1            PAEAPR
     C                   ENDIF
     C     PACLAS        IFLT      0
     C                   MULT      -1            PACLAS
     C                   ENDIF
     C     PAADLD        IFLT      0
     C                   MULT      -1            PAADLD
     C                   ENDIF
     C     PAEMPR        IFLT      0
     C                   MULT      -1            PAEMPR
     C                   ENDIF
     C     PAARAT        IFLT      0
     C                   MULT      -1            PAARAT
     C                   ENDIF
     C     PAECNT        IFLT      0
     C                   MULT      -1            PAECNT
     C                   ENDIF
     C     PANTRT        IFLT      0
     C                   MULT      -1            PANTRT
     C                   ENDIF
     C     PABLRT        IFLT      0
     C                   MULT      -1            PABLRT
     C                   ENDIF
     C     PANBRT        IFLT      0
     C                   MULT      -1            PANBRT
     C                   ENDIF
     C     PAPPAY        IFLT      0
     C                   MULT      -1            PAPPAY
     C                   ENDIF
     C     PAAAPR        IFLT      0
     C                   MULT      -1            PAAAPR
     C                   ENDIF
     C     PAMNPR        IFLT      0
     C                   MULT      -1            PAMNPR
     C                   ENDIF
     C     PANETF        IFLT      0
     C                   MULT      -1            PANETF
     C                   ENDIF
     C     PAFPRM        IFLT      0
     C                   MULT      -1            PAFPRM
     C                   ENDIF
     C     PAAMPR        IFLT      0
     C                   MULT      -1            PAAMPR
     C                   ENDIF

      *** beg add ***** 07/25/17 ******************************************
      * These fields are suppressed from payroll detail if Exclude payroll flag
      * is set to 'Y' in TPA maintenance.
     c                   If        xxExcPayR = 'Y'
     c                   Clear                   PAEAPR
     c                   Clear                   PAADLD
     c                   Clear                   PAEMPR
     c                   Clear                   PAARAT
     c                   Clear                   PAPPAY
     c                   Clear                   PAAAPR
     c                   Clear                   PAMNPR
     c                   Clear                   PAFPRM
     c                   Clear                   PAAMPR
     c                   Clear                   PASAPR
     c                   Endif
      *** end add ***** 07/25/17 ******************************************
       //*** beg add ***** 09/01/17 **********************
      /Free
       paPolFil# = %EditC(ELfnd:'X') + %EditC(ELemp#:'X') + %EditC(ELfyr:'X');
      /End-Free
       //*** end add ***** 09/01/17 **********************
     C*
     C                   MOVEL     PAD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C*
     C* ACCUMULATE TOTALS
     C*
     C     ELFAF         IFEQ      'A'
     C                   ADD       PAAAPR        XXTPAY
     C                   ADD       PAAMPR        XXTMAN
     C                   ELSE
     C                   ADD       PAEAPR        XXTPAY
     C                   ADD       PAEMPR        XXTMAN
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C                   ADD       PAECNT        XXECNT
     C                   ADD       PAADLD        XXTADL
     C                   ADD       PAFPRM        XXTPRM
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C* READ NEXT PAYROLL RECORD
     C*
     C     EPKY          READE     PEP01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C                   ENDSR                                                  PAYRD
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE A VENDOR TRAILER RECORD
     C*
     C     WRTVT         BEGSR
     C*
     C                   CLEAR                   VTD
     C                   CLEAR                   DATA
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'T'           VTRTYP
     C     XXCALL        IFNE      'G'
     C                   Z-ADD     VPVDR#        VTVDR#
     C                   ELSE
     C                   Z-ADD     GSFND         VTVDR#
     C                   ENDIF
     C                   ADD       1             XXRCNT

      *** beg chg ***** 09/01/17 ***************************
     C***********        Z-ADD     XXRCNT        VTRCNT
     C***********        Z-ADD     XXTPAY        VTTPAY
     C***********        Z-ADD     XXTADL        VTTADL
     C***********        Z-ADD     XXTMAN        VTTMAN
     C***********        Z-ADD     XXECNT        VTECNT
     C***********        Z-ADD     XXTPRM        VTTPRM

      /Free
       VTrcnt = xxRCnt;
       VTecnt = xxECnt;
       If xxExcPrem = *Blank;
          VTtman = xxTMan;
          VTTPrm = xxTPrm;
       Endif;
       If xxExcPAYr = *Blank;
          VTtpay = xxTPay;
          VTtadl = xxTAdL;
       Endif;
      /End-Free
      *** end chg ***** 09/01/17 ***************************

     C     VTTPAY        IFLT      0
     C                   MULT      -1            VTTPAY
     C                   ENDIF
     C     VTTADL        IFLT      0
     C                   MULT      -1            VTTADL
     C                   ENDIF
     C     VTTMAN        IFLT      0
     C                   MULT      -1            VTTMAN
     C                   ENDIF
     C     VTECNT        IFLT      0
     C                   MULT      -1            VTECNT
     C                   ENDIF
     C     VTTPRM        IFLT      0
     C                   MULT      -1            VTTPRM
     C                   ENDIF
     C*
     C                   MOVEL     VTD           DATA
     C                   WRITE     PPX01
     C*
     C                   ENDSR                                                  WRTVT
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE A NAME/ADDRESS RECORD
     C*
     C     WRTNA         BEGSR
     C*
      *** BEG DEL ***** 01/05/18 ******************************
     C**FIND CURRENT AGENT.
     C***
     C***                Z-ADD     0             XXAGT#
     C***
     C***  AHKY          SETGT     PAH01
     C***  AHKY          READPE    PAH01                                  60
     C***
     C***  *IN60         DOWEQ     '0'
     C***  AHSTAT        ANDEQ     'I'
     C***  AHKY          READPE    PAH01                                  60
     C***                END
     C***
     C***  *IN60         IFEQ      *OFF
     C***                Z-ADD     AHAGT#        XXAGT#
     C***                ENDIF                                                  *IN60 IFEQ
      *** END DEL ***** 01/05/18 ******************************
     C*
     C* WRITE NAME/ADDRESS RECORD FOR EACH DIVISION.
     C*
      *** BEG CHG ***** 01/05/18 ***************************
     C***  EMKY1         CHAIN     PEM01                              60

     C     XXPTYC        IFEQ      'Y'
     C     EMKY          CHAIN     PEM01                              60
     C                   ELSE
      * Don't duplicate name and address records for each division
     C                   IF        POLNAWRTN = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   EVAL      POLNAWRTN = *ON
     C     EMKY1         CHAIN     PEM01                              60
     C                   ENDIF
      *** END CHG ***** 01/05/18 ***************************

     C     *IN60         DOWEQ     *OFF
     C                   CLEAR                   ADD
     C                   CLEAR                   DATA
     C                   Z-ADD     XXDIV         X1DIV

     C***  XXADNA        IFEQ      'Y'                                          DLT 01/05/18
     C* FIND CURRENT AGENT FROM API
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     XXEMP#        @@EMP#
     C                   Z-ADD     EMDIV         @@DIV
     C                   Z-ADD     XXFYR         @@FYR
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM      *ZEROS        @@AGT#            5 0          CHG 01/05/18
     C                   PARM                    @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
     C*
     C                   Z-ADD     @@AGT#        XXAGT#
     C* FIND CARRIER POLICY #
      *** beg chg ***** 12/26/19 ****************************************
     C***                   MOVE      *BLANKS       XXCAP#
     C***                   Z-ADD     EMDIV         XXDIV
     C***     RPKY1         SETGT     PRP01                              50
     C***     RPKY1         READPE    PRP01                                  50
     C***     *IN50         IFEQ      *OFF
     C***                   MOVE      RPCAP#        XXCAP#
     C***                   ENDIF
      /Free
       If xxCap# = *Blanks;
          Setgt rpKy1 prp01;
          Readpe rpKy1 prp01;
          If Not(%Eof);
             xxNaCap# = RPcap#;
          Endif;
       Else;
          xxNaCap# = xxCap#;
       Endif;
      *** end chg ***** 12/26/19 ****************************************

     C***                ENDIF                                                  DLT 01/05/18
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'A'           ADRTYP
      *** beg chg ***** 12/26/19 ****************************************
     C***                   MOVEL     XXCAP#        ADCAP#
     C                   MOVEL     XXNACAP#      ADCAP#
      *** end chg ***** 12/26/19 ****************************************
     C                   Z-ADD     XXFND         ADFND
     C                   Z-ADD     XXEMP#        ADEMP#
     C                   Z-ADD     EMDIV         ADDIV
     C                   Z-ADD     0             ADFYR
     C                   Z-ADD     XXAGT#        ADAGT#
     C                   MOVE      'D'           ADTYPE
     C                   MOVE      *ZEROS        ADELID
     C                   MOVEL     EMNAME        ADNAME
     C                   MOVEL     EMFDNM        ADFDNM
     C     EMNAME        CAT       EMNAM2        ADN100
     C     EMFDNM        CAT       EMFDN2        ADF100
     c                   movel(p)  emname        xxn100          100
     c                   move      emnam2        xxn100
     c                   movel(p)  xxn100        adn100
     c                   movel(p)  emfdnm        xxf100          100
     c                   move      emfdn2        xxf100
     c                   movel(p)  xxf100        adf100
     C                   MOVEL     EMMAD1        ADMAD1
     C                   MOVEL     EMMAD2        ADMAD2
     C                   MOVEL     EMMCTY        ADMCTY
     C                   MOVEL     EMMST         ADMST
     C                   Z-ADD     EMMZIP        ADMZIP
     C                   MOVEL     EMPAD1        ADPAD1
     C                   MOVEL     EMPAD2        ADPAD2
     C                   MOVEL     EMPCTY        ADPCTY
     C                   MOVEL     EMPST         ADPST
     C                   Z-ADD     EMPZIP        ADPZIP
     C                   MOVEL     EMAREA        ADPHN
     C                   MOVE      EMPHN#        ADPHN
     C                   MOVEL     EMFAXA        ADFAX
     C                   MOVE      EMFAX#        ADFAX
     C                   MOVE      *ZEROS        ADCXDT
     C     EMCAD1        IFNE      *ZERO
     C     19000000      ADD       EMCAD1        ADCXDT
     C                   ENDIF
     C                   MOVEL     EMSIC         ADSIC
     C                   MOVEL     EMNAIC        ADNAIC
     C     19000000      ADD       EMEFD1        ADEFDT                         ADD 01/05/18
     C*
     C                   MOVEL     ADD           DATA
     C     ELKY4         SETGT     PEL02
     C     ELKY5         READPE    PEL02                                  50
     C     *IN50         IFEQ      *OFF
     C                   WRITE     PPX01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ADD       1             XXRCNT
     C*
     C* WRITE NAME/ADDRESS RECORD FOR EACH ADDITIONAL LOCATION.
     C*
      *** BEG CHG ***** 01/05/18 ***************************
     C***  EMKY1         CHAIN     PAL01                              60
     C     EMKY2         CHAIN     PAL01                              60
      *** END CHG ***** 01/05/18 ***************************
     C     *IN60         DOWEQ     *OFF
     C                   CLEAR                   ADD
     C                   CLEAR                   DATA
     C*** BEG DEL ***** 01/05/18 ***************************
     C***  XXADNA        IFEQ      'Y'
     C**FIND CURRENT AGENT FROM API
     C***                Z-ADD     XXCO#         @@CO#
     C***                Z-ADD     XXFND         @@FND
     C***                Z-ADD     XXEMP#        @@EMP#
     C***                Z-ADD     ALDIV         @@DIV
     C***                Z-ADD     XXFYR         @@FYR
     C***                Z-ADD     XXTDT         SRDTI
     C***                CALL      'SRCVTDAT'    DTMDYY
     C***                MOVEL     SRDTO         @@DATE
     C***                MOVEL     '*'           @@PLAN
      ***
     C***                CALL      'WA002R'
     C***                PARM                    @@CO#             3 0
     C***                PARM                    @@FND             3 0
     C***                PARM                    @@EMP#            9 0
     C***                PARM                    @@DIV             5 0
     C***                PARM                    @@FYR             3 0
     C***                PARM      *ZEROS        @@AGT#            5 0          CHG 01/05/18
     C***                PARM                    @@ANAM           60
     C***                PARM                    @@PROD            5 0
     C***                PARM                    @@PNAM           60
     C***                PARM                    @@AREA            3 0
     C***                PARM                    @@PHON            7 0
     C***                PARM                    @@PLAN            1
     C***                PARM                    @@DATE            8 0
     C***
     C***                Z-ADD     @@AGT#        XXAGT#
     C**FIND CARRIER POLICY #
     C***
     C***                MOVE      *BLANKS       XXCAP#
     C***                Z-ADD     ALDIV         XXDIV
     C***  RPKY1         SETGT     PRP01                              50
     C***  RPKY1         READPE    PRP01                                  50
     C***  *IN50         IFEQ      *OFF
     C***                MOVE      RPCAP#        XXCAP#
     C***                ENDIF
     C***                ENDIF
     C*** END DEL ***** 01/05/18 ***************************

      *** beg add ***** 02/21/18 ***************************
      /Free
       // Skip if non reporting locations are excluded
       If isExcNRLoc and ALncci <> 'Y';
          Reade emKy2 pal01;
          *In60 = %Eof;
          Iter;
       Endif;
      /End-Free
      *** end add ***** 02/21/18 ***************************
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'A'           ADRTYP
      *** beg chg ***** 12/26/19 ****************************************
     C***                   MOVEL     XXCAP#        ADCAP#
     C                   MOVEL     XXNACAP#      ADCAP#
      *** beg chg ***** 12/26/19 ****************************************
     C                   Z-ADD     XXFND         ADFND
     C                   Z-ADD     XXEMP#        ADEMP#
     C                   Z-ADD     ALDIV         ADDIV
     C                   Z-ADD     0             ADFYR
     C                   Z-ADD     XXAGT#        ADAGT#
     C                   MOVE      ALELTP        ADTYPE
     C                   MOVEL     ALELID        ADELID
     C                   MOVEL     ALNAME        ADNAME
     C                   MOVEL     ALFDNM        ADFDNM
     C     ALNAME        CAT       ALNAM2        ADN100
     C     ALFDNM        CAT       ALFDN2        ADF100
     c                   movel(p)  alname        xxn100          100
     c                   move      alnam2        xxn100
     c                   movel(p)  xxn100        adn100
     c                   movel(p)  alfdnm        xxf100          100
     c                   move      alfdn2        xxf100
     c                   movel(p)  xxf100        adf100
     C                   MOVEL     ALMAD1        ADMAD1
     C                   MOVEL     ALMAD2        ADMAD2
     C                   MOVEL     ALMCTY        ADMCTY
     C                   MOVEL     ALMST         ADMST
     C                   Z-ADD     ALMZIP        ADMZIP
     C                   MOVEL     ALPAD1        ADPAD1
     C                   MOVEL     ALPAD2        ADPAD2
     C                   MOVEL     ALPCTY        ADPCTY
     C                   MOVEL     ALPST         ADPST
     C                   Z-ADD     ALPZIP        ADPZIP
     C                   MOVEL     ALAREA        ADPHN
     C                   MOVE      ALPHN#        ADPHN
     C                   MOVEL     ALFARA        ADFAX
     C                   MOVE      ALFAX#        ADFAX
     C                   MOVE      *ZEROS        ADCXDT
     C     ALDLDT        IFNE      *ZERO
     C     19000000      ADD       ALDLDT        ADCXDT
     C                   ENDIF
     C                   MOVEL     ALSIC         ADSIC
     C                   MOVEL     ALNAIC        ADNAIC                         ADD 01/05/18
     C     19000000      ADD       ALEFDT        ADEFDT                         ADD 01/05/18
     C*
     C                   MOVEL     ADD           DATA
     C     ALEFDT        IFLE      XXTDT
     C     ALDLDT        ANDEQ     0
     C     ALEFDT        ORLE      XXTDT
     C     ALDLDT        ANDGT     XXFDT
     C                   WRITE     PPX01
     C                   ENDIF
     C*
     C                   ADD       1             XXRCNT
     C*
      *** BEG CHG ***** 01/05/18 ***************************
     C***  EMKY1         READE     PAL01                                  60
     C     EMKY2         READE     PAL01                                  60
      *** END CHG ***** 01/05/18 ***************************
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* WRITE RECORD FOR AGENT INFORMATION.
     C*
     C*** BEG DEL ***** 01/05/18 ***************************
     C***  XXADNA        IFEQ      'Y'                                          DLT 01/05/18
     C**FIND CURRENT AGENT FROM API
     C***                Z-ADD     XXCO#         @@CO#
     C***                Z-ADD     XXFND         @@FND
     C***                Z-ADD     XXEMP#        @@EMP#
     C***                Z-ADD     X1DIV         @@DIV
     C***                Z-ADD     XXFYR         @@FYR
     C***                Z-ADD     XXTDT         SRDTI
     C***                CALL      'SRCVTDAT'    DTMDYY
     C***                MOVEL     SRDTO         @@DATE
     C***                MOVEL     '*'           @@PLAN
      ***
     C***                CALL      'WA002R'
     C***                PARM                    @@CO#             3 0
     C***                PARM                    @@FND             3 0
     C***                PARM                    @@EMP#            9 0
     C***                PARM                    @@DIV             5 0
     C***                PARM                    @@FYR             3 0
     C***                PARM      *ZEROS        @@AGT#            5 0          CHG 01/05/18
     C***                PARM                    @@ANAM           60
     C***                PARM                    @@PROD            5 0
     C***                PARM                    @@PNAM           60
     C***                PARM                    @@AREA            3 0
     C***                PARM                    @@PHON            7 0
     C***                PARM                    @@PLAN            1
     C***                PARM                    @@DATE            8 0
     C***
     C***                Z-ADD     @@AGT#        XXAGT#
     C***
     C***
     C**FIND CARRIER POLICY #
     C***
     C***                MOVE      *BLANKS       XXCAP#
     C***                Z-ADD     X1DIV         XXDIV
     C***  RPKY1         SETGT     PRP01                              50
     C***  RPKY1         READPE    PRP01                                  50
     C***  *IN50         IFEQ      *OFF
     C***                MOVE      RPCAP#        XXCAP#
     C***                ENDIF
     C***                ENDIF
     C*** END DEL ***** 01/05/18 ***************************

     C     XXAGT#        IFNE      *ZERO
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C     AGKY          CHAIN     PAG01                              60
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   ADD
     C                   CLEAR                   DATA
     C*
     C* POPULATE DATA STRUCTURE
     C*
     C                   MOVE      'A'           ADRTYP
      *** beg chg ***** 12/26/19 ****************************************
     C***                   MOVEL     XXCAP#        ADCAP#
     C                   MOVEL     XXNACAP#      ADCAP#
      *** end chg ***** 12/26/19 ****************************************
     C                   Z-ADD     XXFND         ADFND
     C                   Z-ADD     XXEMP#        ADEMP#
     C                   Z-ADD     EMDIV         ADDIV                          CHG 01/05/18
     C                   Z-ADD     0             ADFYR
     C                   Z-ADD     XXAGT#        ADAGT#
     C                   MOVE      'A'           ADTYPE
     C                   MOVE      *ZEROS        ADELID
     C                   MOVEL     AGNAME        ADNAME
     C                   MOVEL     AGFDNM        ADFDNM
     C                   MOVEL     AGMAD1        ADMAD1
     C                   MOVEL     AGMCTY        ADMCTY
     C                   MOVEL     AGMST         ADMST
     C                   Z-ADD     AGMZIP        ADMZIP
     C                   MOVEL     AGPAD1        ADPAD1
     C                   MOVEL     AGPCTY        ADPCTY
     C                   MOVEL     AGPST         ADPST
     C                   Z-ADD     AGPZIP        ADPZIP
     C                   MOVEL     AGARE1        ADPHN
     C                   MOVE      AGPHN1        ADPHN
     C                   MOVEL     AGARE2        ADFAX
     C                   MOVE      AGPHN2        ADFAX
      *** BEG ADD ***** 01/05/18 *****************************
      * AHEFDT is returned in @@Date as a YYYMMDD from WA002R
     C                   IF        @@DATE <> 0
     C     19000000      ADD       @@DATE        ADEFDT
     C                   ENDIF
      *** END ADD ***** 01/05/18 *****************************
     C*
     C                   MOVEL     ADD           DATA
     C                   WRITE     PPX01
     C*
     C                   ADD       1             XXRCNT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  XXAGT# IFNE
     C*
      *** BEG ADD ***** 01/05/18 ***************************
     c                   EVAL      XXDIV = X1DIV
     C     XXPTYC        IFEQ      'Y'
     C     EMKY          READE     PEM01                                  60
     C                   ELSE
     C     EMKY1         READE     PEM01                                  60
     C                   ENDIF
     C                   ENDDO
      *** END ADD ***** 01/05/18 ***************************

     C                   ENDSR                                                  WRTNA
     C*
      *** beg add ***** 07/25/17 ******************************************
      /Free
       //-----------------------------------------------------------
       //Subroutine to Write Payroll Detail Records for Employee
       //-----------------------------------------------------------
       BegSr WrtEE;

       Clear PAE;
       Clear DATA;
       Clear xxEAPR;
       Clear xxSAPR;
       firstEE = *Off;

       Setll (xxco#:xxfnd:xxemp#:xxdiv:xxfyr) pee04;
       Reade (xxco#:xxfnd:xxemp#:xxdiv:xxfyr) pee04;
       If Not %Eof(wmeel4);
        //Save work fields after first read
          firstEE = *On;
          xxLSN   = EELSN;
          xxFSN   = EEFSN;
          xxMID   = EEMID;
          xxSS#   = EESS#;
          xxTITL  = EETITL;
          xxPOSN  = EEPOSN;
          xxCLAS1 = EECLAS;
          xxSPEC  = EESPEC;
          xxEXT   = EEEXT;
          xxEAPR  = EEEAPR;
       Endif;
       Dow not %eof(wmeel4);
          If xxLSN  <> EELSN  or xxFSN  <> EEFSN  or xxMID  <> EEMID or
           xxSS#  <> EESS#  or xxTITL <> EETITL or xxPOSN <> EEPOSN or
           xxCLAS1 <> EECLAS or xxSPEC <> EESPEC or xxEXT  <> EEEXT;
           //Clear Data structure and work fields
             Clear PAE;
             Clear DATA;
           //Populate Data structure
             PERTYP = 'E';
             PECAP# = XXCAP#;
             PEFND  = EEFND;
             PEEMP# = EEEMP#;
             PEDIV  = EEDIV;
             PECLAS = xxCLAS1;
             PESPEC = xxSPEC;
             PEEXT  = xxEXT;
             PELSN  = xxLSN;
             PEFSN  = xxFSN;
             PEMID  = xxMID;
             PETITL = xxTITL;
             PEPOSN = xxPOSN;
             PESS#  = xxSS#;
             If xxExcPayR = *Blank;
                PEEAPR  = xxEAPR;
                PESAPR  = xxSAPR;
             Endif;
           //*** beg add ***** 09/01/17 ******************
             pePolFil# = %EditC(ELfnd:'X') + %EditC(ELemp#:'X')
              + %EditC(ELfyr:'X');
           //*** end add ***** 09/01/17 ******************
             DATA = PAE;
             Write PPX01;
             xxRCNT = xxRCNT + 1;
           //Save work fields
             xxLSN  = EELSN;
             xxFSN  = EEFSN;
             xxMID  = EEMID;
             xxSS#  = EESS#;
             xxTITL = EETITL;
             xxPOSN = EEPOSN;
             xxCLAS1= EECLAS;
             xxSPEC = EESPEC;
             xxEXT  = EEEXT;
             xxEAPR = EEEAPR;
             xxSAPR = EESAPR;
          Else;
             xxSAPR = xxSAPR + EESAPR;
          Endif;
         //Accumulate Self reported payroll
       Reade (xxco#:xxfnd:xxemp#:xxdiv:xxfyr) pee04;
       Enddo;

       If firstEE = *On;
        //Clear Data structure and work fields
          Clear PAE;
          Clear DATA;
        //Populate Data structure
          PERTYP = 'E';
          PECAP# = XXCAP#;
          PEFND  = EEFND;
          PEEMP# = EEEMP#;
          PEDIV  = EEDIV;
          PECLAS = xxCLAS1;
          PESPEC = xxSPEC;
          PEEXT  = xxEXT;
          PELSN  = xxLSN;
          PEFSN  = xxFSN;
          PEMID  = xxMID;
          PETITL = xxTITL;
          PEPOSN = xxPOSN;
          PESS#  = xxSS#;
          If QQEPyrl = ' ';
             PEEAPR  = xxEAPR;
             PESAPR  = xxSAPR;
          Endif;
        //*** beg add ***** 09/01/17 ******************
          pePolFil# = %EditC(ELfnd:'X') + %EditC(ELemp#:'X')
           + %EditC(ELfyr:'X');
        //*** end add ***** 09/01/17 ******************
          DATA = PAE;
          Write PPX01;
          xxRCNT = xxRCNT + 1;
       Endif;

       Endsr;
      /End-Free
      *** end add ***** 07/25/17 ***************************
     C******************************************************
     C* SUBROUTINE - CHKACT : CHECK ACTIVITY
     C******************************************************
     C     CHKACT        BEGSR
     C*
     C                   MOVEL     'N'           XXACTV            1            SEEM 2 BE WRONG
     C* ACTIVITY IN WDEHP
     C*
     C     EMKY1         SETLL     PEH01
     C     EMKY1         READE     PEH01                                  90
     C*
     C     *IN90         DOWEQ     *OFF
     C*
     C     EHTRDT        IFGE      XXFDT
     C     EHTRDT        ANDLE     XXTDT
     C                   Z-ADD     XXDIV         X1DIV             5 0
     C                   Z-ADD     XXFYR         X1FYR
     C* CHECK VENDOR
     C                   Z-ADD     EHDIV         XXDIV
     C                   Z-ADD     EHFYR         XXFYR
     C                   EXSR      CHKVDR
     C                   Z-ADD     X1DIV         XXDIV
     C                   Z-ADD     X1FYR         XXFYR
     C*
     C     X1VDR#        IFEQ      VPVDR#
     C                   MOVEL     'Y'           XXACTV            1
     C                   LEAVESR
     C                   ENDIF                                                  X1VDR#=VPVDR#
     C*
     C                   ENDIF                                                  EHTRDT
     C*
     C     EMKY1         READE     PEH01                                  90
     C*
     C                   ENDDO                                                  *IN90 = *OFF
     C*
     C     XXACTV        IFNE      'Y'
     C* ACTIVITY IN WXEHP
     C     EMKY1         SETLL     PXH01
     C     EMKY1         READE     PXH01                                  90
     C*
     C     *IN90         DOWEQ     *OFF
     C*
     C     XHDT          IFGE      XXFDT
     C     XHDT          ANDLE     XXTDT
     C*
     C                   Z-ADD     XXDIV         X1DIV
     C                   Z-ADD     XXFYR         X1FYR
     C* CHECK VENDOR
     C                   Z-ADD     XHDIV         XXDIV
     C                   Z-ADD     XHFYR         XXFYR
     C                   EXSR      CHKVDR
     C                   Z-ADD     X1DIV         XXDIV
     C                   Z-ADD     X1FYR         XXFYR
     C*
     C     X1VDR#        IFEQ      VPVDR#
     C                   MOVEL     'Y'           XXACTV
     C                   LEAVESR
     C                   ENDIF                                                  X1VDR#=VPVDR#
     C*
     C                   ENDIF                                                  XHDT B/W ACT DT RANG
     C*
     C     EMKY1         READE     PXH01                                  90
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXACTV = 'Y'
     C*
     C* ACTIVITY IN WMMUP IF WMVPP HAS FLAGGED FOR MIDTERM ENDORSEMENTS
     C     VPEXNP        IFEQ      'Y'
     C     XXACTV        ANDNE     'Y'
     C*
     C     EMKY1         SETLL     PMU01
     C     EMKY1         READE     PMU01                                  90
     C*
     C     *IN90         DOWEQ     *OFF
     C*
     C     MUDT          IFGE      XXFDT
     C     MUDT          ANDLE     XXTDT
     C*
     C                   Z-ADD     XXDIV         X1DIV
     C                   Z-ADD     XXFYR         X1FYR
     C* CHECK VENDOR
     C                   Z-ADD     MUDIV         XXDIV
     C                   Z-ADD     MUFYR         XXFYR
     C                   EXSR      CHKVDR
     C                   Z-ADD     X1DIV         XXDIV
     C                   Z-ADD     X1FYR         XXFYR
     C*
     C     X1VDR#        IFEQ      VPVDR#
     C                   MOVEL     'Y'           XXACTV
     C                   LEAVESR
     C                   ENDIF                                                  X1VDR#=VPVDR#
     C*
     C                   ENDIF                                                  MUDT B/W ACT DT RANG
     C*
     C     EMKY1         READE     PMU01                                  90
     C*
     C                   ENDDO                                                  *IN90 = *ON
     C*
     C                   ENDIF                                                  VPEXNP = 'Y'

      *** beg add ***** 02/21/18 **************************
      /Free
       If isIncNBDiv;
          If ELqtfr <> 0;
             xxCmpDate = ELqtfr;
          Else;
             xxCmpDate = ELidt;
          Endif;
          If xxCmpDate >= xxFDt and ELidt <= xxTDt;
             xxActv = 'Y';
          Endif;
       Endif;
      /End-Free
      *** end add ***** 02/21/18 **************************

     C                   ENDSR
     C*****************************************************************
     C* SUBROUTINE - CHKVDR : CHECK VENDOR
     C*****************************************************************
     C     CHKVDR        BEGSR
     C*
     C                   Z-ADD     XXFDT         KKFDT
      *
     C     VHKY          SETGT     PVH01
     C     EHKY1         READPE    PVH01                                  62
     C     *IN62         IFEQ      *OFF
      *
     C                   SELECT
     C     VHEXDT        WHENEQ    *ZERO
     C                   Z-ADD     VHVDR#        X1VDR#
     C     VHEXDT        WHENNE    *ZERO
     C     VHEXDT        ANDGT     XXFDT
     C*** Begin Add *** 11/22/17 *******************************
     C     VHEXDT        ANDNE     VHEFDT
     C*** End   Add *** 11/22/17 *******************************
     C                   Z-ADD     VHVDR#        X1VDR#
     C                   OTHER
     C                   Z-ADD     *ZERO         X1VDR#
     C                   ENDSL
      *
     C                   ELSE
     C                   Z-ADD     *ZERO         X1VDR#
     C                   ENDIF
      *
     C                   ENDSR
     C*****************************************************************
     C* SUBROUTINE - GTLFYR : GET LATEST FUND YEAR
     C*****************************************************************
     C     GTLFYR        BEGSR
      *
     C     F1KY          SETGT     PF101
     C     F1KY1         READPE    PF101                                  50
     C                   Z-ADD     F1FYR         XXLFYR
      *
     C                   ENDSR
     C*****************************************************************
     C* SUBROUTINE - GTLDYR : GET LATEST DIVISION YEAR
     C*****************************************************************
     C     GTLDYR        BEGSR
      *
     C                   Z-ADD     0             XXLDYR
     C                   Z-ADD     ELFYR         X1FYR
      *
     C* CHECK IF DIVISION IS CANCELLED
     C                   CALL      'WA112R'
     C                   PARM                    XXCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    ELDIV
     C                   PARM                    XX8DT             8
     C                   PARM      *BLANKS       XXRTN             1
     C*
     C                   Z-ADD     9999999       XXEFDT
     C     XXRTN         IFEQ      'Y'
     C                   MOVE      XX8DT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   ENDIF                                                  XXRTN
     C*
     C*
     C     ELKY3         SETGT     PEL01
     C     ELKY3         READPE    PEL01                                  70
     C*
     C* CANCEL DATE IS GREATER THAN RUN TO DATE
     C* AND POLICY TO DATE IS GREATER THAN RUN TO DATE
     C     XXEFDT        IFGT      XXTDT
     C     ELPTDT        ANDGT     XXTDT
     C                   Z-ADD     ELFYR         XXLDYR
     C                   ENDIF                                                  XXEFDT
     C*
     C* RESET POINTER
     C     ELKY2         CHAIN     PEL01                              70
     C*
     C                   ENDSR
     C*****************************************************************
     C* SUBROUTINE - VERACT : VERIFY ACTIVITY
     C*****************************************************************
     C     VERACT        BEGSR
     C*
     C                   MOVEL     'N'           XXACTV            1
     C* ACTIVITY IN WDEHP
     C*
     C     EMKY1         SETLL     PEH01
     C     EMKY1         READE     PEH01                                  90
     C     *IN90         DOWEQ     *OFF
     C*
     C     EHTRDT        IFGE      XXFDT
     C     EHTRDT        ANDLE     XXTDT
     C                   MOVEL     'Y'           XXACTV            1
     C                   LEAVESR
     C                   ENDIF                                                  EHTRDT
     C*
     C     EMKY1         READE     PEH01                                  90
     C                   ENDDO                                                  *IN90 = *OFF
     C*
     C     XXACTV        IFNE      'Y'
     C* ACTIVITY IN WXEHP
     C     EMKY1         SETLL     PXH01
     C     EMKY1         READE     PXH01                                  90
     C*
     C     *IN90         DOWEQ     *OFF
     C*
     C     XHDT          IFGE      XXFDT
     C     XHDT          ANDLE     XXTDT
     C                   MOVEL     'Y'           XXACTV
     C                   LEAVESR
     C*
     C                   ENDIF                                                  XHDT B/W ACT DT RANG
     C*
     C     EMKY1         READE     PXH01                                  90
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXACTV = 'Y'
     C*
     C* ACTIVITY IN WMMUP IF WMVPP HAS FLAGGED FOR MIDTERM ENDORSEMENTS
     C     XXMIDTERM     IFEQ      'Y'                                          CHG 01/05/18
     C     XXACTV        ANDNE     'Y'
     C*
     C     EMKY1         SETLL     PMU01
     C     EMKY1         READE     PMU01                                  90
     C*
     C     *IN90         DOWEQ     *OFF
     C*
     C     MUDT          IFGE      XXFDT
     C     MUDT          ANDLE     XXTDT
     C                   MOVEL     'Y'           XXACTV
     C                   LEAVESR
     C                   ENDIF                                                  MUDT B/W ACT DT RANG
     C*
     C     EMKY1         READE     PMU01                                  90
     C*
     C                   ENDDO                                                  *IN90 = *ON
     C*
     C                   ENDIF                                                  xxMidTerm = 'Y'
     C*
      *** beg add ***** 02/21/18 **************************
      /Free
       If isIncNBDiv;
          If ELqtfr <> 0;
             xxCmpDate = ELqtfr;
          Else;
             xxCmpDate = ELidt;
          Endif;
          If xxCmpDate >= xxFDt and ELidt <= xxTDt;
             xxActv = 'Y';
          Endif;
       Endif;
      /End-Free
      *** end add ***** 02/21/18 **************************

     C                   ENDSR
      *** beg add ***** 04/30/18 **************************
      *====================================================
     p GetAddress      b
      *====================================================
     d GetAddress      pi
     d  pAddress                     81
     d  pCity                        25
     d  pState                        2
     d  pZip                         10

     d  pZip1          s              5
     d  pZip2          s              4

      /Free
       Select;
       When FNpeof = *Blank or RRpgfl = *Blank or RRpgfl = '2';
       // Get state reporting division's address
          Setll (ELco#:ELfnd:ELemp#) pem01;
          Dou %Eof(WMEML);
             Reade (ELco#:ELfnd:ELemp#) pem01;
             If %Eof;
                Leave;
             Endif;
             If EMstrp = 'Y';
                Leave;
             Endif;
          Enddo;
       When RRpgfl = '1' or RRpgfl = '3';
       // Get PEO's address
          Chain (ELco#:ELfnd:1:0) pem01;
       EndSl;

       If EMmad1 <> *Blanks;
          If EMmad2 <> *Blanks;
             pAddress = %Trim(EMmad1) + ',' + %Trim(EMmad2);
          Else;
             pAddress = EMmad1;
          Endif;
          pCity    = EMmcty;
          pState   = EMmst;
          pZip1    = %Subst(%Editc(EMmzip:'X'):1:5);
          pZip2    = %Subst(%Editc(EMmzip:'X'):5:4);
          If pZip2 <> *Blanks;
             pZip = pZip1 + '-' + pZip2;
          Else;
             pZip = pZip1;
          Endif;
       Else;
          If EMmad2 <> *Blanks;
             pAddress = %Trim(EMpad1) + ',' + %Trim(EMpad2);
          Else;
             pAddress = EMpad1;
          Endif;
          pCity    = EMpcty;
          pState   = EMpst;
          pZip1    = %Subst(%Editc(EMpzip:'X'):1:5);
          pZip2    = %Subst(%Editc(EMpzip:'X'):5:4);
          If pZip2 <> *Blanks;
             pZip = pZip1 + '-' + pZip2;
          Else;
             pZip = pZip1;
          Endif;
       Endif;
      /End-Free

     p GetAddress      e
     p*** Begin Chg *** 02/22/22 ******************************************
      *====================================================
     p*GetCxDate       b
      *====================================================
     d*GetCxDate       pi             7  0
      *====================================================
     p GetCxDate2      b
      *====================================================
     d GetCxDate2      pi             7  0
     p*** End   Chg *** 02/22/22 ******************************************

      /Free
       // Get latest record as of value date
       Setgt (Elco#:ELfnd:ELemp#:ELdiv:ELfyr:cymdValDate) pxh01;
       Readpe (Elco#:ELfnd:ELemp#:ELdiv:ELfyr) pxh01;
       Select;
       When %Eof;
          Return 0;
       When XHredt <> 0;
          Return 0;
       When XHcxdt > cymdValDate;
          Return 0;
       Other;
          Return XHcxdt;
       EndSl;
      /End-Free

     p*** Begin Chg *** 02/22/22 ******************************************
     p*GetCxDate       e
     p GetCxDate2      e
     p*** End   Chg *** 02/22/22 ******************************************
      *** beg add ***** 01/24/18 ******************************
      *====================================================
      * Get Name, Address, and Fein used on Dec Page
      *====================================================
     p GetDecPageInfo  b
      /Free
       If xxFnd = svFnd and xxEmp# = svEmp#;
          Return;
       Endif;
       svFnd  = xxFnd;
       svEmp# = xxEmp#;
       Chain (xxCo#:ELrtcd) prs01;
       xxJrSt = RSjrst;
       xxEfdt = ELpfdt;
       Setgt  rrky  prr01;
       Readpe rrky1 prr01;
       If Not(%Found) or %Eof;
          xxDecNam1 = *Blanks;
          xxDecNam2 = *Blanks;
          xxDecNam3 = *Blanks;
          xxDecNam4 = *Blanks;
          xxDecAdd1 = *Blanks;
          xxDecAdd2 = *Blanks;
          xxDecAdd3 = *Blanks;
          xxDecFein = *Zeros;
          Return;
       Endif;
       Clear amn;
       ##co#  = %editc(ELco#:'X');
       ##fnd  = %editc(ELfnd:'X');
       ##emp# = %editc(ELemp#:'X');
       ##div  = %editc(ELdiv:'X');
       ##odr  = ' ';
       ##nodr = 'F';
       ##aodr = 'M';
       ##emn1 = ' ';
       ##emn2 = ' ';
       ##emn4 = ' ';
       ##emn5 = ' ';
       ##emn6 = ' ';
       ##emn7 = ' ';
       ##adrl = 0;
       ##naml = 0;
       ##fedl = 0;
       ##peo1 = ' ';
       ##peo2 = ' ';
       ##conl = 0;

       If RRpgfl = *Blank;
          ##pgfl = 'B';
       Else;
          ##pgfl = RRpgfl;
       Endif;

       If xxJrSt = 'NJ';
          ##dbaf = 'Y';
       Else;
          ##dbaf = *Blank;
       Endif;

       ##PeoExt = RRpeoa;

       //*** Begin Chg *** 02/22/22 **************************************
       // SrAddr(##co#:##fnd:##emp#:##div:##odr:##nodr:##aodr:
       // ##emn1:##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:
       // ##adrl:##naml:##fedl:##peo1:##peo2:##pgfl:##conl:
       // ##amn1:##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:
       // ##amn8:##amn8:##peol:##dbaf:##al2:##nl2:##fl2:##pl2:
       // ##cl2:##PeoExt:##Name1:##Name2:##Name3:##Name4:##Fein);

       GetNameAddress(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:
              ##emn1:##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:
              ##adrl:##naml:##fedl:##peo1:##peo2:##conl:##amn1:
              ##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:##amn8:##amn9:
              ##peol:##al2:##nl2:##fl2:##pl2:##cl2:##name1:##name2:
              ##name3:##name4:##fein);
       //*** End   Chg *** 02/22/22 **************************************

       xxDecNam1 = ##Name1;
       xxDecNam2 = ##Name2;
       xxDecNam3 = ##Name3;
       xxDecNam4 = ##Name4;
       xxDecAdd1 = %trim(amn(##adrl));
       xxDecAdd2 = %trim(amn(##adrl+1));
       xxDecAdd3 = %trim(amn(##adrl+2));
       xxDecFein = ##Fein;
      /End-Free

     p GetDecPageInfo  e
      *** end add ***** 01/24/18 ******************************
      *** beg add ***** 01/05/18 ******************************
      *====================================================
      * Get deductible amount per occurrence
      *====================================================
     p GetDedAmpo      b
     d GetDedAmpo      pi                  like(LDamp1)
      /Free
       If lgdFlg = 'Y';
          Select;
          When LDamp2 <> 0;
             Return LDamp2;
          When LDamp1 <> 0;
             Return LDamp1;
          Other;
             Return 0;
          Endsl;
       Else;
          Select;
          When ELdcd2 <> *Blanks;
             Return RtvSmDedInfo(ELdcd2:EL2def);
          When ELdcde <> *Blanks;
             Return RtvSmDedInfo(ELdcde:ELdcef);
          Other;
             Return 0;
          Endsl;
       EndIf;
      /End-Free
     p GetDedAmpo      e
      *====================================================
     p GetDedInfo      b
      *====================================================
     d GetDedInfo      pi
     d pFlgLrgDed                     1
     d pDedPlan                      20
     d pDedAggAmt                          like(LDamg1)
     d pDedAccAmt                          like(LDamp1)

     d pCode           s                   like(ELdcde)
     d pEfDt           s                   like(ELdcef)

      /Free
       pFlgLrgDed = 'N';
       Chain (ELco#:%Editc(ELbscd:'X')) pbs01;
       If BSlpgm = 'L';
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pld01;
          If %Found;
             pFlgLrgDed = 'Y';
             pDedAggAmt = LDamg1;
             pDedAccAmt = LDamp1;
             If LDamp1 >= 100000;
                pDedPlan = 'Large Deductible';
             Else;
                pDedPlan = 'Small Deductible';
             EndIf;
          Endif;
       Else;
          Select;
          When cymdValDate >= EL2def and EL2def <> 0;
             pEfDt = EL2def;
             pCode = ELdcd2;
          When cymdValDate >= ELdcef and ELdcef <> 0;
             pEfDt = ELdcef;
             pCode = ELdcde;
          Endsl;
          Setgt (ELco#:ELfnd:RSjrst:pCode:pEfDt) pdd01;
          Readpe (ELco#:ELfnd:RSjrst:pCode) pdd01;
          If Not(%Eof);
             pDedAggAmt = DDamag;
             pDedAccAmt = DDampo;
             If DDampo >= 100000;
                pDedPlan = 'Large Deductible';
             Else;
                pDedPlan = 'Small Deductible';
             EndIf;
          Endif;
       Endif;
       Return;
      /End-Free

     p GetDedInfo      e
      *====================================================
     p GetFactors      b
      *====================================================
     d GetFactors      pi
     d  pEarnPrem                    11  2
      *** beg chg ***** 03/17/22 ******************
     d***pEarnPremFact                  5  4
     d pEarnPremFact                  7  4
      *** end chg ***** 03/17/22 ******************
     d  pEarnTimeFact                 5  4
     d  pAuditStatus                  1
     d  pWritPrem                    11  2

     d idx             s              3  0
     d pEarnedDays     s              3  0
     d pTotalDays      s              3  0

      /Free
       Clear dsLastEH;
       Setgt (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:cymdValDate) peh01;
       Dou %Eof(WDEHL12);
          Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01 dsLastEH;
          If %Eof;
             Leave;
          Endif;
          If dsLastEH.EHfaf <> 'E' and dsLastEH.EHfaf <> 'R'
           and dsLastEH.EHfaf <> 'A';
             Iter;
          Else;
             Leave;
          Endif;
       Enddo;
       If %Eof(WDEHL12);
          Clear dsLastEH;
          pEarnPremFact = 0;
          pEarnTimeFact = 0;
          pAuditStatus  = ' ';
         // There may be S/R premium
          PopDateArray();
          PopSPremArray();
          pEarnPrem = %Xfoot(sPremArray);
          Return;
       EndIf;
       pWritPrem = dsLastEH.EHwapr;
       If dsLastEH.EHfaf = 'A';
          pEarnPrem  = pWritPrem;
          pEarnPremFact = 1;
          pEarnTimeFact = 1;
          pAuditStatus  = 'A';
          Return;
       Endif;
       pAuditStatus  = ' ';
       Select;
       When dsLastEH.EHpfdt = dsLastEH.EHptdt;
       //////// or (dsLastEH.EHpfdt >= xxCxDate and xxCxDate <> 0);
          Clear dsLastEH;
          pEarnPremFact = 0;
          pEarnTimeFact = 0;
          pEarnPrem = 0;
          pAuditStatus  = ' ';
       Other;
          PopDateArray();
          PopEPremArray();
          PopSPremArray();
         // Xfoot arrays for total written premium
          Eval(h) pWritPrem = %Xfoot(ePremArray) + %Xfoot(sPremArray);
          If pWritPrem = 0;
             pEarnPremFact = 0;
             pEarnTimeFact = 0;
             Return;
          Endif;
         // Find total # of policy days
          idx = %LookUp(*Hival:pDaysAry);
          If idx = 0;
             pTotalDays = maxPDays - 1;
          Else;
             pTotalDays = idx - 1;
          Endif;
         // Find # of days earned as of valuation date
          pEarnedDays = %LookUp(asOfDate:pDaysAry);
         // Check if val date is in policy date range.
         // If not, everything or nothing is earned
          Select;
          When pEarnedDays <> 0;
             Eval(h) pEarnTimeFact = pEarnedDays/pTotalDays;
             Eval(h) pEarnPrem = %Xfoot(%Subarr(ePremArray:1:pEarnedDays))
              + %Xfoot(sPremArray);
             Eval(h) pEarnPremFact = pEarnPrem/pWritPrem;
          When asOfDate < pDaysAry(1);
             Clear dsLastEH;
             pEarnPrem = 0;
             pEarnPremFact = 0;
             pEarnTimeFact = 0;
          When asOfDate >= pDaysAry(pTotalDays);
             pEarnPrem = pWritPrem;
             pEarnPremFact = 1;
             pEarnTimeFact = 1;
          Endsl;
       EndSl;
      /End-Free

     p GetFactors      e
      *** beg add ***** 02/21/18******************
      *=========================================
     p GetGrpInfo      b
      *=========================================
     d GetGrpInfo      pi
     d  inFnd                              like(ELfnd)
      /Free
       Chain (xxCo#:inFnd) pfn01;
       Chain (xxCo#:inFnd) pf200;
       Chain (xxCo#:F2tpac) ptc00;
       If %Found(WDF2L) and %Found(WMTCL);
          isIncNBDiv = TCnbdv = 'Y';
          isExcNRLoc = TCenrl = 'Y';
       Else;
          isIncNBDiv = *Off;
          isExcNRLoc = *Off;
       Endif;
      /End-Free
     p GetGrpInfo      e
      *** end add ***** 02/21/18******************
      *====================================================
     p GetLastRetro    b
      *====================================================
     d GetLastRetro    pi            11  2

      /Free
       Setgt (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:cymdValDate) peh01;
       Dou %Eof(WDEHL12);
          Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
          If %Eof;
             Leave;
          Endif;
          If EHfaf <> 'X';
             Iter;
          Else;
             Return EHwrpr;
          Endif;
       Enddo;
       If %Eof;
          Return 0;
       EndIf;
      /End-Free

     p GetLastRetro    e
      *====================================================
     p GetLiabLimits   b
      *====================================================
     d GetLiabLimits   pi                  like(PXEmpLL)
     d  pAccLimit      s              9  0
     d  pDisLimit      s              9  0
     d  pAggLimit      s              9  0

      /Free
       Select;
       When cymdValDate >= ELi2ef and ELi2ef <> 0;
          Setgt (ELco#:ELfnd:RSjrst:ELinl2:ELi2ef) plm01;
          Readpe (ELco#:ELfnd:RSjrst:ELinl2) plm01;
          If Not(%Eof);
             pAccLimit = LMacc * 1000;
             pDisLimit = LMdis * 1000;
             pAggLimit = LMagg * 1000;
          Endif;
       When cymdValDate >= ELilef and ELilef <> 0;
          Setgt (ELco#:ELfnd:RSjrst:ELilcd:ELilef) plm01;
          Readpe (ELco#:ELfnd:RSjrst:ELilcd) plm01;
          If Not(%Eof);
             pAccLimit = LMacc * 1000;
             pDisLimit = LMdis * 1000;
             pAggLimit = LMagg * 1000;
          Endif;
       Other;
          pAccLimit = RRacc * 1000;
          pDisLimit = RRdis * 1000;
          pAggLimit = RRagg * 1000;
       EndSl;
       If pAccLimit <> *Zeros;
          Return %Trim(%EditC(pAccLimit:'2')) + '/' +
           %Trim(%EditC(pDisLimit:'2')) + '/' +
           %Trim(%EditC(pAggLimit:'2'));
       Else;
          Return *Blanks;
       Endif;
      /End-Free

     p GetLiabLimits   e
      *=====================================================
     p GetMeritPrem    b
      *=====================================================
     d GetMeritPrem    pi            11  2

     d pPremBeforeMod  s             11  2
     d pPremAfterMod   s             11  2

      /Free
       If ELprom <> 'M' or dsLastEH.EHwxp1 = 0;
          Return 0;
       Endif;
       // Total modified prem (with exp constant)
       pPremAfterMod  = dsLastEH.EHwxp1 + dsLastEH.EHwxp2 + dsLastEH.EHwxp3;
       // Subject - Credit 1a and 1b
       pPremBeforeMod = dsLastEH.EHwsbj -
                        dsLastEH.EHwdcm -
                        dsLastEH.EHwdcs;
       // Minus Dev1
       If RRdv1l = 'J';
          pPremBeforeMod -= (dsLastEH.EHwdc2 + dsLastEH.EHd1w2);
       Endif;
       // Minus Dev2
       If RRdv2l = 'J';
          pPremBeforeMod -= (dsLastEH.EHwdv2 + dsLastEH.EHd2w2);
       Endif;
       // Minus waiver
       If ELwvfl = 'J';
          pPremBeforeMod -= (dsLastEH.EHwwav);
       Endif;
       // Minus short rate
       If RRsraf = 'J';
          pPremBeforeMod -= (dsLastEH.EHwsra);
       Endif;
       // Minus large deductible
       Chain (ELco#:%Editc(ELbscd:'X')) pbs01;
       If %found and BSlpgm = 'L';
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pld01;
          If %Found and LDplv1 = 'J';
             pPremBeforeMod -= (dsLastEH.EHwdct + dsLastEH.EHwdd2);
          Endif;
       Endif;
       // Minus additional load (from WritePayInfo)
       If RRasfl = 'J';
          pPremBeforeMod += (xxAddLoad);
       Endif;
       Return pPremAfterMod - pPremBeforeMod;
      /End-Free

     p GetMeritPrem    e
      *=====================================================
     p GetPol#         b
      *=====================================================
     d GetPol#         pi                  Like(RPcap#)

      /Free
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) prp01;
       If %Found and RPcap# <> *Blanks;
          Return RPcap#;
       Else;
          Return %Editc(ELfnd:'X') + %Editc(ELemp#:'X') + %Editc(ELfyr:'X');
       EndIf;
      /End-Free

     p GetPol#         e
      *=====================================================
     p PopDateArray    b
      *=====================================================
     d PopDateArray    pi
     d  startCymd                     7  0 Options(*Nopass)
     d  endCymd                       7  0 Options(*Nopass)

     d begPDate        s               d
     d endPDate        s               d
     d idx             s              3  0

      /Free
       Reset pDaysAry;
       If %Parms >= 2;
          begpDate = %Date(startCymd:*Cymd);
          endPDate = %Date(endCymd:*Cymd);
       Else;
          Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
          Dou %Eof(WDEHL12);
             Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
             If %Eof;
                Leave;
             Endif;
             If EHtrdt <= cymdValDate and EHefdt <= cymdValdate and
              (EHfaf = 'E' or EHfaf = 'R');
                Leave;
             Endif;
          Enddo;
          If Not(%Eof);
             begPDate = %Date(EHpfdt:*Cymd);
             endPDate = %Date(EHptdt:*Cymd);
          Else;
             begPDate = %Date(ELpfdt:*Cymd);
             endPDate = %Date(ELptdt:*Cymd);
          Endif;
       Endif;
       // Load pDaysAry with every policy date except 02/29
       //  unless 02/29 is the policy start date
       For idx = 1 to %Elem(pDaysAry) by 1;
          pDaysAry(idx) = begPDate + %Days(idx - 1);
          If %Subdt(pDaysAry(idx):*M) = 2 and %Subdt(pDaysAry(idx):*D) = 29
           and idx <> 1;
           //*** begin chg ***  04/08/19  ******************************
           //*****  idx -= 1;
             pDaysAry(idx) = *hival;
           //*** end chg *****  04/08/19  ******************************
          Endif;
         // Exp dates don't earn premium
          If pDaysAry(idx) = endPDate - %Days(1);
             Leave;
          Endif;
       Endfor;
       //*** begin add ***  04/08/19  ******************************
       Sorta pDaysAry;
       //*** end add *****  04/08/19  ******************************
      /End-Free

     p PopDateArray    e
      *====================================================
     p PopEPremArray   b
      *====================================================

     d cDate           s               d
     d cdx             s              3  0
     d pDailyPremAdj   s             13  6
     d pDailyExCnAdj   s             13  6
     d edx             s              3  0
     d idx             s              3  0
     d pEarnedDays     s              3  0
     d pExpConAdj      s             15  6
     d pdx             s              3  0
     d pPremAdj        s             15  6

      /Free
       Reset ePremArray;
       Reset expConArray;
       Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
       Dou %Eof(WDEHL12);
          Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
          If %Eof;
             Leave;
          EndIf;
         // Only looking at estimated bills
          If EHfaf <> 'E' and EHfaf <> 'R';
             Iter;
          Endif;
         // Don't include bills written or effective after valuation date
          If EHtrdt > cymdValDate;
             Leave;
          Endif;
          If EHefdt > cymdValDate;
             Iter;
          Endif;
         // The policy period was changed. Reset everything and start again.
          If %Date(EHpfdt:*Cymd) <> pDaysAry(1);
             Reset ePremArray;
             Reset expConArray;
             PopDateArray(EHpfdt:EHptdt);
          Endif;
         // Find total # of policy days. The last day of the policy
         //  doesn't earn premium.
          pdx = %LookUp(*Hival:pDaysAry);
          If pdx = 0;
             pdx = maxPDays - 1;
          Else;
             pdx = pdx - 1;
          Endif;
         // Populate est prem aray with daily prem amts from
         //  effective date of bill to last active day of policy
         //  (EHptdt - 1)
          edx = %LookUp(%Date(EHefdt:*Cymd):pDaysAry);
          If edx <> 0 and EHfaf <> 'A';
             pPremAdj = EHwapr - %Xfoot(ePremArray);
             pExpConAdj = EHprtd - %Xfoot(expConArray);
             Eval(h) pDailyPremAdj = pPremAdj/(pdx - edx + 1);
             Eval(h) pDailyExCnAdj = pExpConAdj/(pdx - edx + 1);
             For idx = edx to pdx;
                ePremArray(idx)  += pDailyPremAdj;
                expConArray(idx) += pDailyExCnAdj;
             Endfor;
          Endif;
       Enddo;
       // Clear est prem array for cx from the cx eff date to policy exp date
       If xxCxDate <> 0;
          cDate = %Date(xxCxDate:*Cymd);
          cdx = %Lookup(cDate:pDaysAry);
          If cdx <> 0;
             For idx = cdx to pdx;
                ePremArray(idx) = 0;
                expConArray(idx) = 0;
             Endfor;
          Endif;
       Endif;
      /End-Free

     p PopEPremArray   e
      *====================================================
     p PopSPremArray   b
      *====================================================

     d idx             s              3  0
     d pdx             s              3  0
     d pMonth          s              2  0
     d pYear           s              4  0
     d sdx             s              3  0
     d sDate           s               d
     d sMonth          s              2  0
     d sYear           s              4  0

      /Free
       Reset sPremArray;
       ////If xxCxDate = ELpfdt or ELpfdt = ELptdt;
       ////   Return;
       ////Endif;
       Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pea23;
       Dou %Eof(WMEAL23);
          Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pea23;
          If %Eof or EApodt > cymdValDate;
             Leave;
          EndIf;
          If EAarcd <> F1srbb or EAtrdt > cymdValDate;
             Iter;
          Endif;
          sDate = %Date(EApodt:*Cymd);
          sdx   = %Lookup(sDate:pDaysAry);
          If sdx = 0;
             Iter;
          Endif;
          sPremArray(sdx) += EAamt;
         // Find total # of policy days. The last day of the policy
         //  doesn't earn premium.
          pdx = %LookUp(*Hival:pDaysAry);
          If pdx = 0;
             pdx = maxPDays - 1;
          Else;
             pdx = pdx - 1;
          Endif;
         // Clear ePremArray for reported month
          sYear  = %Subdt(sDate:*Y);
          sMonth = %Subdt(sDate:*M);
        // Do a day diff between (beg month date of sDate) and pDaysArry(1) and add to idx...
          For idx = 1 to pdx;
             pMonth = %Subdt(pDaysAry(idx):*M);
             pYear  = %Subdt(pDaysAry(idx):*Y);
             Select;
             When pYear = sYear and pMonth = sMonth;
                ePremArray(idx) = expConArray(idx);
             When pYear = sYear and pMonth > sMonth or
              pYear > sYear;
                Leave;
             Endsl;
          Endfor;
       Enddo;
      /End-Free

     p PopSPremArray   e
      *====================================================
     p ProcessGroup    b
      *====================================================

     d idx             s              5  0

      /Free
       Reset procEmpYr;
       Chain (GSco#:GSfnd) pfn01;
       Chain (GSco#:GSfnd) pf200;
       Setll (GSco#:GSfnd:xxFDt) pel03;
       Dou %Eof(WDELL3);
          Reade (GSco#:GSfnd) pel03;
          If %Eof;
             Leave;
          Endif;
          If l3ELpfdt > xxTDt;
             Leave;
          Endif;
          Reset polInfo;
          Reset valInfo;
          Setll (l3ELco#:l3ELfnd:l3ELfyr:l3ELEmp#) pel0l1;
          Dou %Eof(WDELL1);
             Reade (l3ELco#:l3ELfnd:l3ELfyr:l3ELEmp#) pel0l1;
            // Include all divisions if any division falls in date range?
             Select;
             When %Eof(WDELL1);
                idx = %Lookup(*Blanks:procEmpYr);
                procEmpYr(idx) = %Editc(ELemp#:'X') + %Editc(ELfyr:'X');
                WriteWXPXPAF();
                Leave;
             Other;
                idx = %Lookup(%Editc(ELemp#:'X') +
                 %Editc(ELfyr:'X'):procEmpYr);
                If idx <> 0;
                   Leave;
                Endif;
             Endsl;
             WritePolInfo();
          Enddo;
       Enddo;
      /End-Free

     p ProcessGroup    e
      *====================================================
     p RunAfExport     b
      *====================================================
      /Free
       Setll *Loval pgs01;
       Dou %Eof(WXGSL);
          Read pgs01;
          If %Eof;
             Leave;
          EndIf;
          If GSiefl = 'I';
             ProcessGroup();
          Endif;
       Enddo;
      /End-Free
     p RunAfExport     e
      *=========================================================================
      * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
      *=========================================================================
     p RtvSmDedInfo    b
     d RtvSmDedInfo    pi                  like(LDamp1)
     d  inDedCode                          like(ELdcde)
     d  inDedEffDate                       like(ELdcef)
     d  kkefdt         s                   like(DDEFDT)

     C     ddky1         Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    inDedCode
     C                   Kfld                    KKEFDT

     C     ddky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    inDedCode

     C                   Move      ELCO#         qqco#
     C                   Move      ELFND         qqfnd
     C                   Move      ELEMP#        q1emp#
     C                   Move      ELDIV         q1div
     C                   Move      ELFYR         q1fyr
     C                   Move      inDedEffDate  q2date

     C                   Call      'SREFFD'
     C                   Parm                    qqco#             3
     C                   Parm                    qqfnd             3
     C                   Parm                    q1fyr             3
     C                   Parm                    q1emp#            9
     C                   Parm                    q1div             5
     C                   Parm                    q2date            7

     C                   Move      q2date        kkefdt
     C     ddky1         Setgt     pdd01
     C     ddky          Readpe    pdd01
     C                   Dow       not(%eof) and DDSTAT <> 'A'
     C     ddky          Readpe    pdd01
     C                   Enddo

     C                   If        not(%eof)
     C                   Return    DDampo
     C                   Else
     C                   Return    *Zero
     C                   Endif

     P RtvSmDedInfo    e
      *** end add ***** 01/05/18 **************************
      *=====================================================
     p WritePayInfo    b
      *=====================================================
     d WritePayInfo    pi
     d  idx                           5  0
     d  pFlgFClass                    1a
     d  pFlgOccDis                    1a
     d  pAuditStatus                  1a

     d pEfDt8          s              8  0

      /Free
       xxAddLoad = 0;
       Setll (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pep02;
       Dou %Eof(WMEPL1);
          Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pep02;
          If %Eof;
             Leave;
          Endif;
          If EPstat = 'I';
             Iter;
          EndIf;
          xxAddLoad += EPadld;
          pEfDt8 = EPefdt + 19000000;
          If afMode = 'Validate';
             Chain (valInfo(idx).pol#:valInfo(idx).rateState:
              pEfDt8:EPclas) ppp01af;
          Else;
             Chain (polInfo(idx).pol#:polInfo(idx).rateState:
              pEfDt8:EPclas) ppp01af;
          Endif;

          If %Found;
             If pAuditStatus = 'A';
                PPWritExAm += EPaapr;
                PPWritPrAm += EPampr;
             Else;
                PPWritExAm += EPeapr;
                PPWritPrAm += EPempr;
             Endif;
             Update ppp01af;
          Else;
             Clear ppp01af;
             PPPolEffDt = pEfDt8;
             If afMode = 'Validate';
                PPCoid  = valInfo(idx).coid;
                PPPol#  = valInfo(idx).pol#;
                PPPolSt = valInfo(idx).rateState;
             Else;
                PPCoid  = polInfo(idx).coid;
                PPPol#  = polInfo(idx).pol#;
                PPPolSt = polInfo(idx).rateState;
             Endif;
             PPClasCod  = EPclas;
             PPWritExAm = EPeapr;
             PPWritPrAm = EPempr;
             If pAuditStatus = 'A';
                PPWritExAm = EPaapr;
                PPWritPrAm = EPampr;
             Else;
                PPWritExAm = EPeapr;
                PPWritPrAm = EPempr;
             Endif;
             Setgt (ELco#:ELrtcd:EPclas:ELpfdt) ppr01;
             Readpe (ELco#:ELrtcd:EPclas) ppr01;
             If Not %Eof;
                PPPerCapFl = PRpcf;
                If EPspec = 'F' or PRlhcd = 'Y';
                   pFlgFClass = 'Y';
                Endif;
                If PRoccd = 'Y';
                   pFlgOccDis = 'Y';
                Endif;
                Write ppp01af;
             Endif;
          Endif;
       Enddo;
      /End-Free

     p WritePayInfo    e
      *=====================================================
     p WritePolInfo    b
      *=====================================================

     d idx             s              5  0
     d pAuditStatus    s              1
     d pEarnPrem       s             11  2
     d pWrtnPrem       s             11  2
     d pEarnTimeFact   s              5  4
      *** beg chg ***** 03/17/22 ******************
     d***pEarnPremFact   s              5  4
     d pEarnPremFact   s              7  4
      *** end chg ***** 03/17/22 ******************
     d pAddress        s             81
     d pCity           s             25
     d pFlgFClass      s              1
     d pFlgOccDis      s              1
     d pState          s              2
     d pZip            s             10
     d pFlgLrgDed      s              1
     d pDedPlan        s             20
     d pDedAggAmt      s                   like(LDamg1)
     d pDedAccAmt      s                   like(LDamp1)
     d pPol#           s                   like(RPcap#)
     d pLiabLimits     s                   like(PXempll)

      /Free
       //*** Begin Chg *** 02/22/22 **************************************
       //*** xxCxDate = GetCxDate();
       xxCxDate = GetCxDate2();
       //*** End   Chg *** 02/22/22 **************************************
       Chain (ELco#:ELfnd:ELfyr) pf100;
       GetFactors(pEarnPrem:pEarnPremFact:pEarnTimeFact:pAuditStatus:
        pWrtnPrem);
       Chain (ELco#:ELrtcd) prs01;
       Setgt (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
       Readpe (ELco#:ELfnd:RSjrst) prr01;
       pPol# = GetPol#();
       pLiabLimits = GetLiabLimits();
       GetDedInfo(pFlgLrgDed:pDedPlan:pDedAggAmt:pDedAccAmt);
       Select;
       When afMode = 'Validate';
          For idx = 1 to max#OfStEfdts;
             If valInfo(idx).rateState  = *Blanks;
                Leave;
             Endif;
          Endfor;
       Other;
          For idx = 1 to max#OfStEfdts;
             If polInfo(idx).pol#     = pPol# and
              polInfo(idx).rateState  = RSjrst and
              polInfo(idx).lrgDedFlg  = pFlgLrgDed and
              polInfo(idx).dedPlan    = pDedPlan and
              polInfo(idx).dedAggAmt  = pDedAggAmt and
              polInfo(idx).perAccAmt  = pDedAccAmt and
              polInfo(idx).liabLimits = pLiabLimits or
              polInfo(idx).rateState  = *Blanks;
                Leave;
             Endif;
          Endfor;
       EndSl;
       Select;
       When afMode = 'Validate';
          valInfo(idx).coid       = F2NAIC;
          valInfo(idx).pol#       = pPol#;
          valInfo(idx).polEfDt    = ELpfdt + 19000000;
          valInfo(idx).rateState  = RSjrst;
          GetAddress(pAddress:pCity:pState:pZip);
          valInfo(idx).address    = pAddress;
          valInfo(idx).city       = pCity;
          valInfo(idx).state      = pState;
          valInfo(idx).zip        = pZip;
          valInfo(idx).lrgDedFlg  = pFlgLrgDed;
          valInfo(idx).dedPlan    = pDedPlan;
          valInfo(idx).dedAggAmt  = pDedAggAmt;
          valInfo(idx).perAccAmt  = pDedAccAmt;
          valInfo(idx).liabLimits = pLiabLimits;
          WritePayInfo(idx:pFlgFClass:pFlgOccDis:pAuditStatus);
          If pFlgFClass = 'Y';
             valInfo(idx).fClassFlg = 'Y';
          Endif;
          If pFlgOccDis = 'Y';
             valInfo(idx).occDisFlg = 'Y';
          Endif;
          valInfo(idx).earnPrem     += pEarnPrem;
          valInfo(idx).earnDeduct   += dsLastEH.EHwdct * pEarnPremFact;
          valInfo(idx).earnMerit    += GetMeritPrem() * pEarnPremFact;
          If dsLastEH.EHwdc < 0;
             valInfo(idx).earnSchCred  += dsLastEH.EHwdc * pEarnPremFact;
          Else;
             valInfo(idx).earnSchDebit += dsLastEH.EHwdc * pEarnPremFact;
          Endif;
          valInfo(idx).earnPrmDisc  += dsLastEH.EHwdc3 * pEarnPremFact;
          valInfo(idx).earnEmpAsst   = 0;
          valInfo(idx).earnTerrAmt  += dsLastEH.EHwter * pEarnPremFact;
          valInfo(idx).earnCatAmt   += dsLastEH.EHwdtr * pEarnPremFact;
          valInfo(idx).earnSafeCrd  += dsLastEH.EHwdcs * pEarnPremFact;
          valInfo(idx).earnRetro    += GetLastRetro();
          valInfo(idx).earnMCO      += dsLastEH.EHwdcm * pEarnPremFact;
          valInfo(idx).earnDividend  = 0;
          valInfo(idx).earnExpConst += dsLastEH.EHprtd * pEarnTimeFact;
          If dsLastEH.EHwmnp = 'Y';
             valInfo(idx).earnMinPrem  += (dsLastEH.EHwnmp + dsLastEH.EHprtd)
              * pEarnTimeFact;
          Endif;
          valInfo(idx).wrtnPrem     = valInfo(idx).earnPrem;
          valInfo(idx).wrtnDeduct   = valInfo(idx).earnDeduct;
          valInfo(idx).wrtnMerit    = valInfo(idx).earnMerit;
          valInfo(idx).wrtnSchCred  = valInfo(idx).earnSchCred;
          valInfo(idx).wrtnSchDebit = valInfo(idx).earnSchDebit;
          valInfo(idx).wrtnPrmDisc  = valInfo(idx).earnPrmDisc;
          valInfo(idx).wrtnEmpAsst  = 0;
          valInfo(idx).wrtnTerrAmt  = valInfo(idx).earnTerrAmt;
          valInfo(idx).wrtnCatAmt   = valInfo(idx).earnCatAmt;
          valInfo(idx).wrtnSafeCrd  = valInfo(idx).earnSafeCrd;
          valInfo(idx).wrtnRetro    = valInfo(idx).earnRetro;
          valInfo(idx).wrtnMCO      = valInfo(idx).earnMCO;
          valInfo(idx).wrtnDividend = 0;
          valInfo(idx).wrtnExpConst = valInfo(idx).earnExpConst;
          If dsLastEH.EHwmnp = 'Y';
             valInfo(idx).wrtnMinPrem  = valInfo(idx).earnMinPrem;
          Endif;
          valInfo(idx).fund         = %Char(ELfnd);
          valInfo(idx).employer     = %Char(ELemp#);
          valInfo(idx).division     = %Char(ELdiv);
          valInfo(idx).year         = %Char(ELfyr);
          valInfo(idx).indemDedRcvd = *Blanks;
          valInfo(idx).medDedRcvd   = *Blanks;
          valInfo(idx).earnEPrmFact = %Editc(pEarnPremFact:'1');
          valInfo(idx).earnETimFact = %Editc(pEarnTimeFact:'1');
          valInfo(idx).xrtnPrem     = pWrtnPrem;
          valInfo(idx).xrtnDeduct   = dsLastEH.EHwdct;
          valInfo(idx).xrtnMerit    = GetMeritPrem();
          If dsLastEH.EHwdc < 0;
             valInfo(idx).xrtnSchCred  = dsLastEH.EHwdc;
          Else;
             valInfo(idx).xrtnSchDebit = dsLastEH.EHwdc;
          Endif;
          valInfo(idx).xrtnPrmDisc  = dsLastEH.EHwdc3;
          valInfo(idx).xrtnEmpAsst  = 0;
          valInfo(idx).xrtnTerrAmt  = dsLastEH.EHwter;
          valInfo(idx).xrtnCatAmt   = dsLastEH.EHwdtr;
          valInfo(idx).xrtnSafeCrd  = dsLastEH.EHwdcs;
          valInfo(idx).xrtnRetro    = valInfo(idx).earnRetro;
          valInfo(idx).xrtnMCO      = dsLastEH.EHwdcm;
          valInfo(idx).xrtnDividend = 0;
          valInfo(idx).xrtnExpConst = dsLastEH.EHprtd;
          If dsLastEH.EHwmnp = 'Y';
             valInfo(idx).xrtnMinPrem  = (dsLastEH.EHwnmp + dsLastEH.EHprtd);
          Endif;
       Other;
          If polInfo(idx).rateState  = *Blanks;
             polInfo(idx).coid       = F2NAIC;
             polInfo(idx).pol#       = pPol#;
             polInfo(idx).polEfDt    = ELpfdt + 19000000;
             polInfo(idx).rateState  = RSjrst;
             GetAddress(pAddress:pCity:pState:pZip);
             polInfo(idx).address    = pAddress;
             polInfo(idx).city       = pCity;
             polInfo(idx).state      = pState;
             polInfo(idx).zip        = pZip;
             polInfo(idx).lrgDedFlg  = pFlgLrgDed;
             polInfo(idx).dedPlan    = pDedPlan;
             polInfo(idx).dedAggAmt  = pDedAggAmt;
             polInfo(idx).perAccAmt  = pDedAccAmt;
             polInfo(idx).liabLimits = pLiabLimits;
          Endif;
          WritePayInfo(idx:pFlgFClass:pFlgOccDis:pAuditStatus);
          If pFlgFClass = 'Y';
             polInfo(idx).fClassFlg = 'Y';
          Endif;
          If pFlgOccDis = 'Y';
             polInfo(idx).occDisFlg = 'Y';
          Endif;
          polInfo(idx).earnPrem     += pEarnPrem;
          polInfo(idx).earnDeduct   += dsLastEH.EHwdct * pEarnPremFact;
          polInfo(idx).earnMerit    += GetMeritPrem() * pEarnPremFact;
          If dsLastEH.EHwdc < 0;
             polInfo(idx).earnSchCred  += dsLastEH.EHwdc * pEarnPremFact;
          Else;
             polInfo(idx).earnSchDebit += dsLastEH.EHwdc * pEarnPremFact;
          Endif;
          polInfo(idx).earnPrmDisc  += dsLastEH.EHwdc3 * pEarnPremFact;
          polInfo(idx).earnEmpAsst   = 0;
          polInfo(idx).earnTerrAmt  += dsLastEH.EHwter * pEarnPremFact;
          polInfo(idx).earnCatAmt   += dsLastEH.EHwdtr * pEarnPremFact;
          polInfo(idx).earnSafeCrd  += dsLastEH.EHwdcs * pEarnPremFact;
          polInfo(idx).earnRetro    += GetLastRetro();
          polInfo(idx).earnMCO      += dsLastEH.EHwdcm * pEarnPremFact;
          polInfo(idx).earnDividend  = 0;
          polInfo(idx).earnExpConst += dsLastEH.EHprtd * pEarnTimeFact;
          If dsLastEH.EHwmnp = 'Y';
             polInfo(idx).earnMinPrem  += (dsLastEH.EHwnmp + dsLastEH.EHprtd)
              * pEarnTimeFact;
          Endif;
          polInfo(idx).wrtnPrem     = polInfo(idx).earnPrem;
          polInfo(idx).wrtnDeduct   = polInfo(idx).earnDeduct;
          polInfo(idx).wrtnMerit    = polInfo(idx).earnMerit;
          polInfo(idx).wrtnSchCred  = polInfo(idx).earnSchCred;
          polInfo(idx).wrtnSchDebit = polInfo(idx).earnSchDebit;
          polInfo(idx).wrtnPrmDisc  = polInfo(idx).earnPrmDisc;
          polInfo(idx).wrtnEmpAsst  = 0;
          polInfo(idx).wrtnTerrAmt  = polInfo(idx).earnTerrAmt;
          polInfo(idx).wrtnCatAmt   = polInfo(idx).earnCatAmt;
          polInfo(idx).wrtnSafeCrd  = polInfo(idx).earnSafeCrd;
          polInfo(idx).wrtnRetro    = polInfo(idx).earnRetro;
          polInfo(idx).wrtnMCO      = polInfo(idx).earnMCO;
          polInfo(idx).wrtnDividend = 0;
          polInfo(idx).wrtnExpConst = polInfo(idx).earnExpConst;
          If dsLastEH.EHwmnp = 'Y';
             polInfo(idx).wrtnMinPrem  = polInfo(idx).earnMinPrem;
          Endif;
          polInfo(idx).indemDedRcvd = *Blanks;
          polInfo(idx).medDedRcvd   = *Blanks;
       EndSl;
      /End-Free

     p WritePolInfo    e
      *=====================================================
     p WriteWXPXPAF    b
      *=====================================================

     d idx             s              5  0

      /Free
       Select;
       When afMode = 'Validate';
          For idx = 1 to max#ofStEfDts;
             If valInfo(idx).rateState = *Blanks;
                Leave;
             Endif;
             dsWXPXPAFV = valInfo(idx);
             Write ppx01afv;
             Feod WXPXPAFV;
          Endfor;
       Other;
          For idx = 1 to max#ofStEfDts;
             If polInfo(idx).rateState = *Blanks;
                Leave;
             Endif;
             dsWXPXPAF = polInfo(idx);
             Write ppx01af;
             Feod WXPXPAF;
          Endfor;
       EndSl;

      /End-Free

     p WriteWXPXPAF    e
      *** end add ***** 04/30/18 **************************
