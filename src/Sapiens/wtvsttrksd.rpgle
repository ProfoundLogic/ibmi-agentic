      *  Revisions:                                                   *
      *
      * 10/17/22 C:291 Support: 103956                                *
      *                Add new fields for Service Request grid        *
      *                threshold. Also moved some definitions from    *
      *                WTVSTTRK, WTCLMVST and WTPOLTRACK as STUSRDRY  *
      *                was getting compilation error.                 *
      *                                                               *
      *  06/24/22 C:300 Project: WC2395
      *                Add data structure vimds8 for PF wmvip.        *
      *
      * 06/21/21 C:298 Project: SY426
      *                Re-Brand and Config for Visit Tracking
      *
      * 06/09/20  C:33  Project: SY420
      *                 Mask SSN
      *
      * 09/26/19 C:291 Project: 129WVN                                *
      *                Add data structure vimds7 for LF wmvil7.       *
      *                Add parameter inVstMode1 to ValidateVisit PR.  *
      *                                                               *
      *  06/18/19 C:300 Support: 80204 (LUBA)
      *                 Policy/Claim links in Visit Tracking menu call should
      *                 use the security available in Application Configuration.
      *                 If user does not have any access then link should not appear.
      *                 If user have access, select the link then they should go to
      *                 policy or claim in inquiry or edit depending on app config.
      *                                                               *
      * 09/09/15 C:242 Phaseware: #37116                              *
      *                1. Claim Detail not populating                 *
      *                2. Policy/Claims link to be Update/Display     *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
     d*dsWMEMP         ds                  LikeRec(pem0p)                       //?
     d dsWMEML         ds                  LikeRec(pem00)
     d dsWMCMP         ds                  LikeRec(pcm0p)
     d dsWMCML         ds                  LikeRec(pcm00)
     d dsVIvl1         ds                  LikeRec(pvi01)
     d dsVIvl2         ds                  LikeRec(pvi02)
     d dsVIvl3         ds                  LikeRec(pvi03)
      *** Beg Add ***** 09/26/19 ********************************
     d dsVIvl7         ds                  LikeRec(pvi07)
      *** End Add ***** 09/26/19 ********************************

     d vimds0          ds
     d  vilRRN               397    400b 0
     d vimds1          ds
     d  vilRRN1              397    400b 0
     d vimds2          ds
     d  vilRRN2              397    400b 0
     d vimds3          ds
     d  vilRRN3              397    400b 0
     d vimds4          ds
     d  vilRRN4              397    400b 0
     d vimds6          ds
     d  vilRRN6              397    400b 0
      *** Beg Add ***** 09/26/19 ********************************
     d vimds7          ds
     d  vilRRN7              397    400b 0
      *** End Add ***** 09/26/19 ********************************
      *** Beg Add ***** 06/24/22 ********************************
     d vimds8          ds
     d  vilRRN8              397    400b 0
      *** End Add ***** 06/24/22 ********************************
     d ELfds           ds
     d  ELrrn                397    400b 0
      *** Begin Add *** 02/10/16 ********************************      **
     d ELfdsQ          ds
     d  ELrrnQ               397    400b 0
      *** End   Add *** 02/10/16 ********************************      **

     d DetailBoxDsA    ds
     d  DTLVSTASGN                   10A
     d  DT2VSTASGN                   40A
     d  DTLVSTCAT                    10A
     d  DTLVSTNOTE                 5000A
     d  DTLVSTTYP                    40A
     d  OUTAGTCNTY                   40A
     d  OUTAGTDBA                    40A
     d  OUTAGTEMAI                   60A
     d  OUTAGTFED#                   15A
     d  OUTAGTFEDN                   40A
     d  OUTAGTGRP                    40A
     d  OUTAGTAREA                   40A
     d  OUTAGTMKTR                   40A
     d  OUTAGTMLG1                   40A
     d  OUTAGTMLG2                   40A
     d  OUTAGTMLG3                   40A
     d  OUTAGTMREP                   40A
     d  OUTAGTPRD                    50A
     d  OUTAGTSREP                   40A
     d  OUTAGTTYP                    40A
     d  OUTAGTWEBP                   40A
     d  OUTAUDMGR                    50A
     d  OUTAUDRVW                    50A
     d  OUTCARPOL#                   25A
     d  OUTCASENUR                   40A
     d  OUTCLMADJ                    40A
     d  OUTCLMCAUS                   50A
     d*  OUTCLMEML                    60A
     d*  OUTCLMEMLT                   60A
     d  OUTCLMMGR                    40A
     d  OUTCLMNAT                    50A
     d  OUTCLMOADJ                   40A
     d  OUTCLMPART                   50A
     d  OUTCLMSSN                    11A
     d  OUTCLMSTS                    10A
     d  OUTCLMSUP                    40A
     d  OUTCLMTAD1                   40A
     d  OUTCLMTAD2                   40A
     d  OUTCLMTAD3                   40A
     d  OUTCLMTNM                    40A
     d  OUTCLMTYP                    20A
     d  OUTCOUNTY                    40A
     d  OUTDBANM                     40A
     d  OUTEMP#                      20A
     d  OUTFEDNM                     40A
     d  OUTJURIST                    20A
     d  OUTLOCENT#                   10A
     d  OUTLOSCTL                    50A
     d  OUTMLGLN1                    40A
     d  OUTMLGLN2                    40A
     d  OUTMLGLN3                    40A
     d  OUTNAICS                     10A
     d  OUTPHYLN1                    40A
     d  OUTPHYLN2                    40A
     d  OUTPHYLN3                    40A
     d  OUTPOLSVC                    50A
     d  OUTPRDEMAL                   60A
     d  OUTPRMACT                    50A
     d  OUTSIC                        6A
     d  OUTSTCLM#                    25A
     d  OUTTYPBUS                    40A
     d  OUTUNDWRT                    50A

      *** constats
     d Email_NA        c                   Const('No E-Mail Available')
     d conReportPgm    c                   Const('WTRPTVSTP')
     D conClaimPgm     c                   const('WTCLMSUM')
     D conPolicyPgm    c                   const('WTPOLSUM')

      *** work variables
     d AgtUndArr       s              3    dim(25)
     d AgtUndIndex     s              3  0
     d index           s              3  0
     d @@RRN           s              9
     d $$RRN           s              9  0
     d @@Mode          s              1
     d $$Mode          s              1
     d @@TabIndx       s              5
     d @@Category      s             10
     d @@Assign        s             10
     d @@VstFrDt       s              8
     d @@VstToDt       s              8
     d @@Pointer       s               *
     d @@isTeam        s              1
     d error           s               n   inz(*off)
     d VisitFilter     s             10    INZ
     d vstClndrCall    s              1
     d arrTeamList     s             10    dim(9999) inz
     d intCount        s              5  0 inz
     d ptrTeamList     s               *   inz

     d Today           s              7  0 inz
     d OutVarX         s              5  0 inz
     d OutVarY         s              5  0 inz
     d DtlBoxYoffset   s              5  0 inz
     d AdL1            s             40    inz
     d AdL2            s             40    inz
     d AdL3            s             40    inz
     d OnLoadRrn       s              5  0
     d @@mad@          s                   like(AGmad1)
     d ##last          s             56
     d ##frst          s             15
     d ##term          s              1
     d ProcRRN         s              9  0
     d ProcRRNC        s              9
     d stkey           s             10
     d pcnKey          s             10
     d isTeam          s               n
     d isEditable      s               n
     d chkVstDte       s                   like(VIvsDt)
     d polTrakDiv      s                   like(VIdiv)
     d wdEmpLib        s             15

     d ssVstTyp        s                   like(dtlVstTyp)
     d svVstCat        s                   like(ssVstCat)
     d tpVstCat        s                   like(ssVstCat)
     d svVstTyp        s                   like(ssVstTyp)
     d svVstTpCat      s                   like(ssVstTpCat)
     d tpVstTpCat      s                   like(ssVstTpCat)
     d svVstAsgn       s                   like(ssVstAsgn)
     d svVstSts        s                   like(ssVstSts)
     d svVstFrDt       s                   like(ssVstFrDt)
     d svVstToDt       s                   like(ssVstToDt)
     d svCmpFrDt       s                   like(ssCmpFrDt)
     d svCmpToDt       s                   like(ssCmpToDt)
     d svGroup         s                   like(ssGroup)
     d svEmpDivNm      s                   like(ssEmpDivNm)
     d svEmpDiv        s                   like(ssEmpDiv)
     d svLocation      s                   like(ssLocation)
     d svClmLnm        s                   like(ssClmLnm)
     d svClmFnm        s                   like(ssClmFnm)
     d svClaim#        s                   like(ssClaim#)
     d svAgtName       s                   like(ssAgtName)
     d svAgtFein       s                   like(ssAgtFein)
     d svQteFlag       s                   like(ssQteFlag)
     d LiveFlag        s              1
     d empUsrOpn       s               n   Inz(*Off)
      ***** Beg Add ***** 06/21/21 ******************************
     d strVstSts       s              5    inz(*blanks)
     d varVstSts       s              5    inz(*blanks)
     d aryVstSts       s             25    DIM(9999)
     d Indx            s              4  0 inz
     d flgVstSts       s               n   Inz(*Off)
      ***** End Add ***** 06/21/21 ******************************
      ***** Beg Add ***** 10/17/22 ****************************
     D isFilter        s              1n   inz('0')
     D isSubset9999    s              1n   inz('0')
      *** Following definitions moved to this source member and
      *** commented out in WTVSTTRK, WTPOLTRACK and WTCLMVST
     D Callpgm         s              1
     D Callpgm2        s              1
     D saveDT          s                   like(VIdt)
     D saveTM          s                   like(VItime)
     D flgRcdUpdated   s              1    inz
      ***** End Add ***** 10/17/22 ****************************

     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')

      ********* Begin Add *** 06/18/19 **********************************
     D isAuthEr        s               n
     D isPolUpd        s               n
     D isQteUpd        s               n
     D isClmUpd        s               n
     D isPolInq        s               n
     D isQteInq        s               n
     D isClmInq        s               n
     D inUTYP          s              1
     D inULVL          s              3
     D inCTP1          s              1
     D inCLV1          s              3
     D inEXEC          s              1
     D outRes          s              1
     D inCTP2          s              1
     D inCTP3          s              1
     D inCTP4          s              1
     D inCTP5          s              1
     D inCTP6          s              1
     D inCLV2          s              3
     D inCLV3          s              3
     D inCLV4          s              3
     D inCLV5          s              3
     D inCLV6          s              3
     D inCTP7          s              1
     D inCTP8          s              1
     D inCTP9          s              1
     D inCTP0          s              1
     D inCLV7          s              3
     D inCLV8          s              3
     D inCLV9          s              3
     D inCLV0          s              3
     D keySDACL        s             10
      ********* End   Add *** 06/18/19 **********************************

      *** Proto-types

      * Add Visit Detail record
     d AddVisitDtl     pr

      * Load Agency Detail box with the Visit Detail
     d AgencyDetail    pr
     d  inCat                         1    const
     d  inCo#                         3  0 const
     d  inFein                        9  0 const
     d  inMod                         3  0 const

     d* Agent Search
     d AgtSearch       pr                  ExtPgm('W0040R')
     D  @@Kc                          1
     D  @@Kl                          1
     D  @@Call                        1
     D  @@Rrn                         9  0
     D  @@View                        1  0
     D  @@Qte                         1
     D  @@Mode                        1
     D  @@Name                       32

      * Load Claims Detail box with the Visit Detail
     d ClaimsDetail    pr
     d  inCat                         1    const
     d  inCo#                         3  0 const
     d  inGroup                       3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
     d  inFYr                         3  0 const
     d  inCase                        7  0 const

      * Clear Errors
     d ClearErrors     pr

      * Clear Visit Detail
     d ClearVisitDtl   pr

      * Clear Visit Typer Filter
     d ClrVisitFilter  pr

      * Clear Subfiles
     d ClrVstSubFiles  pr

      * Delete/Undelete Visit Detail record
     d DeleteUndelete  pr
     d inVstRRN                       9  0 const

     d* Delete Employer Overrides and Close files
     d DelOverrideEmp  pr
     d  quoteType                     1

      * Edit Visit Detail record
     d EditVisitDtl    pr
     d inVstRRN                       9  0 const

     d* Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     D  @@mode                        1
     D  @@call                        1
     D  @@rrn                         9
     D  @@criteria                         likeds(dsSrchCrit)

      * Initialize Setup
     d InzSetup        pr

      * Initialize Subset
     d InzSubset       pr

      * Load Visit Detail and respective detail box
     d LoadVisitDtl    pr
     d  inVstRrn                      9  0 const

      * Load Visit Tracking tab
     d LoadVisitGrid   pr
     d teamPtrIn                       *   Options(*Nopass)
     d teamCntIn                      5  0 Options(*Nopass)

     d* Setup Employer Overrides and Open files
     d OverrideEmp     pr
     d  quoteType                     1

      * Plot Visit Entry Add Header
     d PlotVisitAdd    pr

      * Plot Visit Entry Detail
     d PlotVisitDtl    pr
     d  inMode                       10    const options(*nopass)

      * Plot Visit Entry Header
     d PlotVisitHdr    pr
     d  inMode                       10    const options(*nopass)

      * Load Policy Detail box with the Visit Detail
     d PolicyDetail    pr
     d  inCat                         1    const
     d  inCo#                         3  0 const
     d  inGroup                       3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
     d  inLoc#                       10    const
     d  inFYr                         3  0 const
     d  inQteFlag                     1    const

      * Process Subfile to check Row-Click, Edit, Delete/Undelete
     d ProcessSFL      pr

     d* Retrieve Libraires
     d RtvClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      * Save Visit Entry
     d SaveVisit       pr
     d  inVstRrn                      9  0 const
     d  inVstMode                    10    const

     d* Agent Search
     d SearchAgt#      pr

     d* Claim Search
     d SearchClm#      pr

     d* Employer Search
     d SearchEmp#      pr

     d* Sets Conditions
     d SetConditions   pr

      * Set Visit Typer Filter
     d SetVisitFilter  pr

     d SrName          pr                  extpgm('SRNAME')
     d  ##last                       56
     d  ##frst                       15
     d  ##term                        1

      ********* Begin Add *** 06/18/19 **********************************
     DCheckSecurity    pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTP1                         1    const
     D inCLV1                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D inCTP2                         1    const
     D inCTP3                         1    const
     D inCTP4                         1    const
     D inCTP5                         1    const
     D inCTP6                         1    const
     D inCLV2                         3    const
     D inCLV3                         3    const
     D inCLV4                         3    const
     D inCLV5                         3    const
     D inCLV6                         3    const
     D inCTP7                         1    const
     D inCTP8                         1    const
     D inCTP9                         1    const
     D inCTP0                         1    const
     D inCLV7                         3    const
     D inCLV8                         3    const
     D inCLV9                         3    const
     D inCLV0                         3    const
      ********* End   Add *** 06/18/19 **********************************

     d* ValidateFilter
     d ValidateFilter  pr              n

     d* Visit Subset
     d VisitSubset     pr              n

      * Validate Visit Entry
     d ValidateVisit   pr
      *** Beg Add ***** 09/26/19 *************************************
     d  inVstMode1                   10    const
      *** End Add ***** 09/26/19 *************************************

      ********* Begin Add *** 09/09/15 **********************************
     DGetEmpAccess     pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DGetGrpAccess     pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const

     DInqUpdClm        pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const
     D inCase                         7  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DInqUpdEmp        pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
      ********* End   Add *** 09/09/15 **********************************
      ***** Beg Add ***** 10/17/22 ****************************
      *** Moved to this source member and commented out
      *** in WTVSTTRK, WTPOLTRACK and WTCLMVST
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1
      ***** End Add ***** 10/17/22 ****************************

       //***  Begin Add  ***  06/09/20  **********************
      /include sPrSecure
      /include sPrMask
       //***  End   Add  ***  06/09/20  **********************
