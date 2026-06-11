     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *=================================================================================
     H* ADD NEW TABLES TO GENERIC TABLE FILE.
     H**
     H* CREATED: 08/05/values. 191 Project: SY255 Priss# 164
     H**
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H* 02/14/22 C:79  Project: WC2797                                *
     H*                EDI: IAIABC 3.1.4 Table Values                 *
     H*                                                               *
     H* 07/20/20 C:79  Project: WC2749                                *
     H*                Add new E-Pay values                           *
     H*                                                               *
     H* 04/03/20 C:62  Project: WC2730                                *
     H*                Add new table values                           *
     H*                                                               *
     H* 08/19/19 C:79  Project: WC2604                                *
     H*                Add IDC Tables                                 *
     H*                                                               *
     H* 06/17/19 C:79  Project: WC2678                                *
     H*                Add Petition types:                            *
     H*                                                               *
     H* 04/04/19 C:33  Project: WC2610                                *
     H*                Add question types:                            *
     H*                 - 'Schedule Rating'                           *
     H*                 - 'Policy'                                    *
     H*                                                               *
     H* 03/29/19 C:79  Support: 99/76719                              *
     H*                Add previous values back in that were removed  *
     H*                in V18R3 and V19R0                             *
     H*                                                               *
     H* 02/13/19 C:79  Support: 99/76719                              *
     H*                Agreement to Compensate Code Values            *
     H*                                                               *
     H* 10/16/18 C:65  Project: WC2640                                *
     H*                Add table for policy page payroll printing     *
     H*                override by state.                             *
     H*                                                               *
     H* 09/05/18 C:33  Project: SY390                                 *
     H*                Add 'Question Type' table for portal           *
     H*                                                               *
     H* 05/30/18 C:079 Project WC2515                                 *
     H*                Update Load generic table program to update    *
     H*                for claims EDI 3.1                             *
     H*                                                               *
     H* 01/31/17 C:306 Project WC2410                                 *
     H*                Update Load generic table program to update    *
     H*                Generic Table file with 'CLTY','PADJ' & 'BADJ' *
     H*                                                               *
     H* 02/07/14 C:279 Project: SY336  Priss: 52                      *
     H*                Modified to load generic table file correctly. *
     H*                                                               *
     H* 10/14/13 C:191 PROJECT WC2192                                 *
     H*                Update Load generic table program to update    *
     H*                Generic Table file with 'COVL' values.         *
     H*                                                               *
     H* 04/04/13 C:191 Redmine: 2327                                  *
     H*                Remove entry clerk from staff tables           *
     H*                                                               *
     H* 11/04/12 C:191 Project: Transform                             *
     H*                Add table id for Claim and policy staff fields *
     H*                                                               *
     H* 06/21/12 C:282 Project: WC2086                                *
     H*                Reinstatement Reason Code added in system      *
     H*                table value.                                   *
     H*                                                               *
     H* 02/20/12 C:190 Project: WC2126                                *
     H*                Check the new field for determining what to    *
     H*                set the cancellation ID code to.               *
     H*                                                               *
     H*****************************************************************
     Fsmcop     if   e             disk
     Fwtgil     uf a e           k disk
     Fwtgtl     uf a e           k disk
     F*
     D*
     D kkco#           S                   like(gico#)
     D kktbid          S                   like(gitbid)
     D kkkey           S                   like(gtkey)
     D i               S              3  0 inz
     D x               S              3  0 inz
     D*
     D tbid            s              4    dim(cntId) ctdata perrcd(1)
     D desc            s             40    dim(cntId) alt(tbid)
     D Vtbid           s              4    dim(cntVal) ctdata perrcd(1)
     D VComp           s             50    dim(cntVal) alt(Vtbid)
     D*
     D xxComp          ds
     D  VKey                   1     10
     D  VDesc                 11     50
     D*
     D                 ds
     D  dsdttm                 1     12  0
     D  dstime                 1      6  0
     D  dsdate                 7     12  0
     D*
     D psds          esds                  extname(SZQ1P)
     D*
     D*** Beg Chg *** 07/20/20 ****************************************
     D*** cntID           c                   37
     D*** cntVal          c                   262
     D*** Beg Chg *** 02/14/22 ****************************************
     D*** cntID           c                   38
     D*** cntVal          c                   266
     D cntID           c                   40
     D cntVal          c                   272
     D*** End Chg *** 02/14/22 ****************************************
     D*** End Chg *** 07/20/20 ****************************************
     D*
     C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C     giky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
     C* Read thru company file
     C                   read      pco01
     C                   dow       not %eof
     C                   eval      kkco# = coco#
     C
     C* Read thru compile time data area
     C                   for       i=1 to cntId
     C                   if        tbid(i) = *blank
     C                   iter
     C                   endif
     C                   eval      kktbid = tbid(i)
     C     giky          chain     pgi01
     C                   if        not %found
     C* Table id not found add new table id
     C                   exsr      srWrtTbId
     C                   endif
      /free
       // Create table values
       Exsr srWrtTbVal;
      /end-free
     C                   endfor
     C* Read next CO#
     C                   read      pco01
     C                   enddo
     C                   eval      *inlr = *on
     C*
     C*****************************************************
     C* srWrtTbID - Write new table id to file
     C*****************************************************
     C     srWrtTbId     begsr
     C                   clear                   pgi01
     C                   eval      gico# = kkco#
     C                   eval      gitbid = kktbid
     C                   eval      gidesc = desc(i)
     C                   eval      gifn01 = 'Value'
     C                   eval      gistat = 'A'
     C                   eval      giuser = q1user
     C                   eval      gipgm = q1pgm
     C                   time                    dsdttm
     C                   movel     dstime        gitime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         gidt
     C                   write     pgi01
     C*
     C                   endsr
     C*****************************************************
     C* srWrtTbVal - Write new table values
     C*****************************************************
     C     srWrtTbVal    begsr
     C*
     C                   for       x = 1 to cntVal
     C                   if        Vtbid(x) = *blank
     C                   iter
     C                   endif
     C                   if        Vtbid(x) = tbid(i)
     C                   eval      xxComp = VComp(x)
     C                   eval      kkkey = VKey
     C     gtky          chain     pgt01
     C                   if        not %found
     C                   clear                   pgt01
     C                   eval      gtco# = kkco#
     C                   eval      gttbid = kktbid
     C                   eval      gtkey = kkkey
     C                   eval      gtdesc = VDesc
     C                   eval      gtstat = 'A'
     C*
     C                   eval      gtuser = q1user
     C                   eval      gtpgm = q1pgm
     C                   time                    dsdttm
     C                   movel     dstime        gttime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         gtdt
     C                   write     pgt01
     C*
     C                   endif                                                  not %found
     C                   endif                                                  Vtbid = tbid
     C                   endfor
     C*
     C                   endsr
     C*
** Table Id
DAREBRING BACK DELETED A/R ON REINST                                             1
TERAINCL TERRORISM CHARGE IN ASSESSMENT CALC                                     2
CSUFCLAIMANT LAST NAME SUFFIXES                                                  3
CXIDCANCEL ID CODE OVERRIDE                                                      4         02/20/12
CLSFCLAIM FILE STAFF FIELDS                                                      5
EMSFEMPLOYER FILE STAFF FIELDS                                                   6
RECDREASON FOR REINSTATEMENT CODE                                                7         06/21/12
COVLCERTIFICATION OF INSURANCE OVERLAY                                           8         10/14/13
CLTY                                                                             9         01/31/17
BADJ                                                                            10         01/31/17
PADJ                                                                            11         01/31/17
CRCDCancel Reason Code                                                           2
SUCFSuspension Reason Code - Full                                                2
SUCPSuspension Reason Code - Partial                                             2
PFTLPart of Body Injured Fingers/Toes Location                                   2
PLOCPart of Body Injured Location Code                                           2
SMTC                                                                             2
DPRL                                                                             2
RBAC                                                                             2
PART                                                                             2
QST Question Type                                                                2
PPPPPolicy Page Payroll Printing Override                                        2
AGCPAgreement to Compensate Code                                                 2
AGCP                                                                             2
PETIPETITION                                                                     8         10/14/13
IJSTIDC Jurisdiction States                                                      8         10/14/13
ASLCAccident State Code (Foreign Locations)                                      8         10/14/13
TXMSTexas Method of Settlement
NMMSNew Mexico Method of Settlement
MEMSMaine Method of Settlement
TBDETemporary Disability Benefit Extinguishment Code
IDCBBenefit Type Code
IBOCBenefit Offset Code
  ACAct - Loss Condition Code
TXACTexas Act - Loss Condition Code
EIDQEmployee ID Type
EAPCAccident Premises Code
EPPTE-PAY VENDOR PAYMENT TYPE
EBCREDI DN0439 Benefit Change Reason Code
BADJ
** Value
DAREA         Current day + original/revised interim                             1
DAREB         Current day                                                        2
DAREC         Next available inst (current + int day)                            3
TERAB         Incl foreign & domestic terr in assessmt                           4
TERAD         Incl domestic terr charge in assessment                            5
TERAF         Incl foreign terr charge in assessment                             6
CSUFII        Second                                                             7
CSUFIII       Third                                                              8
CSUFJR        Junior                                                             9
CSUFSR        Senior                                                             10
CXID1         Cancellation                                                       11        02/20/12
CXID3         Non-renewal                                                        12        02/20/12
CLSFCMAMAN    Account Manager                                                    13
CLSFCMCLEX    Examiner                                                           14
CLSFCMESUP    Excess Adjuster                                                    15
CLSFCMSDFA    SDF Adjuster                                                       16
CLSFCMSUPR    Supervisor                                                         17
CLSFCMUMAN    Unit Manager                                                       18
EMSFEMAMGR    Audit Manager                                                      19
EMSFEMARVW    Audit Reviewer                                                     20
EMSFEMCLMC    Claim Contact                                                      21
EMSFEMLREP    Loss Control Representative                                        22
EMSFEMPACC    Premium Accounts                                                   23
EMSFEMPSRV    Services                                                           24
EMSFEMUWTR    Underwriter                                                        25
RECD01        Regular                                                            26        06/21/12
RECD02        Carrier Error on Last Cancellation                                 27        06/21/12
RECD03        Chg in Cx Eff Dt or Reason for Cx Code                             28        06/21/12
RECD04        Carrier Consideration-Late Re by Carrier                           29        06/21/12
COVLCERT01    Base Overlay                                                       30        10/14/13
COVLCERT0909  Certificate Of Insurance-09/2009                                   31        10/14/13
COVLCERT1005  Certificate Of Insurance-05/2010                                   32        10/14/13
CLTYW         Lost Time With No Paid Indemnity.                                  33        01/31/17
CLTYP         Indemnity w/NLT Beyond Waiting Period.                             34        01/31/17
BADJ3         Post Injury Wage Earning Capacity.                                 35        01/31/17
PADJ220       Employer Paid Permanent Total Benefits.                            36        01/31/17
PADJ221       Employer Paid Permanent Ttl Splmt Benfts.                          37        01/31/17
CLTYB         Became Medical Only                                                33        01/31/17
CLTYI         Indemnity                                                          33        01/31/17
CLTYL         Became Lost Time                                                   33        01/31/17
CLTYM         Medical Only                                                       33        01/31/17
CLTYN         Notification Only                                                  33        01/31/17
CRCDD         Duplicate/Combined Claim                                           1
CRCDJ         Jurisdiction Wrong/Changed                                         1
CRCDN         Not Required by Jurisdiction                                       1
CRCDR         Disputed Request by Jurisdiction                                   1
SUCFS1        RTW or Medically Determined                                        1
SUCFS2        Medical Non-Compliance                                             1
SUCFS3        Administrative Non-Compliance                                      1
SUCFS4        Claimant Death                                                     1
SUCFS5        Incarceration                                                      1
SUCFS6        Claimant Whereabouts Unknown                                       1
SUCFS7        Benefits Exhausted                                                 1
SUCFS8        Jurisdiction Change                                                1
SUCFS9        Pending Settlement Approval                                        1
SUCFSD        Directed by Jurisdiction                                           1
SUCFSJ        Pending Appeal or Judicial Review                                  1
SUCPP1        RTW or Medically Determined                                        1
SUCPP2        Medical Non-Compliance                                             1
SUCPP3        Administrative Non-Compliance                                      1
SUCPP4        Employee Death                                                     1
SUCPP5        Incarceration                                                      1
SUCPP7        Benefits Exhausted                                                 1
SUCPP9        Pending Settlement Approval                                        1
SUCPPJ        Pending Appeal or Judicial Review                                  1
PFTL1         Index Finger or 1st Toe                                            1
PFTL2         Middle Finger or 2nd Toe                                           1
PFTL3         Ring Finger or 3rd Toe                                             1
PFTL4         Little Finger or 4th (little) Toe                                  1
PLOCB         Bilateral                                                          1
PLOCL         Left                                                               1
PLOCR         Right                                                              1
SMTCSU        Sync Up                                                            1
SMTCAC        Acquisition/Indemnity Ceased                                       1
SMTCNT        Narrative                                                          1
SMTCSX        Suspension Reason Code Full                                        1
SMTCPX        Suspension Reason Code Partial                                     1
DPRLA         10                                                                 1
DPRLB         11                                                                 1
DPRLC         12                                                                 1
DPRLD         13                                                                 1
DPRLE         14                                                                 1
DPRLF         15                                                                 1
DPRLG         16                                                                 1
DPRLH         17                                                                 1
DPRLI         18                                                                 1
DPRLJ         19                                                                 1
DPRLK         20                                                                 1
RBACN         No Money Settlement                                                1
RBACD         Decrease in Indemnity                                              1
PART13A       Total deafness of both ears                                        1
PART13B       Total deafness of one ear                                          1
PART13C       Deaf in 1 ear already, D/A caused total                            1
PART14A       Loss of eye by enucleation                                         1
PART14B       Total blindness of one eye                                         1
PART14C       Blindness in both eyes                                             1
PART36A       Loss of index finger/metacarpal bone                               1
PART36B       Loss of index finger at proximal joint                             1
PART36C       Loss of index finger at second joint                               1
PART36D       Loss of index finger at distal joint                               1
PART36E       Loss of second finger/metacarpal bone                              1
PART36F       Loss of middle finger at proximal joint                            1
PART36G       Loss of middle finger at second joint                              1
PART36H       Loss of middle finger at distal joint                              1
PART36I       Loss of third/ring finger at metacarpal                            1
PART36J       Loss of ring finger at proximal joint                              1
PART36K       Loss of ring finger at second joint                                1
PART36L       Loss of ring finger at distal joint                                1
PART36M       Loss of little finger & metacarpal bone                            1
PART36N       Loss of little finger at proximal joint                            1
PART36O       Loss of little finger at second joint                              1
PART36P       Loss of little finger at distal joint                              1
PART37A       Loss of thumb & metacarpal bone                                    1
PART37B       Loss of thumb at proximal joint                                    1
PART37C       Loss of thumb at second distal joint                               1
PART57A       Little toe metatarsal bone                                         1
PART57B       Little toe at distal joint                                         1
PART57C       Loss of other toe/metatarsal bone                                  1
PART57D       Loss of any other toe at proximal joint                            1
PART57E       Other toe at middle joint                                          1
PART57F       Loss any other toe second distal joint                             1
PART57G       Other toe at distal joint                                          1
PART58A       Loss of great toe metatarsal bone                                  1
PART58B       Loss of great toe at proximal joint                                1
PART58C       Loss of great toe at second distal joint                           1
QST ACORD     ACORD
QST EXPOSURE  Exposure Code
QST SCHD_RATE Schedule Rating
QST POLICY    Policy
PPPPDE        D
PPPPNJ        D
AGCPS         Accepting Liability for Medical                                    1
AGCPT         Without Liability for Medical                                      1
AGCPU         Without Liability Indemnity and Medical                            1
PETIN         Petition has not been filed.                                       1
PETIP         Petition on file.                                                  1
PETIY         Petition has been filed.                                           1
IJSTAL        Alabama
IJSTAK        Alaska
IJSTAZ        Arizona
IJSTAR        Arkansas
IJSTCA        California
IJSTCO        Colorado
IJSTCT        Connecticut
IJSTDE        Delaware
IJSTDC        District of Columbia
IJSTFL        Florida
IJSTFT        Foreign Territories
IJSTGA        Georgia
IJSTHI        Hawaii
IJSTID        Idaho
IJSTIL        Illinois
IJSTIN        Indiana
IJSTIA        Iowa
IJSTKS        Kansas
IJSTKY        Kentucky
IJSTLA        Louisiana
IJSTME        Maine
IJSTMD        Maryland
IJSTMA        Massachusetts
IJSTMI        Michigan
IJSTMN        Minnesota
IJSTMS        Mississippi
IJSTMO        Missouri
IJSTMT        Montana
IJSTNE        Nebraska
IJSTNV        Nevada
IJSTNH        New Hampshire
IJSTNJ        New Jersey
IJSTNM        New Mexico
IJSTNY        New York
IJSTNC        North Carolina
IJSTND        North Dakota
IJSTOH        Ohio
IJSTOK        Oklahoma
IJSTOR        Oregon
IJSTPA        Pennsylvania
IJSTPR        Puerto Rico
IJSTRI        Rhode Island
IJSTSC        South Carolina
IJSTSD        South Dakota
IJSTTN        Tennesee
IJSTTX        Texas
IJSTUT        Utah
IJSTVT        Vermont
IJSTVA        Virginia
IJSTWA        Washington
IJSTWV        West Virginia
IJSTWI        Wisconsin
IJSTWY        Wyoming
ASLC62        British Columbia
ASLC55        Canadian Provinces
ASLC56        Canada Zone
ASLC64        New Brunswick
ASLC67        Ontario
ASLC57        Phillippine Islands
ASLC66        Prince Edward Island
ASLC68        Quebec
ASLC58        Puerto Rico
ASLC69        Saskatchewan
TXMS00        Claim Not Subject to Settlement
TXMS05        Dismissal of Take Nothing
TXMS08        Exemplary Damages
NMMS00        Claim Not Subject to Settlement
NMMS03        Stipulated Awards
NMMS04        Findings and Award
NMMS05        Dismissal of Take Nothing
NMMS06        Compromise Settlement
NMMS07        No Safety Devices
NMMS09        All Other Settlements
MEMS00        Claim Not Subject to Settlement
MEMS03        Stipulated Award
MEMS04        Findings and Award
MEMS05        Dismissal of Take Nothing
MEMS06        Compromise Settlement
MEMS09        All Other Settlements
MEMS10        Aggravation of Prior Work Injuries
TBDE1         Return to Work
TBDE2         Release to Work
TBDE3         Maximum Medical Improvement
TBDE4         Maximum Statutory Duration
TBDE5         Medical Non-Compliance
TBDE6         Other
IDCB01        Death Benefits
IDCB02        Permanent Total Disability Benefits
IDCB03        Scheduled Permanent Partial Disability Benefits
IDCB04        Unscheduled Permanent Partial Disability Benefits
IDCB05        Temporary Total Disability Benefits
IDCB09        Disfigurement Benefits
IDCB11        Temporary Partial Disability Benefits
IDCB12        Employers Liability
IDCB15        Supplemental Benefits
IDCB20        Claimant Legal Amount Paid
IDCB30        Indemnity Recovery Reimbursement Amount 3rd Party Actions
IDCB31        Indemnity Recovery Reimbursement Amount State Admin Funds
IDCB48        Penalties, Assessments, Interest
IDCB49        Indemnity and Medical Combined
IDCB50        Other Specified Indemnity Benefits
IDCB60        Vocational Rehabilitation Evaluation Costs
IDCB61        Vocational Rehabilitation Education Costs
IDCB62        Vocational Rehabilitation Maintenance Costs
IDCB63        Vocational Rehabilitation Payment NOC
IDCB79        Lump Sum Including Multiple Indemnity
IDCB99        Other Indemnity Benefits Not Otherwise Specified
IBOC1         None
IBOC2         SSDI
IBOC3         Other
  AC01        State Act or Federal Act excluding USL&HW and Federal
  AC02        USL&HW F-Classes and USL&HW coverage on Non F-Classes
  AC03        Federal Mine Safety and Health Act Only
  AC04        Federal Mine Safety and Health Act and the State Act
TXAC01        State Act or Federal Act excluding USL&HW and Federal
TXAC02        USL&HW F-Classes and USL&HW coverage on Non F-Classes
TXAC03        Federal Mine Safety and Health Act Only
TXAC04        Federal Mine Safety and Health Act and the State Act
TXAC05        Oil, Gas, and Other Mineral Operations On or Over Water
TXAC08        USL&HW Act for Oil, Gas, or Other Mineral Operations On
EIDQT         Employee Individual Taxpayer ID
EAPCE         Employer
EAPCL         Lessee
EAPCX         Other
EAPCR         Employee Residence
EPPTC         Claims Indemnity
EPPTE         Claims Expense
EPPTL         Claims Legal
EPPTM         Claims Medical
EBCRA         Independent Medical Exam (IME) or Claim
EBCRB         Employee Treating Physician Medical Report
EBCRC         Recalculation of Net Weekly Amount Based on Wage Statement
EBCRD         Jurisdiction Directed
EBCRE         Stipulated or negotiated Net Weekly Amount (not juridiction directed)
BADJ4         Withheld Attorney Fees
