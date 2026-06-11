     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h/TITLE WTCLMSUM - Profound Claim Summary
     h DFTACTGRP(*no) ACTGRP('QILE')
     h option(*nodebugio: *srcstmt)
     h/DEFINE   PROFOUNDUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Claim Summary Info.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 2/6/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     H*  03/25/22 C:333 Project: SY434                                     *
     H*                 - Update Grids for Sorting Filtering Upgrade       *
     h*                                                                    *
     H*  06/23/21 C:318 Project WC2302 - Auto tab closure for              *
     H*                deletion.                                           *
     h*                                                                    *
     h*  01/15/21  C:318 Support: 93950                                    *
     h*                  Done initializtion to correct clam indicator      *
     h*                  changing position                                 *
     h*                                                                    *
     h*  06/09/20  C:298 Project: SY420                                    *
     h*                  Mask SSN                                          *
     h*                                                                    *
     h*  03/02/20 C:300 Support: 82990 (LCTA)                              *
     h*                 Performance issues with claims screen              *
     h*                 1) Changed this program from RPGLE to SQLRPGLE     *
     h*                 2) Changes in SCOPYCLMPR copybook                  *
     h*                 3) Also included WTBNDDIR in H specs               *
     h*                                                                    *
     h*  07/19/16 C:308 Project: 114WXY                                    *
     h*                 Add ISO Score fields loading from WMCMP file       *
     h*                                                                    *
     h*  03/23/16 C:308 Project: Elevate                                   *
     h*                 Genericize the toolbar programming               *
     h*                                                                    *
     h*  02/22/16 C:300 Support: 29764                                     *
     h*                 Correct Paid by Benefit Summary garph              *
     h*                 to show only top 5 benefits by amount              *
     h*                                                                    *
     h*  01/14/16 C:300 Support: 37399                                     *
     h*                 Show comma (thosand seperator) in amount field of  *
     h*                 Payee Summary Grid.                                *
     h*                                                                    *
     h*  01/27/16 C:062 Support: 39185(129)                                *
     h*                 Correct total incurred (net and gross)             *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
     h*                                                                    *
     h*  10/14/15 C:300 Support: 37399                                     *
     h*                 Update UI by updating dollar fields with commas    *
     h*                 on Reinsurance Retention on Claim Summary Screen.  *
     h*                                                                    *
     h*  09/22/15 C:267 Support: 37399                                     *
     h*                 Update UI by updating dollar fields with commas    *
     h*                 on the claim financial summary screen.             *
     h*                 I also removed the edit mask for these dollar fields
     h*                                                                    *
     h*  09/04/15 C:308 Support: 37019                                     *
     h*                 Correct Refresh and enter press logic on Claim     *
     h*                 Summary screen  lost in restructuring.             *
     h*                                                                    *
     h*  03/19/15_A C:298 Support: 32825 (Phaseware)
     h*                 Restructured program logic to not exit on enter.
     h*
     h*  04/16/15 C:65  Project: SY251                                     *
     h*                 Add API call to launch new Department letters.     *
     h*                                                                    *
     h*  03/19/15 C:279 Support: 32825 (Phaseware)
     h*                 Restructured program logic to not exit on enter.
     h*
     h*  09/23/14 C:279 Support: WC099 86678                               *
     h*                 Modified to not load zero amount records in        *
     h*                 Payee summary and Benefit Summary grids.           *
     h*                                                                    *
     h*  08/25/14 C:279 Project: 114WOP                                    *
     h*                 Added hover help on state claim# field.            *
     h*                                                                    *
     h*  08/04/14 C:300 Project: WC2247                                    *
     h*                 Claims Financial Summary Changes                   *
     h*                 1) Change (Paid to Date) to (Gross Paid to Date)   *
     h*                 1) Add new column (Net Paid to Date)               *
     h*                                                                    *
     h*  05/05/14 C:280 Support 85550                                      *
     h*                 change managed care files to user open         ro  *
     h*                                                                    *
     h*  03/12/14 C:283 Support WC099 83373                                *
     h*                 Correctly populate the Plantiff Email address.     *
     h*                                                                    *
     h*  03/10/14 C:282 Project: 129WSC Priss 30                           *
     h*                 If Heading for Legal Reserve set as blank then     *
     h*                 labels should use the base "Legal" label.          *
     h*                                                                    *
     h*  02/21/14 C:65  Redmine 3962                                       *
     h*                 Prevent program from blowing up if it hits a zero  *
     h*                 claim status history effective date.               *
     h*                                                                    *
     h*  02/12/14 C:282 Project: 114WMC                                    *
     h*                 Added the new field "Duty Status" and will only    *
     h*                 be displayed for client 114.                       *
     h*                                                                    *
     h*  01/29/14 C:282 Project: 129WSC Priss 15                           *
     h*                 'Recovery' alert will only be displayed for 129    *
     h*                 client only.                                       *
     h*                                                                    *
     h*  01/09/14 C:291 Redmine: 3760   (retrofit by HMK)                  *
     h*                 Claims Benefit Summary grid should not load        *
     h*                 duplicate records from 2 different companies.      *
     h*                                                                    *
     h*  11/25/13 C:282 Project: 129WSC                                    *
     h*                 To display "Recovery" in red at the top of the scr.*
     h*                 if any of the fields 14-6, Misc, Restitution,      *
     h*                 Loss Transfer, Excess in Claims Detail/Management  *
     h*                 tab/Claim Indicator are checked for clitid '129'.  *
     h*                                                                    *
     h*  11/11/13 C:189 Redmine 3342                                       *
     h*                 The value of 'Rinsurance Reportable' not populating*
     h*                                                                    *
     h*  09/12/13 C:279 Support: 83340 WC099                               *
     h*                 Coorected Paid-to-date and Recovery fields links   *
     h*                 functionality.                                     *
     h*                                                                    *
    *  08/20/13 C:267 Support: Redmine #2570 (Haifa Kealy)               *
    *                 Financial Summary links should open in same tab    *
    *                 (ie..paid-to-date and Recovery fields)             *
     h*                                                                    *
     h**********************************************************************
     fwtClmSum  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('WTCLMSUM')
     f                                     Sfile(sflEdtNote:rrnNotes)
     f                                     Sfile(sflReserve:rrnReserve)
     **** Beg Add *** 01/14/16 ************************************************
     f                                     Sfile(sflPayees:rrnPayee)
     f                                     Sfile(sflBenefit:rrnBenefit)
     **** End Add *** 01/14/16 ************************************************
     f                                     InfDs(fids01)
     fwtActPnl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
      *****Begin Add*****  03/23/16  *************************************
     f                                     include(rcdClmPnl)
      ***** End Add *****  03/23/16  *************************************
     fwmcmp     uf   e             Disk
     f                                     ReName(pcm01:dbrcdf)
     f                                     InfDs(dbfds)
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
     fwmchl     if   e           k Disk    ReName(pch01:pch00)
     fwmlll     if   e           k Disk    ReName(pll01:pll00)
     fwdpal     if   e           k Disk    ReName(ppa01:wpa00)
     fwarpl     if   e           k Disk    ReName(prp01:prp00)
     fwamcl     if   e           k Disk    ReName(pmc01:pmc00)
     fwmahl     if   e           k Disk    ReName(pah01:pah00)
     fwmaal1    if   e           k Disk
     fwmagl     if   e           k Disk    ReName(pag01:pag00)
     fwmsil     uf a e           k Disk    ReName(psi01:psi00)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     f                                     InfDs(dbfds2)
     fwmall     if   e           k Disk    ReName(pal01:pal00)
     fwmtpl1    if   e           k Disk
     fwmupl1    if   e           k Disk
     fwtabl     if   e           k Disk
     f                                     Ignore(prs01)
     f                                     Ignore(psy01)
     f                                     ReName(prt01:prt00)
     f                                     ReName(pb201:pb200)
     f                                     ReName(pna01:pna00)
     f                                     ReName(ppa01:ppa00)
     f                                     ReName(pca01:pca00)
     fwtgil     if   e           k Disk    ReName(pgi01:pgi00)
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)
     fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)
     fstabl     if   e           k Disk    ReName(pst01:pst00)
     f*** Beg Add *** 02/12/14 ************************************************
     f*** Beg Chg *** 05/05/14 ************************************************
     f****fawhl     if   e           k Disk    Rename(pwh01:pwh0f)
     ffawhl     if   e           k Disk    Rename(pwh01:pwh0f) usropn
     f****ftdll     if   e           k Disk    Rename(pdl01:pdl00)
     fftdll     if   e           k Disk    Rename(pdl01:pdl00) usropn
     f****fmcmp if   e             Disk    Rename(pcm01:pcm0f) Prefix(fm:2)
     ffmcmp     if   e             Disk    Rename(pcm01:pcm0f) Prefix(fm:2)
     f                                     usropn
     f*** End Chg *** 05/05/14 ************************************************
     f*** End Add *** 02/12/14 ************************************************
     f*                                                                ********
     f*** Beg Add *** 05/05/14 ************************************************
     f*                                                                ********
     FSSYLF     IF   E           K DISK    prefix('SS')
     f*                                                                ********
     f*** End Add *** 05/05/14 ************************************************
     f*                                                                ********
     **** Beg Add *** 01/14/16 ************************************************
     fwtcpp     uf   e             Disk
     fwxcbp     uf   e             Disk
     **** End Add *** 01/14/16 ************************************************
     **** Beg Add *** 02/22/16 ************************************************
     fwxcbp2    uf   e             Disk
     **** End Add *** 02/22/16 ************************************************

     d*** Beg Add *** 05/05/14 ************************************************
       // SA001R parameters
     d   wcmsInst      s              1    Inz(*Blanks)
     d   wcmcInst      s              1    Inz(*Blanks)
     d   wcmcInt       s              1    Inz(*Blanks)
     d*** End Add *** 05/05/14 ************************************************

      ****** beg add ********* 03/19/15_A ****************************************
     d isExit          s               n   inz(*off)
     d callNext        s               n   inz(*off)
      ****** end add ********* 03/19/15_A ****************************************

     d* Proto-types

      ****** beg add ***** 01/14/16 *******************************
     d* Run Sql Statements
     d RunSql          pr                  ExtPgm('RUNSQL')
     d   strPayee                   600
      ****** end add ***** 01/14/16 *******************************

     d* Get Specific Retention
     d GetSpecRet      pr                  ExtPgm('WA010R')
     d   outRRN                       9  0
     d   outFndRet                   13  2
     d   outExcRein                   1

     d UpdateRecent    pr                  extpgm('STUPDRCNT')
     d   inRRN                        9
     d   inType                       1    const
     d   inQte                        1    const
     d   inMode                       1    options(*nopass)

     d* Get Paid by Reserve Type
     d GetPaid         pr            11s 2
     D  reserveType                   1a   Const

     d* Get Payent Requests by Reserve Type
     d GetPayRequests  pr            11s 2
     D  reserveType                   1a   Const

     d* Get Recoveries by Reserve Type
     d GetRecoveries   pr            11s 2
     D  reserveType                   1a   Const

     d* Get Remaining Reserves by Reserve Type
     d GetRemaining    pr            11s 2
     D  reserveType                   1a   Const

     d* Get Reserve Info
     d GetRsvInfo      pr

      *** Beg Add *** 02/12/14 ***************************************
      * Get the RRN of the WCMC case
     d GetRrn          pr                  ExtPgm('FA201R')
     d  charCo#                       3
     d  charFnd                       3
     d  charFyr                       3
     d  charCase                      7
     d  intRRN1                       9  0
     d  charRtcd                      1
     d*** End Add *** 02/12/14 ***************************************
      ****** beg add ***** 09/12/13 *******************************
      * Check Object
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                             like(outName)
     d outType                             like(outType)
     d outRtcd                             like(outRtcd)

     dqcmdExc          pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
      ****** end add ***** 09/12/13 *******************************

     d*** Beg Add *** 05/05/14 ************************************************
     d* Get System Flags
     d GetSysFlags     pr                  ExtPgm('SA001R')
     d   wcmsInst                     1
     d   wcmcInst                     1
     d   wcmcInt                      1

     d*** End Add *** 05/05/14 ************************************************

     d* Load Notes Grid
     d LoadNotes       pr

     d* Load Reserves Grid
     d LoadReserves    pr

      ****** beg add ***** 01/14/16 *******************************
     d* Load Payee Summary Grid
     d LoadPayee       pr

     d* Load Benefit Summary Grid
     d LoadBenefit     pr
      ****** end add ***** 01/14/16 *******************************

     d* Load Special Attributes
     d LoadSpecAtt     pr

      ****** Beg add ***** 07/19/16 *******************************
     d* Load ISO Scores
     d LodISOScore     pr
      ****** End add ***** 07/19/16 *******************************

     d* Save Notes Grid
     d SaveNotes       pr

     d* Write to Claim Notes File
     d WriteNotes      pr

     d* General Prototypes
     d/copy dPrRemark
     d/copy wPrUsrPrms
     d/copy sPrAtrium
     d/Copy sPrString
     d/Copy wPrGroup
     d/Copy sPrDate
     d/Copy wPrClaims
     d/Copy wPrAgent
     d/Copy wPrContact
     d/Copy wPrLoc
     d/Copy wPrEmplr
     d/Copy wPrClmDat
     d* Load Action Panel procedure Prototype and variables
      /Copy sCopyClmPr
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
       //*** begin add ***  04/16/15  ****************************************

     D* System Data Structures
     D dbfds           ds
     D  flstcm           *status
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
     D* Constant
     D htmlBlank       c                   const('&nbsp&nbsp&nbsp&nbsp')

     D*Variables
     D @@RRN           s              9
     D @@Mode          s              1
     D @@Next          s             10
     D @@TabIndex      s              5
     d AdL1            s             40    inz
     d AdL2            s             40    inz
     d AdL3            s             40    inz
     D y               s              1  0
     D i               s              5  0
     D numOfLines      s              5  0
     D infoCount       s              5  0 inz(0)
     D xhOpen          s              1
     D outClientID     s              3    inz('000')
     D kkCase          s             14    inz('00000000000000')
     D addMode         s               n   inz(*Off)
     D chgMode         s               n   inz(*Off)
     D dspMode         s               n   inz(*Off)
     D kkTbId          s                   like(GItbId)
     D kkKey           s                   like(NAkey)
     D kkSeq#          s                   like(SIseq#)
     D kkFein          s                   like(PDfein)
     D kkMod           s                   like(PDmod)
     D kkFrDt          s                   like(TPfrDt) Inz(*Hival)
     D rrnNotes2       s                   like(rrnNotes)
     D inRRNClm        s              9  0
     D inMode          s              1
     D ##Type          s              1
      **** beg add ***** 03/02/20 ****************************************
     D @@co#           s                   like(CMco#)
     D @@fnd           s                   like(CMfnd)
     D @@fYr           s                   like(CMfYr)
     D @@case          s                   like(CMcase)
      **** end add ***** 03/02/20 ****************************************
      *** Beg Add **** 08/20/13 ***********
     D CallRsv         s               n   inz(*Off)
      *** End Add **** 08/20/13 ***********
      ****** beg add ***** 09/12/13 *******************************
     d outName         s             10
     d outType         s              7
     d outRtcd         s              1
     d strCommand      s            255
     d length          s             15  5
      ****** end add ***** 09/12/13 *******************************
      ****** beg add ***** 09/23/14 *******************************
     d strField        s            300
      ****** end add ***** 09/23/14 *******************************
      ****** beg add ***** 01/14/16 *******************************
     d strPayee        s            600
     d strBenefit      s            600
     d intweek         s              3  0
     d intday          s              1  0
      ****** end add ***** 01/14/16 *******************************
      ****** beg add ***** 02/22/16 *******************************
     d strBenChrt      s            600
      ****** end add ***** 02/22/16 *******************************
      *** Beg Add *** 02/12/14 *******************************************
      //FA201R parameters
     dcharCo#          s              3    Inz(*Blanks)
     dcharFnd          s              3    Inz(*Blanks)
     dcharFyr          s              3    Inz(*Blanks)
     dcharCase         s              7    Inz(*Blanks)
     dintRRN1          s              9  0 Inz(*Zeros)
     dcharRtcd         s              1    Inz(*Blanks)
     dcharKey          s             10
     dintDate          s              7  0
      *** End Add *** 02/12/14 *******************************************
     D* Data Structures

     D                 ds
     D  ar                     1     90  2
     D                                     Dim(10)                              CANCEL DATE
     D  CMar01                 1      9  2
     D  CMar02                10     18  2
     D  CMar03                19     27  2
     D  CMar04                28     36  2
     D  CMar05                37     45  2
     D  CMar06                46     54  2
     D  CMar07                55     63  2
     D  CMar08                64     72  2
     D  CMar09                73     81  2
     D  CMar10                82     90  2

     D                 ds
     D  rr                     1     90  2
     D                                     Dim(10)                              CANCEL DATE
     D  CMrr01                 1      9  2
     D  CMrr02                10     18  2
     D  CMrr03                19     27  2
     D  CMrr04                28     36  2
     D  CMrr05                37     45  2
     D  CMrr06                46     54  2
     D  CMrr07                55     63  2
     D  CMrr08                64     72  2
     D  CMrr09                73     81  2
     D  CMrr10                82     90  2

     D                 ds
     D  rt                     1     20
     D                                     Dim(10)                              CANCEL DATE
     D  CMrt01                 1      2
     D  CMrt02                 3      4
     D  CMrt03                 5      6
     D  CMrt04                 7      8
     D  CMrt05                 9     10
     D  CMrt06                11     12
     D  CMrt07                13     14
     D  CMrt08                15     16
     D  CMrt09                17     18
     D  CMrt10                19     20

     D  Reserves       DS                  Dim(5) Inz
     D                                     Qualified
     D  ReserveName                  12A
     D  OutReserves                  11S 2
     D  PaidtoDate                   11S 2
     D  TotalInc                     11S 2
     D  Recoveries                   11S 2
     D  NetRecoveries                11S 2
     D  AvailReserves                11S 2
      ***** beg chg ***** 09/12/13 **************************************
     D  LnkReserve1                   1A
     D  LnkReserve2                   1A
     D***** LnkReserve1                 720A
     D***** LnkReserve2                 720A
      ***** end chg ***** 09/12/13 **************************************
      ***** Beg Add ***** 08/04/14 **************************************
     D  LnkReserve3                   1A
      ***** Beg Add ***** 08/04/14 **************************************

     D* Constants
     D xcMedOnly       c                   Const('Med Only')
     D xcLostTime      c                   Const('Indemnity')
     D xcRecOnly       c                   Const('Record Only')
     D xcComp          c                   Const('Compensation')
     D xcMed           c                   Const('Medical     ')
     D xcExp           c                   Const('Expense     ')
     D xcLeg           c                   Const('Legal       ')
     D xcTotal         c                   Const('Total')
     D xcRecIndP       c                   Const('Pending Investigation')
     D xcRecIndR       c                   Const('Subrogation Recovery')
     D xcRecIndY       c                   Const('Subrogated')
     D xcMultiInd      c                   Const('Multiple claim')
     D xcContrInd      c                   Const('Controverted')
     D xcReinInd       c                   Const('Reinsurance')
     D xcDenyInd       c                   Const('Denied')
     D xcLitiInd       c                   Const('Litigation')
     D xcEmpLiab       c                   Const('Employer Liability')

     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
     C                   Parm                    @@TabIndex
      *
     C     keySTABL      Klist
     C                   Kfld                    kkKey
      *
     C     keyWARPL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
      *
     C     keyWAMCL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    kkCase
      *
     C     keyWDPAL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
      *
     C     keyWMAAL1     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    AHagt#
      *
     C     keyWMAGL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    AAfein
     C                   Kfld                    AAmod
      *
     C     keyWMAHL      Klist
     C                   Kfld                    EMco#
     C                   Kfld                    EMfnd
     C                   Kfld                    EMemp#
     C                   Kfld                    EMdiv
      *
     C     keyWMALL      Klist
     C                   Kfld                    EMco#
     C                   Kfld                    EMfnd
     C                   Kfld                    EMemp#
     C                   Kfld                    EMdiv
     C                   Kfld                    CMctg1
      *
     C     keyWMCHL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
      *
     C     keyWMCXL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
      *
     C     keyWMPDL      Klist
     C                   Kfld                    kkFein
     C                   Kfld                    kkMod

     C     key1WMSIL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
     C                   Kfld                    kkSeq#

     C     key2WMSIL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase

     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

     C     keyWMFNL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
      *
     C     keyWTGIL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkTbId
      *
     C     keyWTGTL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkTbId
     C                   Kfld                    kkKey
    C*
     C     key1WMTPL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
    C*
     C     key2WMTPL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
     C                   Kfld                    kkFrDt
    C*
     C     keyWMLLL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
      *
     C     keyWTABL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkKey
      *** Beg Add *** 02/12/14 *************************************************
     c     key1FAWHL     Klist
     c                   Kfld                    FMco#
     c                   Kfld                    FMsub#
     c                   Kfld                    FMfnd
     c                   Kfld                    FMfyr
     c                   Kfld                    FMcase
     c                   Kfld                    intDate
      *
     c     key2FAWHL     Klist
     c                   Kfld                    FMco#
     c                   Kfld                    FMsub#
     c                   Kfld                    FMfnd
     c                   Kfld                    FMfyr
     c                   Kfld                    FMcase
      *
     c     keyFTDLL      Klist
     c                   Kfld                    FMco#
     c                   Kfld                    charKey
      *** End Add *** 02/12/14 *************************************************
      /Free

       ExSr  ProgramStart;
       UpdateRecent(@@RRN:'C':' ':inMode);


       //****** beg del ******** 03/19/15_A *************************************
       // MoveDbSc();
       // DoU editNotes=*Off;
          // editNotes=*Off;
          // Write rcdClmPnl;
          // Write rcdClmSum;
          // Exfmt rcdClmSum0;
          //****** beg chg ****** 03/19/15 **********************************
          // Read  rcdClmSum;

          // Read rcdClmPnl;
          // Read rcdClmSum;
          //****** end chg ****** 03/19/15 **********************************

       //*** begin add ***  04/16/15  ****************************************
          // If btnDocWndw = *on;
          //    btnDocWndw = *off;
          //    DocWindow(scActRRN:'CS');
          // Endif;
       //*** end add *****  04/16/15  ****************************************
       //*** Beg Add **** 08/20/13 ***********
          // CallRsv=*off;
          // Dou %Eof;
          //    Readc SflReserve;
             //*** Beg Chg **** 08/04/14 ************************************
             //*******     If PaidLink=*on or RecvLink=*on;
          //      If PaidLink=*on or RecvLink=*on or NetPtdLink=*on;
             //*** End Chg **** 08/04/14 ************************************
                  //***** beg add ***** 09/12/13 ********************
          //         outName = 'TYPDTAARA';
          //         outType = '*DTAARA';
          //         outRtcd = ' ';
          //         GetObject(outName:outType:outRtcd);

          //         If outRtcd = '1';
                     //Create dataarea TYPDTAARA in qtemp
          //            strCommand = 'CRTDTAARA DTAARA(QTEMP/TYPDTAARA)' +
          //             ' TYPE(*CHAR) LEN(1) VALUE(''';
          //         Else;
                     //Change dataarea TYPDTAARA in qtemp
          //            strCommand = 'CHGDTAARA DTAARA(QTEMP/TYPDTAARA *ALL)' +
          //             ' VALUE(''';
          //         Endif;
          //         If PaidLink = *on;
          //            strCommand = %trim(strCommand) + scPd2DtMod + ''')';
          //         Endif;
          //         If RecvLink = *on;
          //            strCommand = %trim(strCommand) + scRecMode + ''')';
          //         Endif;
                //*** Beg Add **** 08/04/14 ************************************
          //         If NetPtdLink = *on;
          //            strCommand = %trim(strCommand) + scNetPd2Dt + ''')';
          //         Endif;
                //*** End Add **** 08/04/14 ************************************
          //         callP qcmdExc(strCommand:%len(strCommand));
                  //***** end add ***** 09/12/13 ********************
          //          CallRsv=*on;
          //          Leave;
          //      Endif;
          // EndDo;
          // If CallRsv=*on;
          //    Leave;
          // EndIf;
       //*** End Add **** 08/20/13 ***********

          // If editNotes=*On;
          //    LoadNotes();
          //    DoU cancelNote=*On or updateNote=*On;
          //     exfmt rcdClmNote;
          //     If updateNote=*On;
          //        SaveNotes();
          //       EndIf;
          //    EndDo;
          // EndIf;
        // EndDo;
       //****** end del ******** 03/19/15_A *************************************
       //****** beg add ******** 03/19/15_A *************************************
       Dow isExit = *off;

          // Access the claim record
          Chain(n) inRRNClm dbrcdf;

          Exsr LoadInfo;
          Exfmt rcdClmSum0;
          Read rcdClmPnl;
          Read rcdClmSum;
          Select;
       //*** begin chg ***  01/07/16  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************
          // Refresh
          When refrshData = *On;
             refrshData = *Off;
          When editNotes=*On;
             LoadNotes();
             DoU cancelNote=*On or updateNote=*On;
              exfmt rcdClmNote;
              If updateNote=*On;
                 SaveNotes();
              EndIf;
             EndDo;
          Other;
             exsr CallNextSr;
          Endsl;
          If callNext=*on;
             Leave;
          EndIf;
       EndDo;
       //****** end add ******** 03/19/15_A *************************************
       //If chgMode;
       //   UpdateRecord();
       //EndIf;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

          //***** beg del ****** 03/19/15_A **************************************
          // If %Parms()>0;
          //    If editNotes=*On Or refrshData=*On;
          //       @@Next='WTCLMSUM';
          //       editNotes=*Off;
          //       If refrshData=*On;
          //       refrshData=*Off;
          //    EndIf;
          // Else;
       //*** Beg Add **** 08/20/13 ***********
          // If CallRsv=*on;
          //    @@Next='WTCLMRSV  ';
          //    @@TabIndex= '00001';
          // Else;
       //*** End Add **** 08/20/13 ***********
          //    Select;
          //    When medicalTab =*On;
          //       @@Next='WTCLMDTL  ';
          //       @@TabIndex='00002';
          //    When legalTab =*On;
          //       @@Next='WTCLMDTL  ';
          //       @@TabIndex='00004';
          //    When clmInfoTab =*On;
          //       @@Next='WTCLMDTL  ';
          //       @@TabIndex='00000';
          //    When statusTab =*On;
          //       @@Next='WTCLMRSV  ';
          //       @@TabIndex='00000';
          //    Other;
          //       @@Next=scMenuRsp;
          //       @@TabIndex='00000';
          //    EndSl;
       //*** Beg Add **** 08/20/13 ***********
          // EndIf;
       //*** End Add **** 08/20/13 ***********
          // EndIf;
          // EndIf;
          //***** end del ****** 03/19/15_A **************************************

          // Delete Top Five Summary Data
          DelTopFive(q1job:q1cUsr:q1jobN);

       //*** Beg Add **** 05/05/14 ***********

          // close managed care files

             If wcmcInst='Y' And wcmcInt='Y';
               close fawhl;
               close ftdll;
               close fmcmp;
             endif;

       //*** End Add **** 05/05/14 ***********

          *inlr=*On;
          Return;

       endsr;
       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

          //Load default values or passed in values
          If %Parms()>0;
             inRRNClm=%Int(@@RRN);
             inMode=@@Mode;
             If inRrnClm=0;
                inRRNClm=41;
                @@Rrn='000000041';
                inMode='C';
             EndIf;
          Else;
             inRRNClm=41;
             inMode='C';
          EndIf;

      //**** Begin Add  ***  06/23/21  ****************************************
           OutCmRrn = inRRNClm;
      //**** End Add  ***  06/23/21  ****************************************
          // Set the initial tab
          If %parms()>3;
             tabIndex=%Dec(@@TabIndex:5:0);
          Else;
             tabIndex=0;
          EndIf;

          // Access the claim record
          Chain(n) inRRNClm dbrcdf;

          // Get the client id
          GetClientID(outClientID);

          // Populate hidden fields
          scActCltId=outClientID;
          scActRrn=inRrnClm;

          // Set the Mode
          Select;
          When inMode='D';
             dspMode=*On;
             shwNoteEdt=*Off;
          When inMode='A';
             addMode=*On;
          When inMode='C';
             chgMode=*On;
             shwNoteEdt=*On;
          EndSl;
          scActMode = inMode;

          // Set the Action Panel Height
          aPnlHeight=1350;

          //****** beg del ******** 03/19/15_A *************************************
          // Important Note Grid info

          // scNoteSel = 'sifnd=' + %EditC(cmfnd:'X') +
           // ' and sifYr=' + %EditC(cmfYr:'X') +
           // ' and sicase=' + %EditC(cmcase:'X');

          // Build Payee Sql Statement

          //****** beg chg ****** 09/23/14 **********************************
          // scPayeeLst='select rppnam, ' +
          //  'sum(case when patype in (''R'', ''S'', ''V'')' +
          //  ' then paamt*-1 else paamt end) totAmt from wdpap join ' +
          //  'wdrpp on pafnd=rpfnd and pafYr=rpfYr and pacase=rpcase and ' +
          //  'paseq#=rpseq# and pachk#=rpchk# where ' +
          //  'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
          //  + ' and pacase=' + %EditC(cmcase:'X') +
          //  ' group by rppnam order by totAmt desc';

          // Payee Chart Info

          // scPaySel = 'P5jobN=''' + %TRIM(q1job) +
          //  ''' and P5jobU=''' + %Trim(q1cUsr) +
          //  ''' and P5job#=' + %EditC(q1jobN:'X');

          // Build Benefits SQL Statement

       // scBenLst='select pabr2, b2desc, ' +
       //  'sum(case when patype in (''R'', ''S'', ''V'')' +
       //  ' then paamt*-1 else paamt end), ' +
       //  ' trim(char(sum(pawks + floor(padays/7)))) || ''/'' ||' +
       //  ' trim(char(mod(sum(padays),7)))' +
       //  ' from wdpap join wtb2p on pabr2=b2key where ' +
       //  'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
       //  + ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C''' +
       //  ' group by pabr2, b2desc order by pabr2, b2desc';

       //    scBenFil ='WDPAP, WTB2P';
       //    scBenFld ='pabr2, b2desc, ' +
       //     'sum(case when patype in (''R'', ''S'', ''V'')' +
       //     ' then paamt*-1 else paamt end), ' +
       //     ' trim(char(sum(pawks + floor(padays/7)))) || ''/'' ||' +
       //     ' trim(char(mod(sum(padays),7)))';

          // scBenSel = 'pabr2=b2key and ' +
          //*** Beg Add **** 01/09/14 ***********
          //            'paco#=b2co# and ' +
          //*** End Add **** 01/09/14 ***********
          //  'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
          //  + ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C''' +
          //  ' group by pabr2, b2desc';

          // Build Payee Sql Statement
          // scPayeeLst='select rppnam, ' +
           // 'sum(case when patype in (''R'', ''S'', ''V'')' +
           // ' then paamt*-1 else paamt end) totAmt from wdpap join ' +
           // 'wdrpp on pafnd=rpfnd and pafYr=rpfYr and pacase=rpcase and ' +
           // 'paseq#=rpseq# and pachk#=rpchk# where ' +
           // 'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
           // + ' and pacase=' + %EditC(cmcase:'X') +
           // ' group by rppnam having ' +
           // 'sum(case when patype in (''R'', ''S'', ''V'')' +
           // ' then paamt*-1 else paamt end) <> 0 ' +
           // ' order by totAmt desc';

          // Payee Chart Info
          //scPaySel = 'P5jobN=''' + %TRIM(q1job) +
           // ''' and P5jobU=''' + %Trim(q1cUsr) +
           // ''' and P5job#=' + %EditC(q1jobN:'X') +
           // ' and P5amt <> 0';

          // Build Benefits SQL Statement
          // scBenFil ='WDPAP, WTB2P';
          // strField = *blanks;

          // strField = 'sum(case when ((patype in (''P'',''M'',''A'',''Z'')' +
           // ' and PAamt>0) or (patype in (''R'',''S'',''V'')' +
           // ' and PAamt<0)) then (pawks*7)+padays else' +
           // ' (pawks*-7)+(padays*-1) end)';
          // scBenFld ='pabr2,b2desc,' +
          //  'sum(case when patype in (''R'',''S'',''V'')' +
          //  ' then paamt*-1 else paamt end),' +
          //  'trim(char(case when ' + %trim(strField) + '>0 then floor(' +
          //  %trim(strField) + '/7) else ceiling(' +
           // %trim(strField) + '/7)' +
           // ' end)) || ''/'' ||' + ' trim(char(mod(' +
           // ' sum(case when ((patype in (''P'',''M'',''A'',''Z'')' +
           // ' and PAamt>0) or (patype in (''R'',''S'',''V'')' +
           // ' and PAamt<0)) then' +
           // ' padays else padays*-1 end)' +
           // ' ,7)))';

          // scBenSel = 'pabr2=b2key and ' + 'paco#=b2co# and ' +
           // 'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
           // + ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C''' +
           // ' group by pabr2, b2desc having ' +
           // 'sum(case when patype in (''R'', ''S'', ''V'')' +
           // ' then paamt*-1 else paamt end) <> 0';
          //****** end chg ****** 09/23/14 **********************************

          // scBenSrt = 'pabr2, b2desc';

          // Build Benefits SQL Select for Chart

          // scBenVal = 'case when patype in (''R'', ''S'', ''V'') then ' +
          //  'paamt*-1 else paamt end';

          //****** beg chg ****** 09/23/14 **********************************
          // scBenSelec = 'pafnd=' + %EditC(cmfnd:'X') +
          //  ' and pafYr=' + %EditC(cmfYr:'X') +
          //  ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C''';

          // scBenSelec = 'pafnd=' + %EditC(cmfnd:'X') +
           // ' and pafYr=' + %EditC(cmfYr:'X') +
           // ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C''' +
           // ' and paamt <> 0';
          //****** end chg ****** 09/23/14 **********************************
          //****** end del ******** 03/19/15_A *************************************

       //*** Beg Add **** 05/05/14 ***********

          // Get System Flags
          GetSysFlags(wcmsInst:wcmcInst:wcmcInt);

          // open managed care files

             If wcmcInst='Y' And wcmcInt='Y';
               open fawhl;
               open ftdll;
               open fmcmp;
             endif;

       //*** End Add **** 05/05/14 ***********

       endsr;

       //****** beg add ******** 03/19/15_A *************************************
       //===================================================================
       // LoadInfo - Load Claim Information on the Screen
       //===================================================================
       Begsr LoadInfo;
          MoveDbSc();
          Exsr LoadNoteGrid;
       Endsr;

       //===================================================================
       // LoadNoteGrid - Important Note Grid info
       //===================================================================
       Begsr LoadNoteGrid;
          scNoteSel = 'sifnd=' + %EditC(cmfnd:'X') +
           ' and sifYr=' + %EditC(cmfYr:'X') +
           ' and sicase=' + %EditC(cmcase:'X');

          // Build Payee Sql Statement
          //****** beg chg ****** 01/14/16 **********************************
          // scPayeeLst='select rppnam, ' +
          //  'sum(case when patype in (''R'', ''S'', ''V'')' +
          //  ' then paamt*-1 else paamt end) totAmt from wdpap join ' +
          // 'wdrpp on pafnd=rpfnd and pafYr=rpfYr and pacase=rpcase and ' +
          // 'paseq#=rpseq# and pachk#=rpchk# where ' +
          //'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
          // + ' and pacase=' + %EditC(cmcase:'X') +
          // ' group by rppnam having ' +
          // 'sum(case when patype in (''R'', ''S'', ''V'')' +
          // ' then paamt*-1 else paamt end) <> 0 ' +
          // ' order by totAmt desc';

          // Clear WTCPP claims payee work file
           setll *start wtcpp;
           read pcp01;
           dow not %eof(wtcpp);
              delete pcp01;
              read pcp01;
           enddo;

          // populate WTCPP claims payee work file
          strPayee = 'insert into wtcpp select rppnam, ' +
           'sum(case when patype in (''R'', ''S'', ''V'')' +
           ' then paamt*-1 else paamt end) totAmt from wdpap join ' +
           'wdrpp on pafnd=rpfnd and pafYr=rpfYr and pacase=rpcase and ' +
           'paseq#=rpseq# and pachk#=rpchk# where ' +
           'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
           + ' and pacase=' + %EditC(cmcase:'X') +
           ' group by rppnam having ' +
           'sum(case when patype in (''R'', ''S'', ''V'')' +
           ' then paamt*-1 else paamt end) <> 0 ' +
           ' order by totAmt desc';

             RunSql(strPayee);

          // Load payee Summary Grid using WTCPP claims payee work file
           LoadPayee();
          //****** end chg ****** 01/14/16 **********************************

          // Payee Chart Info
          scPaySel = 'P5jobN=''' + %TRIM(q1job) +
           ''' and P5jobU=''' + %Trim(q1cUsr) +
           ''' and P5job#=' + %EditC(q1jobN:'X') +
           ' and P5amt <> 0';

          // Build Benefits SQL Statement
          //****** beg chg ****** 01/14/16 **********************************
          //scBenFil ='WDPAP, WTB2P';
          //strField = *blanks;

          //strField = 'sum(case when ((patype in (''P'',''M'',''A'',''Z'')' +
          // ' and PAamt>0) or (patype in (''R'',''S'',''V'')' +
          // ' and PAamt<0)) then (pawks*7)+padays else' +
          // ' (pawks*-7)+(padays*-1) end)';
          //scBenFld ='pabr2,b2desc,' +
          // 'sum(case when patype in (''R'',''S'',''V'')' +
          // ' then paamt*-1 else paamt end),' +
          // 'trim(char(case when ' + %trim(strField) + '>0 then floor(' +
          // %trim(strField) + '/7) else ceiling(' +
          // %trim(strField) + '/7)' +
          // ' end)) || ''/'' ||' + ' trim(char(mod(' +
          // ' sum(case when ((patype in (''P'',''M'',''A'',''Z'')' +
          // ' and PAamt>0) or (patype in (''R'',''S'',''V'')' +
          // ' and PAamt<0)) then' +
          // ' padays else padays*-1 end)' +
          // ' ,7)))';

          //scBenSel = 'pabr2=b2key and ' + 'paco#=b2co# and ' +
          // 'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')
          // + ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C''' +
          // ' group by pabr2, b2desc having ' +
          // 'sum(case when patype in (''R'', ''S'', ''V'')' +
          // ' then paamt*-1 else paamt end) <> 0';
          //scBenSrt = 'pabr2, b2desc';

          // Clear WXCBP claims benefit work file
           setll *start wxcbp;
           read pcb01;
           dow not %eof(wxcbp);
              delete pcb01;
              read pcb01;
           enddo;

          // populate WXCBP claims benefit work file
          strBenefit = 'insert into wxcbp select pabr2,b2desc, ' +
           'sum(case when patype in (''R'', ''S'', ''V'')' +
           ' then paamt*-1 else paamt end) totAmt, ' +
           'sum(case when ((patype in (''P'',''M'',''A'',''Z'') ' +
           ' and PAamt>0) or (patype in (''R'', ''S'', ''V'') and ' +
           ' PAamt<0)) then (pawks*7)+padays else (pawks*-7)+(padays*-1) end)'+
           ' from wdpap join wtb2p on pabr2=b2key and paco#=b2co# where ' +
           'pafnd=' + %EditC(cmfnd:'X') + ' and pafYr=' + %EditC(cmfYr:'X')  +
           ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C'' '+
           ' group by pabr2, b2desc having sum(case when patype in ' +
           ' (''R'', ''S'', ''V'') then paamt*-1 else paamt end) <> 0  ';

             RunSql(strBenefit);

          // Load Benefit Summary Grid using WXCBP claims benefit work file
           LoadBenefit();
          //****** end chg ****** 01/14/16 **********************************

          // Build Benefits SQL Select for Chart

          //****** beg del ****** 02/22/16 **********************************
          //scBenVal = 'case when patype in (''R'', ''S'', ''V'') then ' +
          // 'paamt*-1 else paamt end';
          //scBenSelec = 'pafnd=' + %EditC(cmfnd:'X') +
          // ' and pafYr=' + %EditC(cmfYr:'X') +
          // ' and pacase=' + %EditC(cmcase:'X') + ' and paretp=''C''' +
          // ' and paamt <> 0';
          //****** end del ****** 02/22/16 **********************************
          //****** beg add ****** 02/22/16 **********************************
          // Clear WXCBP2 claims Paid by Benefit Chart work file
           setll *start wxcbp2;
           read pcb02;
           dow not %eof(wxcbp2);
              delete pcb02;
              read pcb02;
           enddo;

          // populate WXCBP2 claims Paid by Benefit Chart work file
          strBenChrt = 'insert into wxcbp2 select CBcod,CBamt  ' +
           'from wxcbp order by CBamt desc  ' +
           'fetch first 5 rows only';

             RunSql(strBenChrt);
          //****** end add ****** 02/22/16 **********************************
       Endsr;
       //===================================================================
       // CallNextSr  - Calls Next Screen
       //===================================================================
       Begsr CallNextSr;
          CallRsv=*off;
          Dou %Eof;
             Readc SflReserve;
               If PaidLink=*on or RecvLink=*on or NetPtdLink=*on;
                  //***** beg add ***** 09/12/13 ********************
                  outName = 'TYPDTAARA';
                  outType = '*DTAARA';
                  outRtcd = ' ';
                  GetObject(outName:outType:outRtcd);

                  If outRtcd = '1';
                     //Create dataarea TYPDTAARA in qtemp
                     strCommand = 'CRTDTAARA DTAARA(QTEMP/TYPDTAARA)' +
                      ' TYPE(*CHAR) LEN(1) VALUE(''';
                  Else;
                     //Change dataarea TYPDTAARA in qtemp
                     strCommand = 'CHGDTAARA DTAARA(QTEMP/TYPDTAARA *ALL)' +
                      ' VALUE(''';
                  Endif;
                  If PaidLink = *on;
                     strCommand = %trim(strCommand) + scPd2DtMod + ''')';
                  Endif;
                  If RecvLink = *on;
                     strCommand = %trim(strCommand) + scRecMode + ''')';
                  Endif;
                  If NetPtdLink = *on;
                     strCommand = %trim(strCommand) + scNetPd2Dt + ''')';
                  Endif;
                  callP qcmdExc(strCommand:%len(strCommand));
                   CallRsv=*on;
                   Leave;
               Endif;
          EndDo;
          If CallRsv=*on;
             @@Next='WTCLMRSV  ';
             @@TabIndex= '00001';
             callNext = *on;
          Else;
             Select;
             When medicalTab =*On;
                @@Next='WTCLMDTL  ';
                @@TabIndex='00002';
                callNext = *on;
             When legalTab =*On;
                @@Next='WTCLMDTL  ';
                @@TabIndex='00004';
                callNext = *on;
             When clmInfoTab =*On;
                @@Next='WTCLMDTL  ';
                @@TabIndex='00000';
                callNext = *on;
             When statusTab =*On;
                @@Next='WTCLMRSV  ';
                @@TabIndex='00000';
                callNext = *on;
             Other;
                If scMenuRsp <> *blanks;
                   @@Next=scMenuRsp;
                   @@TabIndex='00000';
                   callNext = *on;
                Endif;
             EndSl;
          EndIf;
          If callNext=*on;
             LeaveSr;
          EndIf;

       Endsr;
       //****** end add ******** 03/19/15_A *************************************

      /End-Free
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // GetPaid - Get paid  by reserve type.
       //===================================================================
     P GetPaid         B
     D GetPaid         PI            11s 2
     D  reserveType                   1a   const
     D paidAmt         s             11s 2 inz(0)
     D i               s              5  0
      /Free

       for i=1 to 10;
          kkKey=rt(i);
          Chain keyWTABL prt00;
          If %Found(wtabl) and RTtype=reserveType;
             paidAmt+=ar(i);
          EndIf;
       EndFor;

       Return paidAmt;

      /End-Free
     P GetPaid         E
       //===================================================================
       // GetPayRequests -Get payment requests by reserve type.
       //===================================================================
     P GetPayRequests  B
     D GetPayRequests  PI            11s 2
     D  reserveType                   1a   const
     D  payReqAmt      s             11s 2 inz(0)
      /Free

      //**** beg chg ***** 03/02/20 ****************************************
      //***** Chain keyWARPL prp00;
      //***** If %Found(warpl);
      //*****    Dou %Eof(warpl);
      //*****       If RPreTp=reserveType;
      //*****          If (CMstat='C' and RPsts<>'PY') or CMstat='O';
      //*****             payReqAmt+=RPamt;
      //*****          EndIf;
      //*****       EndIf;
      //*****    ReadE keyWARPL prp00;
      //*****    EndDo;
      //***** EndIf;

       @@co#  = CMco#;
       @@fnd  = CMfnd;
       @@fYr  = CMfYr;
       @@case = CMcase;

       Exec sql
        select sum(RPamt) into :payReqAmt
         FROM WARPP a, WMCMP b
         where CMco# = :@@co# and CMfnd = :@@fnd and CMfyr = :@@fyr and
         CMcase = :@@case and
         RPco# = :@@co# and RPfnd = :@@fnd and RPfyr = :@@fyr and
         RPcase = :@@case and RPretp = :reserveType and
         ((CMstat='C' and RPsts<>'PY') or CMstat='O');
      //**** end chg ***** 03/02/20 ****************************************

       Return payReqAmt;

      /End-Free
     P GetPayRequests  E
       //===================================================================
       // GetRecoveries - Get the recoveries by reserve type.
       //===================================================================
     P GetRecoveries   B
     D GetRecoveries   PI            11s 2
     D  reserveType                   1a   const
     D  recoveryAmt    s             11s 2 inz(0)
      /Free

      //**** beg chg ***** 03/02/20 ****************************************
      //***** Chain keyWDPAL wpa00;
      //***** If %Found(wdpal);
      //*****    Dou %Eof(wdpal);
      //*****       If PAtype='R' and PAreTp=reserveType;
      //*****          kkKey=PAbr2;
      //*****          Chain keyWTABL pb200;
      //*****          If %Found(wtabl) and B2rcvR='Y';
      //*****             recoveryAmt+=PAamt;
      //*****          EndIf;
      //*****       EndIf;
      //*****    ReadE keyWDPAL wpa00;
      //*****    EndDo;
      //***** EndIf;

       Exec sql
        select sum(PAamt) into :recoveryAmt
         FROM WDPAP a, WTB2P b
         where PAco# = :CMco# and PAfnd = :CMfnd and PAfyr = :CMfyr and
         PAcase = :CMcase and PAtype = 'R' and PAretp = :reserveType and
         B2co#=PAco# and B2key=PAbr2 and B2rcvr='Y';
      //**** end chg ***** 03/02/20 ****************************************

       Return recoveryAmt;

      /End-Free
     P GetRecoveries   E
       //===================================================================
       // GetRemaining - Get the remaining reserves by reserve type.
       //===================================================================
     P GetRemaining    B
     D GetRemaining    PI            11s 2
     D  reserveType                   1a   const
     D remainingAmt    s             11s 2 inz(0)
     D i               s              5  0
      /Free

       for i=1 to 10;
          kkKey=rt(i);
          Chain keyWTABL prt00;
          If %Found(wtabl) and RTtype=reserveType;
             remainingAmt+=rr(i);
          EndIf;
       EndFor;

       Return remainingAmt;

      /End-Free
     P GetRemaining    E
       //===================================================================
       // GetRsvInfo - Get Reserve Info for grid load
       //===================================================================
     P GetRsvInfo      B
     D GetRsvInfo      PI
     D i               s              5  0
      /Free

       //***** Beg Add *** 09/04/15 ****************************************
       reserves(5).OutReserves = *zeros;
       reserves(5).PaidtoDate  = *zeros;
       reserves(5).TotalInc    = *zeros;
       reserves(5).Recoveries  = *zeros;
       reserves(5).NetRecoveries = *zeros;
       reserves(5).AvailReserves = *zeros;
       //***** End Add *** 09/04/15 ****************************************

       reserves(5).ReserveName=xcTotal;

       for i=1 to 4;
          Select;
          When i=1;
             reserves(i).ReserveName=xcComp;
             reserves(i).OutReserves=GetRemaining('C');
             reserves(i).PaidtoDate=GetPaid('C');
             reserves(i).TotalInc=reserves(i).OutReserves +
              reserves(i).PaidtoDate;
             reserves(i).Recoveries=GetRecoveries('C');
             reserves(i).NetRecoveries=reserves(i).TotalInc -
              reserves(i).Recoveries;
             reserves(i).AvailReserves=reserves(i).OutReserves -
              GetPayRequests('C');
             //**** beg chg ***** 09/12/13 ****************************************
             reserves(i).LnkReserve1 = '1';
             reserves(i).LnkReserve2 = '2';
             //*** Beg Add **** 08/04/14 ************************************
             reserves(i).LnkReserve3 = 'A';
             //*** End Add **** 08/04/14 ************************************
             //reserves(i).LnkReserve1 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'1');
             //reserves(i).LnkReserve2 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'2');
             //**** end chg ***** 09/12/13 ****************************************
          When i=2;
             reserves(i).ReserveName=xcMed;
             reserves(i).OutReserves=GetRemaining('M');
             reserves(i).PaidtoDate=GetPaid('M');
             reserves(i).TotalInc=reserves(i).OutReserves +
              reserves(i).PaidtoDate;
             reserves(i).Recoveries=GetRecoveries('M');
             reserves(i).NetRecoveries=reserves(i).TotalInc -
              reserves(i).Recoveries;
             reserves(i).AvailReserves=reserves(i).OutReserves -
              GetPayRequests('M');
             //**** beg chg ***** 09/12/13 ****************************************
             reserves(i).LnkReserve1 = '3';
             reserves(i).LnkReserve2 = '4';
             //*** Beg Add **** 08/04/14 ************************************
             reserves(i).LnkReserve3 = 'B';
             //*** End Add **** 08/04/14 ************************************
             //reserves(i).LnkReserve1 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'3');
             //reserves(i).LnkReserve2 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'4');
             //**** end chg ***** 09/12/13 ****************************************
          When i=3;
             reserves(i).ReserveName=xcExp;
             reserves(i).OutReserves=GetRemaining('E');
             reserves(i).PaidtoDate=GetPaid('E');
             reserves(i).TotalInc=reserves(i).OutReserves +
              reserves(i).PaidtoDate;
             reserves(i).Recoveries=GetRecoveries('E');
             reserves(i).NetRecoveries=reserves(i).TotalInc -
              reserves(i).Recoveries;
             reserves(i).AvailReserves=reserves(i).OutReserves -
              GetPayRequests('E');
             //**** beg chg ***** 09/12/13 ****************************************
             reserves(i).LnkReserve1 = '5';
             reserves(i).LnkReserve2 = '6';
             //*** Beg Add **** 08/04/14 ************************************
             reserves(i).LnkReserve3 = 'C';
             //*** End Add **** 08/04/14 ************************************
             //reserves(i).LnkReserve1 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'5');
             //reserves(i).LnkReserve2 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'6');
             //**** end chg ***** 09/12/13 ****************************************
          When i=4;
             // *** Beg Add ***  11/25/13  *************************************
             If outClientID = '129';
             // *** Beg Add ***  03/10/14  *************************************
                If CXlgrh <> *Blanks;
             // *** End Add ***  03/10/14  *************************************
                   reserves(i).ReserveName=CXlgrh;
             // *** Beg Add ***  03/10/14  *************************************
                Else;
                   reserves(i).ReserveName=xcLeg;
                EndIf;
             // *** End Add ***  03/10/14  *************************************
             Else;
             // *** End Add ***  11/25/13  *************************************
                reserves(i).ReserveName=xcLeg;
             // *** Beg Add ***  11/25/13  *************************************
             EndIf;
             // *** End Add ***  11/25/13  *************************************
             reserves(i).OutReserves=GetRemaining('L');
             reserves(i).PaidtoDate=GetPaid('L');
             reserves(i).TotalInc=reserves(i).OutReserves +
              reserves(i).PaidtoDate;
             reserves(i).Recoveries=GetRecoveries('L');
             reserves(i).NetRecoveries=reserves(i).TotalInc -
              reserves(i).Recoveries;
             reserves(i).AvailReserves=reserves(i).OutReserves -
              GetPayRequests('L');
             //**** beg chg ***** 09/12/13 ****************************************
             reserves(i).LnkReserve1 = '7';
             reserves(i).LnkReserve2 = '8';
             //*** Beg Add **** 08/04/14 ************************************
             reserves(i).LnkReserve3 = 'D';
             //*** End Add **** 08/04/14 ************************************
             //reserves(i).LnkReserve1 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'7');
             //reserves(i).LnkReserve2 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
             // 'WTCLMRSV':'00001':'8');
             //**** end chg ***** 09/12/13 ****************************************
          EndSl;

       // Totals

          reserves(5).OutReserves+=reserves(i).OutReserves;
          reserves(5).PaidToDate+=reserves(i).PaidToDate;
          reserves(5).TotalInc+=reserves(i).TotalInc;
          reserves(5).Recoveries+=reserves(i).Recoveries;
          reserves(5).NetRecoveries+=reserves(i).NetRecoveries;
          reserves(5).AvailReserves+=reserves(i).AvailReserves;
          //**** beg chg ***** 09/12/13 ****************************************
          reserves(5).LnkReserve1 = '9';
          reserves(5).LnkReserve2 = '0';
          //*** Beg Add **** 08/04/14 ************************************
          reserves(5).LnkReserve3 = 'E';
          //*** End Add **** 08/04/14 ************************************
          //reserves(5).LnkReserve1 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
          // 'WTCLMRSV':'00001':'9');
          //reserves(5).LnkReserve2 = CreateURL('STDRVPGM':scClmName:@@RRN:'D':
          // 'WTCLMRSV':'00001':'0');
          //**** end chg ***** 09/12/13 ****************************************

       EndFor;

      /End-Free
     P GetRsvInfo      E
       //===================================================================
       // LoadNotes - Loads the notes data into the notes grid
       //===================================================================
     P LoadNotes       B
     D LoadNotes       PI
      /Free

       cancelNote=*Off;
       updateNote=*Off;
       rrnNotes=0;
       clrNotes=*On;
       //dspEdtNote = *Off;
       Write rcdClmNote;
       clrNotes=*Off;
       //dspEdtNote = *On;
       kkSeq#=*Loval;
       SetLl key1WMSIL psi00;
       ReadE(n) key2WMSIL psi00;
       If %Found(wmsil);
          Dou %Eof(wmsil);
             scComm=SIcomm;
             scSeq#=SIseq#;
             rrnNotes = rrnNotes + 1;
             Write sflEdtNote;
             ReadE(e) key2WMSIL psi00;
          EndDo;
       EndIf;
       rrnNotes2 = rrnNotes;

       // Load Blank Lines for Adding Notes;
       For i=1 to 20;
          scComm = *Blanks;
          scSeq# = scSeq# + 1;
          rrnNotes = rrnNotes + 1;
          Write sflEdtNote;
       EndFor;

       numOfLines=rrnNotes;
       if rrnNotes2<>0;
          If rrnNotes2<10;
             rrnNotes=1;
          Else;
             rrnNotes=rrnNotes2 - 8;
          EndIf;
       Else;
          rrnNotes=1;
       EndIf;

      /End-Free
     P LoadNotes       E
       //===================================================================
       // LoadReserves - Loads the Reserves Data into Reserves Grid
       //===================================================================
     P LoadReserves    B
     D LoadReserves    PI
      /Free

       rrnReserve=0;
       clrReserve=*On;
       //****** Beg Add ****** 03/19/15_A **********************************
       dspReserve = *off;
       Write rcdClmPnl;
       Write rcdClmSum;
       write rcdClmSum0;
       dspReserve = *on;
       //****** End Add ****** 03/19/15_A **********************************
       clrReserve=*Off;
       GetRsvInfo();

       for i=1 to 5;
          scRsvCode=reserves(i).ReserveName;
          scRemRsv=reserves(i).OutReserves;
       //*** Beg Del **** 08/04/14 ************************************
       //********   scPaid=reserves(i).PaidtoDate;
       //*** End Del **** 08/04/14 ************************************
          scTotRsv=reserves(i).TotalInc;
          scRecov=reserves(i).Recoveries;
       //*** Begin Del *** 01/27/16 ***********************************
       //*** Beg Add **** 08/04/14 ************************************
       //*scPaid=reserves(i).PaidtoDate + scRecov;
       //*scNetPtd=reserves(i).PaidtoDate;
       //*** End Add **** 08/04/14 ************************************
       //*** End   Del *** 01/27/16 ***********************************
          scNetRec=reserves(i).NetRecoveries;
          scAvailRsv=reserves(i).AvailReserves;
       //*** Begin Add *** 01/27/16 ************************************
          scPaid=reserves(i).PaidtoDate + scRecov;
          scTotRsv=scTotRsv + scRecov;
          scNetPtd=reserves(i).PaidtoDate;
          scNetRec=scNetRec + scRecov;
       //*** End   Add *** 01/27/16 ************************************
       //*** Beg delete **** 08/20/13 ***********
       //**  scPdTDtLnk=reserves(i).LnkReserve1;
       //**  scRecvLnk=reserves(i).LnkReserve2;
       //*** End delete **** 08/20/13 ***********
          //***** beg add ***** 09/12/13 ***********
       //*** Beg Chg **** 08/04/14 ************************************
       //********   scPd2DtMod = reserves(i).LnkReserve1;
          scPd2DtMod = reserves(i).LnkReserve3;
          scNetPd2Dt = reserves(i).LnkReserve1;
       //*** End Chg **** 08/04/14 ************************************
          scRecMode  = reserves(i).LnkReserve2;
          //***** end add ***** 09/12/13 ***********
          rrnReserve = rrnReserve + 1;
          write sflReserve;
       EndFor;

      /End-Free
     P LoadReserves    E
       //*** Beg Add **** 01/14/16 ************************************
       //===================================================================
       // LoadPayee - Loads Payee Summary Grid
       //===================================================================
     P LoadPayee       B
     D Loadpayee       PI
      /Free

       rrnPayee = 0;
       clrPayee = *On;
       dsppayee = *off;
       Write rcdClmPnl;
       Write rcdClmSum;
       write rcdClmSum0;
       dspPayee = *on;
       clrPayee = *Off;

       setll *start wtcpp;
       read pcp01;
       dow not %eof(wtcpp);
          scPayName = PCnam;
          scTotPaid = PCamt;
          rrnPayee = rrnpayee + 1;
          Write sflPayees;
       read pcp01;
       enddo;

       rrnPayee = 1;

      /End-Free
     P LoadPayee       E
       //*** End Add **** 01/14/16 ************************************
       //*** Beg Add **** 01/14/16 ************************************
       //===================================================================
       // LoadBenefit - Loads Benefit Summary Grid
       //===================================================================
     P LoadBenefit     B
     D LoadBenefit     PI
      /Free

       rrnBenefit = 0;
       clrBenefit = *On;
       dspBenefit = *off;
       Write rcdClmPnl;
       Write rcdClmSum;
       write rcdClmSum0;
       dspBenefit = *on;
       clrBenefit = *Off;

       setll *start wxcbp;
       read pcb01;
       dow not %eof(wxcbp);
          scBenCode = CBcod;
          scBenDesc = CBdes;
          scBenAmt = CBamt;
          intweek = CBday/7;
          intday = %rem(CBday:7);
          scBenDays = %char(intweek) + '/' + %char(intday);
          rrnBenefit = rrnBenefit + 1;
          Write sflBenefit;
       read pcb01;
       enddo;

       rrnBenefit = 1;

      /End-Free
     P LoadBenefit     E
       //*** End Add **** 01/14/16 ************************************
       //*** Beg Add **** 07/19/16 ************************************
       //===================================================================
       // LodISOScore - Load ISO Score values onscreen
       //===================================================================
     P LodISOScore     B
     D LodISOScore     PI
      /Free

       // if client ID is 114
       If isCsi = *on;
          scISOCur = *zeros;
          scISOPrv = *zeros;
          scISOIni = *zeros;
       //Get the ISO Module Flag
          IsISO = IsModEnabled('ISOWorkComp');
          If IsISO = *On;
                scISOCur = CMisoc;
                scISOPrv = CMisop;
                scISOIni = CMisoi;
          Endif;
       EndIf;

      /End-Free
     p LodISOScore     E
       //*** End Add **** 07/19/16 ************************************
       //===================================================================
       // LoadSpecAtt - Loads special attributes
       //===================================================================
     P LoadSpecAtt     B
     D LoadSpecAtt     PI
     D  inRRn          s              9  0
     D  outFndRet      s             13  2 inz(*Zeros)
     D  outExcRein     s              1    inz(*Blanks)
      /Free

       // Initialize Information Lines
       scInfo1=*Blanks;
       scInfo2=*Blanks;
       //*** Beg Add *** 01/15/21 *********************************************
       infoCount=*Zeros;
       //*** End Add *** 01/15/21 *********************************************

       // Reinsurance
       inRRN=inRRNClm;
       GetSpecRet(inRRN:outFndRet:outExcRein);
       scFndRet=outFndRet;
       If outExcRein='Y';
          If infoCount<3;
             If scInfo1=*Blank;
                scInfo1=xcReinInd;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcReinInd;
             EndIf;
             infoCount=infoCount+1;
          Else;
             If scInfo2=*Blank;
                scInfo2=xcReinInd;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcReinInd;
             EndIf;
             infoCount=infoCount+1;
          EndIf;
       EndIf;

       // Recovery Type
       If infoCount<3;
          Select;
          When CMsubr='P';
             If scInfo1=*Blank;
                scInfo1=xcRecIndP;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcRecIndP;
             EndIf;
             infoCount=infoCount+1;
          When CMsubr='R';
             If scInfo1=*Blank;
                scInfo1=xcRecIndR;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcRecIndR;
             EndIf;
             infoCount=infoCount+1;
          When CMsubr='Y';
             If scInfo1=*Blank;
                scInfo1=xcRecIndY;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcRecIndY;
             EndIf;
             infoCount=infoCount+1;
          EndSl;
       Else;
          Select;
          When CMsubr='P';
             If scInfo2=*Blank;
                scInfo2=xcRecIndP;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcRecIndP;
             EndIf;
             infoCount=infoCount+1;
          When CMsubr='R';
             If scInfo2=*Blank;
                scInfo2=xcRecIndR;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcRecIndR;
             EndIf;
             infoCount=infoCount+1;
          When CMsubr='Y';
             If scInfo2=*Blank;
                scInfo2=xcRecIndY;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcRecIndY;
             EndIf;
             infoCount=infoCount+1;
          EndSl;
       EndIf;

       // Multi Claim
       kkCase='0000000' + %EditC(CMcase:'X');
       Chain keyWAMCL pmc00;
       If %Found(wamcl);
          If infoCount<3;
             If scInfo1=*Blank;
                scInfo1=xcMultiInd;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcMultiInd;
             EndIf;
             infoCount=infoCount+1;
          Else;
             If scInfo2=*Blank;
                scInfo2=xcMultiInd;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcMultiInd;
             EndIf;
             infoCount=infoCount+1;
          EndIf;
       EndIf;

       // Controverted
       If CMcvFl='Y';
          If infoCount<3;
             If scInfo1=*Blank;
                scInfo1=xcContrInd;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank  + xcContrInd;
             EndIf;
             infoCount=infoCount+1;
          Else;
             If scInfo2=*Blank;
                scInfo2=xcContrInd;
             Else;
                scInfo2=%Trim(scInfo2) + '     ' + xcContrInd;
             EndIf;
             infoCount=infoCount+1;
          EndIf;
       EndIf;

       // Denied
       If CMdny='Y';
          If infoCount<3;
             If scInfo1=*Blank;
                scInfo1=xcDenyInd;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcDenyInd;
             EndIf;
             infoCount=infoCount+1;
          Else;
             If scInfo2=*Blank;
                scInfo2=xcDenyInd;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcDenyInd;
             EndIf;
             infoCount=infoCount+1;
          EndIf;
       EndIf;

       // Litigation
       If LLlitg='Y';
          If infoCount<3;
             If scInfo1=*Blank;
                scInfo1=xcLitiInd;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcLitiInd;
             EndIf;
             infoCount=infoCount+1;
          Else;
             If scInfo2=*Blank;
                scInfo2=xcLitiInd;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcLitiInd;
             EndIf;
             infoCount=infoCount+1;
          EndIf;
       EndIf;

       // Employer Liability
       If CMliaF='Y';
          If infoCount<3;
             If scInfo1=*Blank;
                scInfo1=xcEmpLiab;
             Else;
                scInfo1=%Trim(scInfo1) + htmlBlank + xcEmpLiab;
             EndIf;
             infoCount=infoCount+1;
          Else;
             If scInfo2=*Blank;
                scInfo2=xcEmpLiab;
             Else;
                scInfo2=%Trim(scInfo2) + htmlBlank + xcEmpLiab;
             EndIf;
             infoCount=infoCount+1;
          EndIf;
       EndIf;
       //*** Beg Add *** 11/25/13 *********************************************
       //*** Beg Chg *** 01/29/14 *********************************************
       // If outClientID = '129' And
       //    CM14x6 = 'Y' OR
       //    CMmisc = 'Y' OR
       //    CMrstn = 'Y' OR
       //    CMlstx = 'Y' OR
       //    CMexcs = 'Y' OR
       //    CMsdf  = 'Y';
       //    isFCC = *On;
          If outClientID = '129';
             If CM14x6 = 'Y' OR
                CMmisc = 'Y' OR
                CMrstn = 'Y' OR
                CMlstx = 'Y' OR
                CMexcs = 'Y' OR
                CMsdf  = 'Y';
                   isFCC = *On;
             EndIf;
       //*** End Chg *** 01/29/14 *********************************************
          Else;
             isFCC = *Off;
          EndIf;
       //*** End Add *** 11/25/13 *********************************************
      /End-Free
     P LoadSpecAtt     E
       //===================================================================
       // MoveDbSc - Move Database fields to Screen fields
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
      /Free

       // Populate Top 5 Payee File
       PopTopFive(inRrnClm);

       // Load the Action Panel Fields
       LoadActPan();

       // Claim Demographic Information
       Chain CMss# pcd00;
       If %Found(wmcdl);

          If cdMid<>*Blanks;
             scClmName=%Trim(CDlsn) + ', ' +
              %Trim(CDfsn) + ' ' + %Trim(CDmid) + '.';
          Else;
             scClmName=%Trim(CDlsn) + ', ' + %Trim(CDfsn);
          EndIf;

          scLsn=CDlsn;
          scFsn=CDfsn;
          scMid=CDmid;

          If CDphn#<>*Zeros;
             scClmPhn#='(' + %EditC(CDarea:'X') + ') ' +
              %Subst(%EditC(CDphn#:'X'):1:3) + '-' +
              %Subst(%EditC(CDphn#:'X'):4:4);
          EndIf;

          FormatAddress(CDAdr1:CDAdr2:CDCty:CDSt:CDZip:AdL1:AdL2:AdL3);
          scClmAd1 = ADL1;
          scClmAd2 = ADL2;
          scClmAd3 = ADL3;

          scMdBn=CDmdBn;
          scClmEmad=CDeMad;
          If scClmEmad=*Blanks;
             scClmEmadT=noEmail;
          EndIf;
       Else;
          scClmName=*Blanks;
          scClmPhn#=*Blanks;
          scClmAd1=*Blanks;
          scClmAd2=*Blanks;
          scClmAd3=*Blanks;
          scMdBn=*Blanks;
          scClmEmad=*Blanks;
          scClmEmadT=noEmail;
       EndIf;

       If scClmAd1 = *Blanks;
          mapClaimIn = *Off;
       Else;
          mapClaimIn = *On;
       EndIf;

       // Claim Information
       scScl#=CMacl#;

       //***** beg add ***** 08/25/14 ****************************************
       If outClientID = '114' and CMascl <> ' ';
          scScl#TlTp  = 'Alternate State Claim # : ' + %Trim(CMascl);
       Else;
          scScl#TlTp  = 'State Claim #';
       Endif;
       //***** end add ***** 08/25/14 ****************************************

       scClm#=%EditC(CMfnd:'X') + '-' + %EditC(CMfYr:'X') + '-' +
        %EditC(CMcase:'X');

       //***  Begin Chg  ***  06/09/20  **********************
       //scSS#=%Subst(CMss#:1:3) + '-' + %Subst(CMss#:4:2) + '-' +
       // %Subst(CMss#:6:4);
       scSS# = DisplaySS#(MaskData('SSN':'CMss#':CMss#));
       //***  End   Chg  ***  06/09/20  **********************

       kkKey=CMjrSt;
       Chain keySTABL pst00;
       If %Found(stabl);
          scJrSt=STname;
       Else;
          scJrSt=*Blanks;
       EndIf;

       scAcDs=%Trim(CMacDs) + ' ' + CMacD2;
       scMBS=CMmbs;
       scReinClm#=CMreCl;
       scCata#=CMcata;

       Select;
       When CMltFl='M';
          scClaimTyp=xcMedOnly;
       When CMltFl='I';
          scClaimTyp=xcLostTime;
       When CMltFl='R';
          scCLaimTyp=xcRecOnly;
       EndSl;
       //
       // Claim Supervisor
       //
       Chain CMsupr pup01;
       If %Found(wmupl1);
          // scSupr=UPusr;
          scSuprNm=UPusNm;
          scSuprEM=UPmail;
          scSuprEMTT=UPmail;
       EndIf;

       If scSuprEM=*Blanks;
          scSuprEMTT=noEmail;
       EndIf;
       //
       // Claim Adjuster.
       //
       Chain CMclEx pup01;
       If %Found(wmupl1);
          // scClEx=UPusr;
          scClExNm=UPusNm;
          scClExEM=UPmail;
          scClExEMTT=UPmail;
       EndIf;

       If scClExEM=*Blanks;
          scClExEMTT=noEmail;
       EndIf;
       //
       // Nature
       //
       kkKey=CMnatr;
       Chain keyWTABL pna00;
       If %Found(WTABL);
          scNatr=%Trim(CMnatr) + ' - ' + NAdesc;
       Else;
          scNatr=CMnatr;
       EndIf;
       //
       // Body Parts
       //
       kkKey=CMpart;
       Chain keyWTABL ppa00;
       If %Found(WTABL);
          scPart=%Trim(CMpart) + ' - ' + PAdesc;
       Else;
          scPart=CMpart;
       EndIf;
       //
       // Cause
       //
       kkKey=CMcaus;
       Chain keyWTABL pca00;
       If %Found(WTABL);
          scCaus=%Trim(CMcaus) + ' - ' + CAdesc;
       Else;
          scCaus=CMcaus;
       EndIf;
       //
       // Category 2
       //
       Chain keyWMCXL pcx00;
       If Not %Found(WMCXL) Or %Found(WMCXL) And CXcat2=*Blanks;
          scCataHdg='Category 2';
       Else;
          scCataHdg=CXcat2;
       EndIf;

       scCata2=CMctg2;
       scEmpCmpRt = CMcrt;
       scEmpAww   = CMaww;
       scRrpt     = CMrirp;

       //
       // Group Information
       //
       Chain keyWMFNL pfn00;
       If %Found(wmfnl);
          scGroup=%EditC(FNfnd:'X') + ' ' + FNname;
       EndIf;

       //
       // Treating Physician Information
       //
       kkFrDt=DateTo7(SystemDate());
       SetLl key2WMTPL1 ptp01;
       ReadE key1WMTPL1 ptp01;
       DoW Not %Eof(wmtpl1);
          If TPpRol='TR' And (TPtoDt>=DateTo7(SystemDate()) or TPtoDt=0);
             scPhyN=TPphyN;
             If TPphyP<>*Zeros;
                scTrPhyPhn='(' + %EditC(TPphyR:'X') + ') ' +
                 %Subst(%EditC(TPphyP:'X'):1:3) + '-' +
                 %Subst(%EditC(TPphyP:'X'):4:4);
             EndIf;
             scEmad=TPeMad;
             scEmadTT=TPeMad;
             If scEmad=*Blanks;
                scEmadTT=NoEMail;
             EndIf;
             Leave;
          EndIf;
          ReadE key1WMTPL1 ptp01;
       EndDo;
       //
       // Legal Information
       //
       Chain keyWMLLL pll00;
       If %Found(wmlll);
          scPlaN=LLplaN;
          If scPlaN=*Blanks;
             kkFein=LLplaA;
             kkMod=LLplaM;
             Chain keyWMPDL ppd00;
             If %Found(wmpdl);
                scPlaN=PDname;
             EndIf;
          EndIf;
          scDefN=LLdefN;
          If scDefN=*Blanks;
             kkFein=LLdefA;
             kkMod=LLdefM;
             Chain keyWMPDL ppd00;
             If %Found(wmpdl);
                scDefN=PDname;
             EndIf;
          EndIf;
          If LLdefP<>*Zeros;
             scDaPhn='(' + %EditC(LLdefR:'X') + ') ' +
              %Subst(%EditC(LLdefP:'X'):1:3) + '-' +
              %Subst(%EditC(LLdefP:'X'):4:4);
          EndIf;
          If LLplaP<>*Zeros;
             scPaPhn='(' + %EditC(LLplaR:'X') + ') ' +
              %Subst(%EditC(LLplaP:'X'):1:3) + '-' +
              %Subst(%EditC(LLplaP:'X'):4:4);
          EndIf;
          //*** begin chg ***  03/12/14  **************************************
          //*********** scEmDt=LLemDt;
          //*********** scEmDtTT=LLemDt;
          scEmDt=LLemDp;
          scEmDtTT=LLemDp;
          //*** end   chg ***  03/12/14  **************************************
          scEmDd=LLemDd;
          scEmDdTT=LLemDd;
       EndIf;

       // EmployER CANCEL DATE

       If scEmDt=*Blanks;
          scEmDtTT=noEmail;
       EndIf;

       If scEmDd=*Blanks;
          scEmDdTT=noEmail;
       EndIf;
       //
       // Claim History
       //
       xhOpen=' ';
       Chain keyWMCHL pch00;
       If %Found(wmchl);
          Dou %Eof(wmchl);
             //*** begin chg ***  02/21/14  **************************************
             //*****date = %Date(CHefdt:*cymd);
             Clear date;
             If CHefdt <> 0;
                date = %Date(CHefdt:*cymd);
             Endif;
             //*** end chg *****  02/21/14  **************************************
             Select;
             When CHocSt='O' and xhOpen<>'Y';
                xhopen='Y';
                scOpDt=%Char(date);
             When CHocSt='O' and xhOpen='Y';
                scRoDt=%Char(date);
             When CHocst='C';
                scClDt=%Char(date);
             EndSl;
          ReadE keyWMCHL pch00;
          EndDo;
       EndIf;

       //*** Beg Add *** 02/12/14 ****************************************
       If outClientID = '114';
          isCSI = *On;
          intDate  = *Hival;
          charCo#  = %EditC(CMco#:'X');
          charFnd  = %EditC(CMfnd:'X');
          charFyr  = %EditC(CMfyr:'X');
          charCase = %EditC(CMcase:'X');

          // Get the rrn of FMCMP from corresponding WMCMP
          GetRrn(charCo#:charFnd:charFyr:charCase:intRRN1:charRtcd);

          If charRtcd <> 'F';
             Chain intRRN1 pcm0f;
             If %Found(FMCMP);
                Setll key1FAWHL pwh0f;
                Reade key2FAWHL pwh0f;
                If not %Eof(FAWHL);
                   scDutyHdg = 'Duty Status';
                   scDutySts = *Blanks;
                   If Whlvl <> *blanks;
                      charKey = WHlvl;
                      Chain keyFTDLL pdl00;
                      If %Found(FTDLL);
                         scDutySts = DLdesc;
                      Endif;
                   Endif;
                EndIf;
             EndIf;

          Endif;
       Else;
          isCSI = *Off;
       EndIf;
       //*** End Add *** 02/12/14 ****************************************
       //
       // Load  Special Attributes
       //
       LoadSpecAtt();

       //*** Beg Add *** 07/19/16 ****************************************
       LodISOScore();
       //*** End Add *** 07/19/16 ****************************************
       //
       // Load Reserves
       //
       LoadReserves();

      /End-Free
     P MoveDbSc        E
       //===================================================================
       // MoveScDb - Move Screen fields to Database fields
       //===================================================================
     P MoveScDb        B
     D MoveScDb        PI
      /Free


      /End-Free
     P MoveScDb        E
       //===================================================================
       // SaveNotes - Save Notes Data
       //===================================================================
     P SaveNotes       B
     D SaveNotes       PI
     D i               s              5  0
      /Free

       ReadC sflEdtNote;
       DoU %Eof;
          kkSeq#=scSeq#;
          Chain key1WMSIL psi00;
          If %Found(wmsil);
             SIcomm=scComm;
             If SIcomm <> *Blanks;
                Update psi00;
             Else;
                Delete psi00;
             EndIf;
          Else;
             If scComm<>*Blanks;
                WriteNotes();
             EndIf;
          EndIf;
          ReadC sflEdtNote;
       EndDo;

      /End-Free
     P SaveNotes       E
       //===================================================================
       // UpdateRecord - Update DataBase
       //===================================================================
     P UpdateRecord    B
     D UpdateRecord    PI
      /Free
       Chain inRRNClm dbrcdf;
       If %Found;
          MoveScDb();
          Update    dbrcdf;
       EndIf;
      /End-Free
     P UpdateRecord    E
       //===================================================================
       // WriteNotes - Writes Claim Notes
       //===================================================================
     P WriteNotes      B
     D WriteNotes      PI
      /Free
       Clear PSI00;
       SIco#=CMco#;
       SIfnd=CMfnd;
       SIfYr=CMfYr;
       SIcase=CMcase;
       SIcomm=scComm;
       SIseq#=scSeq#;
       SIstat='A';
       SIuser=q1User;
       SIpgm=q1Pgm;
       SItime=TimeTo6(SystemTime());
       SIdt=DateTo7(SystemDate());
       SIiUsr=q1User;
       SIiPgm=q1Pgm;
       SIiTm=TimeTo6(SystemTime());
       SIiDt=DateTo7(SystemDate());
       Write PSI00;
      /End-Free
     P WriteNotes      E
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy sCopyClmPi
