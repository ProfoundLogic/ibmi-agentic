      * (c) 2021 Sapiens. All Rights Reserved.                             *
     h**********************************************************************
     h/title WTRPTVSTP - Visit Tracking Report - Prompt
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Prompt for Information that will be
     h*                used to generate a report that lists visits.
     h*
     h*  Called from: Menu
     h*  Calls to:    WTRPTCVTS
     h*
     h**********************************************************************
     h*  Created on 12/29/2014 by C:062 for Project: 114WGU
     h**********************************************************************
     h*  Revisions:
     h*
     h*  06/21/21 C:298 Project: SY426
     h*                 Re-Brand and Config for Visit Tracking
     H*
     h*  03/22/17 C:053 Support: 46241
     h*                 Recompile over changed copy module
      *                                                               *
      *    01/07/16 C:65  Project: WC2401                             *
      *                   Recompile Only                              *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Recompile Only                              *
     h*
     h*  05/01/15  C:65  Project: SY251
     h*                  Restrict action panel to rcdfmt being used.
     h*
     h**********************************************************************
     fwtrptvstp cf   e             workstn extfile('WTRPTVSTP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fsmcol     if   e           k disk    rename(pco01:pco00)
     fsmupl     if   e           k disk    rename(pup01:pup00)
     fwdelp     if   e             Disk    ReName(pel01:pel0p) usropn
     fwmeml     if   e           k Disk    ReName(pem01:pem00) usropn
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     fwmagp     if   e             Disk    ReName(pag01:pag0p)
     fwmaal     if   e           k Disk    ReName(paa01:paa00)
     fwtvil     if   e           k Disk    ReName(pvi01:pvi00)
     fwsvsp     o    e             Disk    ReName(pvs01:pvs0p)

     d psds          esds                  extname(szq1p)

      * constant
     d intFlgDim       c                   10
     d clientID        s              3    inz('000')

     d* Check to see if submitted
     d ChkSubmitted    pr                  ExtPgm('WTRPTVSTD')
     D  callIn                        1
     D  modeIn                        1

     d* Claim Search
     d ClmSearch       pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     d* Clear Error Indicators
     d ClearErrors     pr

     d* Delete Employer Overrides and Close files
     d DelOverrideEmp  pr
     d  quoteType                     1

     d* Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     d* Error Check
     d ErrorCheck      pr              n

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Get client Suffix
     d GetCltSuffix    pr                  extpgm('SA030R')
     d ##Suffix                       3

     d* Group Select
     d GroupSelect     pr                  extpgm('W7944C')
     d ##ProgramID                   10    const

     d* Main Processing
     d Main            pr

     d* Setup Employer Overrides and Open files
     d OverrideEmp     pr
     d  quoteType                     1

     d* Position Fields
     d PositionFields  pr

     d* Program Start
     d ProgramStart    pr

     d QCmdExc         pr                  ExtPgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const

     d* Retrieve Libraires
     d RtvClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Agent Search
     d SearchAgt#      pr

     d* Claim Search
     d SearchClm#      pr

     d* Employer Search
     d SearchEmp#      pr

     d* Sets Conditions
     d SetConditions   pr

     d* Submitter
     d Submitter       pr                  ExtPgm('WTRPTVSTS')
     D companyIn                      3
     D vstTypeIn                     41
     D vstCatIn                      10
     D vstAssgnIn                    10
     D vstStatusIn                    1
     D vstFrDtIn                      8
     D vstToDtIn                      8
     D cmpFrDtIn                      8
     D cmpToDtIn                      8
     D cmpNoDateIn                    1
     D quoteFlagIn                    1
     D outTypeIn                      1
     D agtRrnIn                       9
     D clmRrnIn                       9
     D empRrnIn                       9
     D locationIn                    10
     D useGrpSel                      1
     D visiyKey                      20

     d* Check to see if group select will be used
     d UseGroupSel     pr              n

     d* Write Selection Criteria
     d WriteSelect     pr

     d/copy sPrModule
     d/copy wPrAgent
     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany
     d/copy wPrUsrPrms
     d/copy wPrEmplr
     d/copy wtSearchDs

     D intCo#          s              3  0
     d ##CltId         s              3
     d hdVstCat        s                   Like(scVstCat)
     d hdVstType       s                   Like(scVstType)
     d strCommand      s            255

     D* Visit Tracking Database Fields
     D  visitFieldsDs  ds
     D  VIdtHd1
     D  VIdtHd2
     D  VIdtHd3
     D  VIdtHd4
     D  VIdtHd5
     D  VIdtHd6
     D  VIdtHd7
     D  VIdtHd8
     D  VIdtHd9
     D  VIdtHd10
     D  VIcbHd1
     D  VIcbHd2
     D  VIcbHd3
     D  VIcbHd4
     D  VIcbHd5
     D  VIcbHd6
     D  VIcbHd7
     D  VIcbHd8
     D  VIcbHd9
     D  VIcbHd10
     D  VIinHd1
     D  VIinHd2
     D  VIinHd3
     D  VIinHd4
     D  VIinHd5
     D  VIinHd6
     D  VIinHd7
     D  VIinHd8
     D  VIinHd9
     D  VIinHd10
     D  VIdcHd1
     D  VIdcHd2
     D  VIdcHd3
     D  VIdcHd4
     D  VIdcHd5
     D  VIdcHd6
     D  VIdcHd7
     D  VIdcHd8
     D  VIdcHd9
     D  VIdcHd10
     D  VItfHd1
     D  VItfHd2
     D  VItfHd3
     D  VItfHd4
     D  VItfHd5
     D  VItfHd6
     D  VItfHd7
     D  VItfHd8
     D  VItfHd9
     D  VItfHd10
     D  visitFields                  20    Overlay(visitFieldsDs) Dim(50)

     D* Screen Labels aka Headers
     D  visitFldLds    ds
     D  scDte01L
     D  scDte02L
     D  scDte03L
     D  scDte04L
     D  scDte05L
     D  scDte06L
     D  scDte07L
     D  scDte08L
     D  scDte09L
     D  scDte10L
     D  scChk01L
     D  scChk02L
     D  scChk03L
     D  scChk04L
     D  scChk05L
     D  scChk06L
     D  scChk07L
     D  scChk08L
     D  scChk09L
     D  scChk10L
     D  scItg01L
     D  scItg02L
     D  scItg03L
     D  scItg04L
     D  scItg05L
     D  scItg06L
     D  scItg07L
     D  scItg08L
     D  scItg09L
     D  scItg10L
     D  scDec01L
     D  scDec02L
     D  scDec03L
     D  scDec04L
     D  scDec05L
     D  scDec06L
     D  scDec07L
     D  scDec08L
     D  scDec09L
     D  scDec10L
     D  scTxt01L
     D  scTxt02L
     D  scTxt03L
     D  scTxt04L
     D  scTxt05L
     D  scTxt06L
     D  scTxt07L
     D  scTxt08L
     D  scTxt09L
     D  scTxt10L
     D  visitFldL                    20    Overlay(visitFldLds) Dim(50)

     D* Screen From Date Values
     D  vstDateFVds    ds                  Inz
     D  scDte01FV
     D  scDte02FV
     D  scDte03FV
     D  scDte04FV
     D  scDte05FV
     D  scDte06FV
     D  scDte07FV
     D  scDte08FV
     D  scDte09FV
     D  scDte10FV
     D  vstDateFV                     8  0 Overlay(vstDateFVds) Dim(10)

     D* Screen From Date Error Message
     D  vstDateFMds    ds                  Inz
     D  mgDte01FV
     D  mgDte02FV
     D  mgDte03FV
     D  mgDte04FV
     D  mgDte05FV
     D  mgDte06FV
     D  mgDte07FV
     D  mgDte08FV
     D  mgDte09FV
     D  mgDte10FV
     D  vstDateFM                   100    Overlay(vstDateFMds) Dim(10)

     D* Screen From Date Error Indicator
     D  vstDateFEds    ds                  Inz
     D  erDte01FV
     D  erDte02FV
     D  erDte03FV
     D  erDte04FV
     D  erDte05FV
     D  erDte06FV
     D  erDte07FV
     D  erDte08FV
     D  erDte09FV
     D  erDte10FV
     D  vstDateFE                      n   Overlay(vstDateFEds) Dim(10)

     D* Screen To Date Values
     D  vstDateTVds    ds                  Inz
     D  scDte01TV
     D  scDte02TV
     D  scDte03TV
     D  scDte04TV
     D  scDte05TV
     D  scDte06TV
     D  scDte07TV
     D  scDte08TV
     D  scDte09TV
     D  scDte10TV
     D  vstDateTV                     8  0 Overlay(vstDateTVds) Dim(10)

     D* Screen To Date Error Message
     D  vstDateTMds    ds                  Inz
     D  mgDte01TV
     D  mgDte02TV
     D  mgDte03TV
     D  mgDte04TV
     D  mgDte05TV
     D  mgDte06TV
     D  mgDte07TV
     D  mgDte08TV
     D  mgDte09TV
     D  mgDte10TV
     D  vstDateTM                   100    Overlay(vstDateTMds) Dim(10)

     D* Screen To Date Error Indicator
     D  vstDateTEds    ds                  Inz
     D  erDte01TV
     D  erDte02TV
     D  erDte03TV
     D  erDte04TV
     D  erDte05TV
     D  erDte06TV
     D  erDte07TV
     D  erDte08TV
     D  erDte09TV
     D  erDte10TV
     D  vstDateTE                      n   Overlay(vstDateTEds) Dim(10)

     D* Screen Date Disabled Fields
     D  vstDateDSds    ds                  Inz
     D  scDte01D
     D  scDte02D
     D  scDte03D
     D  scDte04D
     D  scDte05D
     D  scDte06D
     D  scDte07D
     D  scDte08D
     D  scDte09D
     D  scDte10D
     D  vstDateDS                      n   Overlay(vstDateDSds) Dim(10)

     D* Screen No Date
     D  vstDateNDds    ds                  Inz
     D  scDateND1
     D  scDateND2
     D  scDateND3
     D  scDateND4
     D  scDateND5
     D  scDateND6
     D  scDateND7
     D  scDateND8
     D  scDateND9
     D  scDateND10
     D  vstDateND                     1    Overlay(vstDateNDds) Dim(10)

     D* Screen From Decimal Values
     D  vstDecFVds     ds                  Inz
     D  scDec01FV
     D  scDec02FV
     D  scDec03FV
     D  scDec04FV
     D  scDec05FV
     D  scDec06FV
     D  scDec07FV
     D  scDec08FV
     D  scDec09FV
     D  scDec10FV
     D  vstDecFV                     20  6 Overlay(vstDecFVds) Dim(10)

     D* Screen From Decimal Error Message
     D  vstDecFMds     ds                  Inz
     D  mgDec01FV
     D  mgDec02FV
     D  mgDec03FV
     D  mgDec04FV
     D  mgDec05FV
     D  mgDec06FV
     D  mgDec07FV
     D  mgDec08FV
     D  mgDec09FV
     D  mgDec10FV
     D  vstDecFM                    100    Overlay(vstDecFMds) Dim(10)

     D* Screen From Decimal Error Indicator
     D  vstDecFEds     ds                  Inz
     D  erDec01FV
     D  erDec02FV
     D  erDec03FV
     D  erDec04FV
     D  erDec05FV
     D  erDec06FV
     D  erDec07FV
     D  erDec08FV
     D  erDec09FV
     D  erDec10FV
     D  vstDecFE                       n   Overlay(vstDecFEds) Dim(10)

     D* Screen To Decimal Values
     D  vstDecTVds     ds                  Inz
     D  scDec01TV
     D  scDec02TV
     D  scDec03TV
     D  scDec04TV
     D  scDec05TV
     D  scDec06TV
     D  scDec07TV
     D  scDec08TV
     D  scDec09TV
     D  scDec10TV
     D  vstDecTV                     20  6 Overlay(vstDecTVds) Dim(10)

     D* Screen To Decimal Error Message
     D  vstDecTMds     ds                  Inz
     D  mgDec01TV
     D  mgDec02TV
     D  mgDec03TV
     D  mgDec04TV
     D  mgDec05TV
     D  mgDec06TV
     D  mgDec07TV
     D  mgDec08TV
     D  mgDec09TV
     D  mgDec10TV
     D  vstDecTM                    100    Overlay(vstDecTMds) Dim(10)

     D* Screen To Decimal Error Indicator
     D  vstDecTEds     ds                  Inz
     D  erDec01TV
     D  erDec02TV
     D  erDec03TV
     D  erDec04TV
     D  erDec05TV
     D  erDec06TV
     D  erDec07TV
     D  erDec08TV
     D  erDec09TV
     D  erDec10TV
     D  vstDecTE                       n   Overlay(vstDecTEds) Dim(10)

     D* Screen Decimal Disabled Fields
     D  vstDecDSds     ds                  Inz
     D  scDec01D
     D  scDec02D
     D  scDec03D
     D  scDec04D
     D  scDec05D
     D  scDec06D
     D  scDec07D
     D  scDec08D
     D  scDec09D
     D  scDec10D
     D  vstDecDS                       n   Overlay(vstDecDSds) Dim(10)

     D* Screen No Value for Decimal Fields
     D  vstDecNVds     ds                  Inz
     D  scDecNV1
     D  scDecNV2
     D  scDecNV3
     D  scDecNV4
     D  scDecNV5
     D  scDecNV6
     D  scDecNV7
     D  scDecNV8
     D  scDecNV9
     D  scDecNV10
     D  vstDecNV                      1    Overlay(vstDecNVds) Dim(10)

     D* Screen From Integer Values
     D  vstIntFVds     ds                  Inz
     D  scItg01FV
     D  scItg02FV
     D  scItg03FV
     D  scItg04FV
     D  scItg05FV
     D  scItg06FV
     D  scItg07FV
     D  scItg08FV
     D  scItg09FV
     D  scItg10FV
     D  vstIntFV                     20  0 Overlay(vstIntFVds) Dim(10)

     D* Screen From Integer Error Message
     D  vstIntFMds     ds                  Inz
     D  mgItg01FV
     D  mgItg02FV
     D  mgItg03FV
     D  mgItg04FV
     D  mgItg05FV
     D  mgItg06FV
     D  mgItg07FV
     D  mgItg08FV
     D  mgItg09FV
     D  mgItg10FV
     D  vstIntFM                    100    Overlay(vstIntFMds) Dim(10)

     D* Screen From Integer Error Indicator
     D  vstIntFEds     ds                  Inz
     D  erItg01FV
     D  erItg02FV
     D  erItg03FV
     D  erItg04FV
     D  erItg05FV
     D  erItg06FV
     D  erItg07FV
     D  erItg08FV
     D  erItg09FV
     D  erItg10FV
     D  vstIntFE                       n   Overlay(vstIntFEds) Dim(10)

     D* Screen To Integer Values
     D  vstIntTVds     ds                  Inz
     D  scItg01TV
     D  scItg02TV
     D  scItg03TV
     D  scItg04TV
     D  scItg05TV
     D  scItg06TV
     D  scItg07TV
     D  scItg08TV
     D  scItg09TV
     D  scItg10TV
     D  vstIntTV                     20  0 Overlay(vstIntTVds) Dim(10)

     D* Screen To Integer Error Message
     D  vstIntTMds     ds                  Inz
     D  mgItg01TV
     D  mgItg02TV
     D  mgItg03TV
     D  mgItg04TV
     D  mgItg05TV
     D  mgItg06TV
     D  mgItg07TV
     D  mgItg08TV
     D  mgItg09TV
     D  mgItg10TV
     D  vstIntTM                    100    Overlay(vstIntTMds) Dim(10)

     D* Screen To Integer Error Indicator
     D  vstIntTEds     ds                  Inz
     D  erItg01TV
     D  erItg02TV
     D  erItg03TV
     D  erItg04TV
     D  erItg05TV
     D  erItg06TV
     D  erItg07TV
     D  erItg08TV
     D  erItg09TV
     D  erItg10TV
     D  vstIntTE                       n   Overlay(vstIntTEds) Dim(10)

     D* Screen Integer Disabled Fields
     D  vstIntDSds     ds                  Inz
     D  scItg01D
     D  scItg02D
     D  scItg03D
     D  scItg04D
     D  scItg05D
     D  scItg06D
     D  scItg07D
     D  scItg08D
     D  scItg09D
     D  scItg10D
     D  vstIntDS                       n   Overlay(vstIntDSds) Dim(10)

     D* Screen No Value for Integer Fields
     D  vstIntNVds     ds                  Inz
     D  scIntNV1
     D  scIntNV2
     D  scIntNV3
     D  scIntNV4
     D  scIntNV5
     D  scIntNV6
     D  scIntNV7
     D  scIntNV8
     D  scIntNV9
     D  scIntNV10
     D  vstIntNV                      1    Overlay(vstIntNVds) Dim(10)

     D* Screen Text Values
     D  vstTxtVds      ds                  Inz
     D  scTxt01V
     D  scTxt02V
     D  scTxt03V
     D  scTxt04V
     D  scTxt05V
     D  scTxt06V
     D  scTxt07V
     D  scTxt08V
     D  scTxt09V
     D  scTxt10V
     D  vstTxtV                     100    Overlay(vstTxtVds) Dim(10)

     D* Screen Check Box Disable
     D  vstChkVds      ds                  Inz
     D  scChk01V
     D  scChk02V
     D  scChk03V
     D  scChk04V
     D  scChk05V
     D  scChk06V
     D  scChk07V
     D  scChk08V
     D  scChk09V
     D  scChk10V
     D  vstChkV                       1    Overlay(vstChkVds) Dim(10)

     D* Screen Check BoxValues
     D  vstChkDSds     ds                  Inz
     D  scChk01D
     D  scChk02D
     D  scChk03D
     D  scChk04D
     D  scChk05D
     D  scChk06D
     D  scChk07D
     D  scChk08D
     D  scChk09D
     D  scChk10D
     D  vstChkDS                       n   Overlay(vstChkDSds) Dim(10)

     D* Screen No Value for Check Box Fields
     D  vstChkNVds     ds                  Inz
     D  scChkNV1
     D  scChkNV2
     D  scChkNV3
     D  scChkNV4
     D  scChkNV5
     D  scChkNV6
     D  scChkNV7
     D  scChkNV8
     D  scChkNV9
     D  scChkNV10
     D  vstChkNV                      1    Overlay(vstChkNVds) Dim(10)

     D* Layout X position
     D  visitFldXds    ds
     D  scDte01X
     D  scDte02X
     D  scDte03X
     D  scDte04X
     D  scDte05X
     D  scDte06X
     D  scDte07X
     D  scDte08X
     D  scDte09X
     D  scDte10X
     D  scChk01X
     D  scChk02X
     D  scChk03X
     D  scChk04X
     D  scChk05X
     D  scChk06X
     D  scChk07X
     D  scChk08X
     D  scChk09X
     D  scChk10X
     D  scItg01X
     D  scItg02X
     D  scItg03X
     D  scItg04X
     D  scItg05X
     D  scItg06X
     D  scItg07X
     D  scItg08X
     D  scItg09X
     D  scItg10X
     D  scDec01X
     D  scDec02X
     D  scDec03X
     D  scDec04X
     D  scDec05X
     D  scDec06X
     D  scDec07X
     D  scDec08X
     D  scDec09X
     D  scDec10X
     D  scTxt01X
     D  scTxt02X
     D  scTxt03X
     D  scTxt04X
     D  scTxt05X
     D  scTxt06X
     D  scTxt07X
     D  scTxt08X
     D  scTxt09X
     D  scTxt10X
     D  visitFldX                     5  0 Overlay(visitFldXds) Dim(50)

     D* Layout Y position
     D  visitFldYds    ds
     D  scDte01Y
     D  scDte02Y
     D  scDte03Y
     D  scDte04Y
     D  scDte05Y
     D  scDte06Y
     D  scDte07Y
     D  scDte08Y
     D  scDte09Y
     D  scDte10Y
     D  scChk01Y
     D  scChk02Y
     D  scChk03Y
     D  scChk04Y
     D  scChk05Y
     D  scChk06Y
     D  scChk07Y
     D  scChk08Y
     D  scChk09Y
     D  scChk10Y
     D  scItg01Y
     D  scItg02Y
     D  scItg03Y
     D  scItg04Y
     D  scItg05Y
     D  scItg06Y
     D  scItg07Y
     D  scItg08Y
     D  scItg09Y
     D  scItg10Y
     D  scDec01Y
     D  scDec02Y
     D  scDec03Y
     D  scDec04Y
     D  scDec05Y
     D  scDec06Y
     D  scDec07Y
     D  scDec08Y
     D  scDec09Y
     D  scDec10Y
     D  scTxt01Y
     D  scTxt02Y
     D  scTxt03Y
     D  scTxt04Y
     D  scTxt05Y
     D  scTxt06Y
     D  scTxt07Y
     D  scTxt08Y
     D  scTxt09Y
     D  scTxt10Y
     D  visitFldY                     5  0 Overlay(visitFldYds) Dim(50)

     D* Layout Visibility
     D  visitFldVds    ds
     D  scDte01V
     D  scDte02V
     D  scDte03V
     D  scDte04V
     D  scDte05V
     D  scDte06V
     D  scDte07V
     D  scDte08V
     D  scDte09V
     D  scDte10V
     D  scChk01Vs
     D  scChk02Vs
     D  scChk03Vs
     D  scChk04Vs
     D  scChk05Vs
     D  scChk06Vs
     D  scChk07Vs
     D  scChk08Vs
     D  scChk09Vs
     D  scChk10Vs
     D  scItg01V
     D  scItg02V
     D  scItg03V
     D  scItg04V
     D  scItg05V
     D  scItg06V
     D  scItg07V
     D  scItg08V
     D  scItg09V
     D  scItg10V
     D  scDec01V
     D  scDec02V
     D  scDec03V
     D  scDec04V
     D  scDec05V
     D  scDec06V
     D  scDec07V
     D  scDec08V
     D  scDec09V
     D  scDec10V
     D  scTxt01Vs
     D  scTxt02Vs
     D  scTxt03Vs
     D  scTxt04Vs
     D  scTxt05Vs
     D  scTxt06Vs
     D  scTxt07Vs
     D  scTxt08Vs
     D  scTxt09Vs
     D  scTxt10Vs
     D  visitFldV                      n   Overlay(visitFldVds) Dim(50)

     D* From Date Selection File
     D  vstDteFFds     ds                  Inz
     D  vsFDt01
     D  vsFDt02
     D  vsFDt03
     D  vsFDt04
     D  vsFDt05
     D  vsFDt06
     D  vsFDt07
     D  vsFDt08
     D  vsFDt09
     D  vsFDt10
     D  vstDteFF                      7  0 Overlay(vstDteFFds) Dim(10)

     D* To Date Selection File
     D  vstDteTFds     ds                  Inz
     D  vsTDt01
     D  vsTDt02
     D  vsTDt03
     D  vsTDt04
     D  vsTDt05
     D  vsTDt06
     D  vsTDt07
     D  vsTDt08
     D  vsTDt09
     D  vsTDt10
     D  vstDteTF                      7  0 Overlay(vstDteTFds) Dim(10)

     D* No Date Selection File
     D  vstDteNFds     ds                  Inz
     D  vsNDt01
     D  vsNDt02
     D  vsNDt03
     D  vsNDt04
     D  vsNDt05
     D  vsNDt06
     D  vsNDt07
     D  vsNDt08
     D  vsNDt09
     D  vsNDt10
     D  vstDteNF                      1    Overlay(vstDteNFds) Dim(10)

     D* From Decimal Selection File
     D  vstDecFFds     ds                  Inz
     D  vsFDe01
     D  vsFDe02
     D  vsFDe03
     D  vsFDe04
     D  vsFDe05
     D  vsFDe06
     D  vsFDe07
     D  vsFDe08
     D  vsFDe09
     D  vsFDe10
     D  vstDecFF                     20  6 Overlay(vstDecFFds) Dim(10)

     D* To Decimal Selection File
     D  vstDecTFds     ds                  Inz
     D  vsTDe01
     D  vsTDe02
     D  vsTDe03
     D  vsTDe04
     D  vsTDe05
     D  vsTDe06
     D  vsTDe07
     D  vsTDe08
     D  vsTDe09
     D  vsTDe10
     D  vstDecTF                     20  6 Overlay(vstDecTFds) Dim(10)

     D* No Decimal Selection File
     D  vstDecNFds     ds                  Inz
     D  vsNDe01
     D  vsNDe02
     D  vsNDe03
     D  vsNDe04
     D  vsNDe05
     D  vsNDe06
     D  vsNDe07
     D  vsNDe08
     D  vsNDe09
     D  vsNDe10
     D  vstDecNF                      1    Overlay(vstDecNFds) Dim(10)

     D* From Integer Selection File
     D  vstIntFFds     ds                  Inz
     D  vsFIn01
     D  vsFIn02
     D  vsFIn03
     D  vsFIn04
     D  vsFIn05
     D  vsFIn06
     D  vsFIn07
     D  vsFIn08
     D  vsFIn09
     D  vsFIn10
     D  vstIntFF                     20  0 Overlay(vstIntFFds) Dim(10)

     D* To Integer Selection File
     D  vstIntTFds     ds                  Inz
     D  vsTIn01
     D  vsTIn02
     D  vsTIn03
     D  vsTIn04
     D  vsTIn05
     D  vsTIn06
     D  vsTIn07
     D  vsTIn08
     D  vsTIn09
     D  vsTIn10
     D  vstIntTF                     20  0 Overlay(vstIntTFds) Dim(10)

     D* No Integer Selection File
     D  vstIntNFds     ds                  Inz
     D  vsNIn01
     D  vsNIn02
     D  vsNIn03
     D  vsNIn04
     D  vsNIn05
     D  vsNIn06
     D  vsNIn07
     D  vsNIn08
     D  vsNIn09
     D  vsNIn10
     D  vstIntNF                      1    Overlay(vstIntNFds) Dim(10)

     D* To Check Box Selection File
     D  vstChkFds      ds                  Inz
     D  vsCb01
     D  vsCb02
     D  vsCb03
     D  vsCb04
     D  vsCb05
     D  vsCb06
     D  vsCb07
     D  vsCb08
     D  vsCb09
     D  vsCb10
     D  vstChkF                       1    Overlay(vstChkFds) Dim(10)

     D* No Check Box Selection File
     D  vstChkNFds     ds                  Inz
     D  vsNCb01
     D  vsNCb02
     D  vsNCb03
     D  vsNCb04
     D  vsNCb05
     D  vsNCb06
     D  vsNCb07
     D  vsNCb08
     D  vsNCb09
     D  vsNCb10
     D  vstChkNF                      1    Overlay(vstChkNFds) Dim(10)

     D* To Text Box Selection File
     D  vstTxtFds      ds                  Inz
     D  vsTf01
     D  vsTf02
     D  vsTf03
     D  vsTf04
     D  vsTf05
     D  vsTf06
     D  vsTf07
     D  vsTf08
     D  vsTf09
     D  vsTf10
     D  vstTxtF                     100    Overlay(vstTxtFds) Dim(10)

      /free

       // Is visit tracking installed?
       If isModEnabled('VISIT TRACKING') = *Off;
          scTPnlHgt = 810;
          APnlHeight = 870;
          clear scCompName;
          intCo# = userCo#(q1user);
          // Get company name
          scCompName = GetCompNam(intCo#);
          Write rcdComPnl;
          Write rcdVstTrk;
          ExFmt rcdVstTrkE;
          Read rcdComPnl;
          Read rcdVstTrk;
       Else;
          // Main Procesing
          Main();
       Endif;

       *inlr = *on;

      /end-free
       //===================================================================
       // ClearErrors - Clears Error Indicators
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi

      /free

       Clear mgVstType;
       Clear erVstType;
       Clear mgVstCat;
       Clear erVstCat;
       Clear mgVstAssgn;
       Clear erVstAssgn;
       Clear mgVstFrDt;
       Clear erVstFrDt;
       Clear mgVstToDt;
       Clear erVstToDt;
       Clear mgCmpFrDt;
       Clear erCmpFrDt;
       Clear mgCmpToDt;
       Clear erCmpToDt;
       Clear mgAgtName;
       Clear erAgtName;
       Clear mgEmpDivNm;
       Clear erEmpDivNm;
       Clear mgClmFnm;
       Clear erClmFnm;
       Clear mgClmLnm;
       Clear erClmLnm;
       Clear vstDateFMds;
       Clear vstDateFEds;
       Clear vstDateTMds;
       Clear vstDateTEds;
       Clear vstDecFMds;
       Clear vstDecFEds;
       Clear vstDecTMds;
       Clear vstDecTEds;
       Clear vstIntFMds;
       Clear vstIntFEds;
       Clear vstIntTMds;
       Clear vstIntTEds;

      /end-free

     p ClearErrors     e
       //===================================================================
       // DelOverrideEmp - Delete Employer Overrdes and Close Files
       //===================================================================
     p DelOverrideEmp  b
     d DelOverrideEmp  pi
     d  quoteType                     1

     d mode            s              1

      /free

       // Close Employer Files
       If %open(WDELP);
          Close WDELP;
       EndIf;

       If %open(WMEML);
          Close WMEML;
       EndIf;

       // Delete Employer File Overrides If Quote Employer
       If mode = 'Q';
          strCommand = 'DLTOVR FILE(WDELP)';
          Callp QCmdExc(strCommand:%len(strCommand));

          strCommand = 'DLTOVR FILE(WMEML)';
          Callp QCmdExc(strCommand:%len(strCommand));
       Endif;

      /end-free

     p DelOverrideEmp  e
       //===================================================================
       // ErrorCheck - Error Check
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     D i               s              3  0
     D error           s               n   Inz(*Off)

      /free

       // Visit Type is Required
       If scVstType = *Blanks;
          mgVstType = 'This field can not be blank.';
          erVstType = *On;
          error = *On;
       Else;
          erVstType = *Off;
       EndIf;

       // Visit Category is Required
       If scVstCat = *Blanks;
          mgVstCat = 'This field can not be blank.';
          erVstCat = *On;
          error = *On;
       Else;
          erVstCat = *Off;
       EndIf;

       // Visit Assignment Must be Valid if Entered
       If scVstAssgn <> ' ';
          Chain scVstAssgn pup00;
          If not %Found(SMUPL) or (%Found(SMUPL) And UPstat='I');
             mgVstAssgn = 'Invalid Staff Code.';
             erVstAssgn = *On;
             error = *On;
          Else;
             erVstAssgn = *Off;
          EndIf;
       Endif;

       If scVstFrDt<>0 Or scVstToDt<>0;
          // Visit From date must be valid.
          If dtCheck(scVstFrDt)=0;
             mgVstFrDt = 'Invalid date entered.';
             erVstFrDt = *On;
             error = *On;
          Else;
             erVstFrDt = *Off;
          EndIf;

          // Visit To date must be valid.
          If dtCheck(scVstToDt)=0;
             mgVstToDt = 'Invalid date entered.';
             erVstToDt = *On;
             error = *On;
          Else;
             erVstToDt = *Off;
          EndIf;

          // If dates entered then thye must be in ascending order
          If erVstFrDt = *Off And erVstToDt = *Off;
             If dtCYMD(scVstToDt) < dtCYMD(scVstFrDt);
                mgVstFrDt = 'Dates must be in ascending order.';
                erVstFrDt = *On;
                mgVstToDt = 'Dates must be in ascending order.';
                erVstToDt = *On;
                error = *On;
             Else;
                erVstFrDt = *Off;
                erVstToDt = *Off;
             EndIf;
          EndIf;
       EndIf;

       If scCmpFrDt<>0 Or scCmpToDt<>0;
          // Completion From date must be valid.
          If dtCheck(scCmpFrDt)=0;
             mgCmpFrDt = 'Invalid date entered.';
             erCmpFrDt = *On;
             error = *On;
          Else;
             erCmpFrDt = *Off;
          EndIf;

          // Completion To date must be valid.
          If dtCheck(scCmpToDt)=0;
             mgCmpToDt = 'Invalid date entered.';
             erCmpToDt = *On;
             error = *On;
          Else;
             erCmpToDt = *Off;
          EndIf;

          // If dates entered then thye must be in ascending order
          If erCmpFrDt = *Off And erCmpToDt = *Off;
             If dtCYMD(scCmpToDt) < dtCYMD(scCmpFrDt);
                mgCmpFrDt = 'Dates must be in ascending order.';
                erCmpFrDt = *On;
                mgCmpToDt = 'Dates must be in ascending order.';
                erCmpToDt = *On;
                error = *On;
             Else;
                erCmpFrDt = *Off;
                erCmpToDt = *Off;
             EndIf;
          EndIf;
       EndIf;

       // Agency must be selected if name entered
       If scAgtName <> ' ' And scAgtFein = ' ';
          mgAgtName = 'Agency must be selected.  Please use search.';
          erAgtName = *On;
          error = *on;
       Else;
          erAgtName = *Off;
       EndIf;

       // Claim must be selected if name entered
       If (scClmLnm <> ' ' Or scClmFnm <> ' ') And scClaim# = ' ';
          mgClmLnm = 'Claim must be selected.  Please use search.';
          erClmLnm = *On;
          mgClmFnm = 'Claim must be selected.  Please use search.';
          erClmFnm = *On;
          error = *on;
       Else;
          erClmLnm = *Off;
          erClmFnm = *Off;
       EndIf;

       // Employer must be selected if name entered
       If scEmpDivNm <> ' ' And scEmpDiv = ' ';
          mgEmpDivNm = 'Employer must be selected.  Please use search.';
          erEmpDivNm = *On;
          error = *on;
       Else;
          erEmpDivNm = *Off;
       EndIf;

       // Validate From Date Fields
       For i=1 to 10;

          If vstDateFV(i)<>0 Or vstDateTV(i)<>0;
             // From date must be valid.
             If dtCheck(vstDateFV(i))=0;
                vstDateFM(i) = 'Invalid date entered.';
                vstDateFE(i) = *On;
                error = *On;
             EndIf;

             // To date must be valid.
             If dtCheck(vstDateTV(i))=0;
                vstDateTM(i) = 'Invalid date entered.';
                vstDateTE(i) = *On;
                error = *On;
             EndIf;

             // If dates entered then thye must be in ascending order
             If vstDateFE(i) = *Off And vstDateTE(i) = *Off;
                If dtCYMD(vstDateTV(i)) < dtCYMD(vstDateFV(i));
                   vstDateFM(i) = 'Dates must be in ascending order.';
                   vstDateFE(i) = *On;
                   vstDateTM(i) = 'Dates must be in ascending order.';
                   vstDateTE(i) = *On;
                   error = *On;
                EndIf;
             EndIf;
          EndIf;
       EndFor;

       // Decimal From and To values must be in ascending order
       For i=1 to 10;
          If vstDecFV(i)<>0 Or vstDecTV(i)<>0;
             If vstDecTV(i) < vstDecFV(i);
                vstDecFM(i) = 'Numbers must be in ascending order.';
                vstDecFE(i) = *On;
                vstDecTM(i) = 'Numbers must be in ascending order.';
                vstDecTE(i) = *On;
                error = *On;
             EndIf;
          EndIf;
       EndFor;

       // Integer From and To values must be in ascending order
       For i=1 to 10;
          If vstIntFV(i)<>0 Or vstIntTV(i)<>0;
             If vstIntTV(i) < vstIntFV(i);
                vstIntFM(i) = 'Numbers must be in ascending order.';
                vstIntFE(i) = *On;
                vstIntTM(i) = 'Numbers must be in ascending order.';
                vstIntTE(i) = *On;
                error = *On;
             EndIf;
          EndIf;
       EndFor;

       Return error;

      /end-free

     p ErrorCheck      e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     D error           s               n   Inz(*Off)
     D company         s              3
     D visitType       s             41
     D vstFrDt         s              8
     D vstToDt         s              8
     D cmpFrDt         s              8
     D cmpToDt         s              8
     D qteFlag         s              1
     D agtRrn          s              9
     D clmRrn          s              9
     D empRrn          s              9
     d grpSelect       s               n   Inz(*Off)
     d useGrpSel       s              1
     d callIn          s             40    Inz('C')
     d returnCodeOut   s              1

      /free

       // Start Of Program
       ProgramStart();

       // Process until exit button is pressed.
       Dou  ExitNow = *On Or (Submit = *On And error = *Off);

          // Process Payment Value 1 code Screens
          Write rcdComPnl;
          Write rcdVstTrk;
          ExFmt rcdVstTrk0;
          Read rcdComPnl;
          Read rcdVstTrk;

          // Clear Error Indicators
          ClearErrors();

          If ExitNow = *Off;
             Select;
             When selAgtSrch = *On;
                SearchAgt#();
             When selClmSrch = *On;
                SearchClm#();
             When selPolSrch = *On;
                SearchEmp#();
             When Submit = *On;
                error = errorCheck();
             Other;
                SetConditions();
             EndSl;
          EndIf;

          // Submit the Job
          If ExitNow = *Off And Submit = *On And Error = *Off;

             grpSelect = UseGroupSel();
             // Call Group Select Process
             If grpSelect = *On;
                GroupSelect('WTRPTVSTD');
                useGrpSel = 'Y';
             Else;
                useGrpSel = 'N';
             EndIf;

             // If group select is used, make sure it submitted the job
             If useGrpSel = 'Y';
                returnCodeOut = ' ';
                ChkSubmitted(callIn:returnCodeOut);
             Else;
                returnCodeOut = 'S';
             EndIf;

             // Only Submit if this item was submitted
             If returnCodeOut = 'S';
                // Submit Job
                company   = %EditC(intCo#:'X');
                visitType = %Subst(scVstType:1:40) + '*';
                vstFrDt   = %EditC(dtCheck(scVstFrDt):'X');
                vstToDt   = %EditC(dtCheck(scVstToDt):'X');
                cmpFrDt   = %EditC(dtCheck(scCmpFrDt):'X');
                cmpToDt   = %EditC(dtCheck(scCmpToDt):'X');
                qteFlag   = scQteFlag;

                agtRrn    = '000000000';
                clmRrn    = '000000000';
                empRrn    = '000000000';

                Select;
                When scAgtName <> ' ';
                   agtRrn    = %EditC(scAgtRrn:'X');
                When scClmLnm <> ' ' And scClmFnm <> ' ';
                   clmRrn    = %EditC(scClmRrn:'X');
                When scEmpDivNm <> ' ';
                   empRrn    = %EditC(scEmpRrn:'X');
                EndSl;

                // Write Select Criteria Record
                WriteSelect();

                // Submit job
                Submitter(company:visitType:scVstCat:scVstAssgn:scVstSts:
                  vstFrDt:vstToDt:cmpFrDt:cmpToDt:scCmpNoDt:qteFlag:scOutType:
                  agtRrn:clmRrn:empRrn:scLocation:useGrpSel:vsKey);
                Leave;
             Else;
                error = *On;
             EndIf;
          EndIf;

       Enddo;

      /end-free
     p Main            e
       //===================================================================
       // OverrideEmp - Override Employer Files and Open Files
       //===================================================================
     p OverrideEmp     b
     d OverrideEmp     pi
     d  quoteType                     1

     d mode            s              1

      /free

       // Override Employer Files  If Quote Employer
       If mode = 'Q';
          strCommand = 'OVRDBF FILE(WDELP) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WDELP)';
          Callp QCmdExc(strCommand:%len(strCommand));

          strCommand = 'OVRDBF FILE(WMEML) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEML)';
          Callp QCmdExc(strCommand:%len(strCommand));
       Endif;

       // Open Employer Files
       If not %open(WDELP);
          Open WDELP;
       EndIf;

       If not %Open(WMEML);
          Open WMEML;
       EndIf;

      /end-free

     p OverrideEmp     e
       //===================================================================
       // PositionFields - Positions Field on the Screen
       //===================================================================
     p PositionFields  b
     d PositionFields  pi

     D visitCat        s             10
     D visitType       s             40
     D i               s              2  0
     D x               s              5  0 Inz(250)
     D y               s              5  0 Inz(445)
     D toolCount       s              1  0 Inz(5)
     D lineCount       s              2  0 Inz(0)
     d lastToolUsed    s              1  0 Inz(0)

      /free

       Clear visitFieldsDs;
       Clear visitFldXds;
       Clear visitFldYds;
       Clear visitFldVds;
       Clear visitFldLds;

       If scVstType <> hdVstType;
          Clear vstDateFVds;
          Clear vstDateTVds;
          Clear vstDateDSds;
          Clear vstDateNDds;
          Clear vstDecFVds;
          Clear vstDecTVds;
          Clear vstDecDSds;
          Clear vstDecNVds;
          Clear vstIntFVds;
          Clear vstIntFVds;
          Clear vstIntDSds;
          Clear vstIntNVds;
          Clear vstTxtVds;
          Clear vstChkVds;
          Clear vstChkDSds;
          Clear vstChkNVds;
          hdVstType = scVstType;
       EndIf;

       visitCat  = %Subst(scVstType:41:10);
       visitType = %Subst(scVstType:1:40);

       Chain (intCo#:visitCat:visitType) pvi00;
       If %Found(WTVIL);

          For i=1 to 50;
             If visitFields(i) <> ' ';
                visitFldV(i) = *On;
                visitFldL(i) = visitFields(i);

                Select;
                When (i >= 1 and i <= 10) Or (i >= 21 and i <= 40);

                   toolCount = toolCount + 2;
                   If toolCount > 4;
                      x = 250;
                      toolCount = 2;
                      y = 445 + (65 * lineCount);
                      lineCount = lineCount + 1;
                      lastToolUsed = 2;
                   Else;
                      Select;
                      When lastToolUsed = 1;
                      x = x + 274;
                      When lastToolUsed = 2;
                      x = x + 410;
                      EndSl;
                      lastToolUsed = 2;
                   EndIf;

                When i >= 11 and i <= 20;

                   toolCount = toolCount + 1;
                   If toolCount > 3;
                      x = 250;
                      toolCount = 1;
                      y = 445 + (65 * lineCount);
                      lineCount = lineCount + 1;
                      lastToolUsed = 1;
                   Else;
                      Select;
                      When lastToolUsed = 1;
                      x = x + 274;
                      When lastToolUsed = 2;
                      x = x + 410;
                      EndSl;
                      lastToolUsed = 1;
                   EndIf;

                When i >= 41 and i <= 50;

                   x = 250;
                   toolCount = 4;
                   y = 445 + (65 * lineCount);
                   lineCount = lineCount + 1;

                EndSl;

                visitFldX(i) = x;
                visitFldY(i) = y;

             Else;
                visitFldV(i) = *Off;
             EndIf;
          EndFor;

          scBtnHgt = y + 105;
          scTPnlHgt = y + 65;
          APnlHeight = y + 125;

          If scBtnHgt < 850;
             scBtnHgt = 850;
             scTPnlHgt = 810;
             APnlHeight = 870;
          EndIf;

       Else;
          scBtnHgt = 850;
          scTPnlHgt = 810;
          APnlHeight = 870;
       EndIf;

      /end-free

     p PositionFields  e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

     d suffix          s              3    Inz(' ')

      /free

       // Load File Locaion Text
       GetCltSuffix(suffix);
       //***** Beg Chg ***** 06/21/21 ******************************************
       //*****scFileLocD = 'The Visit Tracking Report will produce file WFVIP ' +
       //*****'in library PDSRPT' + %Trim(suffix) +
       scFileLocD = 'The Service Request Tracking Report will produce ' +
        'file WFVIP in library PDSRPT' + %Trim(suffix) +
       //***** End Chg ***** 06/21/21 ******************************************
          ', if File or Both is selected.';

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);

       // Get the client id
       GetClientID(##CltId);

       // Initialize fields
       scCmpNoDt = ' ';
       noDate = *Off;

       // Set Conditions
       SetConditions();

       // Set default report type
       scOutType = 'P';
       scQteFlag = ' ';

      /end-free
     p ProgramStart    e
       //===================================================================
       // SearchAgt# - Agency Search
       //===================================================================
     p SearchAgt#      b
     d SearchAgt#      pi

     D kcIn            s              1    inz('0')
     D klIn            s              1    inz('0')
     D errorIn         s              1    inz(' ')
     D companyIn       s              3  0 inz(0)
     D viewIn          s              1  0 inz(1)
     D quoteIn         s              1    inz(' ')
     D modeIn          s              1    inz(' ')
     D nameIn          s             32
     D rrnOut          s              9  0 inz(0)

      /free

       nameIn=scAgtName;

       rrnOut = AgtSrchAG(kcIn:klIn:errorIn:companyIn:
                          viewIn:quoteIn:modeIn:nameIn);
       If rrnOut<>0;
          Chain rrnOut pag0p;
          If %Found(WMAGP);
             scAgtName=AGname;
             scAgtRrn=rrnOut;
             // Get the Agency Number
             Chain (AGco#:AGfein:AGmod) paa00;
             If Not %Found(WMAAL);
                AAagt# = 0;
             EndIf;
             scAgtFein=%EditC(AGfein:'X') + %EditC(AGmod:'X') +
                 %EditC(AAagt#:'X');
             scClmRrn=0;
             scEmpRrn=0;
          EndIf;
       Else;
          scAgtRrn=0;
          scClmRrn=0;
          scEmpRrn=0;
       EndIf;

      /end-free

     p SearchAgt#      e
       //===================================================================
       // SearchClm# - Claim Search
       //===================================================================
     p SearchClm#      b
     d SearchClm#      pi

     D modeIn          s              1    inz('C')
     D callIn          s              1    inz('2')
     D rrnOut          s              9    inz('000000000')
     D clmRrn          s              9  0 inz(0)
     D selectIn        s           1024    inz(' ')

      /free

       clear dsSrchCrit;
       clear selectIn;
       dsClmGrp=0;
       dsClmLname=scClmLnm;
       dsClmFname=scClmFnm;
       selectIn=dsSrchCrit;
       modeIn = 'C';
       rrnOut = '000000000';

       ClmSearch(modeIn:callIn:rrnOut:selectIn);
       If rrnOut<>'000000000';
          clmRrn=%Dec(rrnOut:9:0);
          Chain clmRrn pcm0p;
          If %Found(WMCMP);
             scClmLnm=CMlsn;
             scClmFnm=CMfsn;
             scClaim#=%EditC(CMfnd:'X') + %EditC(CMfyr:'X') +
                 %EditC(CMcase:'X');
             scAgtRrn=0;
             scClmRrn=clmRrn;
             scEmpRrn=0;
          EndIf;
       Else;
          scAgtRrn=0;
          scClmRrn=0;
          scEmpRrn=0;
       EndIf;

      /end-free

     p SearchClm#      e
       //===================================================================
       // SearchEmp# - Employer Search
       //===================================================================
     p SearchEmp#      b
     d SearchEmp#      pi

     D modeIn          s              1    inz('E')
     D callIn          s              1    inz('2')
     D rrnOut          s              9    inz('000000000')
     D empRrn          s              9  0 inz(0)
     D selectIn        s           1024    inz(' ')

     D kkCo#           s                   like(EMco#)
     D kkFnd           s                   like(EMfnd)
     D kkEmp#          s                   like(EMemp#)
     D kkDiv           s                   like(EMdiv)

     C     keyWMEML      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkFnd
     C                   Kfld                    kkEmp#
     C                   Kfld                    kkDiv

      /free

       clear dsSrchCrit;
       clear selectIn;
       dsEmpGrp=0;
       dsEmpName=scEmpDivNm;
       selectIn=dsSrchCrit;
       modeIn = 'E';
       rrnOut = '000000000';

       EmpSearch(modeIn:callIn:rrnOut:selectIn);
       If rrnOut<>'000000000';

          // Set Quote Flag
          If modeIn = 'Q';
             scQteFlag = 'Q';
          Else;
             scQteFlag = ' ';
          EndIf;

          // Apply Overrides and Open Files
          overrideEmp(scQteFlag);

          empRrn=%Dec(rrnOut:9:0);
          Chain empRrn pel0p;
          If %Found(WDELP);
             kkCo#=ELco#;
             kkFnd=ELfnd;
             kkEmp#=ELEmp#;
             kkDiv=ELdiv;
             Chain keyWMEML pem00;
             If %Found(WMEML);
                scEmpDivNm=EMname;
                scEmpDiv=%EditC(EMemp#:'X') + %EditC(EMdiv:'X') +
                 %EditC(EMfnd:'X');
                scAgtRrn=0;
                scClmRrn=0;
                scEmpRrn=empRrn;
             EndIf;
          EndIf;

          // Delete Employer Overrides if any and close files
          delOverrideEmp(scQteFlag);

       Else;
          scAgtRrn=0;
          scClmRrn=0;
          scEmpRrn=0;
       EndIf;

      /end-free

     p SearchEmp#      e
       //===================================================================
       // SetConditions - Sets Conditons when Screen changes
       //===================================================================
     p SetConditions   b
     d SetConditions   pi

     D i               s              3  0

      /free

       // Check to see if the category changed
       If scVstCat <> hdVstCat;
          If hdVstCat <> ' ';
             scVstType=' ';
          EndIf;
          scAgtName=' ';
          scClmFnm=' ';
          scClmLnm=' ';
          scEmpDivNm=' ';
          scAgtFein=' ';
          scClaim#=' ';
          scEmpDiv=' ';
          scAgtRrn=0;
          scClmRrn=0;
          scEmpRrn=0;
          scQteFlag=' ';
          hdVstCat=scVstCat;
       EndIf;

       // Position fields on screen
       PositionFields();

       // Assign category if type selected
       If scVstType <> ' ';
          scVstCat = %Subst(scVstType:41:10);
          hdVstCat = %Subst(scVstType:41:10);
       EndIf;

       // Set the select for Visit Type if category entered.
       If scVstCat <> ' ';
          scVstTpSel = 'VIcat = ''' + %Trim(scVstCat) + '''';
       Else;
          scVstTpSel = 'VIcat in (''Agency'', ''Claims'', ''Policy'')';
       EndIf;

       // Set Searches
       Select;
       When scVstCat = 'Agency';
          noAgtSrch = *Off;
          noClmSrch = *On;
          noPolSrch = *On;
          shwLoc = *Off;
          shwNoLoc = *On;
       When scVstCat = 'Claims';
          noAgtSrch = *On;
          noClmSrch = *Off;
          noPolSrch = *On;
          shwLoc = *Off;
          shwNoLoc = *On;
       When scVstCat = 'Policy';
          noAgtSrch = *On;
          noClmSrch = *On;
          noPolSrch = *Off;
          shwLoc = *On;
          shwNoLoc = *Off;
       Other;
          noAgtSrch = *On;
          noClmSrch = *On;
          noPolSrch = *On;
          shwLoc = *Off;
          shwNoLoc = *On;
       EndSl;

       // Clear Holding fields if searchable fields are cleared
       If scEmpDivNm = ' ' And scClmLnm = ' ' And scClmFnm = ' ' And
          scAgtFein=' ';
          scClaim#=' ';
          scEmpDiv=' ';
          scAgtRrn=0;
          scClmRrn=0;
          scEmpRrn=0;
          scQteFlag=' ';
       EndIf;

       // Check to see if we are using completion dates
       If scCmpNoDt = 'Y';
          scCmpFrDt = 0;
          scCmpToDt = 0;
          noDate = *On;
       Else;
          noDate = *Off;
       EndIf;

       // Check to see if No Date is being used
       For i=1 to 10;
           If vstDateND(i) <> ' ';
              vstDateFV(i) = 0;
              vstDateTV(i) = 0;
              vstDateDS(i) = *On;
           Else;
              vstDateDS(i) = *Off;
           EndIf;
       EndFor;

       // Check to see if No Integer Value is being used
       For i=1 to 10;
           If vstIntNV(i) <> ' ';
              vstIntFV(i) = 0;
              vstIntTV(i) = 0;
              vstIntDS(i) = *On;
           Else;
              vstIntDS(i) = *Off;
           EndIf;
       EndFor;

       // Check to see if No Decimal Value is being used
       For i=1 to 10;
           If vstDecNV(i) <> ' ';
              vstDecFV(i) = 0;
              vstDecTV(i) = 0;
              vstDecDS(i) = *On;
           Else;
              vstDecDS(i) = *Off;
           EndIf;
       EndFor;

       // Check to see if No Check Box Value is being used
       For i=1 to 10;
           If vstChkNV(i) <> ' ';
              vstChkV(i) = ' ';
              vstChkDS(i) = *On;
           Else;
              vstChkDS(i) = *Off;
           EndIf;
       EndFor;

       If scEmpDiv <> ' ';
          scLocList='ALELID || ''-'' || TRIM(ALNAME) || ''-'' || ' +
           'CASE WHEN ALPAD1<>'' '' THEN TRIM(ALPAD1) || '' '' || ' +
           'TRIM(ALPAD2) || '' '' || TRIM(ALPCTY) || '', '' || ' +
           'ALPST || '' '' || SUBSTR(DIGITS(ALPZIP),1,5) ELSE ' +
           'TRIM(ALMAD1) || '' '' || ' +
           'TRIM(ALMAD2) || '' '' || TRIM(ALMCTY) || '', '' || ' +
           'ALMST || '' '' || SUBSTR(DIGITS(ALMZIP),1,5) END';

          If scQteFlag = 'Q';
             scLocFile = %Trim(RtvQuoteLib('*QTE')) + '/WMALP';
          Else;
             scLocFile = %Trim(RtvQuoteLib('*LIV')) + '/WMALP';
          EndIf;

          scLocSel='ALFND=' + %Subst(scEmpDiv:15:3) + ' AND ALEMP#=' +
           %Subst(scEmpDiv:1:9) + ' AND ALDIV=' + %Subst(scEmpDiv:10:5);

          shwLoc = *On;
          shwNoLoc = *Off;

       Else;
          shwLoc = *Off;
          shwNoLoc = *On;
       EndIf;

       // Reset Error Indicators
       mgVstType  = ' ';
       mgVstCat   = ' ';
       mgVstAssgn = ' ';
       mgVstFrDt  = ' ';
       mgVstToDt  = ' ';
       mgCmpFrDt  = ' ';
       mgCmpToDt  = ' ';
       erVstType  = *Off;
       erVstCat   = *Off;
       erVstAssgn = *Off;
       erVstFrDt  = *Off;
       erVstToDt  = *Off;
       erCmpFrDt  = *Off;
       erCmpToDt  = *Off;

      /end-free

     p SetConditions   e
       //===================================================================
       // UseGroupSel - Check to see if we will be using the group select
       //===================================================================
     p UseGroupSel     b
     d UseGroupSel     pi              n

     D use             s               n   Inz(*Off)

      /free

       If (scVstCat = 'Claims' and scClmRrn = 0) Or
          (scVstCat = 'Policy' and scEmpRrn = 0);
          use = *On;
       EndIf;

       Return use;

      /end-free

     p UseGroupSel     e
       //===================================================================
       // WriteSelect - Write Selection Criteria Record
       //===================================================================
     p WriteSelect     b
     d WriteSelect     pi

     D i               s              3  0

      /free

       Clear pvs0p;
       vsCo#  = intCo#;
       vsUser = q1User;
       vsKey  = %EditC(DateTo8(systemDate()):'X') +
                 %EditC(TimeTo6(systemTime()):'X');
       For i=1 To 10;
          vstDteFF(i) = dtCYMD(vstDateFV(i));
          vstDteTF(i) = dtCYMD(vstDateTV(i));
          vstDteNF(i) = vstDateND(i);
          vstIntFF(i) = vstIntFV(i);
          vstIntTF(i) = vstIntTV(i);
          vstIntNF(i) = vstIntNV(i);
          vstDecFF(i) = vstDecFV(i);
          vstDecTF(i) = vstDecTV(i);
          vstDecNF(i) = vstDecNV(i);
          vstChkF(i) = vstChkV(i);
          vstChkNF(i) = vstChkNV(i);
          vstTxtF(i) = vstTxtV(i);
       EndFor;

       Write pvs0p;

      /end-free

     p WriteSelect     e
