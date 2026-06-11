     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Employee payroll information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLQTE
     h*  Calls to:
     h*
     h*  Created on 07/31/2017 by C:267 for Project Transform - WC2259
     h*
     h**********************************************************************
     h* Revisions:
     h*
     h* 06/09/20  C:298 Project: SY420
     h*                 Mask SSN
     h*
     h**********************************************************************
     FWMEPL1E   if   e           K disk    rename(pep01:pepl1)
     FWMEEP     if   e             disk    rename(pee01:pee0p)
     FWDELL     IF   E           K DISK    rename(pel01:pel00)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPRl1)
     FWMPXL1    IF   E           K DISK    rename(PPX01:PPXl1)
     FWTABL     IF   E           K DISK

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrRateSet
      /copy wPrTable
      /copy wPrEmpLr

      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************

     d* Effective Date
     d SrEffd          pr             7  0
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inDate                         7  0
     d inType                         1    options(*nopass)
     d inClas                         5  0 options(*nopass)

     d* Get details
     d GetEmplDtl      pr

      * *ENTRY
     DQuoteGrid13      pr                  ExtPgm('WTPOLQTE13')
     D                                9  0
     D                            32767
     D                                1
      *
     DQuoteGrid13      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
     d*
     D numEdtCde       c                   const('N')
     D x#Rrn           s              9  0
     D xxType          s              1
     D xxSt            s              2
     D xxPxSt          s              2
     D xxDesc          s             40
     D xxPrem          s             20
     D xxEmpNam        s             55
     D xxStat          s             10
     D xxEfDt          s                   like(ELPFDT)
     D xxEmpSS#        s             11
     d xxKey           s             10

     D* Employee SS#
     D                 DS
     D  EmpSS#_ds              1      9
     D  ESS1                   1      3
     D  ESS2                   4      5
     D  ESS3                   6      9
      *
     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    EECo#
     C                   Kfld                    EEFnd
     C                   Kfld                    EEEmp#
     C                   Kfld                    EEDiv
     C                   Kfld                    EEFyr
      *
     C*
     C* Employer payroll file Key List
     C*
     C     key1WMEPL     Klist
     C                   Kfld                    elCo#
     C                   Kfld                    elFnd
     C                   Kfld                    elFyr
     C                   Kfld                    elEmp#
     C                   Kfld                    elDiv
     C                   Kfld                    EECLAS
     C                   Kfld                    EESPEC
     C                   Kfld                    EEEXT
     C                   Kfld                    elPTDt
     C*
     C     key2WMEPL     Klist
     C                   Kfld                    elCo#
     C                   Kfld                    elFnd
     C                   Kfld                    elFyr
     C                   Kfld                    elEmp#
     C                   Kfld                    elDiv
     C                   Kfld                    EECLAS
     C                   Kfld                    EESPEC
     C                   Kfld                    EEEXT
     C*
     C* Payroll Class File Key List
     C*
     C     keyWMPRL1     Klist
     C                   Kfld                    EECo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EEClas
     C                   Kfld                    xxEfDt
     C*
     C     key1WMPRL1    Klist
     C                   Kfld                    EECo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EEClas
     C*
     C* Payroll Description File Key List
     C*
     C     keyWMPXL1     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    EEClas
     C                   Kfld                    EEExt
     C                   Kfld                    xxPxSt
     C                   Kfld                    xxEfDt
     C     key1WMPXL1    Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    EEClas
     C                   Kfld                    EEExt
     C                   Kfld                    xxPxSt
     C*
     C* Billing Schedule Code File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    xxKey

      *
      /Free

       Chain inRRN pee0p;
       Chain keyWDELL pel00;

       outJSON = ' ';
       GetEmplDtl();

       BuildJSON(outJSON :'DtlEmpPName' :%Trim(xxEmpNam));
       BuildJSON(outJSON :'DtlEmpPPosition' :%Trim(EEPosn));
       BuildJSON(outJSON :'DtlEmpPClass' :%editc(EEClas: 'X'));
       BuildJSON(outJSON :'DtlEmpPSpecDes' :EESPEC);
       BuildJSON(outJSON :'DtlEmpPClassExt' :EEEXT);

       //***  Begin Chg  ***  06/09/20  ***********************
       //BuildJSON(outJSON :'DtlEmpPSS#' :xxEmpSS#);
       BuildJSON(outJSON :'DtlEmpPSS#'
        :DisplaySS#(MaskData('SSN':'DetailEmpPSS#':Eess#)));
       //***  End   Chg  ***  06/09/20  ***********************

       BuildJSON(outJSON :'DtlEmpPRptStartDate' :
          %EditW(dtMDYY(EESdt):'  /  /    '));
       BuildJSON(outJSON :'DtlEmpPRptEndDate' :
          %EditW(dtMDYY(EEEdt):'  /  /    '));
       BuildJSON(outJSON :'DtlEmpPCheckDate' :
          %EditW(dtMDYY(EECKdt):'  /  /    '));
       BuildJSON(outJSON :'DtlEmpPSRPayroll' :xxPrem);
       BuildJSON(outJSON :'DtlEmpPClsDesc' :xxDesc);

       rtnCode = ' ';
       Return;
      /End-Free

       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0
     d   inFND                        3  0
     d   inFYR                        3  0
     d   inEMP#                       9  0
     d   inDIV                        5  0
     d   inDate                       7  0
     d   inType                       1    options(*nopass)
     d   inClas                       5  0 options(*nopass)
     d*
     d* SReffd API prototype
     d SReffdAPI       pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5
     d*
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Date        s              7
     d   ##Type        s              1
     d   ##Clas        s              5
     d   outDate       s              7  0
     d*

      /free

       ##Type = *blank;
       If %parms >= 7;
          ##Type = inType;
       Endif;

       ##Clas = *blanks;
       If %parms >= 8;
          ##Clas = %editc(inClas: 'X');
       Endif;

       ##CO# = %editc(inCO#: 'X');
       ##FND = %editc(inFND: 'X');
       ##FYR = %editc(inFYR: 'X');
       ##EMP# = %editc(inEMP#: 'X');
       ##DIV = %editc(inDIV: 'X');
       ##Date = %editc(inDate: 'X');

       SReffdAPI(##CO#:##FND:##FYR:##EMP#:##DIV:##Date:##Type:##Clas);

       outDate = %int(##Date);

       Return outDate;
      /end-free
     P SrEffd          E
       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p GetEmplDtl      b
     d GetEmplDtl      pi
      /free

       // Employee Name
       xxEmpNam = %Trim(EEtitl) + ' ' + %Trim(EEfsn) + ' ' +
                   %Trim(EEmid) + ' ' + %Trim(EElsn);

       // Employee SS#
       EmpSS#_ds = EESS#;
       xxEmpSS# =  ESS1 + '-' + ESS2 + '-' + ESS3;

       // Employee SR Premium
       xxPrem = %editc(EESapr:numEdtCde);

       // Employee Class Description
       SetLl key1WMEPL PEPl1;
       ReadPE key2WMEPL PEPl1;
       If Not %Eof;
          Clear xxDesc;
          xxType = 'P';
          xxSt   = RatingSt(ELCO#:ELRTCD);
          xxEfDt = SrEffd(ELCo#:ELFnd:ELFyr:ELEmp#:ELDiv:EPEfDt:xxType:
                   EEClas);
          Setgt keyWMPRL1 PPRl1;
          Readpe key1WMPRL1 PPRl1;
          If not %eof;
             xxDesc = PRDesc;
             If EPExt <> *blanks;
                Clear xxPxSt;
                Setgt keyWMPXL1 PPXl1;
                Readpe key1WMPXL1 PPXl1;
                If not %Eof;
                   xxDesc = PXDesc;
                Else;
                   xxPxSt = xxSt;
                   Setgt keyWMPXL1 PPXl1;
                   Readpe key1WMPXL1 PPXl1;
                   If not %Eof;
                      xxDesc = PXDesc;
                   Endif;
                Endif;
             Endif;
          Endif;
       Endif;

      /end-free
     P GetEmplDtl      E

