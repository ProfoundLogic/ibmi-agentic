     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve payroll information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLQTE
     h*  Calls to:
     h*
     h*  Created on 06/27/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwmepp     if   e             disk    rename(pep01:pep0p)
     FWDELL     IF   E           K DISK    rename(pel01:pel00)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPR01)
     FWMPXL1    IF   E           K DISK    rename(PPX01:PPX01)
     FWTABL     IF   E           K DISK

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrRateSet
      /copy wPrTable
      /copy wPrEmpLr

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

      * *ENTRY
     DQuoteGrid06      pr                  ExtPgm('WTPOLQTE06')
     D                                9  0
     D                            32767
     D                                1
      *
     DQuoteGrid06      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d x#Rrn           s              9  0
     D xxType          s              1
     D xxSt            s              2
     D xxPxSt          s              2
     D xxDesc          s             40
     D xxPrem          s             20
     D xxRate          s             10
     D xxStat          s             10
     D xxEfDt          s                   like(ELPFDT)
     D xxHZGR          s                   like(PRHZGR)
     d xxKey           s             10

     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    EPCo#
     C                   Kfld                    EPFnd
     C                   Kfld                    EPEmp#
     C                   Kfld                    EPDiv
     C                   Kfld                    EPFyr
     C*
     C* Payroll Class File Key List
     C*
     C     keyWMPRL1     Klist
     C                   Kfld                    EPCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EPClas
     C                   Kfld                    xxEfDt
     C     key1WMPRL1    Klist
     C                   Kfld                    EPCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EPClas
     C*
     C* Payroll Description File Key List
     C*
     C     keyWMPXL1     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    EPClas
     C                   Kfld                    EPExt
     C                   Kfld                    xxPxSt
     C                   Kfld                    EPEfDt
     C     key1WMPXL1    Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    EPClas
     C                   Kfld                    EPExt
     C                   Kfld                    xxPxSt
     C*
     C* Billing Schedule Code File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    xxKey

      *
      /Free

       Chain inRRN pep0p;
       Chain keyWDELL pel00;

       outJSON = ' ';

       BuildJSON(outJSON :'OutPayDetYear' :%editc(EPFyr: 'X'));
       BuildJSON(outJSON :'OutPayDetDivision' :%editc(EPDiv: 'X'));
       BuildJSON(outJSON :'OutPayDetEffDate' :
        %EditW(dtMDYY(EPEfdt):'  /  /    '));
       BuildJSON(outJSON :'OutPayDetClass' :%editc(EPClas: 'X'));
       BuildJSON(outJSON :'OutPayDetSpec' :EPSPEC);
       BuildJSON(outJSON :'OutPayDetExt' :EPEXT);

       Clear xxDesc;
       Clear xxRate;
       Clear xxHZGR;
       xxType = 'P';
       xxSt   = RatingSt(ELCO#:ELRTCD);
       xxEfDt = SrEffd(ELCo#:ELFnd:ELFyr:ELEmp#:ELDiv:EPEfDt:xxType:
        EPClas);
       Setgt keyWMPRL1 PPR01;
       Readpe key1WMPRL1 PPR01;
       If not %eof;
          xxDesc = PRDesc;
          xxRate = %editc(PRRt:numEdtCde);
          xxHZGR =  PRHzgr;
          If EPExt <> *blanks;
             Clear xxPxSt;
             Setgt keyWMPXL1 PPX01;
             Readpe key1WMPXL1 PPX01;
             If not %Eof;
                xxDesc = PXDesc;
             Else;
                xxPxSt = xxSt;
                Setgt keyWMPXL1 PPX01;
                Readpe key1WMPXL1 PPX01;
                If not %Eof;
                   xxDesc = PXDesc;
                Endif;
             Endif;
          Endif;
       Endif;

       BuildJSON(outJSON :'OutPayDetDesc' :xxDesc);
       BuildJSON(outJSON :'OutPayDetRate' :xxRate);
       BuildJSON(outJSON :'OutPayDetHazard' :xxHZGR);
       BuildJSON(outJSON :'OutPayDetCXDate' :
        %EditW(dtMDYY(EPCadt):'  /  /    '));

       If EPSTAT = 'A';
          xxStat = 'Active';
       Else;
          xxStat = 'Inactive';
       Endif;

       BuildJSON(outJSON :'OutPayDetStatus' :xxStat);

       xxPrem = %editc(EPEapr:numEdtCde);
       BuildJSON(outJSON :'OutPayDetEstPayroll' :xxPrem);

       xxPrem = %editc(EPEmpr:numEdtCde);
       BuildJSON(outJSON :'OutPayDetEstPremium' :xxPrem);

       xxPrem = %editc(EPEcnt: '1' );
       BuildJSON(outJSON :'OutPayDetEstEmployees' :xxPrem);

       If EPSAFL = 'A';
          xxStat = 'Audited';
       Else;
          xxStat = 'Estimated';
       Endif;

       BuildJSON(outJSON :'OutPayDetPayStatus' :xxStat);

       xxPrem = %editc(EPAapr:numEdtCde);
       BuildJSON(outJSON :'OutPayDetAudPayroll' :xxPrem);

       xxPrem = %editc(EPAmpr:numEdtCde);
       BuildJSON(outJSON :'OutPayDetAudPremium' :xxPrem);

       xxPrem = %editc(EPAcnt: '1' );
       BuildJSON(outJSON :'OutPayDetAudEmployees' :xxPrem);

       xxPrem = %editc(EPSapr:numEdtCde);
       BuildJSON(outJSON :'OutPayDetRepPayroll' :xxPrem);

       xxPrem = %editc(EPAdld:numEdtCde);
       BuildJSON(outJSON :'OutPayDetAddLoad' :xxPrem);

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


