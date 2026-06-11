     h**********************************************************************
     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to populate class description
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WPIPOLPOP2
     h*  Calls to:
     h*
     h*  Created on 08/01/2017 by C:267 for Project WC2259
     h*
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     FWMEPL1E   IF   E           K disk    rename(pep01:pepl1)
     FWDELP     IF   E             DISK    rename(pel01:pel0p)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPRl1)
     FWMPXL1    IF   E           K DISK    rename(PPX01:PPXl1)
      *
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

     d* Get Class code description
     d GetClsDesc      pr

      * *ENTRY
     DQuoteGrid14      pr                  ExtPgm('WTPOLQTE14')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     DQuoteGrid14      pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1
      *
     d intCo#          s              3  0
     d intFnd          s              3  0
     d intEmp#         s              9  0
     d intDiv          s              5  0
     d intFyr          s              3  0
     d*intClass        s             13
     D intClassCd      s              5P 0
     D charClassExt    s              5
     D charClassSpc    s              1
     d intElRRn        s              9  0

     d intEffDt        s              7  0
     d intEffDt8       s              8  0
     d charEffDt8      s              8
     d charEffDt10     s             10
     D slash1          s              2  0 inz
     D slash2          s              2  0 inz
     d charState       s              2
     D charClsDesc     s             30
     D xxSt            s              2
     D xxPxSt          s              2
     D xxEfDt          s                   like(ELPFDT)
     D xxType          s              1
     D*
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D dashPos1        s              5  0 inz
     D dashPos2        s              5  0 inz
     D len             s              5  0 inz
     D value           s             41    inz
     C*
     C* Employer payroll file Key List
     C*
     C     key1WMEPL     Klist
     C                   Kfld                    elCo#
     C                   Kfld                    elFnd
     C                   Kfld                    elFyr
     C                   Kfld                    elEmp#
     C                   Kfld                    elDiv
     C                   Kfld                    intClassCd
     C                   Kfld                    charClassSpc
     C                   Kfld                    charClassExt
     C                   Kfld                    elPTDt
     C*
     C     key2WMEPL     Klist
     C                   Kfld                    elCo#
     C                   Kfld                    elFnd
     C                   Kfld                    elFyr
     C                   Kfld                    elEmp#
     C                   Kfld                    elDiv
     C                   Kfld                    intClassCd
     C                   Kfld                    charClassSpc
     C                   Kfld                    charClassExt
     C*
     C* Payroll Class File Key List
     C*
     C     keyWMPRL1     Klist
     C                   Kfld                    elCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    intClassCd
     C                   Kfld                    xxEfDt

     C     key1WMPRL1    Klist
     C                   Kfld                    elCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    intClassCd
     C*
     C* Payroll Class extention File Key List
     C*
     C     keyWMPXL1     Klist
     C                   Kfld                    elCo#
     C                   Kfld                    intClassCd
     C                   Kfld                    charClassExt
     C                   Kfld                    xxPxSt
     C                   Kfld                    xxEfDt

     C     key1WMPXL1    Klist
     C                   Kfld                    elCo#
     C                   Kfld                    intClassCd
     C                   Kfld                    charClassExt
     C                   Kfld                    xxPxSt
      /Free

       intEffDt     = 0;
       intEffDt8    = 0;
       intClassCd   = 0;
       charClassExt = ' ';
       charClassSpc = ' ';
       charEffDt8   = ' ';
       charEffDt10  = ' ';
       charState    = ' ';
       outJSON      = ' ';

       Exsr ProcessGenParm;

       If RtnCode = 'X';
         // RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       GetClsDesc();

       BuildJSON(outJSON :'classdesc' :charClsDesc);

       rtnCode = ' ';
       Return;

       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Rtncode = ' ';
       Monitor;

       // Extract WMELP Employer Detail file RRN
         //startPos = %Scan(',':inGenParm)+1;
         startPos = 1;
         //If startPos <= 1;
         If startPos <  1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
          intElRRn = %Int(value);
         On-Error;
          intElRRN = 0;
         EndMon;

       // Extract Class Code
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
         dashPos1 = %scan('/':value);
         If dashPos1 > 0;
            intClassCd   = %dec(%trim(%subst(value:1:dashPos1-1)):5:0);
            charClassSpc = %trim(%subst(value:dashPos1+1:1));
            dashPos1     = dashPos1 + 1;
            dashPos2     = %scan('/':value:dashPos1);
            If dashPos2 > 0;
               charClassExt = %trim(%subst(value:dashPos2+1:5));
            Else;
               charClassExt = *blanks;
            EndIf;
         Else;
            intClassCd     = %dec(%trim(%subst(value:1:5)):5:0);
            charClassSpc   = *blanks;
            charClassExt   = *blanks;
         EndIf;

         On-Error;
            intClassCd    = 0;
            charClassSpc  = *blanks;
            charClassExt  = *blanks;
         EndMon;

       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;

       //*********************************************************************
       // extract values from genparm
       //*********************************************************************
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
         If endPos > 0;
           len = endPos-startPos;
           If len > 0;
             value = %Subst(inGenParm:startPos:len);
           Endif;
         Endif;
       Endif;
       EndSr;

      /End-Free

       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p GetClsDesc      b
     d GetClsDesc      pi
      /free

       Chain intElRRn pel0p;

       // Employee Class Description
       SetLl key1WMEPL PEPl1;
       ReadPE key2WMEPL PEPl1;
       If Not %Eof;
          Clear charClsDesc;
          xxType = 'P';
          xxSt   = RatingSt(ELCO#:ELRTCD);
          //xxEfDt = EPEfDt;
          xxEfDt = SrEffd(ELCo#:ELFnd:ELFyr:ELEmp#:ELDiv:EPEfDt:xxType:
                   EPClas);
          Setgt keyWMPRL1 PPRl1;
          Readpe key1WMPRL1 PPRl1;
          If not %eof;
             charClsDesc = PRDesc;
             If charClassExt <> *blanks;
                Clear xxPxSt;
                Setgt keyWMPXL1 PPXl1;
                Readpe key1WMPXL1 PPXl1;
                If not %Eof;
                   charClsDesc = PXDesc;
                Else;
                   xxPxSt = xxSt;
                   Setgt keyWMPXL1 PPXl1;
                   Readpe key1WMPXL1 PPXl1;
                   If not %Eof;
                      charClsDesc = PXDesc;
                   Endif;
                Endif;
             Endif;
          Endif;
       Endif;

      /end-free
     P GetClsDesc      E

      *//***************************************************************************//
      *//===================================================================
      *// Effective Date API Call.
      *//===================================================================
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
