     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve agent history information
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
     h*
     h**********************************************************************
     fwmahp     if   e             disk    rename(pah01:pah0p)
     FWDELL     IF   E           K DISK    rename(pel01:pel00)
     FWMARL     IF   E           K DISK    rename(par01:par00)
     FWTABL     IF   E           K DISK

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrRateSet
      /copy wPrTable
      /copy wPrEmpLr
      /copy wPrAgent
      /copy wPrProduce

      * *ENTRY
     DQuoteGrid07      pr                  ExtPgm('WTPOLQTE07')
     D                                9  0
     D                            32767
     D                                1
      *
     DQuoteGrid07      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     D xxDesc          s             40
     D xxDesc2         s             40
     D xxPlan          s             30
     D xxCntr          s             10
     D agtName         s             50
     D prodName        s             50
     D agtFein         s              9  0
     D AgtMod          s              3  0
     D xxNum           s             20
     D xxRate          s                   like(AHRT#)
     D xxStat          s             10
     D xxAGrr          s              9  0
     D xxAArr          s              9  0
     D xxAPrr          s              9  0

     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    AHCo#
     C                   Kfld                    AHFnd
     C                   Kfld                    AHEmp#
     C                   Kfld                    AHDiv
     C                   Kfld                    AHFyr
     C*
     C* Agent Rate File Key List
     C*
     C     keyWMARL      Klist
     C                   Kfld                    AHCo#
     C                   Kfld                    AHFnd
     C                   Kfld                    xxRate
     C                   Kfld                    AHEfdt
     C*
     C     key1WMARL     Klist
     C                   Kfld                    AHCo#
     C                   Kfld                    AHFnd
     C                   Kfld                    xxRate
     C*
      /Free

       Chain inRRN pah0p;
       Chain keyWDELL pel00;

       outJSON = ' ';

       BuildJSON(outJSON :'OutAgt7DetEffDate' :
        %EditW(dtMDYY(AHEfdt):'  /  /    '));

       If AHStat = 'A';
          xxStat = 'Active';
       Else;
          xxStat = 'Inactive';
       Endif;

       BuildJSON(outJSON :'OutAgt7DetStatus' :xxStat);


       //* Primary Agent

       Clear agtName;
       Clear prodName;

       If AHAgt# <> 0;
          agtName = %editc(AHAgt# : 'X') + ' - ' +
           GetAgtName(AHAgt#:xxAGrr:xxAArr:AHFnd:AHCo#);

          agtFein = GetAgtFein(AHCo#:AHFnd:AHAgt#:agtMod);

          ProdName = %editc(AHProd : 'X') + ' - ' +
           GetProdNm(agtFein:agtMod:AHProd:xxAPrr:AHCo#);
       Endif;

       BuildJSON(outJSON :'OutAgt7DetAgency1' : agtName);
       BuildJSON(outJSON :'OutAgt7DetProducer1' : prodName);

       xxRate = AHRt#;
       Exsr Rates;

       BuildJSON(outJSON :'OutAgt7DetRate1' :xxNum);
       BuildJSON(outJSON :'OutAgt7DetNewRate1' :xxDesc);
       BuildJSON(outJSON :'OutAgt7DetRenewRate1' :xxDesc2);
       BuildJSON(outJSON :'OutAgt7DetPlanType1' :xxPlan);

       Clear xxCntr;
       If AHCAF1 = 'Y';
          xxCntr = 'Yes';
       Endif;
       BuildJSON(outJSON :'OutAgt7DetControl1' :xxCntr);


       //* Secondary Agent

       Clear agtName;
       Clear prodName;

       If AHSAgt <> 0;
          agtName = %editc(AHSAgt : 'X') + ' - ' +
           GetAgtName(AHSAgt:xxAGrr:xxAArr:AHFnd:AHCo#);

          agtFein = GetAgtFein(AHCo#:AHFnd:AHSAgt:agtMod);

          ProdName = %editc(AHSPrd : 'X') + ' - ' +
           GetProdNm(agtFein:agtMod:AHSPrd:xxAPrr:AHCo#);
       Endif;

       BuildJSON(outJSON :'OutAgt7DetAgency2' : agtName);
       BuildJSON(outJSON :'OutAgt7DetProducer2' : prodName);

       xxRate = AHSRt#;
       Exsr Rates;

       BuildJSON(outJSON :'OutAgt7DetRate2' :xxNum);
       BuildJSON(outJSON :'OutAgt7DetNewRate2' :xxDesc);
       BuildJSON(outJSON :'OutAgt7DetRenewRate2' :xxDesc2);
       BuildJSON(outJSON :'OutAgt7DetPlanType2' :xxPlan);

       Clear xxCntr;
       If AHCAF2 = 'Y';
          xxCntr = 'Yes';
       Endif;
       BuildJSON(outJSON :'OutAgt7DetControl2' :xxCntr);


       //* Tertiary Agent

       Clear agtName;
       Clear prodName;

       If AH3Agt <> 0;
          agtName = %editc(AH3Agt : 'X') + ' - ' +
           GetAgtName(AH3Agt:xxAGrr:xxAArr:AHFnd:AHCo#);

          agtFein = GetAgtFein(AHCo#:AHFnd:AH3Agt:agtMod);

          ProdName = %editc(AH3Prd : 'X') + ' - ' +
           GetProdNm(agtFein:agtMod:AH3Prd:xxAPrr:AHCo#);
       Endif;

       BuildJSON(outJSON :'OutAgt7DetAgency3' : agtName);
       BuildJSON(outJSON :'OutAgt7DetProducer3' : prodName);

       xxRate = AH3Rt#;
       Exsr Rates;

       BuildJSON(outJSON :'OutAgt7DetRate3' :xxNum);
       BuildJSON(outJSON :'OutAgt7DetNewRate3' :xxDesc);
       BuildJSON(outJSON :'OutAgt7DetRenewRate3' :xxDesc2);
       BuildJSON(outJSON :'OutAgt7DetPlanType3' :xxPlan);


       //* Primary Agent of Record

       Clear agtName;
       Clear prodName;

       If AHAgrc <> 0;
          agtName = %editc(AHAgrc : 'X') + ' - ' +
           GetAgtName(AHAgrc:xxAGrr:xxAArr:AHFnd:AHCo#);

          agtFein = GetAgtFein(AHCo#:AHFnd:AHAgrc:agtMod);

          ProdName = %editc(AHPrrc : 'X') + ' - ' +
           GetProdNm(agtFein:agtMod:AHPrrc:xxAPrr:AHCo#);
       Endif;

       BuildJSON(outJSON :'OutAgt7DetAgencyRec1' : agtName);
       BuildJSON(outJSON :'OutAgt7DetProducerRec1' : prodName);

       xxRate = AHRtrc;
       Exsr Rates;

       BuildJSON(outJSON :'OutAgt7DetRateRec1' :xxNum);
       BuildJSON(outJSON :'OutAgt7DetNewRateRec1' :xxDesc);
       BuildJSON(outJSON :'OutAgt7DetRenewRateRec1' :xxDesc2);
       BuildJSON(outJSON :'OutAgt7DetPlanTypeRec1' :xxPlan);

       Clear xxCntr;
       If AHCRF1 = 'Y';
          xxCntr = 'Yes';
       Endif;
       BuildJSON(outJSON :'OutAgt7DetControlRec1' :xxCntr);


       //* Secondary Agent of Record

       Clear agtName;
       Clear prodName;

       If AH2Arc <> 0;
          agtName = %editc(AH2Arc : 'X') + ' - ' +
           GetAgtName(AH2Arc:xxAGrr:xxAArr:AHFnd:AHCo#);

          agtFein = GetAgtFein(AHCo#:AHFnd:AH2Arc:agtMod);

          ProdName = %editc(AH2Prc : 'X') + ' - ' +
           GetProdNm(agtFein:agtMod:AH2Prc:xxAPrr:AHCo#);
       Endif;

       BuildJSON(outJSON :'OutAgt7DetAgencyRec2' : agtName);
       BuildJSON(outJSON :'OutAgt7DetProducerRec2' : prodName);

       xxRate = AH2Rrc;
       Exsr Rates;

       BuildJSON(outJSON :'OutAgt7DetRateRec2' :xxNum);
       BuildJSON(outJSON :'OutAgt7DetNewRateRec2' :xxDesc);
       BuildJSON(outJSON :'OutAgt7DetRenewRateRec2' :xxDesc2);
       BuildJSON(outJSON :'OutAgt7DetPlanTypeRec2' :xxPlan);

       Clear xxCntr;
       If AHCRF2 = 'Y';
          xxCntr = 'Yes';
       Endif;
       BuildJSON(outJSON :'OutAgt7DetControlRec2' :xxCntr);


       //* Tertiary Agent of Record

       Clear agtName;
       Clear prodName;

       If AH3Arc <> 0;
          agtName = %editc(AH3Arc : 'X') + ' - ' +
           GetAgtName(AH3Arc:xxAGrr:xxAArr:AHFnd:AHCo#);

          agtFein = GetAgtFein(AHCo#:AHFnd:AH3Arc:agtMod);

          ProdName = %editc(AH3Prc : 'X') + ' - ' +
           GetProdNm(agtFein:agtMod:AH3Prc:xxAPrr:AHCo#);
       Endif;

       BuildJSON(outJSON :'OutAgt7DetAgencyRec3' : agtName);
       BuildJSON(outJSON :'OutAgt7DetProducerRec3' : prodName);

       xxRate = AH3Rrc;
       Exsr Rates;

       BuildJSON(outJSON :'OutAgt7DetRateRec3' :xxNum);
       BuildJSON(outJSON :'OutAgt7DetNewRateRec3' :xxDesc);
       BuildJSON(outJSON :'OutAgt7DetRenewRateRec3' :xxDesc2);
       BuildJSON(outJSON :'OutAgt7DetPlanTypeRec3' :xxPlan);


       rtnCode = ' ';
       Return;
      /End-Free

       //===================================================================
       // Rates - Load the rate fields
       //===================================================================
     C     Rates         Begsr
      /free

       Clear xxNum;
       Clear xxDesc;
       Clear xxDesc2;
       Clear xxPlan;
       Setgt keyWMARL PAR00;
       Readpe key1WMARL PAR00;
       Dow not %eof and ARSTAT <> 'A';
          Readpe key1WMARL PAR00;
       Enddo;
       If not %eof;
          xxNum = %editc(xxRate:numEdtCde);
          xxDesc  = %editc(ARNwrt:numEdtCde) + '%';
          xxDesc2 = %editc(ARRnrt:numEdtCde) + '%';
       //***  Begin Chg  ***  10/14/14  ***********************************
       //*Select;**
       //****When*ARPlan = 'A';
       //*******xxPlan = 'Agency Bill';
       //****Other;
       //*******xxPlan = 'Direct Bill';
       //*Endsl;***
          xxPlan = GetPlanTypeDesc(ARplan);
       //***  End   Chg  ***  10/14/14  ***********************************
       Endif;

      /end-free
     C                   Endsr                                                  Rates Subr

