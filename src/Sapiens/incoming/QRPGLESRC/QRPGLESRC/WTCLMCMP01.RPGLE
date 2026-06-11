     h**********************************************************************
     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
      **********************************************************************
      * Created: 8/19/2019 C:079 Project: WC2604
      **********************************************************************
      * Revisions:
      *
      **********************************************************************
     Fwmidctrl  IF   E             disk
     Fwmidcscl  IF   E           k disk
     Fwtgtl     if   e           k Disk
      * Entry
     dGetIDCDetail     pr                  ExtPgm('WTCLMCMP01')
     d                                9  0
     d                            32767
     d                                1
     dGetIDCDetail     pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d RptQtrCd        s              3
     d SubTyCd         s             10
     d TrnAmt          s             12s 2
     d xxJrStCd        s             25    Inz(*Blanks)
     d xxNumDt         s              8  0
     d xxDate          s               d
     d scDate          s             10
     d BenTyCd         s             45
     d BenOffCd        s             45
     d xxKey           s                   Like(GTkey)
     d Lumpsum         s              3    Inz(*Blanks)
     d BenOffAmt       s             11s 2
     d WkBenAmt        s              9s 2

      /copy sPrAtrium
      /copy sPrDate

      /free
       outJSON  = ' ';

       // Load detail for IDC Prior Transactional Files
       Exsr ProgramStart;
       Exsr ProcessIDC;

       RtnCode = ' ';
       Return;
       //==================================================================
       Begsr ProcessIDC;
        Chain(n) inRRN ptr01;
        If %found(wmidctrl);
           Chain TRbchId pcc01;
           If %Found(wmidcscl);

           // Submission date

              BuildJSON(outJSON:'OutSubDateDtl':CCSubDate);

           // Submission file ID

              BuildJSON(outJSON:'OutSubFileIDDtl':CCSubFilID);

           // Submission file type code

              Select;
              When CCsubTyCd = 'O';
                 SubtyCd = 'Original';
              When CCsubTyCd = 'R';
                 SubtyCd = 'Replace';
              Endsl;
              BuildJSON(outJSON:'OutSubTypeCodeDtl':SubTyCd);

           // Reporting quarter

              Select;
              When CCrptQtrCd = '1';
                 RptQtrCd = '1st';
              When CCrptQtrCd = '2';
                 RptQtrCd = '2nd';
              When CCrptQtrCd = '3';
                 RptQtrCd = '3rd';
              When CCrptQtrCd = '4';
                 RptQtrCd = '4th';
              Endsl;
              BuildJSON(outJSON:'OutRptQtrCodeDtl':RptQtrCd);
           Endif;

           // Jurisdiction state code

           Setll (TRco#:'IJST') pgt01;
           Reade (TRco#:'IJST') pgt01;
           Dow Not %Eof(wtgtl);
              If %Trim(GTcF01) = %Trim(TRjrStCd);
                 xxJrStCd = %Trim(GtcF01) + ' - ' + %Trim(GTdesc);
                 Leave;
              Endif;
              Reade (TRco#:'IJST') pgt01;
           Enddo;
           BuildJSON(outJSON:'OutJurisStateCodeDtl':xxJrStCd);

           // From date

           xxNumDt = %Dec(TRtrnFrDt:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutFromDateDtl':scDate);

           // To date

           xxNumDt = %Dec(TRtrnToDt:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutToDateDtl':scDate);

           // Amount
           Monitor;
           TrnAmt = %Dec(TRtrnAmt:12:0)/100;
           On-Error;
           TrnAmt = 0;
           Endmon;

           BuildJSON(outJSON:'OutAmountDtl':%Editc(TrnAmt:'J'));

           // Benefit Type Code

           xxKey = TRbenTyCd;
           Chain (TRco#:'IDCB':xxKey) pgt01;
           If %Found(wtgtl);
              BenTyCd = %Trim(TRbenTyCd) + ' - ' + %Trim(GTdesc);
           Endif;
           BuildJSON(outJSON:'OutBenTypeCodeDtl':BenTyCd);

           // Lump Sum Indicator

           Select;
           When TRlmpSmInd = 'Y';
              LumpSum = 'Yes';
           Other;
              LumpSum = 'No';
           Endsl;

           BuildJSON(outJSON:'OutLumpSumIndDtl':LumpSum);

           // Benefit Offset Code

           xxKey = TRbenOfCd;
           Chain (TRco#:'IBOC':xxKey) pgt01;
           If %Found(wtgtl);
              BenOffCd = %Trim(TRbenOfCd) + ' - ' + %Trim(GTdesc);
           Endif;

           BuildJSON(outJSON:'OutBenOffsetCodeDtl':BenOffCd);

           // Benefit Offset Amount

           Monitor;
           BenOffAmt = %Dec(TRBenOfAmt:11:0)/100;
           On-Error;
           BenOffAmt = 0;
           Endmon;

           BuildJSON(outJSON:'OutBenefitOffsetAmountDtl':%Editc(BenOffAmt:'J'));

           // Weekly Benefit Amount

           Monitor;
           WkBenAmt = %Dec(TRwkBenAmt:9:0)/100;
           On-Error;
           WkBenAmt = 0;
           Endmon;

           BuildJSON(outJSON:'OutWeeklyBenefitAmountDtl':%Editc(WkBenAmt:'J'));

           // Carrier Code

           BuildJSON(outJSON:'OutCarrierCodeDtl':TRcarCd);

           // Policy Number Identifier

           BuildJSON(outJSON:'OutPolicyNumberIDDtl':TRpolID);

           // Policy Effective Date

           Monitor;
           xxNumDt = %Dec(TRpolEfDt:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutPolicyEffectiveDateDtl':scDate);
           On-Error;
              BuildJSON(outJSON:'OutPolicyEffectiveDateDtl':' ');
           Endmon;

           // Claim Number Identifier

           BuildJSON(outJSON:'OutClaimNumberIDDtl':TRclmID);

           // Accident Date

           xxNumDt = %Dec(TRAcDt:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutAccidentDateDtl':scDate);

        Endif;
       Endsr;
       //==================================================================
       Begsr ProgramStart;
        BenTyCd = *blanks;
       Endsr;
      /end-free

