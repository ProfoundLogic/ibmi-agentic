     h**********************************************************************
     h Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     h
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
      **********************************************************************
      * Created: 8/19/2019 C:079 Project: WC2604
      **********************************************************************
      * Revisions:
      *
      * 01/14/25    C:340  Project WC2995
      *                    Add the new fields to the detail display for the
      *                    Prior Quarterly Submissions grid
      *                    # Classification Code
      *                    # Return to Work Date
      *                    # Zip Code of Injury Site
      *                    # Number of Dependents
      *                    # Exposure State Code
      *                    # Indemnity Claim Code
      *
      **********************************************************************
     Fwmidcqtl  IF   E             disk
     Fwmidcscl  IF   E           k disk
     Fwtgtl     if   e           k Disk
      * Entry
     dGetIDCDetail     pr                  ExtPgm('WTCLMCMP02')
     d                                9  0
     d                            32767
     d                                1
     dGetIDCDetail     pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d QtrRptQtrCd     s              3
     d QtrSubTyCd      s             10
     d xxNumDt         s              8  0
     d xxDate          s               d
     d scDate          s             10
     d xxKey           s                   Like(GTkey)
     d EmpStCd         s             22
     d ClmGend         s             25
     d StCode          s             25
     d SettCd          s             25
     d DisCd           s             25
     d MethWwg         s             25
     d ImpBasCd        s             25
     d Percent         s              4
     d PartCd          s             25
     d NatrCd          s             25
     d CausCd          s             25
     d LossCd          s             25
     d TbId            s              4
     d Amt92           s              9s 2
     d Amt72           s              7s 2
     d xxJrStCd        s             25    Inz(*Blanks)
     d xxAccStCd       s             25    Inz(*Blanks)
     d xxJrSt          s              2    Inz(*Blanks)

      /copy sPrAtrium
      /copy sPrDate

      /free
       outJSON  = ' ';

       // Load detail for IDC Prior Quarterly Files
       Exsr ProgramStart;
       Exsr ProcessIDC;

       RtnCode = ' ';
       Return;
       //==================================================================
       Begsr ProcessIDC;
        Chain(n) inRRN pqt01;
        If %found(wmidcqtl);
           Chain QTbchId pcc01;
           If %Found(wmidcscl);

           // Batch ID
              BuildJSON(outJSON:'OutBatchIDQtrDtl':QTbchID);

           // Submission file ID
              BuildJSON(outJSON:'OutQtrSubFileIDDtl':CCSubFilID);

           // Submission date
              BuildJSON(outJSON:'OutQtrSubDateDtl':CCSubDate);

           // Submission file type code
              Select;
              When CCsubTyCd = 'O';
                 QtrSubtyCd = 'Original';
              When CCsubTyCd = 'R';
                 QtrSubtyCd = 'Replace';
              Endsl;
              BuildJSON(outJSON:'OutQtrSubTypeCodeDtl':QtrSubTyCd);

           // Reporting quarter
              Select;
              When CCrptQtrCd = '1';
                 QtrRptQtrCd = '1st';
              When CCrptQtrCd = '2';
                 QtrRptQtrCd = '2nd';
              When CCrptQtrCd = '3';
                 QtrRptQtrCd = '3rd';
              When CCrptQtrCd = '4';
                 QtrRptQtrCd = '4th';
              Endsl;
              BuildJSON(outJSON:'OutQtrRptQtrCodeDtl':QtrRptQtrCd);
           Endif;

           // Jurisdiction state code
           Setll (QTco#:'IJST') pgt01;
           Reade (QTco#:'IJST') pgt01;
           Dow Not %Eof(wtgtl);
              If %Trim(GTcF01) = %Trim(QTjrStCd);
                 xxJrStCd = %Trim(GTcF01) + ' - ' + %Trim(GTdesc);
                 xxJrSt = %Trim(GTkey);
                 Leave;
              Endif;
              Reade (QTco#:'IJST') pgt01;
           Enddo;
           BuildJSON(outJSON:'OutQtrJurisStateCodeDtl':xxJrStCd);

           // Claimaint Gender code
           Select;
           When QTclmGend = '1';
              ClmGend = '1 - Male';
           When QTclmGend = '2';
              ClmGend = '2 - Female';
           Other;
              ClmGend = ' ';
           Endsl;

           BuildJSON(outJSON:'OutGenderCodeDtl':ClmGend);

           // Birth year
           BuildJSON(outJSON:'OutBirthYearDtl':QTbthYear);

           // Hire date
           xxNumDt = %Dec(QThrDate:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutHireDateDtl':scDate);

           // Employment Status code
           Select;
           When QTempStCd = '1';
              EmpStCd = '1 - Regular Full-Time';
           When QTempStCd = '2';
              EmpStCd = '2 - Part-Time';
           When QTempStCd = '8';
              EmpStCd = '8 - Seasonal';
           When QTempStCd = '9';
              EmpStCd = '9 - Volunteer';
           When QTempStCd = 'X';
              EmpStCd = 'X - Other';
           Other;
           Endsl;
           BuildJSON(outJSON:'OutEmpStatusCdDtl':EmpStCd);

           // Closing Date
           xxNumDt = %Dec(QTclDate:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutClosingDateDtl':scDate);

           // Reopen Date
           xxNumDt = %Dec(QTreoDate:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutReopenDateDtl':scDate);

           // MMI Date
           xxNumDt = %Dec(QTmmiDate:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutMMIDateDtl':scDate);

           // Reported to Insurer Date
           xxNumDt = %Dec(QTrptInsDt:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutReportedToInsDateDtl':scDate);

           // Accident state code (Use Accident State or Foreign Location)
           Setll (QTco#:'IJST') pgt01;
           Reade (QTco#:'IJST') pgt01;
           Dow Not %Eof(wtgtl);
              If %Trim(GTcF01) = %Trim(QTaccStCd);
                 xxAccStCd = %Trim(GTcF01) + ' - ' + %Trim(GTdesc);
                 Leave;
              Endif;
              Reade (QTco#:'IJST') pgt01;
           Enddo;

           If xxAccStCd = *blanks;
              Setll (QTco#:'ASLC') pgt01;
              Reade (QTco#:'ASLC') pgt01;
              Dow Not %Eof(wtgtl);
                 If %Trim(GTkey) = %Trim(QTaccStCd);
                    xxAccStCd = %Trim(GTkey) + ' - ' + %Trim(GTdesc);
                    Leave;
                 Endif;
                 Reade (QTco#:'ASLC') pgt01;
              Enddo;
           Endif;
           BuildJSON(outJSON:'OutAccidentStateCdDtl':xxAccStCd);

           // Attorney or Auth Rep Ind

           BuildJSON(outJSON:'OutAttorneyOrAuthRepIndDtl':QTrepInd);

           // Method of Determining Weekly Wage
           xxKey = QTmethWwg;
           Chain (QTco#:'WMTH':xxKey) pgt01;
           If %Found(wtgtl);
              MethWwg = %Trim(QTmethWwg) + ' - ' + %Trim(GTdesc);
           Endif;
           BuildJSON(outJSON:'OutMethodOfDetWeeklyWageDtl':MethWwg);

           // Impairment Percentage Basis Code
           Select;
           When QTimpBasCd = '1';
              ImpBasCd = '1 - Whole Body';
           When QTimpBasCd = '2';
              ImpBasCd = '2 - Part of the body';
           Endsl;
           BuildJSON(outJSON:'OutImpairmentPercentBasisCdDtl':ImpBasCd);

           // Impairment %
           Percent = %EditC(%Dec(QTimpPerc:3:0):'Z') + '%';
           BuildJSON(outJSON:'OutImpairmentPercentDtl':Percent);

           // Disability/LOEC %
           Percent = %EditC(%Dec(QTdisLOECp:3:0):'Z') + '%';
           BuildJSON(outJSON:'OutDisabilityLOECPercentDtl':Percent);

           // Preexisting Disability %
           Percent = %EditC(%Dec(QTpreDisp:3:0):'Z') + '%';
           BuildJSON(outJSON:'OutPreexistingDisabilityPercentDtl':Percent);

           // Part of Body Code
           xxKey = QTpartCd;
           Chain (QTco#:'PART':xxKey) pgt01;
           If %Found(wtgtl);
              PartCd = %Trim(QTpartCd) + ' - ' + %Trim(GTdesc);
           Endif;
           BuildJSON(outJSON:'OutPartOfBodyCodeDtl':PartCd);
           // Nature of Injury Code

           xxKey = QTnatrCd;
           Chain (QTco#:'NATR':xxKey) pgt01;
           If %Found(wtgtl);
              NatrCd = %Trim(QTnatrCd) + ' - ' + %Trim(GTdesc);
           Endif;
           BuildJSON(outJSON:'OutNatureOfInjuryCodeDtl':NatrCd);

           // Cause of Injury Code
           xxKey = QTcausCd;
           Chain (QTco#:'CAUS':xxKey) pgt01;
           If %Found(wtgtl);
              CausCd = %Trim(QTcausCd) + ' - ' + %Trim(GTdesc);
           Endif;
           BuildJSON(outJSON:'OutCauseOfInjuryCodeDtl':CausCd);
           // Act-loss Condition Code

           xxKey = QTlossCd;
           TbId = %Trim(QTjrStCd) + 'AC';
           Chain (QTco#:TbId:xxKey) pgt01;
           If %Found(wtgtl);
              LossCd = %Trim(QTlossCd) + ' - ' + %Trim(GTdesc);
           Else;
              TbId = '  AC';
              Chain (QTco#:TbId:xxKey) pgt01;
              If %Found(wtgtl);
                 LossCd = %Trim(QTlossCd) + ' - ' + %Trim(GTdesc);
              Endif;
           Endif;
           BuildJSON(outJSON:'OutActLossConditionCodeDtl':LossCd);

           // Type of Settlement-loss Condition Code
           xxKey = QTsettCd;
           TbId = xxJrSt + 'MS';
           Chain (QTco#:TbId) pgt01;
           If Not %Found(wtgtl);
              TbId  = '  MS';
           Endif;

           Chain (QTco#:TbId:xxKey) pgt01;
           If %Found(wtgtl);
              SettCd = %Trim(QTsettCd) + ' - ' + %Trim(GTdesc);
           Endif;
           BuildJSON(outJSON:'OutTypeOfSettLossConditionCodeDtl':SettCd);

           // Medical Extinguishment Indicator
           BuildJSON(outJSON:'OutMedicalExtIndDtl':QTmedExtIn);

           // Temporary Disability Benefit Extinguishment Code
           xxKey = QTtmpDisCd;
           Chain (QTco#:'TBDE':xxKey) pgt01;
           If %Found(wtgtl);
              DisCd = %Trim(QTtmpDisCd) + ' - ' + %Trim(GTdesc);
           Endif;
           BuildJSON(outJSON:'OutTempDisBenefitExtCodeDtl':DisCd);

           // Indemnity Paid to Date
           Monitor;
           Amt92 = %Dec(QTindPdTdt:9:0)/100;
           On-Error;
           Amt92 = 0;
           Endmon;
           BuildJSON(outJSON:'OutIndemnityPaidToDateDtl':%Editc(Amt92:'J'));

           // Medical Paid to Date
           Monitor;
           Amt92 = %Dec(QTmedPdTdt:9:0)/100;
           On-Error;
           Amt92 = 0;
           Endmon;
           BuildJSON(outJSON:'OutMedPaidToDateDtl':%Editc(Amt92:'J'));

           // Incurred Indemnity Amount
           Monitor;
           Amt92 = %Dec(QTincInAmt:9:0)/100;
           On-Error;
           Amt92 = 0;
           Endmon;
           BuildJSON(outJSON:'OutIncurredIndemnityAmountDtl':%EditC(Amt92:'J'));

           // Incurred Medical Amount
           Monitor;
           Amt92 = %Dec(QTincMeAmt:9:0)/100;
           On-Error;
           Amt92 = 0;
           Endmon;
           BuildJSON(outJSON:'OutIncurredMedicalAmountDtl':%EditC(Amt92:'J'));

           // Employer Legal Amount Paid
           Monitor;
           Amt92 = %Dec(QTempLgAmt:9:0)/100;
           On-Error;
           Amt92 = 0;
           Endmon;
           BuildJSON(outJSON:'OutEmployerLegalAmountPaidDtl':%EditC(Amt92:'J'));

           // Allocated Loss Adjustment
           Monitor;
           Amt92 = %Dec(QTallLosPd:9:0)/100;
           On-Error;
           Amt92 = 0;
           Endmon;
           BuildJSON(outJSON:'OutAllocLossAdjExpensePaidDtl':%EditC(Amt92:'J'));

           // Pre-injury/Average Weekly Wage Amount
           Monitor;
           Amt72 = %Dec(QTprInjAww:5:0);
           On-Error;
           Amt72 = 0;
           Endmon;
           BuildJSON(outJSON:'OutPreInjAvgWeeklyWageAmtDtl':%EditC(Amt72:'J'));

           // Carrier Code

           BuildJSON(outJSON:'OutQtrCarrierCodeDtl':QTcarCd);

           // Policy Number Identifier

           BuildJSON(outJSON:'OutQtrPolicyNumberIDDtl':QTpolID);

           // Policy Effective Date

           Monitor;
           xxNumDt = %Dec(QTpolEfDt:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutQtrPolicyEffectiveDateDtl':scDate);
           On-Error;
              BuildJSON(outJSON:'OutQtrPolicyEffectiveDateDtl':' ');
           Endmon;

           // Claim Number Identifier

           BuildJSON(outJSON:'OutQtrClaimNumberIDDtl':QTclmID);

           // Accident Date

           xxNumDt = %Dec(QTAcDt:8:0);
           xxDate = CvtToDate(xxNumDt);
           scDate = AlphaUSA(xxDate);
           BuildJSON(outJSON:'OutQtrAccidentDateDtl':scDate);

      *** Beg Add *** 01/14/25 *** WC2995 *****************************
           // Classification Code

           BuildJSON(outJSON:'OutQtrClassificationCode':QTClsCD);

           // Return to Work Date

           Monitor;
              xxNumDt = %Dec(QTRtWD:8:0);
              xxDate  = CvtToDate(xxNumDt);
              scDate  = AlphaUSA(xxDate);
           On-Error;
              scDate = ' ';
           Endmon;
           BuildJSON(outJSON:'OutQtrReturnWorkDate':scDate);

           // Zip Code of Injury Site

           BuildJSON(outJSON:'OutQtrZipCodeofInjurySite':QTZipInSt);

           // Number of Dependents

           BuildJSON(outJSON:'OutQtrNumberofDependents':QTDependt);

           // Exposure State Code

           BuildJSON(outJSON:'OutQtrExposureStateCode':QTExpSt);

           // Indemnity Claim Code

           BuildJSON(outJSON:'OutQtrIndemnityClaimCode':QTIndClmCd);

      *** End Add *** 01/14/25 *** WC2995 *****************************

        Endif;
       Endsr;
       //==================================================================
       Begsr ProgramStart;
       Endsr;
      /end-free

