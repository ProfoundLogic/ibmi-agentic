     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTISOCPRC - ISO CMS Import Processer
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h*****************************************************************
     h*  Created on 08/19/16   by C:309 for Project: 114WYA           *
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h*****************************************************************

     fstctl1    if   e           k disk    rename(pct01:pct01)
     fwmcml1    if   e           k disk    rename(pcm01:pcm01)
     fwmcdl     uf   e           k disk    rename(pcd01:pcd00)
     f                                     usropn
     fwaisocmi1 if   e           k disk    rename(pisocmi1:pisocm10p)
     fwaisocmi2 if   e           k disk    rename(pisocmi2:pisocm20p)
     fwaisocmi3 if   e           k disk    rename(pisocmi3:pisocm30p)
      *

     * Intialize the Screen fields
     d LoadInitVal     pr

     * Main Processing
     d Main            pr

     * Program Start
     d ProgramStart    pr

     * Write iDart
     d WriteiDart      pr

     * Call CSI1325R
     d callCSI1325R    pr                  extPgm('CSI1325R')
     d ##ClmKey                      50
     d ##ClmntK                      50
     d ##Hicn                        12

     * Call DA700R
     d CallDA700R      pr                  extPgm('DA700R')
     d ##Key#                        40                                         01
     d ##Comm#                      256                                         02
     d ##Cdat#                        8                                         03
     d ##Dcid#                       12                                         04
     d ##Pnam#                       10                                         05
     d ##Secl#                        3                                         06
     d ##User#                       10                                         07
     d ##Pgm#                        10                                         08
     d ##Fold#                       80                                         09
     d ##Difl#                        1                                         10
     d ##Ipgm#                       10                                         11
     d ##Fl01#                        1                                         12
     d ##Fl02#                        1                                         13
     d ##Rmc#                         5                                         14
     d ##Seq#                         5                                         15
     d ##Sbyl#                        1    options(*nopass)                     16
     d ##Com2#                       19    options(*nopass)                     17
     d ##Apid#                        2    options(*nopass)                     18
     d ##Idoc#                       12    options(*nopass)                     19
     d ##Ique#                       10    options(*nopass)                     20
     d ##Imgf#                        1    options(*nopass)                     21
     d ##Ipth#                       10    options(*nopass)                     22
     d ##Pidx#                        5    options(*nopass)                     23
     d ##Qpty#                        1    options(*nopass)                     24
     d ##Dctp#                        5    options(*nopass)                     25
     d ##Csid#                        2    options(*nopass)                     26
     d ##Code#                        2    options(*nopass)                     27

     dpsds           esds                  extname(szq1p)
      *
     disoDs            ds
     d strFileDat                     8
     d strIfsLoc                     80
     d strAuth#                       7
     d strFltrElgC                    5
     d strFltrMisC                    5
     d strFltrAlrC                    5
     d strFltrElgS                    5
     d strFltrMisS                    5
     d strFltrAlrS                    5

     * Array to Store Alert Code and their Definitions
     d arrAlrtCode     s              7    dim(ConAlrCnt) ctdata perrcd(1)
     d  arrAlrtDes     s             73    dim(ConAlrCnt) alt(arrAlrtCode)

     * Array to Store Missing Data values and corresponding Definitions
     d arrMissCode     s             47    dim(ConMisCnt) ctdata perrcd(1)
     d  arrMissDes     s             33    dim(ConMisCnt) alt(arrMissCode)

     * Constant variables
     d ConAlrCnt       c                   73
     d ConMisCnt       c                   124

     * Standalone variables
     d  IsoCMSPrmp     s            160    dtaara
     d intIndex        s              3  0 inz
     d intIndex1       s              3  0 inz
     d strAlrtDes      s            500    inz
     d strMissDes      s            500    inz
     d ##Key#          s             40
     d ##Comm#         s            256
     d ##Cdat#         s              8
     d ##Dcid#         s             12
     d ##Pnam#         s             10
     d ##Secl#         s              3
     d ##User#         s             10
     d ##Pgm#          s             10
     d ##Fold#         s             80
     d ##Difl#         s              1
     d ##Ipgm#         s             10
     d ##Fl01#         s              1
     d ##Fl02#         s              1
     d ##Rmc#          s              5
     d ##Seq#          s              5
     d ##Sbyl#         s              1
     d ##Com2#         s             19
     d ##Apid#         s              2
     d ##Idoc#         s             12
     d ##Ique#         s             10
     d ##Imgf#         s              1
     d ##Ipth#         s             10
     d ##Pidx#         s              5
     d ##Qpty#         s              1
     d ##Dctp#         s              5
     d ##Csid#         s              2
     d ##Code#         s              2
     d ##ClmKey        s             50A
     d ##ClmntK        s             50A
     d ##Hicn          s             12A
     d strFile         s             10A   inz
     d intCo#          s                   like(CMCo#)
     d intFnd          s                   like(CMFnd)
     d intFyr          s                   like(CMFyr)
     d intCase         s                   like(CMcase)
     d intSS#          s                   like(CMss#)
     d isFoundCTP      s              1    inz
     d strRemCodeKy    s              5A   inz

      /Free

        // Start Of Program
        LoadInitVal();

        // Main Procesing
        Main();

        *InLr = *on;
      /End-Free

       //===================================================================
       // LoadInitVal - Intialize the fields
       //===================================================================

     p LoadInitVal     b
     d LoadInitVal     pi
      /Free

        in *lock *dtaara;
        isods   = IsoCMSPrmp;

      /End-free
     p LoadInitVal     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /Free

       //-------------------------------------//
       //1. Process Eligibilty Results file   //
       //-------------------------------------//
        strFile = 'WAISOCMI1';
        Read pisocm10p;
        Dow Not %Eof(waisocmi1);
           If %Subst(I1clmkey:17:6) = 'W0070R';
              intCo# = %Dec(%Trim(%Subst(I1clmkey:1:3)):3:0);
              intFnd = %Dec(%Trim(%Subst(I1clmkey:4:3)):3:0);
              intFyr = %Dec(%Trim(%Subst(I1clmkey:7:3)):3:0);
              intCase= %Dec(%Trim(%Subst(I1clmkey:10:7)):7:0);
              ##Key# = %Subst(%Trim(I1clmkey):1:16);                      // 01
              ##Rmc# =  strFltrElgC;                                      // 14
              ##Pnam# = 'W0070R';                                         // 06
              strRemCodeKy = strFltrElgC;
              Chain (intCo#:intFnd:intFyr:intCase) pcm01;
              If %Found(wmcml1);
                 intSS# = CMss#;
                 If Not %Open(wmcdl);
                    Open wmcdl;
                 EndIf;
                 Chain (intSS#) pcd00;
                 If %found(wmcdl);
                    CDhicn = I1hicn;
                    If I1elresult = '01';
                       CDmdbn = 'Y';
                    Endif;
                    Update pcd00;
                    If %Open(wmcdl);
                       Close wmcdl;
                    EndIf;
                 Endif;
              Endif;
           Else;
              ##ClmKey  =  %Trim(I1clmkey)    ;
              ##ClmntK  =  %Trim(I1clmntk)    ;
              ##Hicn    =  %Trim(I1hicn)      ;
              ##Key#    =  %Trim(I1clmkey)    ;                               // 01
              ##Rmc#    =  strFltrElgS        ;                               // 14
              ##Pnam#   =  *Blanks            ;                               // 06
              strRemCodeKy = strFltrElgS;

              callCSI1325R(##ClmKey:##ClmntK:##Hicn);
           Endif;

           If I1elresult = '01';
              WriteiDart();
           Endif;

           Read pisocm10p;
        Enddo;

       //-------------------------------------//
       //2. Process User Alerts file          //
       //-------------------------------------//
        strFile = 'WAISOCMI2';
        Read pisocm20p;
        Dow Not %Eof(waisocmi2);
           WriteiDart();

           // Read next record
           Read pisocm20p;
        Enddo;

       //-------------------------------------//
       //3. Process Missing Data file         //
       //-------------------------------------//
        strFile = 'WAISOCMI3';
        Read pisocm30p;
        Dow Not%Eof(waisocmi3);
            WriteiDart();

            // Read next record
            Read pisocm30p;
        Enddo;
      /End-free
     p Main            e

      *-------------------------------------------------*
      *      Write iDart                                *
      *-------------------------------------------------*
     p WriteiDart      b
     d WriteiDart      pi
      /Free

        Select;
        When strFile = 'WAISOCMI1';
           ##Comm# = 'Medicare Eligibility Results are Positive';       // 02
           ##User# = I1adjkey;                                          // 08

           Chain (strRemCodeKy) pct01;
           If %Found(stctl1);
              ##Dcid# = CTdepg;                                         // 04
              ##Difl# = CTfl01;                                         // 05
              isFoundCtp= 'Y';
           Endif;

        When strFile = 'WAISOCMI2';
           strAlrtDes = *Blanks;
           intIndex = %Lookup(I2alertcd:arrAlrtCode) ;
           If intIndex <> *Zeros;
              Dow intIndex <= ConAlrCnt and
                 arrAlrtCode(intIndex) = I2alertcd;
                 strAlrtDes =  %Trim(strAlrtDes) + ' ' +
                  %Trim(arrAlrtDes(intIndex));
                 intIndex+=1;
              Enddo;
           Endif;

           If %Subst(I2clmkey:17:6) = 'W0070R';
              ##Key#  = %Subst(%Trim(I2clmkey):1:16);                   // 01
              ##Rmc#  = strFltrAlrC;                                    // 14
              ##Pnam# = 'W0070R';                                       // 06
              strRemCodeKy = strFltrAlrC;
           Else;
              ##Key#  = %Trim(I2clmkey);                                // 01
              ##Rmc#  = strFltrAlrS;                                    // 14
              ##Pnam# = *Blanks ;                                       // 06
              strRemCodeKy = strFltrAlrS;
           EndIf;

           ##Comm# = %Char(I2alertdt) + '_' + %Trim(I2alertcd) + '_'    // 02
            + %trim(I2alertnam) + '_' + %Trim(strAlrtDes);

           ##User# = I2adjkey;                                          // 08
           Chain (strRemCodeKy) pct01;
           If %Found(stctl1);
              ##Dcid# = CTdepg;                                         // 04
              ##Difl# = CTfl01;                                         // 05
              isFoundCtp= 'Y';
           Endif;

        When strFile = 'WAISOCMI3';
           strMissDes = *Blanks;
           intIndex1 = %Lookup(I3busFldnm:arrMissCode) ;
           If intIndex1 <> *Zeros;
              Dow intIndex1 <= ConMisCnt and
                 arrMissCode(intIndex1) = I3busFldnm;
                 strMissDes =  %Trim(strMissDes) + ' ' +
                  %Trim(arrMissDes(intIndex1));
                 intIndex1+=1;
              Enddo;
           Endif;

           If %Subst(I3clmkey:17:6) = 'W0070R';
              ##Key#  = %Subst(%Trim(I3clmkey):1:16);                   // 01
              ##Rmc#  = strFltrMisC;                                    // 14
              ##Pnam# = 'W0070R';                                       // 06
              strRemCodeKy = strFltrMisC;
           Else;
              ##Key#  = %Trim(I3clmkey);                                // 01
              ##Rmc#  = strFltrMisS;                                    // 14
              ##Pnam# = *Blanks ;                                       // 06
              strRemCodeKy = strFltrMisS;
           EndIf;

           ##Comm# = %Trim(I3busFldnm) + '_'   +                        // 02
            %Trim(strMissDes);

           ##User# = I3adjkey;                                          // 08
           Chain (strRemCodeKy) pct01;
           If %found(stctl1);
              ##Dcid# = CTdepg;                                         // 04
              ##Difl# = CTfl01;                                         // 05
              isFoundCtp= 'Y';
           Endif;
        Other;
           // Do Nothing
        Endsl;

        ##Cdat#=  %Char( %date():*ISO0);                                // 03
        ##Secl# = *Blanks;                                              // 07
        ##Pgm#  = q1pgm;                                                // 09

        If isFoundCtp= 'Y';
           If CTdprg > *Zeros ;
              ##Fl01# = 'Y';                                            // 10
           Else;
              ##Fl01# = *Blanks;
           Endif;

           If CTdepg <> *Blanks;
              ##Fl02# = 'Y';                                            // 11
           Else;
              ##Fl02# = *Blanks;
           Endif;
        Else;
           ##Fl01# = *Blanks;
           ##Fl02# = *Blanks;
        Endif;
        ##Ipgm# = q1pgm;                                                // 12
        ##Fold# = *Blanks;                                              // 13
        ##Seq#  = *Zeros;                                               // 15
        ##Sbyl# = 'N';                                                  // 16
        ##Com2# = *Blanks;                                              // 17
        ##Apid# = *Blanks;                                              // 18
        ##Idoc# = *Blanks;                                              // 19
        ##Ique# = *Blanks;                                              // 20
        ##Imgf# = *Blanks;                                              // 21
        ##Ipth# = *Blanks;                                              // 22
        ##Pidx# = *Blanks;                                              // 23
        ##Qpty# = *Blanks;                                              // 24
        ##Dctp# = *Blanks;                                              // 25
        ##Csid# = *Blanks;                                              // 26
        ##Code# = *Blanks;                                              // 27

        CallDA700R(##Key#:##Comm#:##Cdat#:##Dcid#:##Pnam#:##Secl#:
         ##User#:##Pgm#:##Fold#:##Difl#:##Ipgm#:##Fl01#:##Fl02#:
         ##Rmc#:##Seq#:##Sbyl#:##Com2#:##Apid#:##Idoc#:##Ique#:
         ##Imgf#:##Ipth#:##Pidx#:##Qpty#:##Dctp#:##Csid#:##Code#);
      /End-Free
     p WriteiDart      e

**   Alert Type
NAV_001There are no reserves open and there has been a large payment.
NAV_001Investigate to determine if claim is closed and if formal
NAV_001settlement has occurred.
NAV_002A claim exists with an ORM termination date but no TPOC settlement
NAV_002date. Investigate this claim to determine if a TPOC settlement
NAV_002date needs to be reported.
NAV_003If a claim note has the type that would indicate a settlement, but
NAV_003no TPOC data has been reported. Investigate this claim to determine
NAV_003if a TPOC settlement date should be reported.
NAV_004The claim payment stream reveals the existence of a Final Payment.
NAV_004Investigate this claim to determine if TPOC data should be reported.
NAV_005The Litigation Settlement Indicator has been set to positive. This
NAV_005indicates that the litigation system shows settlement for this claim.
NAV_005Validate and if valid, populate TPOC Date and TPOC Amount.
NAV_006TPOC date is in the future. The claim will not be reported until the
NAV_006next reporting period following that date.
NAV_007Medical payments are reported for a no fault claim, but the ORM
NAV_007Indicator remains set to No.
NAV_008This alert occurs when payments are reported for a no fault claim, but
NAV_008the ORM indicator remains set to no. This claim should be reviewed to
NAV_008determine if an ORM event has occurred and should be reported.
NAV_009A claim note has the type that would indicate a settlement, but no ORM
NAV_009termination date has been reported. Investigate this claim to determine
NAV_009if valid documentation exists to qualify for termination of ORM.
NAV_010Medical Release Indicator has been set to positive. This indicates that
NAV_010a medical release event may have occurred in the source system.
NAV_010Investigate this claim to determine if valid documentation exists to
NAV_010qualify for termination of ORM.
NAV_011The Litigation Settlement Indicator has been set to positive.
NAV_011Investigate this claim to determine if valid documentation exists to
NAV_011qualify for termination of ORM.
NAV_012ORM Termination Date or TPOC Date is on or after 1/1/2010, ORM Indicator
NAV_012is No and Insurance Type is either D (No Fault) or E (Workers Comp).
NAV_013A Medical Payment has been made on a claim with a Closed status and ORM
NAV_013Indicator set to No.
NAV_014The claim payment stream reveals the existence of a Final Payment.
NAV_014Investigate this claim to determine if valid documentation exists to
NAV_014qualify for termination of ORM.
NAV_015A claim in Open status has a Date of Death entered, but no Non-Medicare
NAV_015Beneficiary has been identified.
NAV_016A claim with an ORM indicator has a claimant who has attained the age of
NAV_01665, but continues to return a negative result on the CMS Query. This
NAV_016could be an indication that the claimant data is incorrect. Investigate
NAV_016this claim to determine if further action may be required to correct
NAV_016claimant data or report TPOC/ORM data.
NAV_017TPOC Date may not be on or before the Date of Incident.
NAV_018ORM Termination Date may not be on or before the Date of Incident.
NAV_019One or more of the following dates results in a date sequence error:
NAV_019Date of Birth, CMS and/or Industry Date of Incident, ORM Termination
NAV_019Date, TPOC Date, No Fault Exhaust Date, Date of Death.
NAV_020This alert occurs when an injured party has a potentially invalid
NAV_020Social Security Number.
NAV_021Medicals Paid is greater than zero and/or First Medical Payment Date is
NAV_021not null. If this is correct, ORM should be set to Yes.
NAV_022Reported medical payments indicate possible exhaustion of no-fault limit.
NAV_022If this is the case, please enter No-Fault Exhaust Date.
NAV_023A change to SSN/HICN has been submitted for a claimant that has been
NAV_023verified by CMS as being a Medicare beneficiary. The returned HICN has
NAV_023been verified, and may not be changed. If the SSN is not correct, a new
NAV_023claim  must be added with the new SSN and, if appropriate, deactivate or
NAV_023purge the original claim.
NAV_026The ORM Status on this reported claim was changed from Yes to No.
NAV_026ORM should not be changed unless the claim was reported in error.
NAV_026Changing the status will cause this claim to be deleted by CMS and
NAV_026may cause a non-reporting penalty. If ORM no longer exists, leave
NAV_026the ORM Status as Yes and enter an ORM Termination Date. Contact
NAV_026your ISO CP representative if you are not certain of this action.
NAV_027A claim involving a Medicare beneficiary was closed with no TPOC.
NAV_029TPOC information was entered on a No-Fault claim. This situation
NAV_029is very rare. Please confirm this is a settlement to ensure
NAV_029accurate reporting to CMS. If this should be ORM, delete the
NAV_029TPOC information and confirm the ORM information for this claim
NAV_029is accurate.
**   Missing Data
Claim: ORM Status                              Required. ORM means Ongoing
Claim: ORM Status                              Responsibility for Medical.
Claim: ORM Status                              In general, this is set to
Claim: ORM Status                              "Y" for Workers' Compensation
Claim: ORM Status                              and No-Fault claims and "N"
Claim: ORM Status                              for Liability claims.
Claim: ORM Termination Date                    The date ORM ended, where
Claim: ORM Termination Date                    applicable. When ORM
Claim: ORM Termination Date                    Termination occurs, the ORM
Claim: ORM Termination Date                    indicator must remain set to
Claim: ORM Termination Date                    "Y".
Claim: CMS Date of Incident                    Occurrence date of the injury.
Claim: CMS Date of Incident                    For an accident, this is the
Claim: CMS Date of Incident                    date of the accident. If the
Claim: CMS Date of Incident                    claim alleges injury caused by
Claim: CMS Date of Incident                    exposure, ingestion or
Claim: CMS Date of Incident                    implantation, this is the date
Claim: CMS Date of Incident                    of first exposure, ingestion or
Claim: CMS Date of Incident                    implantation.
Claim: State of Venue                          2 position state code of the
Claim: State of Venue                          state whose law controls the
Claim: State of Venue                          resolution of the claim. If not
Claim: State of Venue                          in the United States, use "FC"
Claim: State of Venue                          for foreign country.
Claim: TPOC Settlement Amount                  The total amount of the
Claim: TPOC Settlement Amount                  settlement, award or judgment,
Claim: TPOC Settlement Amount                  regardless of how many payments
Claim: TPOC Settlement Amount                  were/will be made. If the
Claim: TPOC Settlement Amount                  settlement involves a structured
Claim: TPOC Settlement Amount                  payout, enter the total amount
Claim: TPOC Settlement Amount                  that will be paid out over the
Claim: TPOC Settlement Amount                  life of the settlement. If the
Claim: TPOC Settlement Amount                  settlement is a life annuity,
Claim: TPOC Settlement Amount                  enter the expected payout based
Claim: TPOC Settlement Amount                  on mortality tables as applicable
Claim: TPOC Settlement Amount                  to the injured party.
Claim: Settlement Date                         The date the award or judgment is
Claim: Settlement Date                         approved, or the date of last
Claim: Settlement Date                         signature on the settlement
Claim: Settlement Date                         agreement. This field will be
Claim: Settlement Date                         used to report the TPOC (Total
Claim: Settlement Date                         Payment Obligation to Claimant)
Claim: Settlement Date                         Date to CMS.
Representative: Type                           If the injured party is
Representative: Type                           represented, select the
Representative: Type                           appropriate type. NOTE: This is
Representative: Type                           only for the injured party's
Representative: Type                           representative. If there is
Representative: Type                           another party to the claim who is
Representative: Type                           represented, see Non-Medicare
Representative: Type                           Beneficiary tab. Attorney;
Representative: Type                           Guardian/Conservator; Power of
Representative: Type                           Attorney; Other; None
Representative: Firm Name                      The name of the firm associated
Representative: Firm Name                      with the injured party's
Representative: Firm Name                      representative. Either this or
Representative: Firm Name                      First Name/Last Name is required.
Representative: First Name                     First name of the injured party's
Representative: First Name                     representative.Either this and
Representative: First Name                     Last Name, or Firm Name is
Representative: First Name                     required.
Representative: Last Name                      Last name of the injured party's
Representative: Last Name                      representative.Either this and
Representative: Last Name                      First Name, or Firm Name is
Representative: Last Name                      required.
Representative: Address #1                     The first address line of the
Representative: Address #1                     injured party's representative.
Representative: Address #1                     If no US address is available,
Representative: Address #1                     type FC for foreign country.
Representative: City                           The city of residence (or office
Representative: City                           location) of the injured party's
Representative: City                           representative. If no US address
Representative: City                           is available, type "FC" for
Representative: City                           foreign country.
Representative: State                          The state of residence (or office
Representative: State                          location) of the injured party's
Representative: State                          representative. If no US address
Representative: State                          is available, type "FC" for
Representative: State                          foreign country.
Representative: Zip                            The US Postal Service code
Representative: Zip                            associated with the address of
Representative: Zip                            the injured partys representative.
Representative: Zip                            If no US address is available,
Representative: Zip                            fill with zeroes.
Representative: Telephone                      The telephone number of the
Representative: Telephone                      injured party's representative.
Insured: Policy Number                         Policy under which the claim is
Insured: Policy Number                         being handled.
Insured: Self Insured Flag                     "Yes" indicates that the claim
Insured: Self Insured Flag                     is against a self-insured entity.
Insured: Self Insured Flag                     "No" indicates that the claim is
Insured: Self Insured Flag                     not against a self-insured entity.
Insured: Self Insured Flag                     (Not applicable for Insurance
Insured: Self Insured Flag                     Type = No Fault).
Insured: Self Insured Type                     If Self-Insured is "Yes"; select
Insured: Self Insured Type                     either "Individual" or "Other
Insured: Self Insured Type                     than individual".
Insured: Self Insured Individual First Name    Provide this information if
Insured: Self Insured Individual First Name    Self-Insured Type = "I".
Insured: Self Insured Individual Last Name     Provide this information if
Insured: Self Insured Individual Last Name     Self-Insured Type = "I".
Insured: Self Insured Legal Name               Provide this information if
Insured: Self Insured Legal Name               Self-Insured Type = "O".
Insured: Self Insured Legal Name               Legal name of "Doing Business As"
Insured: Self Insured Legal Name               name of the self-insured entity.
Insured: Self Insured Legal Name               One of these values must be
Insured: Self Insured Legal Name               entered.
Insured: Self Insured DBA Name                 Provide this information if
Insured: Self Insured DBA Name                 Self-Insured Type = "O".
Insured: Self Insured DBA Name                 Legal name of "Doing Business As"
Insured: Self Insured DBA Name                 name of the self-insured entity.
Insured: Self Insured DBA Name                 One of these values must be
Insured: Self Insured DBA Name                 entered.
Insured: No-Fault Policy Limit                 Enter the dollar limit of
Insured: No-Fault Policy Limit                 coverage on policy.
Insured: No-Fault Exhaust Date For Total AmountEnter the date the No-Fault
Insured: No-Fault Exhaust Date For Total AmountPolicy Limit was reached.
ICD Codes: ICD Code                            Diagnosis Code applicable to the
ICD Codes: ICD Code                            injury or illness. Defined in the
ICD Codes: ICD Code                            International Classification of
ICD Codes: ICD Code                            Diseases. Reporting is valid for
ICD Codes: ICD Code                            any ICD-9 diagnosis code found in
ICD Codes: ICD Code                            any of the latest three versions
ICD Codes: ICD Code                            posted.
