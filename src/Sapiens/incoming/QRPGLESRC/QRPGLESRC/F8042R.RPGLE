     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H Bnddir('FTBNDDIR')
     H Bnddir('F8042R')
     H*                                                               *
     H*  F8042R - Upload Bills - Transfer Program 2                   *
     H*****************************************************************
     H*                                                               *
     H*  Called from: F8041R  - Upload Bills - Transfer Pgm 1         *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/20/2004 by c:62 for project 51MFN              *
     H*****************************************************************
     H*  Revisions:                                                   *
     h*                                                               *
     h*  01/31/25 C:079 Support: 129/118396                           *
     h*                 Check for blanks before validating Provider ID*
     h*                                                               *
     h*  01/04/24 C:079 Support: 152/104126                           *
     h*                 Add EFT Logic                                 *
     h*                                                               *
     h*  09/07/23 C:079 Support: 146-113770                           *
     h*                 Edit Provider ID (U1prov)                     *
     h*                                                               *
     h*  07/18/23 C:342 Project: NY060                                *
     h*                 NY Compliance: CARC/RARC Update               *
     h*                                                               *
     h*  06/29/23 C:079 Support: 146/111883                           *
     h*                 Use original bill ID for recons based on flag *
     h*                                                               *
     h*  04/26/23 C:079 Support: 146/109009                           *
     h*                 Store Transaction ID in FAEOP Remark Desc.    *
     h*                                                               *
     h*  12/22/21 C:079 Support: 146/95084                            *
     h*                 When U1prov is populated check DBA name flag  *
     h*                                                               *
     h*  12/29/21 C:079 Support: 146/100221                           *
     h*                 Monitor for errors when loading U1PROV        *
     h*                                                               *
     h*  12/01/21 C:079 Support: 146/99104                            *
     h*                 Condition logic that righ justifies EOB code  *
     h*                                                               *
     h*  06/11/21 C:079 Support: 90/95609                             *
     h*                 Add logic to load provider info for recons    *
     h*                 into FAEAP fields                             *
     h*                                                               *
     h*  04/19/21 C:079 Support: 90/95537                             *
     h*                 Add Bank Code to API                          *
     h*                                                               *
     h*  02/25/21 C:079 Support: 129/94711                            *
     h*                 Use U1AST as Adj. State even if not installed *
     h*                                                               *
     h*  07/20/20 C:079 Project: WC2749                               *
     h*                 Set EAeftC = V if EOB qualifies (Zero EOB's)  *
     H*                                                               *
     H*  06/15/20 C:079 Support: PHW89431                             *
     H*                 Populate EOB status history date correctly    *
     H*                 for recons and FAEAP transaction date and     *
     H*                 default the user.                             *
     H*                                                               *
    H*  05/28/20 C:079 Project: 152MBM                               *
    H*                 Add Reason Code Description 2                 *
    H*                                                               *
    H*  10/23/19 C:079 Support: 146/84509                            *
    H*                 Other2 Use Add/Sub not Z-Add/Z-Sub for recons *
    H*                                                               *
    H*  10/11/19 C:079 Support: 152/84043                            *
    H*                 Prevent Header Note from being written to     *
    H*                 the detail file                               *
    H*                                                               *
    H*  08/23/19 C:079 Support: 152/82310                            *
    H*                 Remove monitor on U1dFc                       *
    H*                                                               *
    H*  07/24/19 C:079 Project: 152MAP                               *
    H*                 Use larger EOB note fields                    *
    H*                                                               *
    H*  05/03/19 C:079 Project: 152MAP                               *
    H*                 Populate transaction code.                    *
    H*                 Populate FL Revision Indicator                *
    H*                 Add client 152 to orig trans ID recon logic   *
    H*                                                               *
    H*  02/28/19 C:079 Support: 45/77523                             *
    H*                 Add logic to check for error on U1OTRID       *
    H*                 conversion to numeric                         *
    H*                                                               *
    H*  09/12/18 C:079 Support: 99/73314                             *
    H*                 Update the correct FAU2P record for recons    *
     H*                                                               *
     H*  09/05/18 C:079 Support: 146/73185                            *
     H*                 Add logic to update EOSTAT with R for recons  *
     H*                                                               *
     H*  07/16/18 C:079 Project: 133MQS                               *
     H*                 Add client 133 to orig. trans. ID recon logic *
     H*                 for bills from RMS (Rising)                   *
     H*                                                               *
     H*  06/21/18 C:079 Project: TX101                                *
     H*                 Set CArcFl and CAsrc correctly for TX Medicare*
     H*                                                               *
     H*  06/18/18 C:079 Support 146 71291                             *
     H*                 Add reconsideration edits.                    *
     H*                                                               *
     H*  04/02/18 C:079 Project 114MJB                                *
     H*                 Update EABTYP                                 *
     H*                                                               *
     H*  01/12/18 C:079 Support: 146/63667                            *
     H*                 Add client 146 to orig. trans. ID recon logic *
     H*                                                               *
     H*  11/7/17  C:079 Project: 146MGT                               *
     H*                 If the provider check is issue in the batch   *
     H*                 then dont issue here.                         *
     H*                                                               *
     H*  11/3/17  C:079 Project: 146MGT                               *
     H*                 Add logic for client 146 to set EAedi         *
     H*                                                               *
     H*  05/01/17 C:079 Project: MC325                                *
     H*                 Add logic for SA016R                          *
     H*                                                               *
     H*  04/12/17 C:079 Support : 46090                               *
     H*                 Let NOVAR use original transaction ID         *
     H*                                                               *
     H*  03/14/17 C:309 Support : 46147                               *
     H*                 Check U1AST if valid then only load into EOAST*
     H*                                                               *
     H*  02/22/17 C:079 Project : 90MHY                               *
     H*                 For Optum load units with two decimal pos.    *
     H*                                                               *
     H*  01/30/17 C:212 Project : 90MIF                               *
     H*                 Add changes for Auto check for possible       *
     H*                 duplicates                                    *
     H*                                                               *
     H*  12/20/16 C:079 Project : 90MAI                               *
     H*                 Add client 90 to duplicate check              *
     H*                                                               *
     H*  12/05/16 C:079 Support: 89/44549                             *
     H*                 Load DUrate correctly                         *
     H*                                                               *
     H*  11/22/16 C:309 Project : 25MUJ                               *
     H*                 Add call to Detail Line item API "FA786R"     *
     H*                                                               *
     H*  11/10/16 C:079 Support : 90MHT                               *
     H*                 If EOTAPE = Y then set EAEDI = E.             *
     H*                                                               *
     H*  10/21/16 C:212 Project: 89MBT                                *
     H*                 Fix to avoid decimal data error in some cases *
     H*                                                               *
     H*  09/06/16 C:079 Support : 42272/133                           *
     H*                 Add U1PROV logic to WRTEAP                    *
     H*                                                               *
     H*  06/23/16 C:309 Project : 45MKI                               *
     H*                 Changes for Reconsiderations                  *
     H*                                                               *
     H*  05/26/16 C:079 Project : 133MOM                              *
     H*                  Add recon logic using original TRID          *
     H*                                                               *
     H*  07/07/16 C:079 Support : 42351/099                           *
     H*                  Load U1DFC as 8A                             *
     H*                                                               *
     H*  05/24/16 C:307 Support : 40844                               *
     H*                  Map FAU1P/U1PACD to FAEOP/EOFLG8 in WRTEOP   *
     H*                  subroutine                                   *
     H*                                                               *
     H*  03/21/16 C:309 Project : 129MUK                              *
     H*                 NH Automated Bill Repricing                   *
     H*                                                               *
     H*  11/17/15 C:309 Retrofitted Project MC399 changes done on     *
     H*                 "07/30/15" by C:212                           *
     H*                 Make diagnosis code alphanumeric              *
     H*                                                               *
     H*  09/16/15 C:309 Project: 133MOV                               *
     H*                 Add processing for new field U1PV1            *
     H*                                                               *
     H*  07/28/15 C:274 Project: 133MOM                               *
     H*                 Correct DATA pararmeter and related.          *
     H*                                                               *
     H*  07/07/15 C:079 Support: 133 35708                            *
     H*                 Load billed modifier correctly                *
     H*                                                               *
     H*  03/25/15 C:280 Project: 025MUA                               *
     H*                 Add new field for JOPARI import               *
     H*                                                               *
     H*  01/19/15 C:296 Project: 129MTU                               *
     H*                 Modify to skip the edit for inactive provider.*
     H*                                                               *
     H*  12/08/14 C:296 Support: 87494                                *
     H*                 Print zip code on check and EOB.              *
     H*                                                               *
     H*  08/22/14 C:282 Project: 133MOI                               *
     H*                 Replaced the passing parameter EOBTYP by      *
     H*                 $#BYTP.                                       *
     H*                                                               *
     H*  05/22/14 C:296 Project: MC374                                *
     H*                 Load 'N' to ICD10 indiacator field,           *
     H*                 when ICD9 fields are active.                  *
     H*                                                               *
     H*  05/20/14 C:296 Project: MC378                                *
     H*                 Load NDC code an its description for          *
     H*                 LA Medical Bills.                             *
     H*                                                               *
     H*  04/15/14 C:212 Project: MC374                                *
     H*                 Load the new ICD-10 fields                    *
     H*                                                               *
     H*  01/22/14 C:282 Project: 133MOI                               *
     H*                 Calling the program WA052R1 for 133 instead   *
     H*                 of WA052R.                                    *
     H*                                                               *
     H*  12/23/13 C:296 PROJECT  129MSD                               *
     H*                 BACK TO BASE INITIATIVE-MANAGED CARE          *
     H*                 Add a parm to indicate whether                *
     H*                 diaries are created (@@CMPY).                 *
     H*                                                               *
     H*  07/02/13 C:277 SUPPORT: 82439                                *
     H*                 Check Record with key FEIN and MOD for CLT133 *
     H*                                                               *
     H*  05/23/13 C:079 Project: 46WHE                                *
     H*                 Load adjudication state correctly             *
     H*
     H*  02/08/13 C:280 Project: 114MEA                               *
     H*                 import of re-priced CAMDEN JIF BILLS          *
     H*                                                               *
     H*  01/17/13 C:079 PROJECT: 114WGD                               *
     H*                 Edit U2TAX if blank.  Move zeros.             *
     H*                                                               *
     H*  12/21/12 C:284 PROJECT: 133MMV PRISS 19                      *
     H*                 Move changes U1SZIP from zoned to alpha       *
     H*           C:277 Move changes U1SFEI from zoned to alpha       *
     H*                                                               *
     H*  08/07/12 C:280 SUPPORT 78368
     H*                 P1ST PROCESSING
     H*                                                               *
     H*  12/21/12 C:277 PROJECT: 146MFP                               *
     H*                 1. Add field EPRNLN & EPRNFN                  *
     H*                 2. Commented out to system assign correct info*
     H*                    on bills.                                  *
     H*                                                               *
     H*  11/07/12 C:277 SUPPORT: 81064                                *
     H*                 Comment out the Movement of U2TAX in DS       *
     H*                 VALUE and corresponding logic                 *
     H*                                                               *
     H*  06/24/12 C:277 PROJECT: 144MCD                               *
     H*                 Map the Rendering Prov NPI and Lincence No    *
     H*                                                               *
     H*  05/10/12 C:251 SUPPORT: 79793 MC114                          *
     H*                 Rectify update to recon line where recon and  *
     H*                 original bill is on the same file             *
     H*                                                               *
     H*  05/10/12 C:251 PROJECT: 133MMV                               *
     H*                 Modify the Bill Import Process to load AR     *
     H*                 Provider Fields                               *
     H*                                                               *
     H*  04/24/12 C:251 PROJECT: 133MMK Priss 18                      *
     H*                 Modify the Bill Import Process to load LA     *
     H*                 Pharmacy Tax - U2TAX                          *
     H*                                                               *
     H*  03/15/12 C:280 PROJECT: 133MMB                               *
     H*                 Activate duplicate payment edit for pended    *
     H*                 bills                                         *
     H*                                                               *
     H*  03/19/12 C:180 SUPPORT 79550 WC139
     H*                 OVRST & DLTOVR subroutines for client 146 only.
     H*                 Client-id 03/23/11 changes (For CLT 146 ONLY).
     F*
     H*  03/16/12 C:180 SUPPORT 79508 WC139                           *
     H*                 Prevent decimal data error on PROV MOD.
     H*
     H*  01/23/12 C:280 Project: 114MEA                               *
     H*                 Add  processing for CAMDEN JIF BILLS          *
     H*                                                               *
     H*  01/10/12 C:251 Project: 25MSO                                *
     H*                 Add  processing for fields U1AREA, U1PHN,     *
     H*                 U1RFLC, U1ATLC                                *
     H*                                                               *
     H*  11/11/11 C:280 Project: 144MCC                               *
     H*                 ADD FIELDS U1ADSC                             *
     H*                            U1DCST                            *
     H*                            U1PREA                             *
     H*                            U1IMST                             *
     H*                                                               *
     H*                            U2RNPI                             *
     H*                            U2RNLC                             *
     H*                                                               *
     H*  08/03/11 C:273 Project: 146MEK                               *
     H*                 Present SCR02 with Rendering Provider data    *
     H*                                                               *
     H*  06/24/11 C:079 Project: SY255                                *
     H*                 Load provider name/address correctly          *
     H*                                                               *
     H*  06/23/11 C:079 Project: 139MQF                               *
     H*                 Take into account EOBR codes are left justify *
     H*                                                               *
     H*  06/22/11 C:198 Project: 133MJV                               *
     H*                 Deleted changes for 09/23/10 from WRTEOP      *
     H*                                                               *
     H*  06/10/11 C:198 Project: 139MQF                               *
     H*                 Updated in order incorporate new fields into *
     H*                 processing.                                   *
     H*                                                               *
     H*  06/06/11 C:198 Project: 133MJV                               *
     H*                 Changed F8042R to incorporate new line items  *
     H*                 in case of recon.                             *
     H*                                                               *
     H*  04/15/11 C:251 PROJECT: 144MBW                               *
     H*                 Move referring provider to EURLIC as well .   *
     H*                 Move Attending provider for hospital          *
     H*                                                               *
     H*  03/23/11 C:257 SUPPORT: 75927                                *
     H*                 Add logic to set fields correctly in the      *
     H*                 detail charges file FDCAP when the bill       *
     H*                 is for a U&C state or a state that allows     *
     H*                 use of the U&C file.                          *
     H*                                                               *
     H*  02/02/11 C:079 PROJECT: 146MDQ                               *
     H*                 Activate duplicate payment edit for pended    *
     H*                 bills                                         *
     H*                                                               *
     H*  12/18/10 C:251 PROJECT: 146MCV                               *
     H*                 Set EOMBUP ='K' in Karvy Upload               *
     H*                                                               *
     H*  12/14/10 C:251 PROJECT: 146MDQ                               *
     H*                 - Patch for default adjudication code for     *
     H*                   Pharmacy bills (Karvy - 146 )               *
     H*                 - Check U1PROV for zeros and amounts = blank  *
     H*                 - If U2CDSC = blank then retrieve description *
     H*                   from the U&C File                           *
     H*                 - Blank out U1MCAR if filled with zeros       *
     H*                 - Comment changes for 146MDN -100 drg line    *
     H*           C:079  If U2CDSC = blank then retrieve description  *
     H*                  from the file                                *
     H*                                                               *
     H*  11/08/10 C:227 SUPPORT: 74789                                *
     H*                 Fixed NPS payment issue.                      *
     H*                                                               *
     H*  10/25/10 C:227 PROJECT: 133MJT                               *
     H*                 MODIFY PROCESS TO HANDLE FEIN/MOD             *
     H*                                                               *
     H*  10/01/10 C:251 PROJECT: 146MDN                               *
     H*                 MODIFY PROCESS TO HANDLE CHARGEMASTER SERVICE *
     H*                 CODE AND PROCESS DRG CODES                    *
     H*                                                               *
     H*  09/23/10 C:079 PROJECT: MC346                                *
     H*                 MODIFY PROCESS TO HANDLE FEIN/MOD             *
     H*                                                               *
     H*  09/14/10 C:189 PROJECT: 114MXA (2)                           *
     H*                 CHANGE THE MODIFIER VALUE TO LOAD 001 INSTEAD *
     H*                 OF EOMOD                                      *
     H*                                                               *
     H*  08/30/10 C:227 PROJECT: 114MXA                               *
     H*                 TRACK PHARMACY ADMIN FEES FOR PACE            *
     H*                                                               *
     H*  07/20/10 C:251 SUPPORT: 73598                                *
     H*                 ISSUE WITH MANUALLY ENTERED BILL FOR SECURITY *
     H*                 FUND                                          *
     H*                                                               *
     H*  07/19/10 C:198 Project: 144MBV                               *
     H*                 Populated 2 new fields -                      *
     H*                 Ref/Att/Admit Physician's Last Name           *
     H*                 Referring Provider NPI#                       *
     H*                                                               *
     H*  06/23/10 C:251 SUPPORT: 73303                                *
     H*                 REMOVE SIGN OF U2DFEE BEFORE WRITING TO FDDPP *
     H*                                                               *
     H*  02/24/10 C:079 PROJECT: 144MBT                               *
     H*                 Map billing type code into FAEPP              *
     H*                                                               *
     H*  02/24/10 C:227 PROJECT: 144MBT                               *
     H*                 Upadte Adjudication code.                     *
     H*                                                               *
     H*  11/02/09 C:232 PROJECT: 139WMR                               *
     H*                 OFAC Reporting-Add Pending Status Indicator.  *
     H*                                                               *
     H*  01/28/09 C:62  PROJECT 139MIV                                *
     H*                 ALLOW CHECKS TO BE CREATED ON RECONS.         *
     H*                                                               *
     H*  07/25/08 C:47  PROJECT 144MBK                                *
     H*                 REMOVE ALL CLIENT 144 CONDITIONED CODE.       *
     H*                                                               *
     H*  07/21/08 C:180 PROJECT 139MJQ                                *
     H*                 - ADD ADJUDICATION STATE TO THE MEDICAL BILL  *
     H*                   UPLOAD.                                     *
     H*                                                               *
     H*  03/26/08 C:062 PROJECT: 25MPE                                *
     H*                 -POPULATE DEA#                                *
     H*                 -CORRECT THE EDI FLAG                         *
     H*                                                               *
     H*  02/27/08 C:062 SUPPORT: 62489                                *
     H*                 POPULATE EAEDI                                *
     H*                                                               *
     H*  02/27/08 C:062 PROJECT: 114MOZ                               *
     H*                 ENABLE AUTO RELEASE IF CLIENT IS CSI          *
     H*                                                               *
     H*  11/07/07 C:062 PROJECT: 25MPE                                *
     H*                 ADD PEND LOGIC.                               *
     H*                                                               *
     H*  10/24/07 C:062 PROJECT: 139MIF                               *
     H*                 POPULATE EONDSC WITH U1NDSC.                  *
     H*                                                               *
     H*  10/16/07 C:062 PROJECT: MC307                                *
     H*                 ADD ADDITIONAL FIELD FOR REFERRING FIRST NAME *
     H*                                                               *
     H*  10/12/07 C:198 SUPPORT: 60469                                *
     H*                 INFORMATION FROM MCMC IS NOT BEING PULLED     *
     H*                 AND PRINTED ON, EOOTH2 IN FAEOP DOESNT HAVE   *
     H*                 VALUE FROM U1OTH2 IN FAU1P.                   *
     H*                                                               *
     H*  08/16/07 C:062 PROJECT: MC307                                *
     H*                 ADD NEW EDI FIELDS.                           *
     H*                                                               *
     H*  08/01/07 C:193 PROJECT: MC299                                *
     H*                 RECORD TRANSMISSION VERIFICATION FILES        *
     H*                 (IMPORT/EXPORT TO P2P)                        *
     H*                                                               *
     H*  04/23/07 C:62 PROJECT: 139MHG                                *
     H*                ADD MCMC PROCESSING.                           *
     H*                                                               *
     H*  12/12/06 C:62 PROJECT: 144MAB                                *
     H*                ADD TML PROCESSING.                            *
     H*                                                               *
     H*  07/14/06 C:79 PROJECT: MC279                                 *
     H*                USE 4 NEW EXPANDED EOB CODE/NOTE FIELDS        *
     H*                                                               *
     H*  06/30/06 C:62 PROJECT 132MFO                                 *
     H*                ADD ADDITIONAL EDIT TO CHECK DATE AGAINST      *
     H*                LINE ITEMS.                                    *
     H*                                                               *
     H*  01/24/06 C:79 PROJECT 132MDN                                 *
     H*                CHECK FOR GROUP PROVIDERS - MAKE THIS BASE     *
     H*                                                               *
     H*  08/15/05 C:62 PROJECT 132MDN                                 *
     H*                ADD CLT132 SPECIFIC CHANGES                    *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     F* Transfer files
     F*
     FFAU1P     UF   E             DISK
     FFAU1L1    UF   E           K DISK
     F                                     RENAME(PU101:PU102)
     FFAU1L2    UF   E           K DISK
     F                                     RENAME(PU101:PU103)
     FFAU2L1    UF   E           K DISK
     FFAU2L2    UF   E           K DISK
     F                                     RENAME(PU201:PU202)
     FFAU9P     UF   E             DISK
     F*
     F* Header files
     F*
     FFAEOL4    UF A E           K DISK
     F                                     INFDS(DBFDS)
     FFAEOL31   UF   E           K DISK    USROPN
     F                                     RENAME(PEO01:PEO02)
      *
     FFAEAL14   UF   E           K DISK    USROPN
     F                                     RENAME(PEA01:PEA03)
     FFAEAL1    UF A E           K DISK
     FFAEAL37   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFAEEL     IF   E           K DISK
     FFAEEL3    IF A E           K DISK
     F                                     RENAME(PEE01:PEE02)
     FNMNWL     IF   E           K DISK
     FFTBSL     IF   E           K DISK
     FFAESL2    IF A E           K DISK
     FFAEDL     IF A E           K DISK
     FFAEHL     IF A E           K DISK
     FFAEPL     UF A E           K DISK
     FFAEUL     IF A E           K DISK
     F*
     F* Detail files
     F*
     FFDCAP     O    E             DISK
     FFDDAL2    IF A E           K DISK
     FFDDDL     IF A E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDHL     IF A E           K DISK
     FFDDPL     IF A E           K DISK
     FFDDUL     IF A E           K DISK
     F*
     F* Check Request file
     F*
     FWARPLN    IF A E           K DISK    USROPN
     F*
     F* Miscellaneous files
     F*
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDCAL19   UF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFMC1L     UF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCMLB    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMF1L     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FSTSTL     IF   E           K DISK
     FFMBEL     IF   E           K DISK
     FWARPL     IF   E           K DISK    USROPN
     F                                     RENAME(PRP01:PRP02)
     FWMCML1    IF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM03)
     FWMCIL     IF   E           K DISK    USROPN
     FWTABL     IF   E           K DISK    USROPN
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWTPSL     IF   E           K DISK    USROPN
     FWMNRL     IF   E           K DISK    USROPN
     F*
     FFMPRL     IF   E           K DISK    USROPN
     FFMSVL1    IF   E           K DISK    USROPN
     FFMRCL     IF   E           K DISK
     F*  Fee Schedule Base Control File
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FSMUPL1    IF   E           K DISK
     FSMIEP     IF   E           K DISK
     f*
     ffmaml2    if   e           k Disk
     f*
     ffmdfl1    if   e           k Disk
     f*
     ffmdml     if   e           k Disk
     f*
     fftmel2tx  if   e           k Disk
     f*
     fftrvl3    if   e           k Disk
     f*
     fftlbl2    if   e           k Disk
     d pgmStr          pr
     d rtvSettings     pr                  Extpgm('SA016R')
     d  @@Cchg                        1
     d  @@Cifs                        1
     d  @@Csts                        1
     d  @@Cdny                        1
     d  @@Ccvfl                       1
     d  @@Cgrp                        1
     d  @@Cemp                        1
     d  @@Eifs                        1
     d  @@Eagt                        1
     d  @@Eemp                        1
     d  @@Fifs                        1
     d  @@Ftyp                        1
     d  @@Udup                        1
     d  @@Udrc                        1
     d  @@Urel                        1
     d  @@Upcl                        1
     d  @@Updn                        1
     d  @@Udpn                        1
     d  @@ClDays                      5  0
     d  @@IncZro                      1
     d  @@Crcd                        1
     d  @@CrDth                       1
     d  @@RtCd                        1
     d  @@Cchg         s              1
     d  @@Cifs         s              1
     d  @@Csts         s              1
     d  @@Cdny         s              1
     d  @@Ccvfl        s              1
     d  @@Cgrp         s              1
     d  @@Cemp         s              1
     d  @@Eifs         s              1
     d  @@Eagt         s              1
     d  @@Eemp         s              1
     d  @@Fifs         s              1
     d  @@Ftyp         s              1
     d  @@Udup         s              1
     d  @@Udrc         s              1
     d  @@Urel         s              1
     d  @@Upcl         s              1
     d  @@Updn         s              1
     d  @@Udpn         s              1
     d  @@ClDays       s              5  0
     d  @@IncZro       s              1
     d  @@Crcd         s              1
     d  @@CrDth        s              1
     d  @@RtCd         s              1
     d  isSettings     s              1
     d  isUseOtrId     s              1n
     d*** Beg Chg *** 01/04/24 *********************************************************
     d*** GetBankAcc      pr                  ExtPgm('WA023R')
     d GetBankAccount  pr                  ExtPgm('WA023R')
     d*** End Chg *** 01/04/24 *********************************************************
     d inCo#                          3
     d inFnd                          3
     d inAppl                         2
     d inSrDt                         8
     d outAcct                       20
     d inCall                         1
     d outHdCy                        1
     d outChkQ                       10
     d outFtyp                       10
     d outRtCd                        1
     d outEfDt                        7
     d outCpyQ                       10
     d outCkTp                       10
     d outCoid                       20
     d inEftC                         1

     d cvtDate         pr                  ExtPgm('SRCVTDAT')
     d  inDate                        8  0
     d  inFfmt                        8
     d  inTfmt                        8
     d  outDate                      10
     d  outErr                        1
     d  inDate         s              8  0
     d  inFfmt         s              8
     d  inTfmt         s              8
     d  outDate        s             10
     d  outErr         s              1
     d  xxRlin         s              1
     D CLM             S              1    DIM(20)
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)
     D* Program constants.
     D COMAPI          C                   CONST('FA007R')
     D REAS1           C                   CONST('Completed by -
     D                                     bill upload')
     D REAS2           C                   CONST('Reconsidered by -
     D                                     bill upload')
     D REAS3           C                   CONST('Pended by -
     D                                     bill upload')
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     D CODEDS          C                   CONST('MEDICINE DISPENSED -
     D                                     BY PHYSICIA')
     D TRANS           C                   CONST('Transaction ID:')
     D DIGITS          C                   CONST('0123456789')
     D USERID          C                   CONST('PDSPGMR   ')
     D XXDESC          C                   CONST('All incl room and -
     D                                     board')
     D XXPROV          C                   CONST('0000000000000')
     D XXMCAR          C                   CONST('000000000000')
     D COMM            C                   CONST('Bill Reviewer -
     D                                     pended from -
     D                                     outside vendor')
     D DI1             C                   CONST('MEDREV1')
     D DI2             C                   CONST('MEDREV2')
     D DI3             C                   CONST('MEDREV3')
     D DI4             C                   CONST('KARVY1')
     D DI5             C                   CONST('KARVY2')
     D DI6             C                   CONST('KARVY3')
     D DI7             C                   CONST('MEDREV9')
     D LO              c                   'abcdefghijklmnopqrstuvwxyz'
     D UP              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     Dxxlen            s              2  0
     D xxDFC           s              8  0
     D erDFC           s               n
     d inCo#           s              3
     d inFnd           s              3
     d inAppl          s              2
     d inSrDt          s              8
     d outAcct         s             20
     d inCall          s              1
     d outHdCy         s              1
     d outChkQ         s             10
     d outFtyp         s             10
     d outEfDt         s              7
     d outCpyQ         s             10
     d outCkTp         s             10
     d inEftC          s              1
     d outCoid         s             20
     d outRtCd         s              1
     d inEfDt          s              7  0
     d intCo#          s              3  0
     d intFnd          s              3  0
     d inAcct          s             20
     d strEPayVn       s              5
     d inCkTp          s             10
     d decProv         s             13s 0
       //*** Beg Add *** 01/04/24 *********************************************
          dcl-s inPtype Char(1);
          dcl-s inKey Char(30);
          dcl-s inChrDate Char(7);
          dcl-s inPre Char(1);
          dcl-s inEkey Char(30);
          dcl-s inEobType Char(9);
          dcl-s outRRN Char(9);
          dcl-s isEft Ind;
       //*** End Add *** 01/04/24 *********************************************
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CLAIM#                 1     20
     D  GROUP                  1      3
     D  YEAR                   4      6
     D  CASE                   7     13
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3    INZ('000')
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11    INZ('000')
     D*
     D                 DS
     D  XXDXR                  1      4
     D  DX1                    1      1
     D  DX2                    2      2
     D  DX3                    3      3
     D  DX4                    4      4
     D*
     D                 DS
     D  DATA                   1     75
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D  QXSEQ#                30     34  0
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D*
     D                 DS
     D  VALUE                  1     11
     D  SIGN                   1      1
     D  AMOUNT                 2     11  2
     D                 DS
     D  CHRG1                  1     11
     D  CHRG2                  2     11  2
     D                 DS
     D  MRA1                   1     11
     D  MRA2                   2     11  2
     D                 DS
     D  NDIS1                  1     11
     D  NDIS2                  2     11  2
     D                 DS
     D  PAID1                  1     11
     D  PAID2                  2     11  2
     D                 DS
     D  U1PROV                 1     13
     D  U1FEI1                 1      9  0
     D  U1MOD1                10     12
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  40     41
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3    INZ('000')
     D  K1RATE                 4      8
     D  K1UNIT                 9     11    INZ('000')
     D XXEDS           DS
     D  @@EDS1                 1    256
     D  @@EDS2               257    280
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D Key2            DS
     D  X2CO#                  1      3  0
     D  X2SUB#                 4      8  0
     D  X2FND                  9     11  0
     D  X2FYR                 12     14  0
     D  X2CASE                15     21  0
     D  X2EOB#                22     30  0
      /Copy sPrModule
     d/copy fprEpay
     D*** Beg Add ****** 01/04/24 **************************
     D/copy wPrBank
     D*** End Add ****** 01/04/24 **************************
     DLineValidation   pr                  extpgm('FA786R')
     D ##Mode                         1    Const
     D ##KL                           1    Const
     D ##EopRrn                       9  0
      *

     D ##Mode          s              1    inz
     D ##KL            s              1    inz
     D ##EopRrn        s              9  0 inz
     d xxFound         s              1
     d user            s             10A

     IPCM03
     I              CMOSCL                      CWOSCL
     I*
     I*================================================================
     I*  /COPY required constants
     I*================================================================
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@NAME           10
     C                   PARM                    @@RRN             9 0          FAU9P RRN
     C                   PARM                    @@RRN2            9 0          FAU1P RRN
     C                   PARM                    @@PRVC            1            PROV CHANGE
     C                   PARM                    @@CMPY            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C*
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
     C*
     C     CAKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKDSEQ
     C*
     C     CIKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKPTYP
     C                   KFLD                    KKSYS
     C*
     C     CAKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C*
     C     CAKY3         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C                   KFLD                    KKLIN#
     C*
     C     CAPKY1        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     CAPKY2        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKRCN
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*
     C     CMKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C*
     C     CMKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CMKY4         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C*
     C     CMKY5         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSRC
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSINV
     C*
     C     EEKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     EEKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EEKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    KKSEQ#
     C*
     C     NWKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKNWCD
     C*
     C     BSKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     EOBKY1        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C*
     C     EOBKY2        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C                   KFLD                    U1RCN
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     F1KY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C*
     C     GZKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKZIPA
     C*
     C     PDKY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     PDKY2         KLIST
     C                   KFLD                    U1PFEI
     C                   KFLD                    U1MOD
     C*
     C     P1KY          KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     C*
     C     RPKY1         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     C*
     C     RPKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     U1KY1         KLIST
     C                   KFLD                    @@NAME
     C*
     C     U1KY2         KLIST
     C                   KFLD                    U1EOB#
     C                   KFLD                    U1RCN
     C*
     C     U2KY1         KLIST
     C                   KFLD                    U1TRID
     C*
     C     U2KY2         KLIST
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     NRKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C*
     C     SVKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1CMSV
     C                   KFLD                    KKFDAT
     C*
     C     SVKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1CMSV
     C*
     C*
     C     RCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKPRCD
     C     SBKY          KLIST
     C                   KFLD                    KKEY              2
     C     UPKY          Klist
     C                   Kfld                    #2CLAD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    RPSEQ#        XXSEQ#
     C     *LIKE         DEFINE    PCKEY         KKKEY
     C     *LIKE         DEFINE    EOAST         XXAST
     C     *LIKE         DEFINE    GZZIPA        KKZIPA
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    RPEOBS        KKEOBS
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    EASINV        KKSINV
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        HDSEQ#
     C     *LIKE         DEFINE    CIPTYP        KKPTYP
     C     *LIKE         DEFINE    CISYS         KKSYS
     C     *LIKE         DEFINE    CMSRC         KKSRC
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C     *LIKE         DEFINE    CALIN#        XXLIN#
     C     *LIKE         DEFINE    DHPRCD        KKPRCD
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    DENOT1        XXEDS1
     C     *LIKE         DEFINE    DENOT1        XXEDS2
     C     *LIKE         DEFINE    DENOT1        XXEDS3
     C     *LIKE         DEFINE    DENOT1        XXEDS4
     C     *LIKE         DEFINE    CATAX         XXTAX
     C     *like         Define    STmfs         xxMfs
     C* Call program start procedure
      /free
                 callp pgmStr();
      /end-free
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* ERRCK - ERROR CHECK SUBROUTINE
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C                   SETOFF                                       90
     C                   SETOFF                                       91
     C                   SETOFF                                       92
     C                   SETOFF                                       93
     C**                   SETOF                     94
     C                   SETOFF                                       95
     C                   Z-ADD     *ZEROS        KKRCN
     C*
     C* LETS SEE IF THIS IS A RECONSIDERATION
     C*
     C*
     C     u1rcnf        IFNE      'Y'
     C*
     C* Check to see if recon flag was set correctly
     C*
     C                   MOVEL(P)  U1TRID        KKSINV
     C     EAKY          CHAIN     PEA02                              99
     C     *IN99         IFEQ      *ON
     C*
     C* Check WCMC Claim Master to see if SS#/ACDT exists.
     C*
     C                   EXSR      GETCLM
     C     CMKY3         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        COUNT             3 0
     C                   MOVEL     U1SS#         KKSS#
     C                   Z-ADD     U1DOI         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKACDT
     C*
     C     CMKY1         CHAIN     PCM02                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             COUNT
     C     CMKY1         READE     PCM02                                  99
     C                   ENDDO
     C                   ELSE
     C                   Z-ADD     1             COUNT
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C*
     C* CHECK TO SEE IF THIS AN OUTSIDE ACCOUNT
     C*
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C                   MOVE      *OFF          OUTS              1
     C     ##OUTS        IFEQ      'Y'
     C                   MOVE      *ON           OUTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* SS#/ACDT doesn't exist.
     C*
     C     COUNT         WHENEQ    0
     C                   SETON                                        90
     C*
     C* Let's get the WCMS group number
     C*
     C     COUNT         WHENEQ    1
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C*
     C* CHECK TO SEE IF THIS AN OUTSIDE ACCOUNT
     C*
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C                   MOVE      *OFF          OUTS              1
     C     ##OUTS        IFEQ      'Y'
     C                   MOVE      *ON           OUTS
     C                   ENDIF
     C*
     C* SS#/ACDT occurs more than once.
     C*
     C     COUNT         WHENGT    1
     C                   SETON                                        90
     C*
     C                   ENDSL
       // Edit the provider ID and give an error if its not numeric
           //*** Beg Add *** 01/31/25 **************************************
           If U1prov <> *blanks;
           //*** End Add *** 01/31/25 **************************************
              Monitor;
                 decProv = %Dec(U1prov:13:0);
              On-Error;
                 *in90 = *on;
              EndMon;
           //*** Beg Add *** 01/31/25 **************************************
           Endif;
           //*** End Add *** 01/31/25 **************************************
     C*
     C* FEIN/MOD does not exist. If the full FEIN/Mod was passed in
     C* then use that, otherwise use the FEIN only and set mod = 000
     C*
     C     *IN90         IFEQ      *OFF
     C     U1PROV        IFNE      *BLANKS
     C     U1PROV        ANDNE     XXPROV
     C                   MOVE      U1FEI1        KKFEIN
     C                   MOVE      U1MOD1        KKMOD
     C                   ELSE
     C                   MOVE      U1FEIN        KKFEIN
     C                   Z-ADD     *ZEROS        KKMOD
     C                   ENDIF
     C*
     C     PDKY1         CHAIN     PPD01                              99
      /Free
       If *in99 = *on;                                           //Provider not found.
          *in90 = *on;
       Else;                                                     //Provider found.
       Chain (##CO#:U1NtWk) PNW01;
          If %found(NMNWL) and NWFL03 = 'Y';                     //Check if ina. prov. flg. is on.
             // Skip the check for inactive provider.
          Elseif PDstat <> 'A' and PDstat <> *blanks;             //Check if prov. is active.
             *in90 = *on;
          Endif;
       Endif;
      /End-Free
     C                   ENDIF
     C*
     C* IF NOT PROVIMA CHECK THEN CHECK TO SEE IF DUPLICATE
     C*
     C     C132          IFEQ      *ON
     C     C135          OREQ      *ON
     C     C114          OREQ      *ON
     C     ##CLT#        OREQ      '146'
     C     ##CLT#        OREQ      '133'
     C     C090          OREQ      *ON
     C     isSettings    oreq      '1'
     C     @@Udup        andne     ' '
     C     U1RCNF        IFEQ      ' '
     C     *IN90         ANDEQ     *OFF
     C     U1RCNF        OREQ      'N'
     C     *IN90         ANDEQ     *OFF
     C*
     C                   MOVE      U1PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        $#AMT
     C                   ELSE
     C                   Z-ADD     AMOUNT        $#AMT
     C                   ENDIF
     C*
     C     $#AMT         IFNE      *ZEROS
     C     ##CLT#        OREQ      '146'
     C*
     C*
     C                   Z-ADD     U1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C*
     C                   Z-ADD     U1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*
     C     C139          IFEQ      *ON
     C                   MOVEL     '11'          $#RECD
     C                   ELSE
     C                   MOVEL     'ME'          $#RECD
     C                   ENDIF
     C*
     C* Check for duplicate request/payment
     C*

     C     ##CLT#        IFEQ      '133'
     C                   EVAL      $#BYTP = *BLANK
     C                   Select
     C     U1BTYP        WHENEQ    'M'
     C                   EVAL      $#BYTP = 'N'
     C     U1BTYP        WHENEQ    'P'
     C                   EVAL      $#BYTP = 'X'
     C                   OTHER
     C                   EVAL      $#BYTP = U1BTYP
     C                   ENDSL
     C*
     C                   CALL      'WA052R1'
     C                   PARM      CMCO#         $#CO#             3 0          Company #
     C                   PARM      KKWCF#        $#FND             3 0          Fund
     C                   PARM      CMFYR         $#FYR             3 0          Fund Year
     C                   PARM      CMCASE        $#CASE            7 0          Case
     C                   PARM      XXFRDT        $#FROM            7 0          From Date
     C                   PARM      XXTODT        $#TO              7 0          To Date
     C                   PARM                    $#AMT             9 2          Amount
     C                   PARM                    $#RECD            2            Reserve Code
     C                   PARM      PDFEIN        $#FEIN            9 0          FEIN #
     C                   PARM      'M'           $#RETP            1            Reserve Type
     C                   PARM      *BLANKS       $#RTCD            2
     C                   PARM                    $#BYTP            1            Bill Type
     C                   ELSE
     C                   CALL      'WA052R'
     C                   PARM      CMCO#         $#CO#             3 0          Company #
     C                   PARM      KKWCF#        $#FND             3 0          Fund
     C                   PARM      CMFYR         $#FYR             3 0          Fund Year
     C                   PARM      CMCASE        $#CASE            7 0          Case
     C                   PARM      XXFRDT        $#FROM            7 0          From Date
     C                   PARM      XXTODT        $#TO              7 0          To Date
     C                   PARM                    $#AMT             9 2          Amount
     C                   PARM                    $#RECD            2            Reserve Code
     C                   PARM      PDFEIN        $#FEIN            9 0          FEIN #
     C                   PARM      'M'           $#RETP            1            Reserve Type
     C                   PARM      *BLANKS       $#RTCD            2
     C                   ENDIF
     C*
     C* Check for duplicates hard coded not using the setting file
     C*
     C     $#RTCD        IFNE      *BLANKS
     C     isSettings    ANDNE     '1'
     C     $#RTCD        IFEQ      'DR'
     C     $#RTCD        OREQ      'DP'
     C                   SETON                                        9390
     C                   ENDIF
     C     ##CLT#        IFEQ      '146'
     C     $#RTCD        ANDEQ     'PR'
     C     ##CLT#        OREQ      '146'
     C     $#RTCD        ANDEQ     'PP'
     C                   SETON                                        9590
     C                   ENDIF
     C     ##CLT#        IFEQ      '133'
     C     $#RTCD        ANDEQ     'PR'
     C     ##CLT#        OREQ      '133'
     C     $#RTCD        ANDEQ     'PP'
     C                   SETON                                        9590
     C                   ENDIF
     C                   ENDIF
       //* Check for duplicates using the new settings file
       //* If @@Udup = blank then dont check for duplicates at all
      /free
           if $#rtCd <> ' ' and isSettings = '1';
           //* Complete duplicates
              if @@Udup = 'C' or @@Udup = 'A';
                 if $#rtCd = 'DR' or $#rtCd = 'DP';
                    *in93 = *on;
                    *in90 = *on;
                 endif;
              endif;
           //* Partial duplicates
              if @@Udup = 'P' or @@Udup = 'A';
                 if $#rtCd = 'PR' or $#rtCd = 'PP';
                    *in95 = *on;
                    *in90 = *on;
                 endif;
              endif;
           endif;
      /end-free
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        9091
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* This is a reconsideration.  Lets find the original bill
     C*
       Chain (##CO#:U1NtWk) PNW01;
          If %Found(NMNWL) and NWfl04 = 'Y';
             isUseOtrid = *on;
          Else;
             isUseOtrid = *off;
          Endif;
     C                   IF        ##CLT# = '133'
     C                             and U1NTWK = 'MITCH'
     C                             or (##clt# = '090' and U1ntwk = 'NOVAR')
     C                             or (##clt# = '146' and U1ntwk = 'MITCH')
     C                             or (##clt# = '133' and U1ntwk = 'RMS')
     C                             or (##clt# = '152')
     C                             or isUseOtrId = *on
     C                   IF        U1OTRID = *BLANK
     C                   EVAL      KKSINV = 'Original Transaction ID not sent'
     C                   ELSE
     C                   MOVEL(P)  U1OTRID       KKSINV
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  U1TRID        KKSINV
     C                   ENDIF
     C     EAKY          CHAIN     PEA02                              99
     C     *IN99         IFEQ      *ON
      /Free
        if ##CLT# ='045';
          xxlen = %len(%Trim(U1oTrid));
          If xxLen >= 1 and xxLen <= 9;
          open Faeol31;
          chain (##Co#:%Dec(%Subst(%Trim(U1oTrid):1:xxlen):9:0)) Faeol31;
          if %Found(Faeol31);
            EOsinv = U1Trid;
            update peo02;

            open Faeal14;
            chain (##co#:EOeob#:EOrcn:EOsub#:EOfnd:EOfyr:EOcase) Faeal14;
            if %Found(Faeal14);
              EAsinv = U1Trid;
              update pea03;
            endif;
          endif;

          if %open(Faeol31);
            Close Faeol31;
          endif;
          if %open(Faeal14);
            Close Faeal14;
          endif;
          Endif;
        endif;
      /End-Free
     C                   SETON                                        9091
     C                   ELSE
      /Free
       //
       // Check to see if the Detail line items were sent correctly
       //
                         Chain(n) U1trId pu201;
                         if %Found(fau2l1);
                            xxRlin = 'N';
                            dow not %Eof(fau2l1);
                               If u2rLin > 0;
                                  xxRlin = 'Y';
                               Endif;
                               Reade(n) U1trId pu201;
                            Enddo;
                            If xxRlin = 'N';
                               *in90 = *on;
                               *in91 = *on;
                            Endif;
                         Else;
                            *in90 = *on;
                               *in91 = *on;
                         Endif;
      /End-free
     C*
     C     U1EOB#        IFEQ      *ZEROS
     C     EOKY          CHAIN(N)  PEO01                              99
     C     EORCN         ADD       1             KKRCN
     C                   ELSE
     C                   Z-ADD     U1RCN         KKRCN
     C                   ENDIF
     C*
     C                   SETON                                        92
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DLTOVR - DELETE OVERRIDES
     C*================================================================
     C     DLTOVR        BEGSR
     C*
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FMPRL                                99
     C                   CLOSE     FMSVL1                               99
     C*
     C                   ENDSR
     C*================================================================
     C* GETAST - GET THE ADJUDICATION STATE
     C*================================================================
     C     GETAST        BEGSR
     C*
     C                   MOVEL(P)  'UC'          XXAST
     C                   MOVEL(P)  CMAST         KKST
     C*
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STINST        ANDEQ     'Y'
     C                   MOVE(P)   CMAST         XXAST
     C                   ELSE
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C     P1ST          IFNE      *BLANKS
     C                   MOVEL     P1ST          KEY              10
     C     KEY           CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1ST          XXAST
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     U1ST          KEY
     C     KEY           CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  U1ST          XXAST
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Get flag to determine if we use FMRCP file for Adj State
     C*  when the HCPCS code is not found in FMPRP,FMSVP files
     C*
     C                   MOVEL     'N'           ##RC              1
     C                   MOVEL(P)  XXAST         KKST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STBS02        ##RC
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR
     C*================================================================
     C* GETCLM - GET THE CLAIM NUMBER
     C*================================================================
     C     GETCLM        BEGSR
     C*
     C                   MOVEL     *BLANKS       XXCLM#           20
     C                   Z-ADD     *ZEROS        XXONE             1 0
     C                   Z-ADD     *ZEROS        XXTWO             2 0
     C                   Z-ADD     *ZEROS        XXTHR             3 0
     C                   Z-ADD     *ZEROS        XXFOU             4 0
     C                   Z-ADD     *ZEROS        XXFIV             5 0
     C                   Z-ADD     *ZEROS        XXSIX             6 0
     C                   Z-ADD     *ZEROS        XXSEV             7 0
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   Z-ADD     *ZEROS        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKFYR
     C                   Z-ADD     *ZEROS        KKCASE
     C*
     C* LET'S EXTRACT THE GROUP
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     U1CLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     '-'           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     U1CLM#        XXONE
     C                   Z-ADD     XXONE         KKWCF#
     C     X             WHENEQ    3
     C                   MOVEL     U1CLM#        XXTWO
     C                   Z-ADD     XXTWO         KKWCF#
     C     X             WHENEQ    4
     C                   MOVEL     U1CLM#        XXTHR
     C                   Z-ADD     XXTHR         KKWCF#
     C                   ENDSL
     C*
     C                   ADD       1             X
     C                   MOVEA(P)  CLM(X)        XXCLM#
     C*
     C* LET'S GET THE YEAR
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     XXCLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     '-'           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     XXCLM#        XXONE
     C                   Z-ADD     XXONE         KKFYR
     C     X             WHENEQ    3
     C                   MOVEL     XXCLM#        XXTWO
     C                   Z-ADD     XXTWO         KKFYR
     C     X             WHENEQ    4
     C                   MOVEL     XXCLM#        XXTHR
     C                   Z-ADD     XXTHR         KKFYR
     C                   ENDSL
     C                   ADD       1             X
     C                   MOVEA(P)  CLM(X)        XXCLM#
     C                   ENDIF
     C*
     C* LET'S GET THE YEAR
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     XXCLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     ' '           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     XXCLM#        XXONE
     C                   Z-ADD     XXONE         KKCASE
     C     X             WHENEQ    3
     C                   MOVEL     XXCLM#        XXTWO
     C                   Z-ADD     XXTWO         KKCASE
     C     X             WHENEQ    4
     C                   MOVEL     XXCLM#        XXTHR
     C                   Z-ADD     XXTHR         KKCASE
     C     X             WHENEQ    5
     C                   MOVEL     XXCLM#        XXFOU
     C                   Z-ADD     XXFOU         KKCASE
     C     X             WHENEQ    6
     C                   MOVEL     XXCLM#        XXFIV
     C                   Z-ADD     XXFIV         KKCASE
     C     X             WHENEQ    7
     C                   MOVEL     XXCLM#        XXSIX
     C                   Z-ADD     XXSIX         KKCASE
     C     X             WHENEQ    8
     C                   MOVEL     XXCLM#        XXSEV
     C                   Z-ADD     XXSEV         KKCASE
     C                   ENDSL
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     U1CLM#        CLAIM#
     C                   MOVE      GROUP         KKWCF#
     C                   MOVE      YEAR          KKFYR
     C                   MOVE      CASE          KKCASE
     C                   ENDIF
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      KKWCF#        #1WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1SUB#        KKSUB#
     C                   MOVE      #1FND         KKFND
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   ENDIF
     C*
     C                   ENDSR
       //==============================================================
       // GetEobEPayMC - Set EOB EFTC flag to V if E-Pay
       //==============================================================
      /Free
       Begsr GetEpayStat;
        strEPayVn =  *Blanks;
        If isModEnabled('WorkComp') and isModEnabled('E-Pay');
           // Get the bank account
           inCo#  = %EditC(EOco#:'X');
           inFnd  = %EditC(EOfnd:'X');
           inAppl = 'WC';
           inDate = DSdate;
           inFfmt = *blanks;
           inTfmt = '*MDYY';
           Callp cvtDate(Indate:inFfmt:inTfmt:outDate:outErr);
           inSrDt = outDate;
           inCall = ' ';
           inEfTc = 'V';

           // Get the Bank Account Number
           //*** Beg Chg *** 01/04/24 *********************************************************
           //***getBankAcc(inCo#:inFnd:inAppl:inSrDt:outAcct:inCall:outHdCy:
           //*** outChkQ:outFtyp:outRtCd:outEfDt:outCpyQ:outCkTp:
           //*** outCoid:inEftC);
           getBankAccount(inCo#:inFnd:inAppl:inSrDt:outAcct:inCall:outHdCy:
            outChkQ:outFtyp:outRtCd:outEfDt:outCpyQ:outCkTp:
            outCoid:inEftC);
           //*** End Chg *** 01/04/24 *********************************************************
           If outAcct <> *blanks;
              Monitor;
                 inEfDt = %Dec(outEfDt:7:0);
              On-error;
              Endmon;
              intCo# = EOco#;
              intFnd = EOfnd;
              inAcct = outAcct;
              inCkTp = outCkTp;
              strEPayVn =  GetEobEPayMC(intCo#:inAppl:intFnd:inAcct:inEfDt:
               inCkTp);
           Endif;
        Endif;
       EndSr;
      /End-Free
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   EXSR      OPNFIL
     C                   MOVE      *OFF          P2P               1
     C                   MOVE      *OFF          RELEAS            1
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C                   MOVE      *OFF          C132              1
     C     ##CLT#        IFEQ      '132'
     C                   MOVE      *ON           C132
     C                   ENDIF
     C                   MOVE      *OFF          C139              1
     C     ##CLT#        IFEQ      '139'
     C                   MOVE      *ON           C139
     C                   ENDIF
     C                   MOVE      *OFF          C133              1
     C     ##CLT#        IFEQ      '133'
     C                   MOVE      *ON           C133
     C                   ENDIF
     C                   MOVE      *OFF          C114              1
     C     ##CLT#        IFEQ      '114'
     C                   MOVE      *ON           C114
     C                   ENDIF
     C                   MOVE      *OFF          C135              1
     C     ##CLT#        IFEQ      '135'
     C                   MOVE      *ON           C135
     C                   ENDIF
     C*
     C* IS THIS TML?
     C*
     C                   MOVE      *OFF          C144              1
     C     ##CLT#        IFEQ      '144'
     C                   MOVE      *ON           C144
     C                   ENDIF
     C                   MOVE      *OFF          C146              1
     C     ##CLT#        IFEQ      '146'
     C                   MOVE      *ON           C146
     C                   ENDIF
     C                   MOVE      *OFF          C090              1
     C     ##CLT#        IFEQ      '090'
     C                   MOVE      *ON           C090
     C                   ENDIF
     C                   CALL      COMAPI        COMPL                  99
     C                   SETOFF                                       16
     C                   Z-ADD     *ZEROS        XXCCNT            9 0
     C                   Z-ADD     *ZEROS        XXRCNT            9 0
     C                   Z-ADD     *ZEROS        XXUCNT            9 0
     C*
     C     Q1PRMS        IFGT      2
     C     @@RRN2        ANDGT     0
     C*
     C     @@RRN2        CHAIN(N)  PU101                              98
     C  N98CMKY2         CHAIN     PCM01                              98
     C  N98PDKY2         CHAIN     PPD01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C*
     C* CHECK TO SEE IF THIS AN OUTSIDE ACCOUNT
     C*
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C                   MOVE      *OFF          OUTS              1
     C     ##OUTS        IFEQ      'Y'
     C                   MOVE      *ON           OUTS
     C                   ENDIF
     C*
     C                   MOVE      'FB'          KKEY
     C     SBKY          CHAIN     SFB01                              99
     C     @@PRVC        IFEQ      'Y'
     C                   SETON                                        16
     C                   ENDIF
     C                   EXSR      WRITER
      /Free
           If IsModEnabled('Line Validation');
             LineValidation(*Blanks:*Blanks:##EopRrn);
           EndIf;
      /End-Free
     C                   EXSR      UPDAT1
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     U1KY1         CHAIN     PU102                              98
     C     *IN98         IFEQ      *OFF
     C     *IN98         DOWEQ     *OFF
     C*
     C                   EXSR      CHKAM1
     C                   EXSR      ERRCK
     C                   EXSR      UPDT1A
     C     *IN90         IFEQ      *OFF
     C                   EXSR      WRITER
      /Free
           If IsModEnabled('Line Validation');
             LineValidation(*Blanks:*Blanks:##EopRrn);
           EndIf;
      /End-Free
     C                   EXSR      UPDT1B
     C                   ENDIF
     C*
     C     U1KY1         READE     PU102                                  98
     C   98              EXSR      UPDAT9
     C                   ENDDO
     C                   ELSE
     C                   EXSR      UPDAT9
     C                   ENDIF
     C                   ENDIF
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* OPNFIL - OPEN FILES IF WCMS
     C*================================================================
     C     OPNFIL        BEGSR
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      ' '           @@OS              1
16   C                   PARM      ' '           @@ARLS            1
17   C                   PARM      ' '           @@CCOD            1
18   C                   PARM      ' '           @@CRVW            1
19   C                   PARM      ' '           @@PROM            1
20   C                   PARM      ' '           @@MCRV            1
21   C                   PARM      ' '           @@FREQ            1
22   C                   PARM      ' '           @@CRAC            1
23   C                   PARM      ' '           @@PREO            1
24   C                   PARM      ' '           @@PARN            1
25   C                   PARM      ' '           @@MGRI            1
26   C                   PARM      ' '           @@KCIF            1
27   C                   PARM      ' '           @@CURS            1
28   C                   PARM      ' '           @@PCIB            1
     C*
     C     ##WCIF        IFEQ      'Y'
     C                   OPEN      WARPL
     C                   OPEN      WMCML1
     C                   OPEN      WMCIL
     C                   OPEN      WTABL
     C                   OPEN      WTPSL
     C                   OPEN      WMNRL
     C                   OPEN      WARPLN
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OVRST - OVERRIDE TO THE ADJUDICATION STATE
     C*================================================================
     C     OVRST         BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL     XXAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(2)        QQCMD
     C                   MOVEL     XXAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      FMPRL                                51
     C                   OPEN      FMSVL1                               51
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     C132          IFEQ      *ON
     C     RELEAS        ANDEQ     *ON
     C*
     C                   MOVE      ##CO#         $$CO#
     C*
     C                   CALL      'W7009C'
     C                   PARM                    $$CO#             3
     C                   PARM      '0000000'     $$DATE            7
     C                   PARM      'PD'          $$PCDE            2
     C                   PARM      'PY'          $$RCDE            2
     C                   PARM      '000'         $$GRP             3
     C*
     C                   ENDIF
     C     Q1PRMS        IFEQ      2
     C     P2P           ANDEQ     *ON
     C                   CALL      'FA750C'
     C                   PARM      @@NAME        NAME             10
     C                   PARM      '1'           CALL              1
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT1A - Update Provima Bill Header Record (FAU1P) - 1ST UPDATE
     C*================================================================
     C     UPDT1A        BEGSR
     C*
     C     *IN90         IFEQ      *ON
     C     *IN91         IFEQ      *OFF
     C                   MOVEL(P)  'E'           U1PRFL
     C                   ADD       1             XXCCNT
     C                   ELSE
     C                   MOVEL(P)  'R'           U1PRFL
     C                   ADD       1             XXRCNT
     C                   ENDIF
     C     *IN93         IFEQ      *ON
     C                   MOVE      'D'           U1DLRE
     C                   ENDIF
     C     *IN95         IFEQ      *ON
     C                   MOVE      'P'           U1DLRE
     C                   ENDIF
     C                   ELSE
     C                   ADD       1             XXUCNT
     C*
     C     *IN92         IFEQ      *OFF
     C     U1EOB#        IFEQ      *ZEROS
     C*
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1EOB#
     C                   UPDATE    PC101
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     C1EOB#        U1EOB#
     C                   ENDIF
     C*
     C                   Z-ADD     CMCO#         U1CO#
     C                   Z-ADD     CMSUB#        U1SUB#
     C                   Z-ADD     CMFND         U1FND
     C                   Z-ADD     CMFYR         U1FYR
     C                   Z-ADD     CMCASE        U1CASE
     C                   Z-ADD     PDFEIN        U1PFEI
     C                   Z-ADD     PDMOD         U1MOD
     C                   MOVEL(P)  CMSS#         U1PSS#
     C                   Z-ADD     CMACDT        U1ACDT
     C                   Z-ADD     *ZEROS        U1RCN
     C                   ELSE
     C     U1EOB#        IFEQ      *ZEROS
     C                   Z-ADD     EOCO#         U1CO#
     C                   Z-ADD     EOSUB#        U1SUB#
     C                   Z-ADD     EOFND         U1FND
     C                   Z-ADD     EOFYR         U1FYR
     C                   Z-ADD     EOCASE        U1CASE
     C                   Z-ADD     EOEOB#        U1EOB#
     C                   Z-ADD     KKRCN         U1RCN
     C                   Z-ADD     EOFEIN        U1PFEI
     C                   Z-ADD     EOMOD         U1MOD
     C                   MOVEL(P)  EOSS#         U1PSS#
     C                   Z-ADD     EOACDT        U1ACDT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PU102
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT1B - Update Provima Bill Header Record (FAU1P) - 2ND UPDATE
     C*================================================================
     C     UPDT1B        BEGSR
     C*
     C     U1KY2         CHAIN     PU103                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U1PRDT
     C*
     C                   MOVEL     *BLANKS       U1DLUS
     C                   MOVEL     *BLANKS       U1DLPG
     C                   Z-ADD     *ZEROS        U1DLTM
     C                   Z-ADD     *ZEROS        U1DLDT
     C                   MOVE      *BLANKS       XXDLRE           69
     C                   MOVEL     XXDLRE        U1DLRE
     C                   MOVEL(P)  'Y'           U1PRFL
     C*
     C                   UPDATE    PU103
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT1 - Update Provima Bill Header Record (FAU1P) - SINGLE MODE
     C*================================================================
     C     UPDAT1        BEGSR
     C*
     C     @@RRN2        CHAIN     PU101                              98
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'Y'           U1PRFL
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U1PRDT
     C*
     C                   UPDATE    PU101
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT2A - Update Provima Bill Detail Record (FAU2P) - 1ST UPDATE
     C*================================================================
     C     UPDT2A        BEGSR
     C*
     C                   Z-ADD     CAEOB#        U2EOB#
     C                   Z-ADD     CADSEQ        U2DSEQ
     C*
     C                   UPDATE    PU201
     C*
     C                   ENDSR
     C*================================================================
     C* UPDT2B - Update Provima Bill Detail Record (FAU2P) - 2ND UPDATE
     C*================================================================
     C     UPDT2B        BEGSR
     C*
      /Free

       // For recons read/update the last record for the EOB/Dseq number

                         Select;

                         When U1rcnF = 'Y';
                            Setgt U2ky2 pu202;
                            Readpe u2ky2 pu202;
                            If not %Eof(fau2l2);
                               U2prFl = 'Y';
                               Update pu202;
                            Endif;

                         When U1rcnF <> 'Y';
                            Chain U2ky2 pu202;
                            If not %Eof(fau2l2);
                               U2prFl = 'Y';
                               Update pu202;
                            Endif;

                         Endsl;
      /End-free
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT9 - Update Provima Bill Transmission File (FAU9P).
     C*================================================================
     C     UPDAT9        BEGSR
     C*
     C     U1KY1         CHAIN(N)  PU102                              99
     C     *IN99         IFEQ      *ON
     C     @@RRN         CHAIN     PU901                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     U9CCNT        IFEQ      *ZEROS
     C     U9RCNT        ANDEQ     *ZEROS
     C     U9UCNT        ANDEQ     *ZEROS
     C                   Z-ADD     XXCCNT        U9CCNT
     C                   Z-ADD     XXRCNT        U9RCNT
     C                   Z-ADD     XXUCNT        U9UCNT
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1USER        U9USER
     C                   MOVEL(P)  Q1PGM         U9PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U9PRDT
     C                   MOVEL     SRDTO         U9DT
     C                   Z-ADD     DSTIME        U9PRTM
     C*
     C                   UPDATE    PU901
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDCAP - Update FDCAP record
     C*================================================================
     C     UPDCAP        BEGSR
     C*
     C                   Z-ADD     U2LIN#        KKLIN#
     C*
     C     CAKY3         CHAIN     PCA03                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  EOUSER        CAUSER
     C                   MOVEL(P)  EOPGM         CAPGM
     C                   Z-ADD     EODT          CADT
     C                   Z-ADD     EOTIME        CATIME
     C*
     C                   MOVE      U2CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        CACHRG
     C                   ELSE
     C                   ADD       AMOUNT        CACHRG
     C                   ENDIF
     C*
     C                   MOVE      U2MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        CAMRA
     C                   ELSE
     C                   ADD       AMOUNT        CAMRA
     C                   ENDIF
     C*
     C     U2OTH2        IFNE      *BLANKS
     C                   MOVE      U2OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        CAOTH2
     C                   ELSE
     C                   ADD       AMOUNT        CAOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U2NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        CANDIS
     C                   ELSE
     C                   ADD       AMOUNT        CANDIS
     C                   ENDIF
     C*
     C                   MOVE      U2PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        CAPAID
     C                   ELSE
     C                   ADD       AMOUNT        CAPAID
     C                   ENDIF
      /Free
          If u2tax = *blanks;
             u2tax = *zeros;
          Endif;
          If U2tax <> *blanks;
          Endif;
      /End-Free
     C*
     C                   UPDATE    PCA03
     C                   ELSE
     C     U2RLIN        IFNE      *ZEROS
     C                   EXSR      WRTCAP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* Get the adjudication state
     C*
     C     u1btyp        IfEQ      'H'
     C     ##CLT#        AndEQ     '129'
     C                   Eval      eospcd = *Blanks
     C                   Eval      eocsdt = *Zeros
     C                   Eval      euicn  = 'HO'
     C                   Eval      eumcar = *Blanks
     C                   EndIf
     C*
     C*
     C     ##CLT#        IfEQ      '129'
     C                   ExSr      GetAst129
     C                   Else
     C                   EXSR      GETAST
     C                   EndIf
     C     C146          IFEQ      *ON
     C                   EXSR      OVRST
     C                   ENDIF
     C*
     C* Write the header records
     C*
     C                   EXSR      WRTEOP                                       FAEOP
     C                   EXSR      WRTEAP                                       FAEAP
     C                   EXSR      WRTEEP                                       FAEEP
     C                   EXSR      WRTESP                                       FAESP
     C*
     C* Write the DARTS diary item for load balancing
     C     q1prms        IfGT      4
     C     ##CLT#        AndEQ     '129'
     C     @@cmpy        AndNE     'K'
     C                   Exsr      SRDART
     C                   Endif
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      WRTEDP                                       FAEDP
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      WRTEUP                                       FAEUP
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      WRTEHP                                       FAEHP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      WRTEPP                                       FAEPP
     C                   ENDSL
     C*
     C* Write the detail records
     C*
     C                   EXSR      WRTDTL
     C     EOBTYP        IFEQ      'X'
     C     C133          ANDEQ     *ON
     C     FBST09        ANDEQ     'Y'
     C                   EXSR      UPDTAX
     C                   ENDIF
     C*
     C*
     C* Write the check request
     C*
     C     EAPLNK        IFEQ      '3'
     C     @@PCIB        ANDNE     'Y'
     C                   EXSR      WRTRPP                                       WARPP
     C     ##CLT#        IFEQ      '114'
     C     EONTWK        ANDEQ     'PACE'
     C                   EXSR      WRTRPX                                       WARPP
     C                   ENDIF
     C                   ENDIF
     C     C146          IFEQ      *ON
     C                   EXSR      DLTOVR
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDE1 - Write FDDEP record
     C*================================================================
     C     WRTDE1        BEGSR
     C*
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C                   MOVEL(P)  XXEOB         DEEOBCD
     C     *IN72         IFEQ      *ON
     C                   MOVEL(P)  XXEDS1        DENOT1
     C                   MOVEL(P)  XXEDS2        DENOT2
     C                   MOVEL(P)  XXEDS3        DENOT3
     C                   MOVEL(P)  XXEDS4        DENOT4
     C                   ELSE
     C                   MOVEL(P)  XXEODA        DENOT1
     C                   MOVEL(P)  XXEODB        DENOT2
     C     1             SUBST     DENOT2:1      XXPOS1            1
     C     XXPOS1        IFEQ      *BLANK
     C     69            SUBST(P)  DENOT2:2      DENOT2
     C                   ENDIF
     C                   ENDIF                                                  *IN72 = *ON
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEOP - Write FAEOP record
     C*================================================================
     C     WRTEOP        BEGSR
     C*
     C* WRITE FIRST EOMB
      // Make sure User ID is added where original bill or recon
      /Free
                         User = userId;
      /End-free
     C*
     C     EOBKY1        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEO01
     C                   Z-ADD     U1CO#         EOCO#
     C                   Z-ADD     U1FND         EOFND
     C                   Z-ADD     U1FYR         EOFYR
     C                   Z-ADD     U1CASE        EOCASE
     C*
     C                   Z-ADD     U1TRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C                   MOVE      U1PFEI        EOFEIN
     C                   Z-ADD     U1MOD         EOMOD
     C                   MOVE      U1AREA        EOAREA
     C                   MOVE      U1PHN         EOPHN
     C                   MOVEL(P)  U1RLIC        EORLIC
     C*
     C                   Z-ADD     U1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOFRDT
     C*
     C                   Z-ADD     U1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOTODT
     C                   MOVEL(P)  U1NTWK        KKNWCD
     C
     C                   MOVE      U1PACD        EOFLG8
     C
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   NW10A3
     C                   CLEAR                   NWFL01
     C                   ENDIF
     C*
     C     *IN16         IFEQ      *ON
     C     PDDBA1        IFNE      'Y'
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVE      PDNAME        EOPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EOPNAM
     C                   ENDIF
     C                   MOVE      PDADR1        EOADR1
     C                   MOVE      PDADR2        EOADR2
     C                   ELSE
     C                   MOVE      PDADR1        EOPNAM
     C                   MOVE      PDADR2        EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C                   ENDIF
     C                   MOVE      PDCTY         EOCTY
     C                   MOVE      PDST          EOST
     C                   MOVE      PDZIP         EOZIP
     C                   ELSE
     C     U1PFSN        IFEQ      *BLANKS
     C                   MOVEL(P)  U1PNAM        EOPNAM
     C                   ELSE
     C     U1PFSN        CAT(P)    U1PNAM:1      EOPNAM
     C                   ENDIF
     C                   MOVEL(P)  U1ADR1        EOADR1
     C                   MOVEL(P)  U1ADR2        EOADR2
     C                   MOVEL(P)  U1CTY         EOCTY
     C                   MOVEL(P)  U1ST          EOST
     C*
     C                   MOVEL     U1ZIP         TEST4             4
     C     TEST4         IFEQ      '0000'
     C     TEST4         OREQ      '    '
     C                   MOVE      U1ZIP         ZIP               5
     C                   MOVEL     ZIP           EOZIP
     C                   ELSE
     C                   MOVE      U1ZIP         EOZIP
     C                   ENDIF
     C*
     C*
     C     U1PROV        IFNE      *BLANKS
     C     U1PROV        ANDNE     XXPROV
     C*
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        EOPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EOPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        EOADR1
     C                   MOVEL(P)  PDADR2        EOADR2
     C                   MOVEL(P)  PDCTY         EOCTY
     C                   MOVEL(P)  PDST          EOST
     C                   MOVEL(P)  PDZIP         EOZIP
       If PDdbA1 = 'Y';
          EOpNam = PDadr1;
          EOadr1 = PDadr2;
          EOadr2 = *blanks;
       Endif;
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL     PDSPCD        EOEOB1
     C*
     C     U1PEND        IFEQ      'Y'
     C     NW10A3        ANDNE     *BLANKS
     C     NWFL01        OREQ      'Y'
     C     NW10A3        ANDNE     *BLANKS
     C                   Z-ADD     *ZEROS        EOCHK#
     C                   MOVEL(P)  'P'           EOBLST
     C                   MOVEL     '1'           EOCSCD
     C                   ELSE
     C                   MOVEL(P)  'C'           EOBLST
     C                   MOVEL     '2'           EOCSCD
     C                   eval      eocrne = 'N'
     C     C135          IFEQ      *ON
     C     CMDNY         ANDEQ     'Y'
     C     C135          OREQ      *ON
     C     CMSTAT        ANDEQ     'C'
     C                   MOVEL     'P'           EOBLST
     C                   ENDIF
     C                   Z-ADD     U1EOB#        EOCHK#
     C                   ENDIF
     C     NW10A4        IFNE      *BLANKS
     C                   MOVEL(P)  NW10A4        EOCSCD
     C                   ENDIF
     C                   Z-ADD     U1EOB#        EOEOB#
     C                   Z-ADD     KKRCN         EORCN
     C*
     C     U1BTYP        IFEQ      'H'
     C                   MOVEL(P)  'H'           EOSYS
     C                   ELSE
     C                   MOVEL(P)  'P'           EOSYS
     C                   ENDIF
     C                   MOVEL(P)  U1IDOC        EOIDOC
     C                   MOVEL(P)  U1CONT        EOCONT
     C                   Z-ADD     U1FFEI        EOFFEI
     C                   MOVEL(P)  U1FNAM        EOFNAM
     C                   Z-ADD     U1FZIP        EOFZIP
     C                   MOVEL(P)  U1BLIC        EOBLIC
     C                   MOVEL(P)  U1BTYC        EOBTYC
     C                   MOVEL(P)  U1RNAM        EORNAM
     C                   Z-ADD     U1RFEI        EOSFEI
     C                   MOVEL(P)  U1RFNM        EORFNM
     C                   MOVEL(P)  U1RLNM        EORLNM
     C                   MOVEL(P)  U1BLID        EOBLID
     C*
     C                   SELECT
     C     C132          WHENEQ    *ON
     C*
     C                   SELECT
     C     U1NTWK        WHENEQ    'COMP'
     C                   MOVEL(P)  'COMPIQ'      USER
     C     U1NTWK        WHENEQ    'BC'
     C                   MOVEL(P)  'BC'          USER
     C                   ENDSL
     C     C139          WHENEQ    *ON
     C                   MOVEL(P)  'MCMC'        USER
     C*
     C                   ENDSL
     C*
     C                   MOVEL(P)  USER          EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL(P)  USER          EOIUSR
     C                   MOVEL     SRDTO         EOIDT
     C*
     C                   MOVEL(P)  U1ICD1        EODIA1
     C                   MOVEL(P)  U1ICD2        EODIA2
     C                   MOVEL(P)  U1ICD3        EODIA3
     C                   MOVEL(P)  U1ICD4        EODIA4
     C*
     C*
     C*
     C     U1SZIP        IFNE      *ZEROS
     C                   MOVEL     U1SZIP        EOFLIC
     C                   ELSE
     C*
     C*
     C     U1ZIPA        IFNE      *ZEROS
     C                   MOVEL     U1ZIPA        EOFLIC
     C                   ELSE
     C                   MOVEL     *BLANKS       EOFLIC
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     1             EO#BLL
     C                   MOVEL     XXAST         EOAST
     C     U1AST         IFNE      *BLANK
      /Free
      /End-Free
     C                   MOVEL(P)  U1AST         EOAST
     C                   ENDIF
     C*
     C                   Z-ADD     U1SUB#        EOSUB#
     C*
     C     U1ZIPA        IFNE      *ZEROS
     C                   MOVEL(P)  U1ZIPA        KKZIPA
     C                   ELSE
     C                   MOVEL     *BLANKS       KKZIPA
     C                   ENDIF
     C*
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF
     C*
     C                   MOVEL(P)  U1PSS#        EOSS#
     C                   Z-ADD     U1ACDT        EOACDT
     C*
     C                   MOVEL(P)  Q1PGM         EOIPGM
     C                   Z-ADD     DSTIME        EOITIM
     C*
     C                   SELECT
     C     U1BTYP        WHENEQ    'M'
     C                   MOVEL(P)  'N'           EOBTYP
     C     U1BTYP        WHENEQ    'H'
     C                   MOVEL(P)  'H'           EOBTYP
     C     U1BTYP        WHENEQ    'P'
     C                   MOVEL(P)  'X'           EOBTYP
     C     U1BTYP        WHENEQ    'D'
     C                   MOVEL(P)  'D'           EOBTYP
     C                   ENDSL
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BEEDI         EOTAPE
     C                   MOVEL(P)  BEREVI        EOREVI
     C                   ELSE
     C     U1LIC#        IFEQ      *BLANKS
     C     U1RLIC        ANDEQ     *BLANKS
     C     U1LIC#        OREQ      *BLANKS
     C     U1RLIC        ANDEQ     'UNKNOWN'
     C                   MOVEL(P)  'N'           EOTAPE
     C                   ELSE
     C                   MOVEL(P)  'Y'           EOTAPE
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     U1CSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOCSDT
     C*
     C     u1btyp        IfEQ      'H'
     C     ##CLT#        AndEQ     '129'
     C                   Eval      eocsdt = *Zeros
     C                   EndIf
     C*
     C                   Z-ADD     U1CRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EOCRDT
     C*
     C                   MOVEL     U1TRID        EOSINV
     C                   MOVEL(P)  U1CLM#        EOCID#
     C*
     C                   MOVE      U1CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EOCHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        EOCHRG
     C                   ENDIF
     C*
     C                   MOVE      U1MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EOMRA
     C                   ELSE
     C                   Z-ADD     AMOUNT        EOMRA
     C                   ENDIF
     C*
     C     U1OTH2        IFNE      *BLANKS
     C                   MOVE      U1OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EOOTH2
     C                   ELSE
     C                   Z-ADD     AMOUNT        EOOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U1NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EONDIS
     C                   ELSE
     C                   Z-ADD     AMOUNT        EONDIS
     C                   ENDIF
     C*
     C                   MOVE      U1PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EOPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        EOPAID
     C                   ENDIF
     C*
     C                   MOVEL(P)  U1NTWK        EONTWK
     C     U1NTWK        IFEQ      'P2P'
     C     U1NTWK        OREQ      'DCRX'
     C                   MOVE      *ON           P2P
     C                   ENDIF
     C*
     C     U1SPCD        IFEQ      *BLANKS
     C                   MOVEL     PDSPCD        EOSPCD
     C                   ELSE
     C                   MOVEL     U1SPCD        EOSPCD
     C                   ENDIF
     C*
     C     u1btyp        IfEQ      'H'
     C     ##CLT#        AndEQ     '129'
     C                   Eval      eospcd = *Blanks
     C                   EndIf
     C*
     C                   MOVEL(P)  U1PSS#        KKSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CDLSN         EOLSN
     C                   MOVEL(P)  CDFSN         EOFSN
     C                   MOVEL(P)  CDMID         EOMID
     C                   ELSE
     C                   MOVEL     *BLANKS       EOLSN
     C                   MOVEL     *BLANKS       EOFSN
     C                   MOVEL     *BLANKS       EOMID
     C                   ENDIF
     C                   MOVEL(P)  U1NDSC        EONDSC
     C*
     C* THIS IS TML SO COMPLETE THE BILL
     C*
     C*
     C*
     C** If bill is uploaded by karvy and client = 146 then EOMBUP= 'K'
     C*
     C     C146          IFEQ      *ON
     C     U1NTWK        ANDEQ     'KARVY'
     C                   MOVEL     'K'           EOMBUP
     C                   ENDIF
     C*
     C     TRANS         CAT(P)    U1TRID:1      EORMDS
     C*
     C                   WRITE     PEO01
     C                   ELSE
     C*
     C* This is a Reconsideration
     C*
     C     *IN92         IFEQ      *ON
     C                   MOVEL(P)  U1IDOC        EOIDOC
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  USER          EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   MOVE      U1CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        EOCHRG
     C                   ELSE
     C                   ADD       AMOUNT        EOCHRG
     C                   ENDIF
     C*
     C                   MOVE      U1MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        EOMRA
     C                   ELSE
     C                   ADD       AMOUNT        EOMRA
     C                   ENDIF
     C*
     C     U1OTH2        IFNE      *BLANKS
     C                   MOVE      U1OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        EOOTH2
     C                   ELSE
     C                   ADD       AMOUNT        EOOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U1NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        EONDIS
     C                   ELSE
     C                   ADD       AMOUNT        EONDIS
     C                   ENDIF
     C*
     C                   MOVE      U1PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   SUB       AMOUNT        EOPAID
     C                   ELSE
     C                   ADD       AMOUNT        EOPAID
     C                   ENDIF
     C*
      /Free
                         If U1rcnF = 'Y' and kkRcn > EOrcn;
                            EOstat = 'R';
                         Endif;
      /End-free
     C                   Z-ADD     KKRCN         EORCN
     C                   MOVEL(P)  U1TRID        EOSINV
     C                   ENDIF
     C*
     C* THIS IS TML SO COMPLETE THE BILL
     C*
     C*
     C*
     C** If bill is uploaded by karvy and client = 146 then EOMBUP= 'K'
     C*
     C     C146          IFEQ      *ON
     C     U1NTWK        ANDEQ     'KARVY'
     C                   MOVEL     'K'           EOMBUP
     C                   ENDIF
     C*
     C*** Beg Add *** 04/26/23 *********************************************
     C     TRANS         CAT(P)    U1TRID:1      EORMDS
     C*** End Add *** 04/26/23 *********************************************
     C*
     C                   UPDATE    PEO01
     C                   ENDIF
     C                   Eval      ##EopRrn = DBRRN
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEAP - Write FAEAP record
     C*================================================================
     C     WRTEAP        BEGSR
     C*
     C     EOBKY2        CHAIN     PEA01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEA01
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C                   Z-ADD     EODT          EATRDT
     C     EOBLST        IFEQ      'P'
     C                   MOVEL(P)  'E'           EAEPFL
     C                   ENDIF
     C*
     C                   MOVE      U1CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EACHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        EACHRG
     C                   ENDIF
     C*
     C                   MOVE      U1MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EAMRA
     C                   ELSE
     C                   Z-ADD     AMOUNT        EAMRA
     C                   ENDIF
     C*
     C     U1OTH2        IFNE      *BLANKS
     C                   MOVE      U1OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EAOTH2
     C                   ELSE
     C                   Z-ADD     AMOUNT        EAOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U1NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EANDIS
     C                   ELSE
     C                   Z-ADD     AMOUNT        EANDIS
     C                   ENDIF
     C*
     C                   MOVE      U1PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        EAPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        EAPAID
     C                   ENDIF
     C                   MOVEL(P)  EOTAPE        EAEDI
     C     C139          IFEQ      *ON
     C                   MOVEL     'E'           EAEDI
     C                   ENDIF
     C     C133          IFEQ      *ON
     C                   MOVEL     'E'           EAEDI
     C                   ENDIF
      /Free
                         If C146 = *On;
                            EAedi = 'E';
                         Endif;
        //** Set EAedi for Paid Date Export
                         if EOtape = 'Y';
                           EAedi = 'E';
                         Endif;
      /End-Free
     C*
     C                   MOVEL(P)  'A'           EASTAT
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  USER          EAUSER
     C                   MOVEL(P)  Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL(P)  USER          EAIUSR
     C                   MOVEL(P)  Q1PGM         EAIPGM
     C                   Z-ADD     DSTIME        EAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAIDT
     C*
     C                   Z-ADD     U1COST        EANAMT
     C                   MOVEL(P)  'H'           EABHRF
     C                   MOVEL(P)  'H'           EANHRF
     C                   MOVEL(P)  EOSYS         EASYS
     C                   MOVE      EOBTYP        EABTYP
     C                   Z-ADD     U1COST        EANCAM
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C                   ELSE
     C     EOPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF                                                  EOPAID LE 0
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C                   MOVEL(P)  U1TRID        EASINV
     C*
     C     *IN16         IFEQ      *ON
     C     PDDBA1        IFNE      'Y'
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVE      PDNAME        EAPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EAPNAM
     C                   ENDIF
     C                   MOVE      PDADR1        EAADR1
     C                   MOVE      PDADR2        EAADR2
     C                   ELSE
     C                   MOVE      PDADR1        EAPNAM
     C                   MOVE      PDADR2        EAADR1
     C                   MOVE      *BLANKS       EAADR2
     C                   ENDIF
     C                   MOVE      PDCTY         EACTY
     C                   MOVE      PDST          EAST
     C                   MOVE      PDZIP         EAZIP
     C                   ELSE
     C     U1PFSN        IFEQ      *BLANKS
     C                   MOVEL(P)  U1PNAM        EAPNAM
     C                   ELSE
     C     U1PFSN        CAT(P)    U1PNAM:1      EAPNAM
     C                   ENDIF
     C                   MOVEL(P)  U1ADR1        EAADR1
     C                   MOVEL(P)  U1ADR2        EAADR2
     C                   MOVEL(P)  U1CTY         EACTY
     C                   MOVEL(P)  U1ST          EAST
     C*
     C                   MOVEL     U1ZIP         TEST4
     C     TEST4         IFEQ      '0000'
     C     TEST4         OREQ      '    '
     C                   MOVE      U1ZIP         ZIP
     C                   MOVEL     ZIP           EAZIP
     C                   ELSE
     C                   MOVE      U1ZIP         EAZIP
     C                   ENDIF
     C*
     C     U1PROV        IFNE      *BLANKS
     C     U1PROV        ANDNE     XXPROV
       // Load provider demographics file again to make sure provider info gets loaded
       Monitor;
       kkFein = %Dec(U1fei1:9:0);
       On-Error;
          kkFein = 0;
       Endmon;
       Monitor;
       kkmod  = %Dec(U1Mod1:3:0);
       On-Error;
          kkMod = 0;
       Endmon;
       Chain pdky1 ppd01;
     C*
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        EAPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EAPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        EAADR1
     C                   MOVEL(P)  PDADR2        EAADR2
     C                   MOVEL(P)  PDCTY         EACTY
     C                   MOVEL(P)  PDST          EAST
     C                   MOVEL(P)  PDZIP         EAZIP
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* THIS IS TML SO COMPLETE THE BILL
     C*
     C*
      /Free
       If EApLnk = '2' or EApLnk = '3';
          Exsr GetEpayStat;
          If strEPayVn <> *Blanks;
             EAeftC = 'V';
          Endif;
       Endif;
      /End-free
     C                   WRITE     PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEDP - Write FAEDP record
     C*================================================================
     C     WRTEDP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EOBKY1        CHAIN     PED01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PED01
     C                   Z-ADD     EOCO#         EDCO#
     C                   Z-ADD     EOSUB#        EDSUB#
     C                   Z-ADD     EOFND         EDFND
     C                   Z-ADD     EOFYR         EDFYR
     C                   Z-ADD     EOCASE        EDCASE
     C                   Z-ADD     EOEOB#        EDEOB#
     C                   MOVEL     EOSS#         EDSS#
     C                   Z-ADD     EOACDT        EDACDT
     C                   MOVEL     U1LIC#        EDLIC#
     C*
     C                   MOVEL     EOTAPE        EDEDI
     C*
     C                   MOVEL     'A'           EDSTAT
     C                   MOVEL     U1PCON        EDACCT
     C                   MOVEL(P)  U1PPLN        EDPPLN
     C                   MOVEL(P)  U1ACCT        EDACCT
     C                   MOVEL(P)  U1RLIC        EDFLIC
     C                   MOVEL(P)  U1RFLC        EDRFLC
     C                   MOVEL(P)  U1ATLC        EDATLC
     C*
     C                   MOVEL(P)  EOUSER        EDUSER
     C                   MOVEL(P)  EOPGM         EDPGM
     C                   Z-ADD     EOTIME        EDTIME
     C                   Z-ADD     EODT          EDDT
     C                   MOVEL(P)  EOUSER        EDIUSR
     C                   MOVEL(P)  EOPGM         EDIPGM
     C                   Z-ADD     EOITIM        EDITIM
     C                   Z-ADD     EOIDT         EDIDT
     C*
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C                   MOVEL(P)  EODIA1        EDDIA1
     C                   MOVEL(P)  EODIA2        EDDIA2
     C                   MOVEL(P)  EODIA3        EDDIA3
     C                   MOVEL(P)  EODIA4        EDDIA4
     C* Make ICD10 indicator to 'N' when ICD9 fields are loaded.
      /Free
         EDICDIND = 'N';
      /End-Free
     C                   EndIf
      /free
        If U1ICD10Ind = 'Y';
           EDICD1001 = U1ICD1001;
           EDICD1002 = U1ICD1002;
           EDICD1003 = U1ICD1003;
           EDICD1004 = U1ICD1004;
           EDICDIND = 'Y';
        EndIf;
      /end-free
     C                   MOVEL(P)  U1POTX        EDPOTX
     C                   MOVEL(P)  U1RNAM        EDRLSN
     C                   Z-ADD     U1RFEI        EDRFEI
     C*
     C                   MOVE      U1SFEI        EDRFEI
     C                   MOVE      U1SFNM        EDRFSN
     C                   MOVE      U1SLNM        EDRLSN
     C                   MOVE      U1SAD1        EDRAD1
     C                   MOVE      U1SAD2        EDRAD2
     C                   MOVE      U1SCTY        EDRCTY
     C                   MOVE      U1SST         EDRST
     C                   MOVE      U1SZIP        EDRZIP
      /Free
          EDtnpi = U1tnpi;
      /End-Free
     C                   WRITE     PED01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEEP - Write FAEEP record
     C*================================================================
     C     WRTEEP        BEGSR
     C*
     C     U1BADJ        IFNE      *BLANKS
     C     U1BACD        ORNE      *BLANKS
     C     U1EDS1        ORNE      *BLANKS
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     EEKY2         SETGT     PEE01
     C     EEKY1         READPE    PEE01                                  99
     C     *IN99         IFEQ      *OFF
     C     EESEQ#        ADD       1             HDSEQ#
     C                   ELSE
     C                   Z-ADD     1             HDSEQ#
     C                   ENDIF
     C*
     C     EEKY          CHAIN     PEE02                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEE02
     C                   Z-ADD     EOCO#         EECO#
     C                   Z-ADD     EOSUB#        EESUB#
     C                   Z-ADD     EOFND         EEFND
     C                   Z-ADD     EOFYR         EEFYR
     C                   Z-ADD     EOCASE        EECASE
     C                   Z-ADD     EOEOB#        EEEOB#
     C                   Z-ADD     EORCN         EERCN
     C                   MOVEL     EOSS#         EESS#
     C                   Z-ADD     EOACDT        EEACDT
     C                   Z-ADD     HDSEQ#        EESEQ#
     C                   MOVEL(P)  U1BADJ        EEEOBCD
     C                   MOVEL(P)  U1BAGP        EEBADG
     C     U1EDS1        IFNE      *Blanks
     C                   MOVEL(P)  U1EDS1        XXEDS
     C                   CLEAR                   XXEDS1
     C                   CLEAR                   XXEDS2
     C                   CLEAR                   XXEDS3
     C                   CLEAR                   XXEDS4
     C                   MOVEL     'H'           XXPROC            1
     C                   EXSR      LODNOT
     C                   MOVEL(P)  XXEDS1        EENOT1
     C                   MOVEL(P)  XXEDS2        EENOT2
     C                   MOVEL(P)  XXEDS3        EENOT3
     C                   MOVEL(P)  XXEDS4        EENOT4
     C                   ELSE                                                   C139 = *ON
     C                   MOVEL(P)  U1BACD        EENOT1
     C                   ENDIF                                                  C139 = *ON
     C                   MOVEL(P)  'Y'           EEPRNT
     C*
     C                   MOVEL     'A'           EESTAT
     C                   MOVEL(P)  EOUSER        EEUSER
     C                   MOVEL(P)  EOPGM         EEPGM
     C                   Z-ADD     EOTIME        EETIME
     C                   Z-ADD     EODT          EEDT
     C                   MOVEL(P)  EOUSER        EEIUSR
     C                   MOVEL(P)  EOPGM         EEIPGM
     C                   Z-ADD     EOTIME        EEITIM
     C                   Z-ADD     EODT          EEIDT
     C                   WRITE     PEE02
     C*
     C                   ENDIF
     C                   ENDIF
      /Free
                         // Write the expanded Reason Code Description #2 if not blank and
                         // Description 1 is also not blanks
                         If U1eDs2 <> *blanks and U1eDs2 <> *blanks;
                         // Get next avail sequence number
                            kkSeq# = *Hival;
                            Setgt eeKy2 pee01;
                            Readpe eeKy1 pee01;
                            If not %Eof(FAEEL);
                               hdSeq# = EEseq# + 1;
                            Else;
                               hdSeq# = 1;
                            Endif;
                         // Format the 4 note fields and write to FAEEP
                            Clear pee02;
                            EEco#  = EOco#;
                            EEsub# = EOsub#;
                            EEfnd  = EOfnd;
                            EEfYr  = EOfYr;
                            EEcase = EOcase;
                            EEeob# = EOeob#;
                            EErcn  = EOrcn;
                            EEss#  = %Trim(EOss#);
                            EEacDt = EOacDt;
                            EEseq# = hdSeq#;
                            xxEds  = U1eDs2;
                            Clear xxEds1;
                            Clear xxEds2;
                            Clear xxEds3;
                            Clear xxEds4;
                            xxProc = 'H';
                            Exsr LodNot;
                            EEnot1 = xxEds1;
                            EEnot2 = xxEds2;
                            EEnot3 = xxEds3;
                            EEnot4 = xxEds4;
                            EEprnt = 'Y';
                            EEstat = 'A';
                            EEuser = Q1user;
                            EEpgm  = EOpgm;
                            EEtime = EOtime;
                            EEdt   = EOdt;
                            EEiUsr = Q1user;
                            EEiPgm = EOpgm;
                            EEiTim = EOtime;
                            EEiDt  = EOdt;
                            Write pee02;
                         Endif;
      /End-Free
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEHP - Write FAEHP record
     C*================================================================
     C     WRTEHP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EOBKY1        CHAIN     PEH01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEH01
     C                   Z-ADD     EOCO#         EHCO#
     C                   Z-ADD     EOSUB#        EHSUB#
     C                   Z-ADD     EOFND         EHFND
     C                   Z-ADD     EOFYR         EHFYR
     C                   Z-ADD     EOCASE        EHCASE
     C                   Z-ADD     EOEOB#        EHEOB#
     C                   MOVEL(P)  EOSS#         EHSS#
     C                   Z-ADD     EOACDT        EHACDT
     C                   MOVEL     U1PCON        EHACCT
     C*
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C                   MOVEL(P)  EODIA1        EHDIA1
     C                   MOVEL(P)  EODIA2        EHDIA2
     C                   MOVEL(P)  EODIA3        EHDIA3
     C                   MOVEL(P)  EODIA4        EHDIA4
     C* Make ICD10 indicator to 'N' when ICD9 fields are loaded.
      /Free
         EHICDIND = 'N';
      /End-Free
     C                   EndIf
      /free
        If U1ICD10Ind = 'Y';
           EHICD1001 = U1ICD1001;
           EHICD1002 = U1ICD1002;
           EHICD1003 = U1ICD1003;
           EHICD1004 = U1ICD1004;
           EHICDIND = 'Y';
        EndIf;
      /end-free
     C                   MOVEL(P)  U1LIC#        EHLIC#
     C*
     C                   MOVEL     EOTAPE        EHEDI
     C*
     C                   MOVEL(P)  'Y'           EHEMPL
     C                   MOVEL(P)  'A'           EHSTAT
     C                   MOVEL(P)  U1PPLN        EHPPLN
     C                   MOVEL(P)  U1ACCT        EHACCT
     C                   MOVEL(P)  U1RLIC        EHFLIC
     C     U1BLIC        IFNE      *BLANKS
     C                   MOVEL(P)  U1BLIC        EHFLIC
     C                   ENDIF
     C                   MOVEL(P)  U1RFLC        EHRFLC
     C                   MOVEL(P)  U1ATLC        EHATLC
     C                   MOVEL(P)  U1RFNP        EHRNPI
     C*
     C                   MOVEL(P)  EOUSER        EHUSER
     C                   MOVEL(P)  EOPGM         EHPGM
     C                   Z-ADD     EOTIME        EHTIME
     C                   Z-ADD     EODT          EHDT
     C                   MOVEL(P)  EOUSER        EHIUSR
     C                   MOVEL(P)  EOPGM         EHIPGM
     C                   Z-ADD     EOITIM        EHITIM
     C                   Z-ADD     EOIDT         EHIDT
     C                   MOVEL(P)  U1RNAM        EHRLSN
     C                   Z-ADD     U1RFEI        EHRFEI
     C                   MOVE      U1SFEI        EHRFEI
     C                   MOVE      U1SFNM        EHRFSN
     C                   MOVE      U1SLNM        EHRLSN
     C                   MOVE      U1SAD1        EHRAD1
     C                   MOVE      U1SAD2        EHRAD2
     C                   MOVE      U1SCTY        EHRCTY
     C                   MOVE      U1SST         EHRST
     C                   MOVE      U1SZIP        EHRZIP
     C*
     C*
     C*
      /Free
                         If %Check(digits:U1dFc) > 0;
                            erDfC = *on;
                         Else;
                            erDfC = *off;
 001                        xxDFC = %dec(U1dfc:8:0);
                         Endif;
      /End-Free
     C     erDfC         IfEq      *off
     C                   Z-ADD     xxDFC         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EHDFC
     C                   Endif
     C*
     C                   MOVE      U1SFNPI       EHSNPI
     C*
     C                   WRITE     PEH01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEPP - Write FAEPP record
     C*================================================================
     C     WRTEPP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EOBKY1        CHAIN     PEP01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEP01
     C                   Z-ADD     EOCO#         EPCO#
     C                   Z-ADD     EOSUB#        EPSUB#
     C                   Z-ADD     EOFND         EPFND
     C                   Z-ADD     EOFYR         EPFYR
     C                   Z-ADD     EOCASE        EPCASE
     C                   Z-ADD     EOEOB#        EPEOB#
     C                   MOVEL(P)  EOSS#         EPSS#
     C                   Z-ADD     EOACDT        EPACDT
     C                   MOVEL     EOTAPE        EPEDI
     C                   MOVEL     U1PCON        EPACCT
     C                   MOVEL     U1PHAR        EPPHAR
     C                   MOVEL(P)  U1RLIC        EPPLIC
     C                   MOVEL(P)  U1ACCT        EPACCT
     C                   MOVEL(P)  U1RLIC        EPFLIC
     C                   MOVEL(P)  U1PDEA        EPPDEA
     C                   MOVEL(P)  U1BTYC        EPBTCD
     C                   MOVEL(P)  U1RFNP        EPRFNP
     C*
     C                   MOVEL(P)  EOUSER        EPUSER
     C                   MOVEL(P)  EOPGM         EPPGM
     C                   Z-ADD     EOTIME        EPTIME
     C                   Z-ADD     EODT          EPDT
     C                   MOVEL(P)  EOUSER        EPIUSR
     C                   MOVEL(P)  EOPGM         EPIPGM
     C                   Z-ADD     EOITIM        EPITIM
     C                   Z-ADD     EOIDT         EPIDT
      /Free
          EPrfnp = U1pnpi;
          EPrnln = U1rnam;
          EPrnfn = U1sfnm;
      /End-Free
     C*
     C                   WRITE     PEP01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTESP - Write FAESP record
     C*================================================================
     C     WRTESP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EOBKY2        CHAIN     PES01                              99
     C     *IN99         IFEQ      *ON
     C*
     C* WRITE RECON RECORD IF THIS IS A RECONSIDERATION
     C*
     C     *IN92         IFEQ      *ON
     C                   CLEAR                   PES01
     C                   Z-ADD     EOCO#         ESCO#
     C                   Z-ADD     EOSUB#        ESSUB#
     C                   Z-ADD     EOFND         ESFND
     C                   Z-ADD     EOFYR         ESFYR
     C                   Z-ADD     EOCASE        ESCASE
     C                   Z-ADD     EOEOB#        ESEOB#
     C                   Z-ADD     EORCN         ESRCN
     C                   MOVEL     EOSS#         ESSS#
     C                   Z-ADD     EOACDT        ESACDT
     C                   MOVEL(P)  'R'           ESBLST
     C                   MOVEL(P)  REAS2         ESREAS
     C*
     C                   Z-ADD     U1CRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         ESRRDT
     C*
     C                   ADD       1             EOTIME
     C                   MOVEL     'A'           ESSTAT
     C                   MOVEL(P)  EOUSER        ESUSER
     C                   MOVEL(P)  EOPGM         ESPGM
     C                   Z-ADD     EOTIME        ESTIME
     C                   Z-ADD     EODT          ESDT
     C                   MOVEL(P)  EOUSER        ESIUSR
     C                   MOVEL(P)  EOPGM         ESIPGM
     C                   Z-ADD     EOTIME        ESITIM
     C                   Z-ADD     EODT          ESIDT
     C                   ADD       1             EOTIME
     C                   WRITE     PES01
     C                   ENDIF
     C*
     C                   CLEAR                   PES01
     C                   Z-ADD     EOCO#         ESCO#
     C                   Z-ADD     EOSUB#        ESSUB#
     C                   Z-ADD     EOFND         ESFND
     C                   Z-ADD     EOFYR         ESFYR
     C                   Z-ADD     EOCASE        ESCASE
     C                   Z-ADD     EOEOB#        ESEOB#
     C                   Z-ADD     EORCN         ESRCN
     C                   MOVEL     EOSS#         ESSS#
     C                   Z-ADD     EOACDT        ESACDT
     C     EOBLST        IFEQ      'P'
     C                   MOVEL(P)  'P'           ESBLST
     C                   MOVEL(P)  NW10A3        ESSTCD
     C     C135          IFEQ      *ON
     C     CMDNY         ANDEQ     'Y'
     C     C135          OREQ      *ON
     C     CMSTAT        ANDEQ     'C'
     C                   MOVEL(P)  'DNY'         ESSTCD
     C                   ENDIF
     C                   MOVEL(P)  'P'           KKTYPE
     C                   MOVEL(P)  NW10A3        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSDESC        ESREAS
     C                   ELSE
     C                   MOVEL(P)  REAS3         ESREAS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  'C'           ESBLST
     C                   MOVEL(P)  REAS1         ESREAS
     C                   ENDIF
     C*
     C                   MOVEL     'A'           ESSTAT
     C                   MOVEL(P)  EOUSER        ESUSER
     C                   MOVEL(P)  EOPGM         ESPGM
     C                   Z-ADD     EOTIME        ESTIME
     C                   Z-ADD     EODT          ESDT
     C                   MOVEL(P)  EOUSER        ESIUSR
     C                   MOVEL(P)  EOPGM         ESIPGM
     C                   Z-ADD     EODT          ESIDT
     C                   Z-ADD     EOTIME        ESITIM
     C*
     C                   WRITE     PES01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEUP - Write FAEUP record
     C*================================================================
     C     WRTEUP        BEGSR
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EOBKY1        CHAIN     PEU01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PEU01
     C                   Z-ADD     EOCO#         EUCO#
     C                   Z-ADD     EOSUB#        EUSUB#
     C                   Z-ADD     EOFND         EUFND
     C                   Z-ADD     EOFYR         EUFYR
     C                   Z-ADD     EOCASE        EUCASE
     C                   MOVEL     EOSS#         EUSS#
     C                   Z-ADD     EOACDT        EUACDT
     C                   Z-ADD     EOEOB#        EUEOB#
     C                   MOVE      '0'           EUPCM
     C     U1MCAR        IFEQ      XXMCAR
     C                   MOVEL     *BLANKS       EUMCAR
     C                   ELSE
     C                   MOVEL(P)  U1MCAR        EUMCAR
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     U1CFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUSCPF
     C*
     C                   Z-ADD     U1CTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUSCPT
     C*
     C                   MOVEL(P)  U1PCON        EUACCT
     C*
     C                   MOVEL(P)  U1BLTY        EUBLTY
     C*
     C                   Z-ADD     U1ADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUADDT
     C*
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C                   MOVEL(P)  EODIA1        EUDIA1
     C                   MOVEL(P)  EODIA2        EUDIA2
     C                   MOVEL(P)  EODIA3        EUDIA3
     C                   MOVEL(P)  EODIA4        EUDIA4
     C* Make ICD10 indicator to 'N' when ICD9 fields are loaded.
      /Free
         EUICDIND = 'N';
      /End-Free
     C                   EndIf
      /free
        If U1ICD10Ind = 'Y';
           EUICD1001 = U1ICD1001;
           EUICD1002 = U1ICD1002;
           EUICD1003 = U1ICD1003;
           EUICD1004 = U1ICD1004;
           EUICD1005 = U1ICD1005;
           EUICD1006 = U1ICD1006;
           EUICD1007 = U1ICD1007;
           EUICD1008 = U1ICD1008;
           EUICDIND = 'Y';
        EndIf;
      /end-free
     C                   MOVEL     U1LIC#        EULIC#
     C*
     C                   MOVEL     EOTAPE        EUEDI
     C                   MOVEL(P)  U1OLIC        EUOLIC
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C                   MOVEL(P)  U1ICD5        EUDIA5
     C                   MOVEL(P)  U1ICD6        EUDIA6
     C                   MOVEL(P)  U1ICD7        EUDIA7
     C                   MOVEL(P)  U1ICD8        EUDIA8
     C                   MOVEL(P)  U1PRC3        EUPRC3
     C                   MOVEL(P)  U1PRC4        EUPRC4
     C                   MOVEL(P)  U1PRC5        EUPRC5
     C                   MOVEL(P)  U1CAU1        EUCAU1
     C                   MOVEL(P)  U1CAU2        EUCAU2
     C                   MOVEL(P)  U1CAU3        EUCAU3
     C                   EndIf
      /free
        If U1ICD10Ind = 'Y';
           EUCAU1001 = U1CAU1001;
           EUCAU1002 = U1CAU1002;
           EUCAU1003 = U1CAU1003;
        EndIf;
      /end-free
     C                   MOVEL(P)  U1SCHD        EUSCHD
     C                   MOVEL(P)  U1SVTY        EUICN
     C                   Z-ADD     U1ADHR        EUADHR
     C*
     C                   Z-ADD     U1DCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUDCDT
     C*
     C                   Z-ADD     U1DCHR        EUDCHR
     C                   MOVEL(P)  U1ADTY        EUADTY
     C                   MOVEL(P)  U1TSS         EUDCN
     C                   MOVEL(P)  U1PCM         EUPCM
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C                   MOVEL(P)  U1PRC1        EUPRC1
     C                   MOVEL(P)  U1PRC2        EUPRC2
     C                   EndIf
      /free
        If U1ICD10Ind = 'Y';
           EUPRC1001 = U1PRC1001;
           EUPRC1002 = U1PRC1002;
           EUPRC1003 = U1PRC1003;
           EUPRC1004 = U1PRC1004;
           EUPRC1005 = U1PRC1005;
           EUPRC1006 = U1PRC1006;
        EndIf;
      /end-free
     C                   MOVEL(P)  U1PPLN        EUPPLN
     C                   MOVEL(P)  U1ACCT        EUACCT
     C                   MOVE(P)   U1DRG#        EUDRG#
     C*
     C                   Z-ADD     U1PPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUPRD1
     C*
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C                   MOVEL(P)  U1ADDG        EUADDI
     C                   EndIf
      /free
        If U1ICD10Ind = 'Y';
           EUADIA1001 = U1ADIA1001;
        EndIf;
      /end-free
     C                   MOVEL(P)  U1RLIC        EUFLIC
     C                   MOVEL(P)  U1ATLC        EUATLC
     C                   MOVEL(P)  U1RLIC        EURLIC
     C                   MOVEL(P)  U1ANPI        EUANPI
     C*
     C                   Z-ADD     U1PCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EUPRD2
     C*
     C                   MOVEL     'A'           EUSTAT
     C                   MOVEL(P)  EOUSER        EUUSER
     C                   MOVEL(P)  EOPGM         EUPGM
     C                   Z-ADD     EOTIME        EUTIME
     C                   Z-ADD     EODT          EUDT
     C                   MOVEL(P)  EOUSER        EUIUSR
     C                   MOVEL(P)  EOPGM         EUIPGM
     C                   Z-ADD     EOITIM        EUITIM
     C                   Z-ADD     EOIDT         EUIDT
     C                   MOVEL(P)  U1RNAM        EURLSN
     C                   Z-ADD     U1RFEI        EURFEI
     C*
     C*
     C                   MOVE      U1ADSC        EUADSC
     C                   MOVE      U1DCST        EUDCST
     C                   MOVE      U1PREA        EUPREA
     C                   MOVE      U1IMST        EUIMST
     C*
     C                   MOVE      U1SFEI        EURFEI
     C                   MOVE      U1SFNM        EURFSN
     C                   MOVE      U1SLNM        EURLSN
     C                   MOVE      U1SAD1        EURAD1
     C                   MOVE      U1SAD2        EURAD2
     C                   MOVE      U1SCTY        EURCTY
     C                   MOVE      U1SST         EURST
     C                   MOVE      U1SZIP        EURZIP
     C*
     C                   WRITE     PEU01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTCAP - Write FDCAP record
     C*================================================================
     C     WRTCAP        BEGSR
     C*
     C     U2EOB#        IFEQ      *ZEROS
     C     U2DSEQ        ANDEQ     *ZEROS
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C     CAKY          SETGT     PCA02
     C     CAKY2         READPE    PCA02                                  99
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   CLEAR                   PCA01
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     EOCSCD        CACSCD
     C*
     C                   Z-ADD     U2SVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         CAFDAT
     C*
     C                   Z-ADD     CAFDAT        CATDAT
     C                   Z-ADD     EOTRDT        CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL     '1'           CAPRTY
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL     '4'           CAPRTY
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL     'D'           CAPRTY
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL     'X'           CAPRTY
     C                   ENDSL
     C*
     C     U2UNTS        IFNE      *ZEROS
     C                   Z-ADD     U2UNTS        CAUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      CAunts = U2unts/100
     C                   endif
     C                   ELSE
     C                   Z-ADD     1             CAUNTS
     C                   ENDIF
     C*
     C                   Z-ADD     XXDSEQ        CADSEQ
     C     EOBLST        IFEQ      'P'
     C                   Z-ADD     0             CACHK#
     C                   ELSE
     C                   Z-ADD     EOCHK#        CACHK#
     C                   ENDIF
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  'D'           CARCFL
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL(P)  'S'           CARCFL
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  'C'           CARCFL
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL(P)  'X'           CARCFL
     C                   ENDSL
       //
       // Set CArCfl and CAsrc based on State flag
       //
      /Free
                         SetCodeType();
      /End-Free
     C*
     C                   MOVEL(P)  EOUSER        CAUSER
     C                   MOVEL(P)  EOPGM         CAPGM
     C                   Z-ADD     EODT          CADT
     C                   Z-ADD     EOTIME        CATIME
     C                   MOVEL(P)  EOUSER        CAIUSR
     C                   MOVEL(P)  EOPGM         CAIPGM
     C                   Z-ADD     EOIDT         CAIDT
     C                   Z-ADD     EOITIM        CAITIM
     C*
     C                   Z-ADD     EOSUB#        CASUB#
     C                   MOVE      EOZIPA        CAZIPA
     C                   MOVEL(P)  EOAST         CAAST
     C*
     C                   MOVE      U2CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CACHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        CACHRG
     C                   ENDIF
     C*
     C                   MOVE      U2MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CAMRA
     C                   ELSE
     C                   Z-ADD     AMOUNT        CAMRA
     C                   ENDIF
     C*
     C     U2OTH2        IFNE      *BLANKS
     C                   MOVE      U2OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CAOTH2
     C                   ELSE
     C                   Z-ADD     AMOUNT        CAOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U2NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CANDIS
     C                   ELSE
     C                   Z-ADD     AMOUNT        CANDIS
     C                   ENDIF
     C*
     C                   MOVE      U2PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CAPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        CAPAID
     C                   ENDIF
     C*
     C                   Z-ADD     U2LIN#        CALIN#
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     U2CODE        IFEQ      *BLANKS
     C                   MOVEL(P)  '96370'       CACODE
     C                   MOVEL(P)  '96370'       CACPT4
     C                   MOVEL(P)  '96370'       CABPRC
     C                   ELSE
     C                   MOVEL(P)  U2CODE        CACODE
     C                   MOVEL(P)  U2CODE        CACPT4
     C                   MOVEL(P)  U2CODE        CABPRC
     C                   ENDIF
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  U2CODE        CACODE
     C                   MOVEL(P)  U2CODE        CACPT4
     C                   MOVEL(P)  U2CODE        CABPRC
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL(P)  U2NDC         CACODE
     C     EOBTYP        WHENEQ    'H'
     C                   RESET                   KKCMSV
     C     DIGITS        CHECK     U2RVCD        N                 3 0    99
     C     *IN99         IFEQ      *ON
     C     U2CODE        IFEQ      *BLANKS
     C                   MOVEL     U2RVCD        KKHCPC
     C                   ELSE
     C                   MOVEL(P)  U2CODE        KKHCPC
     C                   ENDIF
     C                   ELSE
     C                   MOVE      U2RVCD        KKRVCD
     C                   MOVEL(P)  U2CODE        KKHCPC
     C                   ENDIF
     C                   MOVE      KKRVCD        CASVCD
     C                   MOVEL     KKCMSV        CACODE
     C                   ENDSL
     C*
     C* THIS IS TML SO COMPLETE THE BILL
     C*
     C     C146          IFEQ      *ON
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     U2CODE        KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C     ##RC          ANDNE     'N'
     C*
     C                   MOVEL     CAZIPA        KKZIPA
     C*
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  'Y'           CARCFL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
      /Free
         If C133 = '1'  and EOAST = 'LA' and U2CODE = *blanks and
         U2NDC <> *blanks ;
            CABPRC = *blanks ;
            CACPT4 = *blanks ;
            CACODE = U2NDC ;
         Endif ;
      /End-Free
     C                   WRITE     PCA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDAP - Write FDDAP record
     C*================================================================
     C     WRTDAP        BEGSR
     C*
     C                   MOVE      U1CHRG        CHRG1
     C                   MOVE      U1MRA         MRA1
     C                   MOVE      U1NDIS        NDIS1
     C                   MOVE      U1PAID        PAID1
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY2        CHAIN     PDA01                              99
     C     *IN99         IFEQ      *ON
     C     *IN92         IFEQ      *OFF
     C     *IN92         OREQ      *ON
     C     U2RLIN        ANDNE     *ZEROS
     C     CHRG2         ANDNE     *ZEROS
     C     *IN92         OREQ      *ON
     C     U2RLIN        ANDNE     *ZEROS
     C     MRA2          ANDNE     *ZEROS
     C     *IN92         OREQ      *ON
     C     U2RLIN        ANDNE     *ZEROS
     C     NDIS2         ANDNE     *ZEROS
     C     *IN92         OREQ      *ON
     C     U2RLIN        ANDNE     *ZEROS
     C     PAID2         ANDNE     *ZEROS
     C*
     C                   CLEAR                   PDA01
     C                   Z-ADD     CACO#         DACO#
     C                   Z-ADD     CASUB#        DASUB#
     C                   Z-ADD     CAFND         DAFND
     C                   Z-ADD     CAFYR         DAFYR
     C                   Z-ADD     CACASE        DACASE
     C                   MOVEL     CASS#         DASS#
     C                   Z-ADD     CAACDT        DAACDT
     C                   Z-ADD     CAEOB#        DAEOB#
     C                   Z-ADD     EORCN         DARCN#
     C                   Z-ADD     CADSEQ        DADSEQ
     C                   Z-ADD     CARDAT        DATRDT
     C*
     C                   MOVE      U2CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DACHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        DACHRG
     C                   ENDIF
     C*
     C                   MOVE      U2MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DAMRA
     C                   ELSE
     C                   Z-ADD     AMOUNT        DAMRA
     C                   ENDIF
     C*
     C     U2OTH2        IFNE      *BLANKS
     C                   MOVE      U2OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DAOTH2
     C                   ELSE
     C                   Z-ADD     AMOUNT        DAOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U2NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DANDIS
     C                   ELSE
     C                   Z-ADD     AMOUNT        DANDIS
     C                   ENDIF
     C*
     C                   MOVE      U2PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DAPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        DAPAID
     C                   ENDIF
      /Free
          If u2tax = *blanks;
             u2tax = *zeros;
          Endif;
          If U2tax <> *blanks;
          Endif;
      /End-Free
     C*
     C*
     C                   MOVEL     'A'           DASTAT
     C                   MOVEL(P)  CAUSER        DAUSER
     C                   MOVEL(P)  CAPGM         DAPGM
     C                   Z-ADD     CATIME        DATIME
     C                   Z-ADD     CADT          DADT
     C                   MOVEL(P)  CAUSER        DAIUSR
     C                   MOVEL(P)  CAPGM         DAIPGM
     C                   Z-ADD     CAITIM        DAITIM
     C                   Z-ADD     CAIDT         DAIDT
     C*
     C                   WRITE     PDA01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDDP - Write FDDDP record
     C*================================================================
     C     WRTDDP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDD01
     C                   Z-ADD     CACO#         DDCO#
     C                   Z-ADD     CASUB#        DDSUB#
     C                   Z-ADD     CAFND         DDFND
     C                   Z-ADD     CAFYR         DDFYR
     C                   Z-ADD     CACASE        DDCASE
     C                   MOVEL     CASS#         DDSS#
     C                   Z-ADD     CAACDT        DDACDT
     C                   Z-ADD     CAEOB#        DDEOB#
     C                   Z-ADD     CADSEQ        DDDSEQ
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DDUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DDUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DDunts = U2unts/100
     C                   endif
     C                   ENDIF
     C*
     C                   MOVEL     U2CODE        DDPRCD
     C                   MOVEL     U2CDSC        DDPRDS
     C                   MOVEL     U2CODE        DDBPRC
     C                   MOVEL     U2CDSC        DDBPRD
     C                   MOVEL     U2POS         DDPOS
     C*
     C     C146          IFEQ      *ON
     C     DDPRDS        ANDEQ     *BLANKS
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DDPRCD        KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PRDSC1        DDPRDS
     C                   MOVEL     PRDSC1        DDBPRD
     C*
     C                   ELSE
     C* If not found in above files and state uses R&C file then find
     C* description from FMRCP
     C*
     C     ##RC          IFNE      'N'
     C*
     C                   MOVEL     CAZIPA        KKZIPA
     C*
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        DDPRDS
     C                   MOVEL(P)  RCDSC1        DDBPRD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C     DDPOS         IFEQ      *BLANKS
     C     DDPOS         OREQ      '00'
     C                   MOVEL     '11 '         DDPOS
     C                   ENDIF
     C*
     C                   MOVE      'C'           DDRCFL
     C*
     C                   MOVE      U2DXR         XXDXR
     C*
     C     DX1           IFEQ      '1'
     C     DX1           OREQ      'A'
     C     DX2           OREQ      '1'
     C     DX2           OREQ      'A'
     C     DX3           OREQ      '1'
     C     DX3           OREQ      'A'
     C     DX4           OREQ      '1'
     C     DX4           OREQ      'A'
     C                   MOVEL     'X'           DDDXR1
     C                   ENDIF
     C*
     C     DX1           IFEQ      '2'
     C     DX1           OREQ      'B'
     C     DX2           OREQ      '2'
     C     DX2           OREQ      'B'
     C     DX3           OREQ      '2'
     C     DX3           OREQ      'B'
     C     DX4           OREQ      '2'
     C     DX4           OREQ      'B'
     C                   MOVEL     'X'           DDDXR2
     C                   ENDIF
     C*
     C     DX1           IFEQ      '3'
     C     DX1           OREQ      'C'
     C     DX2           OREQ      '3'
     C     DX2           OREQ      'C'
     C     DX3           OREQ      '3'
     C     DX3           OREQ      'C'
     C     DX4           OREQ      '3'
     C     DX4           OREQ      'C'
     C                   MOVEL     'X'           DDDXR3
     C                   ENDIF
     C*
     C     DX1           IFEQ      '4'
     C     DX1           OREQ      'D'
     C     DX2           OREQ      '4'
     C     DX2           OREQ      'D'
     C     DX3           OREQ      '4'
     C     DX3           OREQ      'D'
     C     DX4           OREQ      '4'
     C     DX4           OREQ      'D'
     C                   MOVEL     'X'           DDDXR4
     C                   ENDIF
     C*
     C                   SELECT
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA1        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR1
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA2        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR2
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA3        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR3
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA4        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR4
     C                   ENDSL
     C                   MOVEL(P)  U2TOO#        DDPART
     C                   MOVEL(P)  U2BPRC        DDBPRC
     C                   If        U2mod = *blanks
     C                   MOVEL(P)  U2BMD1        DDMOD1
     C                   Else
     C                   Eval      DDmod1 = U2mod
     C*
     C                   Endif
     C*
     C                   MOVEL     'A'           DDSTAT
     C                   MOVEL(P)  CAUSER        DDUSER
     C                   MOVEL(P)  CAPGM         DDPGM
     C                   Z-ADD     CATIME        DDTIME
     C                   Z-ADD     CADT          DDDT
     C                   MOVEL(P)  CAUSER        DDIUSR
     C                   MOVEL(P)  CAPGM         DDIPGM
     C                   Z-ADD     CAITIM        DDITIM
     C                   Z-ADD     CAIDT         DDIDT
     C*
     C                   WRITE     PDD01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDEP - Write FDDEP record
     C*================================================================
     C     WRTDEP        BEGSR
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY2         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             HDSEQ#
     C                   ELSE
     C                   Z-ADD     1             HDSEQ#
     C                   ENDIF
     C*
     C* This code checks for EOB Codes/Notes in the expanded fields
     C*
     C                   SETOFF                                       71
     C     U2EOB1        IFNE      *BLANKS
     C     U2EOD1        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB1        XXEOB             3
      /Free
                         If %Subst(U2eob1:4:1) <> ' ' and
                           %Subst(U2eob1:2:1) <> ' ';
                            xxEob = %Subst(U2eob1:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod1 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD1:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD1:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD1:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB2        IFNE      *BLANKS
     C     U2EOD2        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB2        XXEOB
      /Free
                         If %Subst(U2eob2:4:1) <> ' ' and
                           %Subst(U2eob2:2:1) <> ' ';
                            xxEob = %Subst(U2eob2:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod2 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD2:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD2:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD2:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB3        IFNE      *BLANKS
     C     U2EOD3        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB3        XXEOB
      /Free
                         If %Subst(U2eob3:4:1) <> ' ' and
                           %Subst(U2eob3:2:1) <> ' ';
                            xxEob = %Subst(U2eob3:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod3 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD3:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD3:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD3:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF
     C*
     C     U2EOB4        IFNE      *BLANKS
     C     U2EOD4        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB4        XXEOB
      /Free
                         If %Subst(U2eob4:4:1) <> ' ' and
                           %Subst(U2eob4:2:1) <> ' ';
                            xxEob = %Subst(U2eob4:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod4 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD4:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD4:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD4:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF
     C*
     C*
     C* Use the old fields if the new fields are not populated
     C*
     C     *IN71         IFEQ      *OFF
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOBD        ORNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOB         CAT(P)    '-':1         DENOT1
     C                   CAT       U2EOBD:1      DENOT1
     C                   ELSE
     C                   MOVEL(P)  U2EOBD        DENOT1
     C                   ENDIF
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C                   ENDIF                                                  IN71=OFF
     C*
     C     U2REAS        IFNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   MOVEL(P)  U2REAS        DENOT1
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDHP - Write FDDHP record
     C*================================================================
     C     WRTDHP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDH01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDH01
     C                   Z-ADD     CACO#         DHCO#
     C                   Z-ADD     CASUB#        DHSUB#
     C                   Z-ADD     CAFND         DHFND
     C                   Z-ADD     CAFYR         DHFYR
     C                   Z-ADD     CACASE        DHCASE
     C                   MOVEL     CASS#         DHSS#
     C                   Z-ADD     CAACDT        DHACDT
     C                   Z-ADD     CAEOB#        DHEOB#
     C                   Z-ADD     CADSEQ        DHDSEQ
     C*
     C     U2CODE        IFEQ      *BLANKS
     C                   MOVEL     '96370'       DHPRCD
     C                   MOVEL(P)  CODEDS        DHPRDS
     C                   MOVEL     '96370'       DHBPRC
     C                   MOVEL(P)  CODEDS        DHBPRD
     C                   ELSE
     C                   MOVEL     U2CODE        DHPRCD
     C                   MOVEL     U2CDSC        DHPRDS
     C                   MOVEL     U2CODE        DHBPRC
     C                   MOVEL     U2CDSC        DHBPRD
     C     C146          IFEQ      *ON
     C     DHPRDS        ANDEQ     *BLANKS
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DHPRCD        KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PRDSC1        DHPRDS
     C                   MOVEL     PRDSC1        DHBPRD
     C*
     C                   ELSE
     C* If not found in above files and state uses R&C file then find
     C* description from FMRCP
     C*
     C     ##RC          IFNE      'N'
     C                   MOVEL     CAZIPA        KKZIPA
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        DHPRDS
     C                   MOVEL(P)  RCDSC1        DHBPRD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U2POS         DHPOS
     C*
     C     DHPOS         IFEQ      *BLANKS
     C     DHPOS         OREQ      '00'
     C                   MOVEL     ' 11'         DHPOS
     C                   ENDIF
     C*
     C                   MOVEL     U2MOD         DHMOD1
     C                   MOVE      'C'           DHRCFL
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DHUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DHUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DHunts = U2unts/100
     C                   endif
     C                   ENDIF
     C*
     C                   MOVE      U2DXR         XXDXR
     C*
     C     DX1           IFEQ      '1'
     C     DX1           OREQ      'A'
     C     DX2           OREQ      '1'
     C     DX2           OREQ      'A'
     C     DX3           OREQ      '1'
     C     DX3           OREQ      'A'
     C     DX4           OREQ      '1'
     C     DX4           OREQ      'A'
     C                   MOVEL     'X'           DHDXR1
     C                   ENDIF
     C*
     C     DX1           IFEQ      '2'
     C     DX1           OREQ      'B'
     C     DX2           OREQ      '2'
     C     DX2           OREQ      'B'
     C     DX3           OREQ      '2'
     C     DX3           OREQ      'B'
     C     DX4           OREQ      '2'
     C     DX4           OREQ      'B'
     C                   MOVEL     'X'           DHDXR2
     C                   ENDIF
     C*
     C     DX1           IFEQ      '3'
     C     DX1           OREQ      'C'
     C     DX2           OREQ      '3'
     C     DX2           OREQ      'C'
     C     DX3           OREQ      '3'
     C     DX3           OREQ      'C'
     C     DX4           OREQ      '3'
     C     DX4           OREQ      'C'
     C                   MOVEL     'X'           DHDXR3
     C                   ENDIF
     C*
     C     DX1           IFEQ      '4'
     C     DX1           OREQ      'D'
     C     DX2           OREQ      '4'
     C     DX2           OREQ      'D'
     C     DX3           OREQ      '4'
     C     DX3           OREQ      'D'
     C     DX4           OREQ      '4'
     C     DX4           OREQ      'D'
     C                   MOVEL     'X'           DHDXR4
     C                   ENDIF
     C*
     C                   SELECT
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA1        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR1
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA2        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR2
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA3        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR3
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA4        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR4
     C                   ENDSL
     C                   MOVEL(P)  U2MOD2        DHMOD2
     C                   MOVEL(P)  U2MOD3        DHMOD3
     C                   MOVEL(P)  U2MOD4        DHMOD4
     C                   MOVEL(P)  U2BLFC        DHBLFC
     C                   MOVEL(P)  U2DUCD        DHDUCD
     C                   MOVEL(P)  U2BPRC        DHBPRC
     C                   If        DHmod1 = *blanks
     C                   MOVEL(P)  U2BMD1        DHMOD1
     C                   Endif
     C*
     C                   Eval      DHbMd1 = U2bMd1
     C*
     C                   MOVEL     'A'           DHSTAT
     C                   MOVEL(P)  CAUSER        DHUSER
     C                   MOVEL(P)  CAPGM         DHPGM
     C                   Z-ADD     CATIME        DHTIME
     C                   Z-ADD     CADT          DHDT
     C                   MOVEL(P)  CAUSER        DHIUSR
     C                   MOVEL(P)  CAPGM         DHIPGM
     C                   Z-ADD     CAITIM        DHITIM
     C                   Z-ADD     CAIDT         DHIDT
      /Free
          DHnpir = U2rnpi;
          DHrnlc = U2rnlc;
      /End-Free
     C*
     C     u1btyp        IfEQ      'M'
     C     ##CLT#        AndEQ     '129'
     C     eoast         AndEQ     'NY'
     C     u2code        AndGE     '70000'
     C     u2code        AndLE     '79999'
     C                   Eval      Dharea = 'R'
     C                   EndIf
      /Free
         If C133 = '1'  and EOAST = 'LA' and U2CODE = *blanks and
         U2NDC <> *blanks ;
            DHPRCD = *blanks ;
            DHBPRC = *blanks ;
            DHNDC  = U2NDC ;
            DHNDCD = U2CDSC ;
         Endif ;
      /End-Free
     C                   WRITE     PDH01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDPP - Write FDDPP record
     C*================================================================
     C     WRTDPP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDP01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDP01
     C                   Z-ADD     CACO#         DPCO#
     C                   Z-ADD     CASUB#        DPSUB#
     C                   Z-ADD     CAFND         DPFND
     C                   Z-ADD     CAFYR         DPFYR
     C                   Z-ADD     CACASE        DPCASE
     C                   MOVEL     CASS#         DPSS#
     C                   Z-ADD     CAACDT        DPACDT
     C                   Z-ADD     CAEOB#        DPEOB#
     C                   Z-ADD     CADSEQ        DPDSEQ
     C                   MOVEL(P)  U2NDC         DPNDC
     C                   MOVEL(P)  U2RX#         DPRX#
     C                   MOVEL(P)  DPNDC         DPBNDC
     C                   Z-ADD     U2UNTS        XXQTY             9 0
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      XXQTY = U2unts/100
     C                   endif
     C                   Z-ADD     XXQTY         DPRXMQ
     C                   MOVEL(P)  U2CDSC        DPDESC
     C*
     C                   Z-ADD     U2SVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         DPRXFD
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DPUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DPUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DPunts = U2unts/100
     C                   endif
     C                   ENDIF
     C                   MOVEL(P)  U2RXPT        DPRXPT
     C*
     C     EOBTYP        IFEQ      'X'
     C     DPRXPT        ANDEQ     'S'
     C                   MOVEL(P)  U2PRIN        DPPRIN
     C                   MOVEL(P)  U2CODE        DPHCPC
     C                   ENDIF
     C*
     C                   MOVEL(P)  U2NWRF        DPNWRF
     C                   MOVEL(P)  U2CERT        DPCERT
     C                   MOVEL(P)  U2LIC#        DPLIC#
     C*
     C*
     C                   MOVEL(P)  U2NDCD        DPDESC
     C                   MOVEL(P)  U2RXDY        DPRXDY
     C*
     C*
     C                   MOVE      U2DFEE        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DPDFEE
     C                   ELSE
     C                   Z-ADD     AMOUNT        DPDFEE
     C                   ENDIF
     C*
     C*
     C                   MOVEL(P)  U2RXDI        DPRXDI
     C                   MOVEL(P)  U2BCDT        DPBCDT
     C                   MOVEL(P)  U2BNDC        DPBNDC
     C*
     C                   MOVEL     'A'           DPSTAT
     C                   MOVEL(P)  CAUSER        DPUSER
     C                   MOVEL(P)  CAPGM         DPPGM
     C                   Z-ADD     CATIME        DPTIME
     C                   Z-ADD     CADT          DPDT
     C                   MOVEL(P)  CAUSER        DPIUSR
     C                   MOVEL(P)  CAPGM         DPIPGM
     C                   Z-ADD     CAITIM        DPITIM
     C                   Z-ADD     CAIDT         DPIDT
     C*
     C                   WRITE     PDP01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDTL - Write Detail Records
     C*================================================================
     C     WRTDTL        BEGSR
     C*
     C* Initialize XXTAX
     C                   MOVE      *ZEROS        XXTAX
     C     *IN92         IFEQ      *OFF
     C     U2KY1         CHAIN     PU201                              99
     C     *IN99         DOWEQ     *OFF
     C     U2RLIN        IFEQ      *ZEROS
     C                   EXSR      CHKAM2
     C*
     C                   EXSR      WRTCAP                                       FDCAP
     C                   EXSR      UPDT2A
     C                   EXSR      WRTDAP                                       FDDAP
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      WRTDDP                                       FADDP
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      WRTDUP                                       FADUP
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      WRTDHP                                       FADHP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      WRTDPP                                       FADHP
     C                   ENDSL
     C     U2EDS1        IFNE      *Blanks
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS1        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB1        XXEOB             3
       If %Subst(xxEob:3:1) = ' ';
     C     ' '           CAT       XXEOB         XXEOB
       Endif;
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS2        XXEDS
       If %Subst(xxEob:3:1) = ' ';
     C     ' '           CAT       XXEOB         XXEOB
       Endif;
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB2        XXEOB             3
       If %Subst(xxEob:3:1) = ' ';
     C     ' '           CAT       XXEOB         XXEOB
       Endif;
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS3        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB3        XXEOB             3
       If %Subst(xxEob:3:1) = ' ';
     C     ' '           CAT       XXEOB         XXEOB
       Endif;
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS4        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB4        XXEOB             3
       If %Subst(xxEob:3:1) = ' ';
     C     ' '           CAT       XXEOB         XXEOB
       Endif;
     C                   EXSR      WRTEDS                                       FADEP
     C*
     C                   ELSE                                                   C139 = *ON
     C                   EXSR      WRTDEP                                       FADEP
     C*
     C                   ENDIF                                                  C139 = *ON
     C*
     C                   EXSR      UPDT2B
     C*
     C                   ENDIF
     C     U2KY1         READE     PU201                                  99
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C     U2KY1         CHAIN     PU201                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     U2RLIN        IFNE      *ZEROS
     C                   EXSR      UPDCAP                                       FDCAP
     C                   ENDIF
     C                   EXSR      UPDT2A
     C     U2RLIN        IFNE      *ZEROS
     C                   EXSR      WRTDAP                                       FDDAP
     C     U2EDS1        IFNE      *Blanks
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS1        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB1        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS2        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB2        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS3        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB3        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS4        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB4        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C*
     C                   ELSE                                                   C139 = *ON
     C                   EXSR      WRTDEP                                       FDDEP
     C*
     C                   ENDIF                                                  C139 = *ON
     C                   ENDIF
     C                   EXSR      UPDT2B
     C*
     C     U2KY1         READE     PU201                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDUP - Write FDDUP record
     C*================================================================
     C     WRTDUP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDU01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDU01
     C                   Z-ADD     CACO#         DUCO#
     C                   Z-ADD     CASUB#        DUSUB#
     C                   Z-ADD     CAFND         DUFND
     C                   Z-ADD     CAFYR         DUFYR
     C                   Z-ADD     CACASE        DUCASE
     C                   MOVEL     CASS#         DUSS#
     C                   Z-ADD     CAACDT        DUACDT
     C                   Z-ADD     CAEOB#        DUEOB#
     C                   Z-ADD     CADSEQ        DUDSEQ
     C*
     C                   MOVEL     KKRVCD        DURVCD
     C                   MOVEL     KKHCPC        DURATE
     C*
     C                   MOVE      U2POS         DUPOS
     C*
     C     DUPOS         IFEQ      *BLANKS
     C     DUPOS         OREQ      '00'
     C                   MOVEL     ' 21'         DUPOS
     C                   ENDIF
     C*
     C                   Z-ADD     U2UNTS        DUUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DUunts = U2unts/100
     C                   endif
     C                   MOVE      DURVCD        DUBPRC
     C                   MOVE      DURATE        DUBRAT
     C                   MOVEL(P)  U2CDSC        DUDESC
     C                   MOVEL(P)  DUDESC        DUBPRD
     C                   MOVEL(P)  U2DUCD        DUDUCD
     C                   MOVEL(P)  U2BPRC        DUBRAT
     C                   MOVEL(P)  U2BMD1        DUPMOD
     C*
     C                   MOVEL(P)  U2MOD         DUPMOD
     C                   MOVEL(P)  U2MOD2        DUMOD2
     C                   MOVEL(P)  U2PSCD        DUPSCD
     C                   MOVEL(P)  U2CDSC        DUSCDS
     C     DUDESC        IFEQ      *BLANKS
     C     C146          ANDEQ     *ON
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DURVCD        K1RVCD
     C                   MOVEL     DURATE        K1RATE
     C                   MOVE      *ZEROS        K1UNIT
     C*
     C                   SELECT
     C*
     C     DURVCD        WHENNE    *ZEROS
     C     DURATE        ANDEQ     *BLANKS
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY1         READPE    PSV01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     SVDESC        DUDESC
     C                   MOVEL     SVDESC        DUBPRD
     C                   ENDIF
     C*
     C     DURATE        WHENNE    *BLANKS
     C*
     C                   MOVEL     DURATE        KKPRCD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PRDSC1        DUDESC
     C                   MOVEL     PRDSC1        DUBPRD
     C*
     C                   ELSE
     C* If not found in above files and state uses R&C file then find
     C* description from FMRCP
     C*
     C     ##RC          IFNE      'N'
     C*
     C                   MOVEL     CAZIPA        KKZIPA
     C*
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        DUDESC
     C                   MOVEL(P)  RCDSC1        DUBPRD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  DUDESC IFEQ
     C*
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL(P)  CAUSER        DUUSER
     C                   MOVEL(P)  CAPGM         DUPGM
     C                   Z-ADD     CATIME        DUTIME
     C                   Z-ADD     CADT          DUDT
     C                   MOVEL(P)  CAUSER        DUIUSR
     C                   MOVEL(P)  CAPGM         DUIPGM
     C                   Z-ADD     CAITIM        DUITIM
     C                   Z-ADD     CAIDT         DUIDT
     C*
     C                   WRITE     PDU01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTRPP - Write WARPP record
     C*================================================================
     C*
     C     WRTRPP        BEGSR
     C*
b01  C     EAEPFL        IFEQ      ' '
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM      *BLANKS       #1WCF#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*
b02  C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
x02  C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
e02  C                   ENDIF
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKEOBS
     C*
     C     RPKY1         CHAIN     PRP01                              99
     C*
b02  C     *IN99         IFEQ      *ON
     C*
     C     RPKY2         SETGT     PRP02
     C     RPKY2         READPE    PRP02                                  99
     C*
b03  C     *IN99         IFEQ      *OFF
     C     RPSEQ#        ADD       1             XXSEQ#
x03  C                   ELSE
     C                   Z-ADD     1             XXSEQ#
e03  C                   ENDIF
     C*
     C                   CLEAR                   PRP01
     C                   Z-ADD     EOCO#         RPCO#
     C                   Z-ADD     KKWCF#        RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   Z-ADD     XXSEQ#        RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   Z-ADD     EOFEIN        RPFEIN
     C                   Z-ADD     EOMOD         RPMOD
     C                   Z-ADD     EAPAID        RPAMT
     C*
b03  C     ##CLT#        IFEQ      '114'
     C     EONTWK        ANDEQ     'PACE'
     C                   MOVE      *BLANKS       RPPGTP
x03  C                   ELSE
     C*
     C     NRKY          CHAIN     PNR01                              99
     C*
b04  C     *IN99         IFEQ      *OFF
     C                   MOVE      'G'           RPPGTP
e04  C                   ENDIF
     C*
e03  C                   ENDIF
     C*
     C     CMKY4         CHAIN     PCM03                              99
     C                   MOVEL     'P'           KKPTYP
     C                   MOVEL     EOSYS         KKSYS
     C     CIKY          CHAIN     PCI01                              99
     C*
b03  C     *IN99         IFEQ      *OFF
     C*
b04  C     CMCVFL        IFEQ      'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
x04  C                   ELSE
     C*
b05  C     EOBTYP        IFEQ      'X'
     C                   MOVE(P)   CIXBR2        RPBR2
x05  C                   ELSE
     C                   MOVE(P)   CIBR2         RPBR2
e05  C                   ENDIF
     C*
e04  C                   ENDIF
     C*
b04  C     CMLTFL        IFEQ      'I'
     C                   MOVE(P)   CIBR7L        RPBR7
x04  C                   ELSE
     C                   MOVE(P)   CIBR7N        RPBR7
e04  C                   ENDIF
     C*
     C                   MOVEL(P)  CISTS         RPSTS
     C*
x03  C                   ELSE
     C*
b04  C     EOSYS         IFEQ      'P'
     C                   MOVEL(P)  'MD'          RPBR2
x04  C                   ELSE
     C                   MOVEL(P)  'HP'          RPBR2
e04  C                   ENDIF
     C*
b04  C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'LT'          RPBR7
x04  C                   ELSE
     C                   MOVEL(P)  'NL'          RPBR7
e04  C                   ENDIF
     C*
     C                   MOVEL(P)  'PD'          RPSTS
     C*
     C*
b04  C     ##CLT#        IFEQ      '114'
     C                   MOVEL(P)  '01'          RPBR2
     C*
b05  C     EONTWK        IFEQ      'PACE'
     C     EOBTYP        ANDEQ     'X'
     C*
b06  C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'PC'          RPBR7
e06  C                   ENDIF
     C*
b06  C     CMLTFL        IFEQ      'M'
     C                   MOVEL(P)  'PD'          RPBR7
e06  C                   ENDIF
     C*
b06  C     CMCVFL        IFEQ      'Y'
     C                   MOVEL(P)  'EP'          RPTRCD
     C                   MOVEL(P)  'EQ'          RPBR2
     C                   ENDIF
     C*
X05  C                   ELSE
     C*
     C*
B06  C     EONTWK        IFEQ      'CAMJF'
     C*
     C                   MOVEL(P)  'MP'          RPTRCD
     C*
B07  C     EOSYS         IFEQ      'P'
     C*
B08  C     EOBTYP        IFEQ      'X'
     C                   MOVEL(P)  '01'          RPBR2
X08  C                   ELSE
     C                   MOVEL(P)  'MD'          RPBR2
E08  C                   ENDIF
     C*
E07  C                   ENDIF
     C*
B07  C     EOSYS         IFEQ      'H'
     C                   MOVEL(P)  'HP'          RPBR2
E07  C                   ENDIF
     C*
B07  C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'LT'          RPBR7
X07  C                   ELSE
     C                   MOVEL(P)  'NL'          RPBR7
E07  C                   ENDIF
     C*
     C                   MOVEL(P)  'PD'          RPSTS
     C*
E06  C                   ENDIF
     C*
E05  C                   ENDIF
     C*
e04  C                   ENDIF
     C*
e03  C                   ENDIF
     C*
     C/free

        If %open (WTABL) and U1PV1 <> *blanks;
           // verify if payment value is valid
           kkkey = U1PV1;
           Chain tbky pb201;
            if %found(wtabl);      // file wtb2p
               RPBR2 = U1PV1;
            endif;
        EndIf;

        //
        // Popuate transaction code if passed
        If U1trCd <> ' ';
           // Now let's get the reserve code/type/desc
           RPtrCd = U1trCd;
           kkKey  = RPtrCd;
           Chain TBky ppc01;
           If %Found(wtabl);
              RPreCd = PCreCd;
              RPtype = PCtrTp;
              RPdesc = PCdesc;
           EndIf;

           // Now let's get the reserve type
           kkKey = RPreCd;
           Chain TBky prt01;
           If %Found(wtabl);
              RPreTp = RTtype;
           Else;
              RPreTp = ' ';
           EndIf;
        EndIf;

      /end-free
     C* CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
     C*
b03  C     C132          IFEQ      *ON
     C*
b04  C     RPAMT         IFGT      250
     C     CMDNY         OREQ      'Y'
     C     CMSTAT        OREQ      'C'
     C                   MOVEL(P)  'PR'          RPSTS
x04  C                   ELSE
     C                   MOVE      *ON           RELEAS
e04  C                   ENDIF
     C*
e03  C                   ENDIF
     C*
     C                   MOVEL(P)  CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C*
b03  C     ##CLT#        IFEQ      '114'
     C     EONTWK        ANDEQ     'PACE'
     C     EOBTYP        ANDEQ     'X'
     C     CMCVFL        ANDEQ     'Y'
     C                   MOVEL(P)  'EP'          RPTRCD
x03  C                   ELSE
     C*
     C                   MOVEL(P)  'MP'          RPTRCD
e03  C                   ENDIF
     C                   MOVEL(P)  'M'           RPRETP
     C*
     C                   MOVEL(P)  RPTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
b03  C     *IN99         IFEQ      *OFF
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL(P)  PCDESC        RPDESC
e03  C                   ENDIF
     C                   If        C090 = *On  and U1acct <> ' '
     C                   Movel(P)  U1acct        RPdesc
     C                   Endif
     C*
     C                   MOVEL(P)  RPRECD        KKKEY
     C     TBKY          CHAIN     PRT01                              60
b03  C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
x03  C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
e03  C                   ENDIF
     C*
     C                   MOVEL(P)  EOPNAM        RPPNAM
     C                   MOVEL(P)  EOADR1        RPADR1
     C                   MOVEL(P)  EOADR2        RPADR2
     C                   MOVEL(P)  EOCTY         RPCTY
     C                   MOVEL(P)  EOST          RPST
     C                   Z-ADD     EOZIP         RPZIP
     C                   MOVEL     'P'           RPSTAT
     C                   MOVEL(P)  EOUSER        RPUSER
     C                   MOVEL(P)  EOPGM         RPPGM
     C                   Z-ADD     EOTIME        RPTIME
     C                   Z-ADD     EODT          RPDT
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   Z-ADD     EORCN         RPEOBS
     C                   Z-ADD     EODT          RPRCVD
     C*
     C     TRANS         CAT(P)    U1TRID:1      RPRMDS
     C*
     C                   MOVEL(P)  RPSTS         KKKEY
     C     TBKY          CHAIN     PPS01                              99
b03  C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   PSSRT         RPSORT
e03  C                   ENDIF
     C*
     C                   MOVEL(P)  'N'           RPSENT
     C                   MOVEL(P)  USER          RPRQST
     C*
     C*  Call API to determine the Bank "Code".
     C*
     C                   MOVE      RPCO#         $$CO#
     C                   MOVE      RPFND         $$FND
     C                   MOVE      RPFYR         $$FYR
     C                   MOVE      RPCASE        $$CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
      /Free
           QXseq# = RPcmsq;
           QXeob# = %Char(RPeob#);
           QXfein = RPfein;
           QXmod  = RPmod ;
           QXdsq# = RPdsq#;
      /End-Free
     C*
     C                   CALL      'WA023R1'
     C                   PARM      '2'           $$MODE            1            Mode
     C                   PARM                    $$CO#             3            Company
     C                   PARM                    $$FND             3            Group #
     C                   PARM                    $$FYR             3            Year
     C                   PARM                    $$CASE            7            Case
     C                   PARM      'WC'          $$APPL            2            Applic Code
     C                   PARM      DATA          $$DATA           75            Selection criteria
     C                   PARM      *BLANKS       $$CKTP           10            Bank Acct Code
     C                   PARM      *BLANKS       $$RTCD            1            Return Code
     C*** Beg Add *** 01/04/24 ************************************************
     C                   PARM      *BLANKS       inEfTc            1            EFT Return Value
       RPefTc = inEfTc;
       // If the entity is not setup for EFT then blank out EFT flag
       If RPefTc = 'E';
          inPtype = 'P';
          inEkey = %Editc(RPfein:'X') + %Editc(RPmod:'X');
          inChrDate = %Editc(EAiDt:'X');
          inPre = *off;
          inEobType = *blanks;
          inCkTp = $$CkTp;
          inCo# = $$co#;
          inFnd = $$fnd;
          If ##WCIF = 'Y';
             isEft = GetEFTStatus(inCo#:inPtype:inFnd:inEkey:
              inChrDate:inPre:outRRN:inEobType:inCkTp);
          Endif;
          If isEft = *off;
             RPefTc = ' ';
          Endif;
       Endif;
     C*** End Add *** 01/04/24 ************************************************
     C*
     C                   MOVEL(P)  $$CKTP        RPCKTP                         Bank Acct# Code
     C*
     C                   MOVEL(P)  EAIUSR        RPIUSR
     C                   MOVEL(P)  EAPGM         RPIPGM
     C                   Z-ADD     EAITIM        RPITM
     C                   Z-ADD     EAIDT         RPIDT
     C*
b03  C     RPAMT         IFGT      *ZEROS
       If EAeftC = 'V';
          RPeftC = 'V';
       Endif;
     C                   WRITE     PRP01
e03  C                   ENDIF
     C*
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTRPX - Write WARPP record for PACE bills for client 114
     C*================================================================
     C     WRTRPX        BEGSR
     C*
     C     EAEPFL        IFEQ      ' '
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM      *BLANKS       #1WCF#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKEOBS
     C     RPKY1         CHAIN     PRP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     RPMOD         ANDNE     001
     C     RPFEIN        ANDEQ     EOFEIN
     C*
     C     RPKY2         SETGT     PRP02
     C     RPKY2         READPE    PRP02                                  99
     C     *IN99         IFEQ      *OFF
     C     RPSEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PRP01
     C                   Z-ADD     EOCO#         RPCO#
     C                   Z-ADD     KKWCF#        RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   Z-ADD     XXSEQ#        RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   Z-ADD     EOFEIN        RPFEIN
     C                   Z-ADD     001           RPMOD
     C                   MOVEL     'EP'          RPTRCD
     C                   Z-ADD     EANAMT        RPAMT
     C*
     C*
     C     CMKY4         CHAIN     PCM03                              99
     C                   MOVEL     'P'           KKPTYP
     C                   MOVEL     EOSYS         KKSYS
     C     CIKY          CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
     C                   ELSE
     C     EOBTYP        IFEQ      'X'
     C                   MOVE(P)   CIXBR2        RPBR2
     C                   ELSE
     C                   MOVE(P)   CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE(P)   CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE(P)   CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVEL(P)  CISTS         RPSTS
     C*
     C                   ELSE
     C*
     C     EOSYS         IFEQ      'P'
     C                   MOVEL(P)  'MD'          RPBR2
     C                   ELSE
     C                   MOVEL(P)  'HP'          RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'LT'          RPBR7
     C                   ELSE
     C                   MOVEL(P)  'NL'          RPBR7
     C                   ENDIF
     C*
     C                   MOVEL(P)  'PD'          RPSTS
     C*
     C     ##CLT#        IFEQ      '114'
     C                   MOVEL(P)  '98'          RPBR2
     C                   MOVEL(P)  'EX'          RPRECD
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL(P)  'PC'          RPBR7
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'M'
     C                   MOVEL(P)  'PD'          RPBR7
     C                   ENDIF
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVEL(P)  'EP'          RPTRCD
     C                   MOVEL(P)  'EQ'          RPBR2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
     C*
     C                   MOVEL(P)  CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C*
     C     EOBTYP        IFEQ      'X'
     C     CMCVFL        ANDEQ     'Y'
     C                   MOVEL(P)  'EP'          RPTRCD
     C                   ENDIF                                                  ##CLT# = 114
     C*
     C                   MOVEL(P)  'M'           RPRETP
     C*
     C                   MOVEL(P)  RPTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL(P)  PCDESC        RPDESC
     C                   ENDIF                                                  *IN99 = *OFF
     C                   If        C090 = *On and U1acct <> ' '
     C                   Movel(P)  U1acct        RPdesc
     C                   Endif
     C*
     C                   MOVEL(P)  RPRECD        KKKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF                                                  *IN60 = '0'
     C*
     C                   MOVEL(P)  EOPNAM        RPPNAM
     C                   MOVEL(P)  EOADR1        RPADR1
     C                   MOVEL(P)  EOADR2        RPADR2
     C                   MOVEL(P)  EOCTY         RPCTY
     C                   MOVEL(P)  EOST          RPST
     C                   Z-ADD     EOZIP         RPZIP
     C                   MOVEL     'P'           RPSTAT
     C                   MOVEL(P)  EOUSER        RPUSER
     C                   MOVEL(P)  EOPGM         RPPGM
     C                   Z-ADD     EOTIME        RPTIME
     C                   Z-ADD     EODT          RPDT
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   Z-ADD     EORCN         RPEOBS
     C                   Z-ADD     EODT          RPRCVD
     C*
     C     TRANS         CAT(P)    U1TRID:1      RPRMDS
     C*
     C                   MOVEL(P)  RPSTS         KKKEY
     C     TBKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   PSSRT         RPSORT
     C                   ENDIF
     C*
     C                   MOVEL(P)  'N'           RPSENT
     C                   MOVEL(P)  USER          RPRQST
     C*
     C*  Call API to determine the Bank "Code".
     C*
     C                   MOVE      RPCO#         $$CO#
     C                   MOVE      RPFND         $$FND
     C                   MOVE      RPFYR         $$FYR
     C                   MOVE      RPCASE        $$CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C*
     C                   CALL      'WA023R1'
     C                   PARM      '2'           $$MODE            1            Mode
     C                   PARM                    $$CO#             3            Company
     C                   PARM                    $$FND             3            Group #
     C                   PARM                    $$FYR             3            Year
     C                   PARM                    $$CASE            7            Case
     C                   PARM      'WC'          $$APPL            2            Applic Code
     C                   PARM      DATA          $$DATA           75            Selection criteria
     C                   PARM      *BLANKS       $$CKTP           10            Bank Acct Code
     C                   PARM      *BLANKS       $$RTCD            1            Return Code
     C*
     C                   MOVEL(P)  $$CKTP        RPCKTP                         Bank Acct# Code
     C*
     C                   MOVEL(P)  EAIUSR        RPIUSR
     C                   MOVEL(P)  EAPGM         RPIPGM
     C                   Z-ADD     EAITIM        RPITM
     C                   Z-ADD     EAIDT         RPIDT
     C*
     C     RPAMT         IFGT      *ZEROS
     C                   WRITE     PRP01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDRG - Write a detail line with zero charges
     C*================================================================
     C*================================================================
     C* CHKAM1 - Check amounts in upload file and convert to zero if blanks
     C*================================================================
     C     CHKAM1        BEGSR
     C*
     C     U1PAID        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U1PAID
     C                   ENDIF
     C*
     C     U1CHRG        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U1CHRG
     C                   ENDIF
     C*
     C     U1MRA         IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U1MRA
     C                   ENDIF
     C*
     C     U1NDIS        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U1NDIS
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKAM2 - Check amounts in upload file and convert to zero if blanks
     C*================================================================
     C     CHKAM2        BEGSR
     C*
     C     U2PAID        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2PAID
     C                   ENDIF
     C*
     C     U2CHRG        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2CHRG
     C                   ENDIF
     C*
     C     U2MRA         IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2MRA
     C                   ENDIF
     C*
     C     U2NDIS        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2NDIS
     C                   ENDIF
     C*
     C     U2DFEE        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2DFEE
     C                   ENDIF
     C*
     C                   IF        %trim(U2dfee) = *zeros
     C                   MOVE      *ALL'0'       U2DFEE
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEDS - Write FDDEP record
     C*================================================================
     C     WRTEDS        BEGSR
     C*
     C                   MOVEL     ' '           XXPROC            1
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY2         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             HDSEQ#
     C                   ELSE
     C                   Z-ADD     1             HDSEQ#
     C                   ENDIF
     C*
     C* This code checks for EOB Codes/Notes in the expanded fields
     C*
     C                   SETOFF                                       72
     C*** Preparation to populate into denot1
     C                   CLEAR                   XXEDS1
     C                   CLEAR                   XXEDS2
     C                   CLEAR                   XXEDS3
     C                   CLEAR                   XXEDS4
     C*
     C                   EXSR      LODNOT
     C*
     C* Use the old fields if the new fields are not populated
     C*
     C     *IN72         IFEQ      *OFF
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOBD        ORNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOB         CAT(P)    '-':1         DENOT1
     C                   CAT       U2EOBD:1      DENOT1
     C                   ELSE
     C                   MOVEL(P)  U2EOBD        DENOT1
     C                   ENDIF
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C                   ENDIF                                                  IN72=OFF
     C*
     C     U2REAS        IFNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   MOVEL(P)  U2REAS        DENOT1
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
     C     LODNOT        BEGSR
     C*
     C     XXEDS         IFNE      ' '
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     XXEDS:N1      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  XXEDS:1       XXEDS1
     C     N1            ADD       70            N2                3 0
     C     N2            DOWNE     N1
     C     1             SUBST     XXEDS:N2      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N2
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N2            SUB       N1            P2                3 0
     C     P2            SUBST(P)  XXEDS:N1      XXEDS2
     C     N2            ADD       70            N3                3 0
     C     N3            DOWNE     N2
     C     1             SUBST     XXEDS:N3      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N3
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N3            SUB       N2            P3                3 0
     C     P3            SUBST(P)  XXEDS:N2      XXEDS3
     C     N3            ADD       70            N4                3 0
     C     N4            DOWNE     N3
     C     1             SUBST     XXEDS:N3      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N4
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N4            SUB       N3            P4                3 0
     C     P4            SUBST(P)  XXEDS:N3      XXEDS4
     C                   SETON                                        72
     C     XXPROC        IFNE      'H'
     C                   EXSR      WRTDE1
     C                   SETOFF                                       72
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXEDS <>  ' '
     C*
     C                   ENDSR
     C*================================================================
     C* UPDTAX - Update Tax in header files
     C*================================================================
     C     UPDTAX        BEGSR
     C*
     C     EOBKY1        CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C     XXTAX         ANDNE     *ZEROS
     C                   MOVE      'Y'           EPTAX
     C*
     C     EOBKY1        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     XXTAX         EOTAX
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C     EOBKY2        CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     XXTAX         EATAX
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      'N'           EPTAX
     C                   ENDIF
     C*
     C                   UPDATE    PEP01
     C*
     C                   ENDSR
     C*****************************************************************
     C*   SRDART - WRITE DARTS RECORD                                 *
     C*****************************************************************
     C     SRDART        BEGSR
     C*
     C                   MOVEL(P)  'F0350C'      #2DCID
     C*
     C                   Z-ADD     EOCO#         X2CO#
     C                   Z-ADD     EOSUB#        X2SUB#
     C                   Z-ADD     EOFND         X2FND
     C                   Z-ADD     EOFYR         X2FYR
     C                   Z-ADD     EOCASE        X2CASE
     C                   Z-ADD     EOEOB#        X2EOB#
     C*
     C                   MOVEL(P)  KEY2          #2FOLD
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        #2WCIF            1              WC INST
     C                   PARM      *BLANK        #2MCIF            1              MC INST
     C                   PARM      *BLANK        #2WCIN            1              WC INTERFACE
     C                   PARM      *BLANK        #2AR              1              MC AR INST
     C                   PARM      *BLANK        #2NTWK            1              NETWORK INST
     C                   PARM      *BLANK        #2UR              1              PRECERT INST
     C                   PARM      *BLANK        #2MED             1              MEDMGMT INST
     C                   PARM      *BLANK        #2EDI             1              EDI INST
     C                   PARM      *BLANK        #2WCDK            1              WC DARTS KEY
     C*
     C*  WRITE RECORD TO DARTS
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      CMCO#         #2CO#
     C                   MOVE      CMFND         #2FND
     C                   MOVE      CMFYR         #2FYR
     C                   MOVE      CMCASE        #2CASE
     C*
     C                   CALL      'FA024R'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2CASE
     C                   PARM      *BLANKS       #2CMNU            3
     C                   PARM      *BLANKS       #2CLAD            3
     C                   PARM      *BLANKS       #2CMSU            3
     C                   PARM      *BLANKS       #2CLSU            3
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPUSR         #2USER
     C   99              MOVEL(P)  Q1USER        #2USER
     C*
     C*  DETERMINE IF THE GROUP IS LINKED OR STANDALONE.
     C*
     C                   MOVE      CMFND         #2WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       #2RTCD            1
     C                   PARM                    #2CO#             3
     C                   PARM                    #2WCF#            3
     C                   PARM      *BLANKS       #2SUB#            5
     C                   PARM      *BLANKS       #2FND             3
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           #2RTCD            1
     C                   PARM                    #2WCIN            1
     C                   PARM                    #2WCDK            1
     C                   PARM                    #2CO#             3
     C                   PARM                    #2SUB#            5
     C                   PARM                    #2FND             3
     C                   PARM                    #2FYR             3
     C                   PARM                    #2CASE            7
     C                   PARM      *BLANKS       #2KEY            40
     C                   PARM      *BLANKS       #2PNAM           10
     C*
     C                   MOVEL(P)  COMM          #2COMM
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        #2CDAT
     C*
     C* ASSIGN DIARY USERS
     C*
     C                   Movel     @@CMPY        @@VEND            1
     C     @@VEND        IFEQ      'M'
     C                   MOVEL(P)  DI1           DIARY1           10
     C                   MOVEL(P)  DI2           DIARY2           10
     C                   MOVEL(P)  DI3           DIARY3           10
     C                   MOVEL(P)  DI7           DIARY4           10
     C                   ELSE
     C                   MOVEL(P)  DI4           DIARY1           10
     C                   MOVEL(P)  DI5           DIARY2           10
     C                   MOVEL(P)  DI6           DIARY3           10
     C                   ENDIF
     C                   SELECT
     C*
     C     EOFND         WHENEQ    5
     C     EOFND         OREQ      7
     C     EOFND         OREQ      10
     C     EOFND         OREQ      12
     C     EOFND         OREQ      22
     C     EOFND         OREQ      23
     C*
     C                   MOVEL(P)  DIARY1        #2USER           10
     C*
     C     EOFND         WHENEQ    3
     C     EOFND         OREQ      9
     C     EOFND         OREQ      11
     C     EOFND         OREQ      13
     C     EOFND         OREQ      16
     C*
     C                   MOVEL(P)  DIARY2        #2USER           10
     C*
     C     EOFND         WHENEQ    30
     C                   MOVEL(P)  DIARY4        #2USER           10
     C*
     C                   OTHER
     C*
     C                   MOVEL(P)  DIARY3        #2USER           10
     C*
     C                   ENDSL
     C*
     C*  ESTABLISH RECORD
     C*
     C                   CALL      'DA700R'
     C                   PARM                    #2KEY            40
     C                   PARM                    #2COMM          256
     C                   PARM                    #2CDAT            8
     C                   PARM                    #2DCID           12
     C                   PARM      'W0070R'      #2PNAM           10
     C                   PARM      '000'         #2SECL            3
     C                   PARM                    #2USER           10
     C                   PARM      Q1PGM         #2PGM            10
     C                   PARM                    #2FOLD           80
     C                   PARM      'Y'           #2DIFL            1
     C                   PARM      Q1PGM         #2IPGM           10
     C                   PARM      'Y'           #2FL01            1
     C                   PARM      'Y'           #2FL02            1
     C                   PARM      'BRUPL'       #QRMCD            5
     C                   PARM      '00000'       #QSEQ#            5
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* GETAST129 - GET THE ADJUDICATION STATE FOR CLIENT 129
     C*================================================================
     C     GETAST129     BEGSR
     C*
     C                   MOVEL(P)  'UC'          XXAST
     C     CMAST         IFNE      *BLANKS
     C                   MOVE(P)   CMAST         XXAST
     C                   ENDIF
     C     CMFND         IFEQ      1
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C     P1ST          IFNE      *BLANKS
     C                   MOVEL(P)  P1ST          XXAST
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     P*================================================================
     P* PGMSTR - Start of program processing
     P*================================================================
     PpgmStr           b
     DpgmStr           PI
      /Free
            rtvSettings(@@Cchg:@@Cifs:@@Csts:@@Cdny:@@Ccvfl:@@Cgrp:
                         @@Cemp:@@Eifs:@@Eagt:@@Eemp:@@Fifs:@@Ftyp:
                         @@Udup:@@Udrc:@@Urel:@@Upcl:@@Updn:@@Udpn:
                         @@ClDays:@@IncZro:@@Crcd:@@CrDth:@@RtCd);
                         if @@RtCd = '1';
                           isSettings = '1';
                         Else;
                           isSettings = '0';
                         Endif;
      /End-Free
     PpgmStr           e
      //===========================================================
      // SetCodeType - Set CArcFl and CAsrc based on State flag
      //===========================================================
     p SetCodeType     b
     d SetCodeType     pi
      /Free
       kkSt = EOast;
       Chain kkSt pst01;
       If %Found(ststl);
          xxMfs = STmfs;
       Else;
          xxMfs = ' ';
       Endif;
       Select;
       When EOast = 'TX' and (xxMfs = 'B' or xxMfs = 'Y');
          CArcFl = 'M';
          kkPrCd = %Subst(U2code:1:5);
          kkFdat = CAfDat;
          //
          // Set CAsrc based on the file where the code is found
          //
          // Ambulance
          //
          xxFound = 'N';
          SetGt (kkPrCd:kkFdat) pam01;
          Readpe kkPrcd pam01;
          Dow not %Eof(fmaml2);
             If AMstat = 'I';
                Readpe kkPrcd pam01;
             Else;
                CAsrc = 'A';
                xxFound = 'Y';
                Leave;
             Endif;
          Enddo;
          If xxFound = 'N';
          //
          // Drug Fee
          //
             SetGt (kkPrCd:kkFdat) pdf01;
             Readpe kkPrcd pdf01;
             Dow not %Eof(fmdfl1);
                If DFstat = 'I';
                   Readpe kkPrcd pdf01;
                Else;
                   CAsrc = 'F';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // DME
          //
          If xxFound = 'N';
             SetGt (kkCo#:kkPrCd:kkFdat) pdm01;
             Readpe (kkCo#:kkPrcd) pdm01;
             Dow not %Eof(fmdml);
                If DMstat = 'I';
                   Readpe (kkCo#:kkPrcd) pdm01;
                Else;
                   CAsrc = 'D';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // Medicaid DME
          //
          If xxFound = 'N';
             SetGt (kkPrCd:kkFdat) pme01;
             Readpe (kkPrcd) pme01;
             Dow not %Eof(ftmel2tx);
                If MEstat = 'I';
                   Readpe kkPrcd pme01;
                Else;
                   CAsrc = 'M';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // RVU
          //
          If xxFound = 'N';
             SetGt (kkPrCd:kkFdat) prv01;
             Readpe (kkPrcd) prv01;
             Dow not %Eof(ftrvl3);
                If RVstat = 'I';
                   Readpe kkPrcd prv01;
                Else;
                   CAsrc = 'R';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // Clinical Lab
          //
          If xxFound = 'N';
             SetGt (kkPrCd:kkFdat) plb01;
             Readpe (kkPrcd) plb01;
             Dow not %Eof(ftlbl2);
                If LBstat = 'I';
                   Readpe kkPrcd plb01;
                Else;
                   CAsrc = 'L';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
       Other;
       Endsl;
      /End-Free
     p SetCodeType     e
**  (QCMDEX)
OVRDBF FILE(FMPRL)   TOFILE(*LIBL/FMPRL  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
DLTOVR FILE(FMPRL)
DLTOVR FILE(FMSVL1)
