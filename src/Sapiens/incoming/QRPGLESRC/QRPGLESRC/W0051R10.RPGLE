     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt:*SHOWCPY)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R10 - EMPLOYER PREMIUM INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO VIEW         *
     H*                THE EMPLOYER PREMIUM INFORMATION.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R   - WORK WITH EMPLOYER POLICY YEARS                 *
     H*    W0090R   - REVISED BILLING                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/03/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/30/12 C:136 SUPPORT: WA025 80122                           *
     H*                ACCOUNT FOR LARGE DED FILE OVER-RIDES IN       *
     H*                REVISED BILLING                                *
     H*                                                               *
     H* 07/13/11 C:061 PROJECT WC2037                                 *
     H*                Print "Unearned Premium Surcharge" value.      *
     H*                                                               *
     H* 03/16/11 C:190 PROJECT: 132WPI                                *
     H*                Print the new optional heading field for the
     H*                contractor's credit line item if it is not     *
     H*                blank((client 132 only).                       *
     H*                                                               *
     H* 01/28/11 C:183 SEQ#: 75586                                    *
     H*                Refresh the Descriptions of payroll.           *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*               -Add new line item for HCN credit for GIC.      *
     H*                This credit will be applied just before subject*
     H*                premium.                                       *
     H*          C:33 -Add an Fkey for HCN program.                   *
     H*                Function keys will be modified to be flexible. *
     H*                                                               *
     H* 10/05/10  C:189 PROJECT WC1776                                *
     H*                 Modify the screeN to show decimal places (2)  *
     H*                 for payroll amounts                           *
     H*                                                               *
     H* 07/09/10 C:190 SEQ#: 73385                                    *
     H*                SHOW ONLY ACTIVE PAYROLLS.                     *
     H*                                                               *
     H* 06/21/10 C:255 Project: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM TO RECOGNIZE FIELDS FROM*
     H*                FILE WDELP,ADDED OF 2ND PERIOD DEVIATION FOR   *
     H*                SPLIT DEVIATIONS.                              *
     H*                                                               *
     H* 03/12/10 C:65  PROJECT: WC1838                                *
     H*                SHIFT THE LARGE DEDUCTIBLE APPLICATION TO BE   *
     H*                LAST ELEMENT AT EACH PREMIUM LEVEL.            *
     H*                                                               *
     H* 02/25/10 C:65  PROJECT: 25WRC                                 *
     H*                CORRECT THE BILLING RETROFIT TO ENSURE 2ND     *
     H*                DEVIATION LISTS FOR TASB POLICIES.             *
     H*                                                               *
     H* 02/05/10 C:65  PROJECT 25WRB                                  *
     H*                Add field "ACCUMULATED CLAIMS LIABILITY TO     *
     H*                LOSSES WINDOW.                                 *
     H*                                                               *
     H* 01/14/10 C:191 PROJECT: 25WRC                                 *
     H*                BILLING ALGORITHM RETROFIT                     *
     H*                                                               *
     H* 01/04/10 C:190 PROJECT 25WRE                                  *
     H*                Add field "Total # claims".                    *
     H*                                                               *
     H* 12/30/09 C:232 PROJECT: WC1838                                *
     H*                MODIFIED FOR THE LARGE DEDUCTIBLE PROCESS.     *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use spec. fld for owners/off, subcont, etc.    *
     H*
     H* 03/02/09 C:65  PROJECT: 81WEI                                 *
     H*                Use optional headings for terrorism charges.   *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*
     H* 08/16/07 C:61  PROJECT: 132WEW                                *
     H*                Do not over write "Harbor Exposure Fee".       *
     H*                                                               *
     H* 04/13/07 C:65  PROJECT: 114WMF/135WJD/WC1566                  *
     H*                CORRECT PRINING OF ESTIMATED ANNUAL PREMIUM.   *
     H*                                                               *
     H* 04/06/07 C:65  PROJECT: WC1514                                *
     H*                APPLY SHORT RATE PENALTY BASED ON STAT FLAG.   *
     H*                                                               *
     H* 04/02/07 C:132 PROJECT: 45WFN                                 *
     H*                Back to Base on Audits                         *
     H*                - ADD Code from 45WBT for Prompt Pay Discount  *
     H*                  to print correctly.                          *
     H*                                                               *
     H* 08/23/06 C:180 PROJECT:114WMF
     H*               REF 114WBX
     H*               IF AN EMPLOYER HAS THE PRORATA FACTOR OVER-     *
     H*               RIDDEN, PULL EXP CONSTANT DIRECTLY FOR PREM.    *
     H*
     H* 07/06/06 C:168 PROJECT: 132WEW                                *
     H*                ADDED "FACULTATIVE REINSURANCE CHARGE" LINE    *
     H*
     H* 06/19/06 C:61  PROJECT 133WCV                                 *
     H*                Add "Harbor Exposure Fee" below "Total Normal" *
     H*                and above "Expense Constant".                  *
     H*
     H* 04/12/06 C:132 PROJECT: WC1417                                *
     H*                Change "Terrorism Risk Act" in table at end to *
     H*                "Foreign Terrorism", no other changes to code. *
     H*                                                               *
     H* 01/24/06 C:65  PROJECT: 114WHO                                *
     H*                PRINT RETRO ADJUSTMENT AND PREMIUM ON SCREEN.  *
     H*                CONDITION IT AND STATE TAX TO NOT PRINT IF     *
     H*                THERE IS NOT ENOUGH ROOM LEFT.                 *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC CHANGES FOR DEBIT/CREDIT       *
     H*                FACTOR                                         *
     H*                                                               *
     H* 04/07/05        PROJECT: 126WDK                               *
     H*          C:33   PROJECT: 126WCD                               *
     H*                 Multiple Deductible discounts per Year.       *
     H*                                                               *
     H* 01/07/05 C:136  PROJECT WC1250                                *
     H*                 DISPLAY DOMESTIC TERRORISM CHARGE.            *
     H*                                                               *
     H* 09/01/04 C:136  PROJECT WC1237                                *
     H*                 DISPLAY VOLUNTARY LABOR CHARGE IF MIN PREM    *
     H*                 IS NOT INVOKED.                               *
     H*                                                               *
     H* 07/02/04 C:136  PROJECT: WC1227 (REF 109DL)                   *
     H*                 ADD 4TH AND 5TH ASSESSMENTS TO THE FEE        *
     H*                 BILLED WINDOW.                                *
     H*                                                               *
     H* 04/26/04 C:136 PROJECT: WC1169                                *
     H*                ADD PROCESSING FOR OCCUPATIONAL DISEASE        *
     H*                PAYROLL CLASS.                                 *
     H*                                                               *
     H* 01/26/04 C:65  PROJECT: SUPP#41017/WC744                      *
     H*                CHANGE CONDITION FOR PRINTING NON-AUDITED ARAP.*
     H*                                                               *
     H* 01/27/03 C:136 PROJECT: WC744                                 *
     H*                PRINT A LINE ITEM FOR MASS ARAP SURCHARGE.     *
     H*                                                               *
     H* 01/17/03 C:65  PROJECT: WC1120                                *
     H*                ADD TERRORISM RISK ACT TO ANNUAL PREMIUM.      *
     H*                THE AMOUNT IS DETERMINED OFF PAYROLL AND       *
     H*                IS ADDED TO ANNUAL PREMIUM, WHETHER OR NOT     *
     H*                MINIMUM PREMIUM IS INVOKED.                    *
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT WC1099                                  *
     H*               IF A PAYROLL CLASS CODE HAS AN EXTENSION WITH   *
     H*               A STAT CODE AND AN ADDITIONAL LOAD AMOUNT THEN  *
     H*               DO NOT ADD INTO THE ADDITIONAL LOAD TOTAL.      *
     H*                                                               *
     H* 09/20/02 C:65 PROJECT: CM045/#33798                           *
     H*               BILL WAIVER SURCHARGE ON TOP OF MINIMUM PREM.   *
     H*                                                               *
     H* 04/10/02  C:65  PROJECT WC705                                 *
     H*                 ALWAYS USE FLEXIBLE HEADING FOR DEVIATION'S.  *
     H*                                                               *
     H* 02/07/02 C:65 PROJECT WC1067                                  *
     H*               BREAK OUT EXPENSE CONSTANT ON A MIN PREM POLICY *
     H*               IN A/R IF INDICATED.                            *
     H*                                                               *
     H* 11/27/01  C:65  PROJECT WC1059                                *
     H*                 PULL EXPENSE CONSTANT FROM NEW FIELD.         *
     H*                                                               *
     H* 11/20/01  C:65  PROJECT WC1058                                *
     H*                 PULL OPTIONAL HEADINGS FOR FEE WINDOW.        *
     H*                                                               *
     H* 10/04/01  C:65  PROJECT WC1043                                *
     H*                 APPLY CONTRACTORS CREDIT IMMEDIATELY AFTER    *
     H*                 EXPERIENCE MODIFIER, INSTEAD OF JUST BEFORE   *
     H*                 STANDARD PREMIUM.                             *
     H*                                                               *
     H* 10/01/01  C:65  PROJECT WC1045                                *
     H*                 PRINT THE LOSS CONSTANT IF NEEDED.            *
     H*                                                               *
     H* 08/20/01 C:90   PROJECT: WC1041                               *
     H*                 Expanded percentage from 3.2 to 5.3           *
     H*                                                               *
     H* 01/02/01  C:65  PROJECT WC1024                                *
     H*                 ADD 3RD ASSESSMENT TO THE FEE BILLED WINDOW.  *
     H*                                                               *
     H* 11/15/99  C:65  PROJECT WC936                                 *
     H*                 ADD A WINDOW TO DISPLAY FEES BILLED.          *
     H*                                                               *
     H* 10/25/99  C:65  PROJECT WC908                                 *
     H*                 REMOVE UNNECESSARY REFFERENCE TO WDF1P        *
     H*                                                               *
     H* 10/13/99  C:65  PROJECT WC942                                 *
     H*                 EXCLUDE FLAT CANCELLED DIVISIONS WHEN         *
     H*                 CHECKING FOR HIGHEST EXPENSE CONSTANT         *
     H*                                                               *
     H* 09/29/99  C:65  PROJECT CM045                                 *
     H*                 CHANGE WAIVER PROCESS TO CHECK PREM FLAG AT   *
     H*                 THE EMPLOYER DETAIL LEVEL.                    *
     H*                                                               *
     H* 08/10/99  C:65 PROJECT:  SUPPORT# 19216                       *
     H*                DO A USER CONTROL FOR OPENING WDELL1.          *
     H*                                                               *
     H* 07/22/99  C:65 PROJECT:  WC577                                *
     H*                DISPLAY AMOUNT OF SCHEDULE DB/CR RESULT ON     *
     H*                THE LINE ITEM.                                 *
     H*                                                               *
     H*  07/15/99 C:65 PROJECT WC705                                  *
     H*                USE OPTIONAL HEADINGS FOR SCHEDULE DB/CR.      *
     H*                                                               *
     H*  06/24/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H*  06/01/99 C:65 PROJECT: CM045                                 *
     H*                ADD WAIVER SURCHARGE TO PREMIUM INFORMATION    *
     H*                AND ALLOW ADDL LOAD TO APPLY AT J,E,S,D.       *
     H*                                                               *
     H*  03/31/99 C:65 PROJECT: WC705                                 *
     H*                GET EXPENSE CONSTANT FROM HIGHEST STATE RATHER *
     H*                THAN CALCULATING IT.                           *
     H*                                                               *
     H*  01/29/99 C:65 SEQ#: WA096/16322                              *
     H*                GET EXPENSE CONSTANT FROM DETAIL AND DIVIDE    *
     H*                BY ELPROR IF NECESSARY.                        *
     H*                                                               *
     H*  11/16/98 C:33 SEQ#: WA048/14618                              *
     H*                TURN OFF *IN92 WHEN ENTERING PROGRAM SO THAT   *
     H*                PAYROLLS WILL LOAD UPON ENTRY.                 *
     H*                                                               *
     H*  11/12/98 C:65 SEQ#: SUPPORT                                  *
     H*                ADD CALCULATED DISCOUNT PERCENTAGE TO SCREEN.  *
     H*                                                               *
     H*  11/04/98 C:33 SEQ#: WA091/14817                              *
     H*                CHANGE PAGING KEY TO INCLUDE THE SPECIAL DESC  *
     H*                                                               *
     H*  10/15/98 C:33 SEQ#: WA033/14059                              *
     H*                CHANGE LOAD FROM BEGINNING SUBROUTINE TO BE    *
     H*                SURE THAT THE PAYROLLS ARE FOR THE CORRECT EMP *
     H*                                                               *
     H*  09/01/98 C:33 SEQ#: DR095/12876                              *
     H*                CHANGE Q1PRMS TO 9 FOR @@QTE                   *
     H*                                                               *
     H* 08/10/98 C:61 Project: 48WLC                                  *
     H*               Display old Emp#/Div from Employer Cross        *
     H*               Reference (WZEXP).                              *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                LOAD @@QTE('Q') OR BLANKS IN QCQTE FOR QUOTE   *
     H*                MODE.                                          *
     H*                                                               *
     H* 06/24/98  C:33  SEQ#: WA095/12329                             *
     H*                 DO NOT ALLOW FOR A PAGE DOWN IF THE # OF      *
     H*                 RECORDS FITS EVENLY ON THE SCREEN.            *
     H*                                                               *
     H* 06/11/98  C:65  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H* 06/04/98  C:65  PROJECT: WC705                                *
     H*                 USE FLEXIBLE HEADING FOR STATE TAX.           *
     H*                                                               *
     H* 05/05/98  C:65  PROJECT: WC825                                *
     H*                 DISPLAY 'MORE KEYS' ON THE FIRST LIST.        *
     H*                                                               *
     H* 03/24/98  C:78  PROJECT: WC825                                *
     H*                 ADD 'F14=DARTS' IN SCR02                      *
     H*                                                               *
     H* 02/13/98  C:65  PROJECT: WC705                                *
     H*                 DO NOT DISPLAY TAXABLE PREMIUM UNLESS THERE   *
     H*                 IS A STATE TAX PERCENTAGE FOR THAT ACCOUNT.   *
     H*                                                               *
     H* 02/12/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H*  02/09/98 C:65 PROJECT: WC705                                 *
     H*                ADD OPTIONAL HEADING FOR STOCK DISCOUNT.       *
     H*                                                               *
     H*  02/04/98 C:33 PROJECT: WC764                                 *
     H*                RECHAIN TO ORIGINAL EMPLOYER DETAIL RECORD     *
     H*                AFTER DIVISION COUNT                           *
     H*                                                               *
     H*  02/02/98 C:65 PROJECT: WC705                                 *
     H*                ADDED INDICATOR FOR EST/AUD PREMIUM CAPTION.   *
     H*                                                               *
     H*  11/04/97 C:85 PROJECT: WC764                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                ADDED DIVISION COUNT.                          *
     H*                ADDED IND. 70 FOR DISPLAYING CANCEL DATE.      *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  18 - ROLL DOWN                                               *
     H*  19 - ROLL UP                                                 *
     H*  23-39 UNDERLINE FOR TOTAL FIELDS                             *
     H*  31 - QUOTE MODE                                              *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - CALLED FOR REVISED BILL                                 *
     H*  42 - MORE KEYS                                               *
     H*  46-53 UNDERLINE FOR TOTAL FIELDS                             *
     H*  63 - Display old Emp#/Div in header info                     *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  67 - COMMERCIAL APPLICATION INSTALLED (WCFL07=Y)             *
     H*  70 - CANCEL DATE <> ZERO                                     *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  59 - UNAPPLIED BILLING REVISION EXISTS                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*****************************************************************
     FW0051F10  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
     FWDELP     UF   E             DISK    USROPN
     F                                     RENAME(PEL01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*** BEGIN CHG ***  08/10/99  ************************************
     F*****WDELL1  IF  E           K        DISK
     FWDELL1    IF   E           K DISK    USROPN
     F*** END CHG *****  08/10/99  ************************************
     F                                     RENAME(PEL01:PEL02)
      *
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS2)
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      ***** END ADD ******* 08/10/98 **********************************
      *
     FWMFNL     IF   E           K DISK
      *
     F*** BEGIN DEL ***  10/25/99  ************************************
     F*****WDF1L   IF  E           K        DISK
     F*** END DEL *****  10/25/99  ************************************
      *
     FWDF2L     IF   E           K DISK
      *
     FWMRSL     IF   E           K DISK
      *
     FWDRRL     IF   E           K DISK
      *
     F*** BEGIN DEL ***  01/29/99  ************************************
     F***WDECL   IF  E           K        DISK
     F*** END DEL *****  01/29/99  ************************************
     F*** BEGIN ADD ***  03/31/99  ************************************
     F*** BEGIN DEL ***  11/27/01  ************************************
     F*****WDECL   IF  E           K        DISK
     F*** END DEL *****  11/27/01  ************************************
     F*** END ADD *****  03/31/99  ************************************
      *
     FWTLML1    IF   E           K DISK
      *
     F*** BEG CHG *****  04/20/09  ************************************
     F**** WMEPL  IF  E           K        DISK                       UC
     FWMEPLE    IF   E           K DISK    USROPN
     F*** END CHG *****  04/20/09  ************************************
     F*** BEG ADD *****  04/20/09  ************************************
     FWMPRL4    IF   E           K DISK
     F                                     RENAME(PPR01:PPR02)
     F*** END ADD *****  04/20/09  ************************************
      *
     FWMUPL     IF   E           K DISK
      *
     F*** BEG ADD *****  04/26/04  ************************************
     FWMPRL1    IF   E           K DISK
     F*** END ADD *****  04/26/04  ************************************
     F***  Begin Add  ***  11/10/10  **************************************
     FWMHCL     IF   E           K DISK
     F                                     RENAME(PHC01:PHC00)
     F***  End   Add  ***  11/10/10  **************************************
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*** BEGIN ADD ***  11/14/02  ************************************
     FWMPXL1    IF   E           K DISK
     F*** END ADD *****  11/14/02  ************************************
      *
     F*** BEGIN ADD ***  12/30/09  ************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
      *
      *** BEG CHG ***** 08/30/12 **************************************
     F*********** WMLDL1  IF  E           K        DISK
     FWMLDL1    IF   E           K DISK    USROPN
      *** END CHG ***** 08/30/12 **************************************
     F*** END ADD *****  12/30/09  ************************************
     F*** BEGIN ADD ***  02/05/10  ************************************
     FWDDAL1    IF   E           K DISK
     F*** BEGIN ADD ***  02/05/10  ************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
     D*** BEGIN CHG ***  06/01/99  ************************************
     D*******             CON     1  24 22
     D*** BEGIN CHG ***  10/01/01  ************************************
     D*********           CON     1  25 22
     D*** BEGIN CHG ***  11/20/01  ************************************
     D*****************   CON     1  26 22
     D*** BEGIN CHG ***  01/17/03  ************************************
     D*********           CON     1  30 22
     D*** BEG CHG *****  01/27/03  ************************************
     D*********           CON     1  31 22
     D*** BEG CHG *****  04/26/04  ************************************
     D*********           CON     1  32 22
     D*** BEG CHG *****  07/02/04  ************************************
     D*********           CON     1  33 22
     D*** BEG CHG *****  09/01/04  ************************************
     D*****************   CON     1  35 22
     D***  BEGIN CHG  ***  04/07/05 ***********************************
     D*****************   CON     1  40 22
     D***  Begin Chg  ***  12/11/08  **************************************
     D**********          CON     1  48 22
     D CON             S             22    DIM(60) CTDATA PERRCD(1)
     D***  End   Chg  ***  12/11/08  **************************************
     D***  END   CHG  ***  04/07/05 ***********************************
     D*** END CHG *****  09/01/04  ************************************
     D*** END CHG *****  07/02/04  ************************************
     D*** END CHG *****  04/26/04  ************************************
     D*** END CHG *****  01/27/03  ************************************
     D*** END CHG *****  01/17/03  ************************************
     D*** END CHG *****  11/20/01  ************************************
     D*** END CHG *****  10/01/01  ************************************
     D*** END CHG *****  06/01/99  ************************************
      /COPY SCOPYMTF1D
     D TMP             S              1    DIM(22)
     D NUM             S              1    DIM(5)
     D**********BEG CHG****10/05/10 ******************************
     D**************      AM          6  9 0
     D**********END CHG****10/05/10 ******************************
     D KEYF            S              8  0 DIM(2)
     D KEYM            S              5  0 DIM(2)
     D***  BEGIN ADD  ***  11/04/98  *********************************
     D KEYS            S              1    DIM(2)
     D***  END   ADD  ***  11/04/98  *********************************
     D X0              S              1    DIM(5)
     D XB              S              1    DIM(5)
     D***  BEG ADD ***** 12/18/05 ************************************
     D MOD             S              1    DIM(6)
     D***  END ADD ***** 12/18/05 ************************************
     D***  BEG ADD ***** 04/20/09 ************************************
     D KEYE            S              5    DIM(2)
     D***  END ADD ***** 04/20/09 ************************************
     D***  BEG ADD ***** 04/20/09 ************************************
     D***  END ADD ***** 04/20/09 ************************************
     D*** BEG ADD ***** 12/30/09 *************************************
     D NUM1            S              1    DIM(12)
     D*** END ADD ***** 12/30/09 *************************************
     D*** BEGIN ADD ****** 01/14/10 ************************************
     D NUM8            S              1    DIM(8)
     D*** END   ADD ****** 01/14/10 ************************************
     D***  Begin Add  ***  11/10/10  **************************************
     D***PY SCOPYMTF1E
     D***  End   Add  ***  11/10/10  **************************************
     D*
     D***  Begin Chg  ***  11/10/10  **************************************
     D**********COPY SCOPYMT01I
     D***PY SCOPYMTF1I
     D***  End   Chg  ***  11/10/10  **************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Employer Premium In-
     D                                     formation')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D MORE            C                   CONST('                    -
     D                                               More...')
     D BOTTOM          C                   CONST('                    -
     D                                                Bottom')
     D*
      **** BEGIN ADD **** 02/12/98 *********************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      **** END   ADD **** 02/12/98 *********************************************
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  HDG                    1    550
     D                                     DIM(25)
     D  SCHD01                 1     22
     D  SCHD02                23     44
     D  SCHD03                45     66
     D  SCHD04                67     88
     D  SCHD05                89    110
     D  SCHD06               111    132
     D  SCHD07               133    154
     D  SCHD08               155    176
     D  SCHD09               177    198
     D  SCHD10               199    220
     D  SCHD11               221    242
     D  SCHD12               243    264
     D  SCHD13               265    286
     D  SCHD14               287    308
     D  SCHD15               309    330
     D  SCHD16               331    352
     D  SCHD17               353    374
     D  SCHD18               375    396
     D  SCHD19               397    418
     D  SCHD20               419    440
     D  SCHD21               441    462
     D  SCHD22               463    484
     D  SCHD23               485    506
     D  SCHD24               507    528
     D  SCHD25               529    550
     D                 DS
     D  TAM                    1    275  2
     D                                     DIM(25)
     D  SCTA01                 1     11  2
     D  SCTA02                12     22  2
     D  SCTA03                23     33  2
     D  SCTA04                34     44  2
     D  SCTA05                45     55  2
     D  SCTA06                56     66  2
     D  SCTA07                67     77  2
     D  SCTA08                78     88  2
     D  SCTA09                89     99  2
     D  SCTA10               100    110  2
     D  SCTA11               111    121  2
     D  SCTA12               122    132  2
     D  SCTA13               133    143  2
     D  SCTA14               144    154  2
     D  SCTA15               155    165  2
     D  SCTA16               166    176  2
     D  SCTA17               177    187  2
     D  SCTA18               188    198  2
     D  SCTA19               199    209  2
     D  SCTA20               210    220  2
     D  SCTA21               221    231  2
     D  SCTA22               232    242  2
     D  SCTA23               243    253  2
     D  SCTA24               254    264  2
     D  SCTA25               265    275  2
     D                 DS
     D  CL                     1     36
     D                                     DIM(6)
     D  SCCL01                 1      6
     D  SCCL02                 7     12
     D  SCCL03                13     18
     D  SCCL04                19     24
     D  SCCL05                25     30
     D  SCCL06                31     36
     D***  BEG   ADD  ***  04/20/09  *********************************
     D                 DS
     D  EX                     1     30
     D                                     DIM(6)
     D  SCEX01                 1      5
     D  SCEX02                 6     10
     D  SCEX03                11     15
     D  SCEX04                16     20
     D  SCEX05                21     25
     D  SCEX06                26     30
     D*
     D                 DS
     D  DX                     1    150
     D                                     DIM(6)
     D  SCDX01                 1     25
     D  SCDX02                26     50
     D  SCDX03                51     75
     D  SCDX04                76    100
     D  SCDX05               101    125
     D  SCDX06               126    150
     D***  END   ADD  ***  04/20/09  *********************************
     D                 DS
     D  DT                     1     48  0
     D                                     DIM(6)
     D  SCDT01                 1      8  0
     D  SCDT02                 9     16  0
     D  SCDT03                17     24  0
     D  SCDT04                25     32  0
     D  SCDT05                33     40  0
     D  SCDT06                41     48  0
     D                 DS
     D  TP                     1      6
     D                                     DIM(6)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D**********BEG CHG****10/05/10 ******************************
     D*********** DS
     D***********                             1  54 AM
     D***********                             1   90SCAM01
     D***********                            10  180SCAM02
     D***********                            19  270SCAM03
     D***********                            28  360SCAM04
     D***********                            37  450SCAM05
     D***********                            46  540SCAM06
     D                 DS
     D  AM                     1     66  2
     D                                     DIM(6)
     D  SCAM01                 1     11  2
     D  SCAM02                12     22  2
     D  SCAM03                23     33  2
     D  SCAM04                34     44  2
     D  SCAM05                45     55  2
     D  SCAM06                56     66  2
     D**********END CHG****10/05/10 ******************************
     D                 DS
     D  SS                     1      6
     D                                     DIM(6)
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D***  BEG   Add  ***  06/21/10  **************************************
     D @2NPER          C                   CONST('2nd prd')

     I***  END   Add  ***  06/21/10  **************************************

     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *
      *  GROUP MASTER/AUXILIARY FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      *  GROUP DETAIL FILE
      *
     C*** BEGIN DEL ***  10/25/99  ************************************
     C******     F1KY      KLIST
     C******               KFLD           ELCO#
     C******               KFLD           ELFND
     C******               KFLD           ELFYR
     C*** END DEL *****  10/25/99  ************************************
      *
      *  EMPLOYER DETAIL CHANGED RECORD FILE
      *
     C     E2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
      *** BEGIN ADD *** 11/04/97 **************************************
      *
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
      *
     C     ELKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
      *
     C*** BEGIN ADD ***  03/31/99  ************************************
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXEMP#
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*** END ADD *****  03/31/99  ************************************
      *** END   ADD *** 11/04/97 **************************************
      *
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      ***** END ADD ******* 08/10/98 **********************************
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY
      *
      *  RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *  STATISTICAL REPORTING/RATING FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
      *
     C     RRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
      *
      *  EXPENSE CONSTANT DEFINITION FILE
      *
     C     ECKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
     C     ECKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
      *
      *  INCREASED LIMIT TABLE FILE
      *
     C     LMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    ELPFDT
      *
     C     LMKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
      *
      *  EMPLOYER PAYROLL FILE
      *
     C     EPKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     EPKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    XXEFD1
     C                   KFLD                    XXCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   KFLD                    XXSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   KFLD                    XXEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
      *
     C***  END   ADD  ***  04/20/09  *********************************
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** BEGIN ADD ***  11/14/02  ************************************
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XPST              2
     C*** END ADD *****  11/14/02  ************************************
     C*** BEG ADD *****  04/20/09  ************************************
      *
     C     PXKY3         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C                   KFLD                    EPEFDT
      *
     C     PXKY4         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*** END ADD *****  04/20/09  ************************************
      *
     C*** BEG ADD *****  04/26/04  ************************************
     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
      *
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C*** END ADD *****  04/26/04  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
      *  TABLE FILE (WTBSP)
      *
     C     TBKEY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY1
      *
      * KEY LIST OF THE LARGE DEDUCTIBLE FILE (WMLDP)
     C     LDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C*** END ADD ***** 12/30/09 *************************************
      *
     C***  Begin Add  ***  11/10/10  **************************************
     C     HCKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C*
     C     HCKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
     C***  End   Add  ***  11/10/10  **************************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@KF              1
     C                   PARM                    @@KG              1
     C                   PARM                    @@KI              1
      ****** BEGIN ADD ****** 08/07/98 ********************************
     C                   PARM                    @@QTE             1
      ****** END   ADD ****** 08/07/98 ********************************
     C***  Begin Add  ***  11/10/10  **************************************
     C                   PARM                    @@KS              1
     C***  End   Add  ***  11/10/10  **************************************
      *
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    LMKEY         XXKEY
     C***  BEGIN CHG  ***  01/29/99  *********************************
     C*****      *LIKE     DEFN ECST      XXST
     C*****      *LIKE     DEFN ECEXCO    XXEXCO
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    ELPRTD        XXEXCO
     C***  END CHG ******  01/29/99  *********************************
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    ELASP2        XXASP2
     C     *LIKE         DEFINE    EPEFDT        XXEFD1
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C     *LIKE         DEFINE    EPSPEC        XXSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C     *LIKE         DEFINE    EPEXT         XXEXT
     C***  END   ADD  ***  04/20/09  *********************************
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ELANPR        XXANPR
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C     *LIKE         DEFINE    ELSTPR        XXSTPR
     C     *LIKE         DEFINE    ELMAPR        XXMAPR
     C     *LIKE         DEFINE    ELBSCD        XXBSCD
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 02/12/98 *********************************************
      *
     C***  Begin Chg  ***  11/10/10  **************************************
     C**********COPY SCOPYMT01C
     C/COPY SCOPYMTF1C
     C***  End   Chg  ***  11/10/10  **************************************
      *
     C***  Begin Add  ***  11/10/10  **************************************
     C*================================================================
     C* Check for HCN availablity
     C*================================================================
     C     CHKHCN        BEGSR
     C*
     C                   MOVE      *OFF          HCN               1
     C*
     C     HCKY1         SETGT     PHC00
     C     HCKY          READPE    PHC00                                  60
     C     *IN60         IFEQ      *OFF
     C     HCEXDT        IFEQ      *ZERO
     C     HCEXDT        ORGT      ELPFDT
     C                   MOVE      *ON           HCN
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  ***  11/10/10  **************************************
      *
      *===============================================================
      *  CLR - SUBROUTINE TO CLEAR SCREEN ARRAYS FOR PAYROLL SEARCH
      *===============================================================
      *
     C     CLR           BEGSR
      *
     C                   MOVE      *ZEROS        DT
     C                   MOVE      *BLANKS       CL
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVE      *BLANKS       EX
     C***  END   ADD  ***  04/20/09  *********************************
     C***  Beg   add  ***  01/28/11  *********************************
     C                   MOVE      *BLANKS       DX
     C***  End   add  ***  01/28/11  *********************************
     C                   MOVE      *BLANKS       SS
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *ZEROS        AM
     C                   Z-ADD     1             I1                3 0
     C                   Z-ADD     *ZERO         I                 3 0
      *
     C                   ENDSR                                                  CLR
      *
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     C     ERRCK1        BEGSR
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK3
      *
      *===============================================================
      *  FILL - SUBROUTINE TO FILL SCREEN ARRAYS
      *===============================================================
      *
     C     FILL          BEGSR
      *
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
      *
     C                   MOVE      EPCLAS        XXCLS
     C                   MOVEA     XXCLS         X0
     C                   SETOFF                                       60
      *
      *  PROCESS EACH ELEMENT IN THE ARRAY X0.
      *
     C     1             DO        5             I
      *
      *  IF THE ELEMENT IS ZERO MOVE IN A BLANK
      *
     C  N60X0(I)         IFEQ      *ZERO
     C                   MOVE      ' '           XB(I)
     C                   ENDIF                                                  X0,I=*ZERO
      *
      *  IF THE ELEMENT IS NOT A ZERO STOP THE ZERO CHECK(60)
      *
     C     X0(I)         IFNE      *ZERO
     C     X0(I)         OREQ      *ZERO
     C     *IN60         ANDEQ     *ON
     C                   MOVE      X0(I)         XB(I)
     C                   SETON                                        60
     C                   ENDIF                                                  X0,I<>*ZERO
      *
     C                   ENDDO                                                  I->5
      *
     C                   MOVEA     XB            XXCLS             5
     C                   MOVEL     XXCLS         XXCLSP            6
     C                   MOVE      EPSPEC        XXCLSP
     C                   MOVEL     XXCLSP        CL(I1)
     C                   MOVEL     EPSTAT        SS(I1)
     C                   MOVEL     EPSAFL        TP(I1)
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVEL     EPEXT         EX(I1)
      ****************************
     C     PRKY          SETGT     PPR02
     C     PRKY1         READPE    PPR02                                  60
     C   60              MOVEL     *BLANKS       PRDESC
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       PRDESC
     C                   MOVE      *BLANKS       XXST
     C     PXKY3         SETGT     PPX01
     C     PXKY4         READPE    PPX01                                  60
     C  N60              MOVEL     PXDESC        PRDESC
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVE      RSJRST        XXST
     C  N61PXKY3         SETGT     PPX01
     C  N61PXKY4         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL     PXDESC        PRDESC
     C                   MOVEL     PRDESC        DX(I1)
     C                   ENDIF                                                  EPEXT IFNE
      *
     C     EPEXT         IFEQ      *BLANKS
     C                   MOVEL     PRDESC        DX(I1)
     C                   ENDIF                                                  EPEXT IFEQ
      *
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C     EPSAFL        IFEQ      'A'
     C                   Z-ADD     EPAAPR        AM(I1)
     C                   ELSE
     C                   Z-ADD     EPEAPR        AM(I1)
     C                   ENDIF                                                  EPSAFL=A
      *
     C                   ENDSR                                                  FILL
      *
     C***  BEG   ADD  ***  04/20/09  *********************************
      *===============================================================
      * KKDO - SUBROUTINE FOR F11
      *===============================================================
      *
      *  F11 =CHANGE INFO
      *
     C     KKDO          BEGSR
      *
     C     *IN68         IFEQ      *OFF
     C                   MOVE      *ON           *IN68
     C                   ELSE
     C                   MOVE      *OFF          *IN68
     C                   ENDIF
      *
      *
     C                   ENDSR                                                  KKDO
     C***  END   ADD  ***  04/20/09  *********************************
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
      *
      *
     C                   ENDSR                                                  KJDO
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14
      *===============================================================
      *
      *  F14=DARTS
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
      ***** BEGIN ADD ***** 03/24/98 **********************************
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
      ***** END   ADD ***** 03/24/98 **********************************
      ***** BEGIN ADD ***** 08/07/98 **********************************
     C                   MOVE      XXQTE         QCQTE
      ***** END   ADD ***** 08/07/98 **********************************
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   MOVE      *ON           *IN92
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR                                                  KNDO
      *
      *===============================================================
      * KPDO - SUBROUTINE FOR F15
      *===============================================================
      *
      *  F15=CLAIM SUMMARY
      *
     C     KPDO          BEGSR
      *
     C                   EXFMT     WDWCLM
      *
     C                   ENDSR                                                  KPDO
     C*** BEGIN ADD ***  11/15/99  ************************************
      *===============================================================
      * KQDO - SUBROUTINE FOR F16
      *===============================================================
      *
      *  F16=FEES
      *
     C     KQDO          BEGSR
      *
     C                   EXFMT     WDWFEE
      *
     C                   ENDSR                                                  KQDO
     C*** END ADD *****  11/15/99  ************************************
      *
     C***  Begin Del  ***  11/10/10  **************************************
      *===============================================================
      **KYDO*-*SUBROUTINE FOR F24
      *===============================================================
      **********
      ***F24=MORE KEYS
      **********
     C********** KYDO      BEGSR
      **********
     C********** *IN42     IFEQ *OFF
     C**********           MOVE *ON       *IN42
     C**********           ELSE
     C**********           MOVE *OFF      *IN42
     C**********           ENDIF                           *IN42=*OFF
      **********
     C**********           ENDSR                           KJDO
     C***  End   Del  ***  11/10/10  **************************************
      *
      *===============================================================
      *  LOAD - SUBROUTINE TO LOAD FORWARD THRU THE FILE
      *===============================================================
      *
     C     LOAD          BEGSR
      *
     C                   EXSR      CLR
      *
     C                   DO        6             I1
      *
      *  END OF FILE
      *
     C   60              MOVE      *OFF          XXDOWN            1
     C   60              MOVEL(P)  BOTTOM        SCUPDN
     C   60              LEAVE
      *
      *  FILL SCREEN AND GET NEXT RECORD
      *
     C***  BEG   ADD  ***  07/09/10  *********************************
     C     EPSTAT        IFNE      'I'
     C***  END   ADD  ***  07/09/10  *********************************
     C                   EXSR      FILL
     C***  BEG   ADD  ***  07/09/10  *********************************
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF                                                  EPSTAT<>I
     C***  END   ADD  ***  07/09/10  *********************************
      *
     C     EPKY1         READE     PEP01                                  60
      *
     C                   ENDDO                                                  I1->10
      *
      ***  BEGIN CHG  ***  06/24/98  *********************************
      **N60*****           MOVE *ON       XXDOWN
      **N60*****           MOVELMORE      SCUPDN    P
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           XXDOWN
     C                   MOVEL(P)  MORE          SCUPDN
     C                   ELSE
     C                   MOVE      *OFF          XXDOWN            1
     C                   MOVEL(P)  BOTTOM        SCUPDN
     C                   ENDIF                                                  *IN60=*OFF
      ***  END   CHG  ***  06/24/98  *********************************
      *
     C                   EXSR      SETKEY
      *
     C                   ENDSR                                                  LOAD
      *
      *===============================================================
      *  LOADB - SUBROUTINE TO LOAD FROM BEGINNING OF FILE
      *===============================================================
      *
     C     LOADB         BEGSR
      *
     C                   EXSR      CLR
      *
     C                   MOVE      *OFF          XXUP              1
      *
      *  POSITION AT BEGINNING OF FILE
      *
     C                   Z-ADD     *ZERO         XXEFD1
     C                   Z-ADD     *ZERO         XXCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   MOVEL     *BLANK        XXSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVEL     *BLANK        XXEXT
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C***  BEGIN CHG  ***  10/15/98  *********************************
     C********** EPKY2     SETLLPEP01
     C********** EPKY1     READEPEP01                    60
     C     EPKY1         CHAIN     PEP01                              60
     C***  END   CHG  ***  10/15/98  *********************************
      *
     C                   EXSR      LOAD
      *
     C                   ENDSR                                                  LOADB
      *
      *===============================================================
      *  LOADP - SUBROUTINE TO LOAD BACKWARD THRU THE FILE
      *===============================================================
      *
     C     LOADP         BEGSR
      *
     C                   EXSR      CLR
      *
     C                   MOVE      *ON           XXUP
     C                   MOVE      *ON           XXDOWN
     C                   MOVEL(P)  MORE          SCUPDN
      *
     C                   Z-ADD     7             I1
      *
     C     I1            DOUEQ     1
      *
      *  BEGINNING OF FILE
      *
     C   60              EXSR      LOADB
     C   60              LEAVE
      *
      *  FILL SCREEN AND GET NEXT RECORD
      *
     C***  BEG   ADD  ***  07/09/10  *********************************
     C     EPSTAT        IFNE      'I'
     C***  END   ADD  ***  07/09/10  *********************************
     C                   SUB       1             I1
      *
     C                   EXSR      FILL
     C***  BEG   ADD  ***  07/09/10  *********************************
     C                   ENDIF
     C***  END   ADD  ***  07/09/10  *********************************
      *
     C     EPKY1         READPE    PEP01                                  60
      *
     C                   ENDDO                                                  I1=1
      *
     C   60              MOVE      *OFF          XXUP
      *
     C                   EXSR      SETKEY
      *
     C                   ENDSR                                                  LOADP
      *
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
      *  CALCULATE EARNED PREMIUM TO DATE
      *
     C                   TIME                    DSDTTM
     C                   MOVEL     ELCO#         SRCO#             3
     C                   MOVEL     ELFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     ELEMP#        SREMP#            9
     C                   MOVEL     ELDIV         SRDIV             5
     C                   MOVEL     DSDATE        QQDTE             8
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
      *
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
      *
     C                   MOVE      SRAMT         SCPRTD
     C                   Z-ADD     ELINMP        SCINTD
     C                   Z-ADD     ELTXPR        SCTXPR
      *
      *
      *  LOAD PSUEDO-SUBFILE
      *
     C  N92              EXSR      CLR
     C  N92              EXSR      LOADB
      *
      *  INITIALIZE SCREEN ARRAYS
      *
     C                   CLEAR                   HDG
     C                   CLEAR                   TAM
     C                   MOVEA     '000000'      *IN(23)
     C                   MOVEA     '000000'      *IN(29)
     C                   MOVEA     '00000'       *IN(35)
     C                   MOVEA     '00000000'    *IN(46)
      *
     C     ELFAF         IFNE      *BLANK
     C                   MOVE      *ON           *IN53
     C                   ELSE
     C                   MOVE      *OFF          *IN53
     C                   ENDIF                                                  ELFAF IFNE
      *
      *  LOAD SCREEN ARRAYS
      *
     C                   Z-ADD     1             A                 3 0          MANUAL PREM
      *
      *  MANUAL PREMIUM
      *
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(1)        HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(49)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
      *
     C                   Z-ADD     ELAMPR        TAM(A)
      *
     C     ELAMNP        IFEQ      'Y'
     C                   SUB       ELAIN1        TAM(A)
     C                   SUB       ELAIN2        TAM(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C                   SUB       ELAWAV        TAM(A)
     C*** END ADD *****  09/20/02  ************************************
     C                   ENDIF                                                  ELAMNP=Y
      *
     C                   ELSE
      *
     C                   Z-ADD     ELMAPR        TAM(A)
      *
     C     ELEMNP        IFEQ      'Y'
     C                   SUB       ELEIN1        TAM(A)
     C                   SUB       ELEIN2        TAM(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C                   SUB       ELEWAV        TAM(A)
     C*** END ADD *****  09/20/02  ************************************
     C                   ENDIF                                                  ELEMNP=Y
      *
     C                   ENDIF                                                  ELFAF=A
      *
     C                   ADD       1             A
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     ELEWAV        IFNE      *ZERO
     C     ELWVFL        ANDEQ     *BLANK
     C     ELAWAV        ORNE      *ZERO
     C     ELWVFL        ANDEQ     *BLANK
     C                   MOVE      'M'           ELWVFL
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C*** BEGIN ADD ***  04/06/07  ************************************
     C*
     C* ADD SHORT RATE PENALTY PREMIUM TO MANUAL PREMIUM.
     C*
     C     RRSRAF        IFEQ      'M'
     C     ELSRAM        ANDNE     *ZERO
     C     RRSRAF        OREQ      ' '
     C     ELSRAM        ANDNE     *ZERO
     C*
     C                   MOVEL     CON(45)       HDG(A)
     C                   Z-ADD     ELSRAM        TAM(A)
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*** END ADD *****  04/06/07  ************************************
     C*
     C*** BEGIN ADD ***  01/13/03  ************************************
     C*
     C* DEVIATION #1 APPLIED TO MANUAL
     C*
     C     RRDV1L        IFEQ      'M'
     C     ELASP2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADC2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDC2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
      *
     C     ELASP2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELASP2        IFLT      *ZERO
     C     ELASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELASP2        XXASP1            5
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELASP2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASP2<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C*** BEG   ADD ***  06/21/10  *********************************************
     C* DEVIATION #1 2ND PERIOD APPLIED TO MANUAL
     C     RRDV1L        IFEQ      'M'
     C     ELD1S2        ANDNE     *ZERO
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD1A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD1E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25           25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*
     C     ELD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     ELD1S2        IFLT      *ZERO
     C     ELD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP1            5
     C                   ENDIF                                                  ELD1S2<*ZERO
     C*
     C                   MOVEA     XXASP1        NUM
     C     ELD1S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
     C*
     C                   ENDIF                                                  ELD1S2<*ZERO
     C*
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C*
     C                   ENDIF                                                  ELD1S2<>*ZERO
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV1L IFEQ
     C*** END   ADD ***  06/21/10  *********************************************
     C*  LOAD 2ND DEVIATION IF APPLIED TO MANUAL PREMIUM
     C*
     C     RRDV2L        IFEQ      'M'
     C     ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'M'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*** END ADD ******** 02/25/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADV2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDV2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
      *
     C     ELDEV2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELDEV2        IFLT      *ZERO
     C     ELDEV2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP1            5
     C                   ENDIF                                                  ELdev2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELDEV2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELDEV2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDEV2<>*ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C*
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      ELSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     ELSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*** END ADD *****  01/13/03  ************************************
      *
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C     RRDV2L        IFEQ      'M'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'M'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD2A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD2E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C*
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
      *
     C     ELD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     ELD2S2        IFLT      *ZERO
     C     ELD2S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP1            5
     C                   ENDIF                                                  ELD2S2<*ZERO
     C*
     C                   MOVEA     XXASP1        NUM
     C     ELD2S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
     C*
     C                   ENDIF                                                  ELD2S2<*ZERO
     C*
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
     C*
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C*
     C                   ENDIF                                                  ELD2S2<>*ZERO
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C*
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      ELSTFC        XXASP8            8
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     ELSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*** END   ADD ***  06/21/10  *********************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
      *
      *  WAIVER SURCHARGE
      *
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'M'
     C     ELWVFL        IFEQ      'M'
     C*** END CHG *****  09/29/99  ************************************
     C     ELWVTP        ANDNE     *BLANK
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(25)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRWVFL IFEQ
     C*** END ADD *****  06/01/99  ************************************
      *
      *  INCREASED LIMITS
      *
     C     ELILCD        IFNE      *BLANKS
     C*** BEGIN DEL ***  10/25/99  ************************************
     C************         MOVE *BLANKS   XXEMLB 30
     C************         MOVELF1EMLB    XXEMLB
     C*** END DEL *****  10/25/99  ************************************
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELILCD        XXKEY
      *
     C     LMKY          SETGT     PLM01                              60
     C     LMKY2         READPE    PLM01                                  60
      *** BEGIN ADD ***  06/11/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *** END ADD *****  06/11/98  ************************************
      *
     C   60              Z-ADD     0             LMPCT
     C                   MOVEA     CON(2)        TMP
      *
     C     LMPCT         IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE '.'       TMP,19
     C                   MOVE      '.'           TMP(18)
      **** END   CHG **** 08/20/01 ************************************
     C                   MOVE      LMPCT         XXLMPC            5
     C                   MOVEA     XXLMPC        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE NUM,3     TMP,18
     C                   MOVE      NUM(3)        TMP(19)
      **** END   CHG **** 08/20/01 ************************************
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  LMPCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAIN1        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEIN1        TAM(A)
     C                   ENDIF                                                   ELFAF=A
      *
     C                   ADD       1             A
     C                   ENDIF                                                  ELILCD IFNE
      *
      *  2ND INCREASED LIMITS
      *
     C     ELINL2        IFNE      *BLANKS
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELINL2        XXKEY
      *
     C     LMKY          SETGT     PLM01                              60
     C     LMKY2         READPE    PLM01                                  60
      *** BEGIN ADD ***  06/11/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *** END ADD *****  06/11/98  ************************************
      *
     C   60              Z-ADD     0             LMPCT
     C                   MOVEA     CON(20)       TMP
      *
     C     LMPCT         IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE '.'       TMP,19
     C                   MOVE      '.'           TMP(18)
      **** END   CHG **** 08/20/01 ************************************
     C                   MOVE      LMPCT         XXLMPC            5
     C                   MOVEA     XXLMPC        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE NUM,3     TMP,18
     C                   MOVE      NUM(3)        TMP(19)
      **** END   CHG **** 08/20/01 ************************************
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  LMPCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAIN2        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEIN2        TAM(A)
     C                   ENDIF                                                   ELFAF=A
      *
     C                   ADD       1             A
     C                   ENDIF                                                  ELINL2 IFNE
      *** BEG ADD ***** 09/01/04 **************************************
      *
      *  VOLUNTARY LABOR CHARGE
      *
     C     ELVLCA        IFNE      *ZERO
     C     ELEMNP        ANDNE     'Y'
      *
     C                   Z-ADD     ELVLCA        TAM(A)
      *
     C     RRVLCH        IFEQ      *BLANKS
     C                   MOVEL     CON(26)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRVLCH        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRWVFL IFEQ
      *** END ADD ***** 09/01/04 **************************************
      *
      *  DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
      *
     C     RRRDPR        IFEQ      'M'
     C     ELDDCT        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCT        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(3)        TMP
      *
     C     ELDDCT        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDDCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
      ***  BEGIN ADD  ***  04/07/05 *************************************
     C     ELDD2P        IFNE      *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADD2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(41)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *                             C
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDD2P<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      ***  END   ADD  ***  04/07/05 *************************************
      *
     C                   ENDIF                                                  RRRDPR IFEQ
      *
      *
     C*** BEG ADD ***** 12/30/09 *************************************
      *  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
      *
     C     LDPFLG        IFEQ      'Y'
      *
     C     LDPLV1        IFEQ      'M'
     C     LDPCT1        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCT        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(3)        TMP
      *
     C     LDPCT1        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   ENDIF                                                  ELDDCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPLV1= 'M' O
      *
      *
     C     LDPLV2        IFEQ      'M'
     C     LDPCT2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADD2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(41)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPCT2<>*ZERO
      *
     C                   ENDIF                                                  LDPFLG IFEQ
     C*** END ADD ***** 12/30/09 *************************************
     C*** BEGIN ADD *** 11/10/10 *************************************
      *
      *  HCN CREDIT
      *
     C     ELHCN         IFNE      *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELAHCN        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEHCN        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(53)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '.'           TMP(17)
     C                   CLEAR                   XXASP1
     C                   MOVE      ELHCN         XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(16)
     C                   MOVE      NUM(2)        TMP(18)
     C                   MOVE      NUM(3)        TMP(19)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  ELHCN IFNE
     C*** END ADD ***** 11/10/10 *************************************
      *
      *  SUBJECT PREMIUM
      *
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(22)       HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(51)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELASBJ        TAM(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     ELAMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C                   SUB       ELAWAV        TAM(A)
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C                   ELSE
     C                   Z-ADD     ELESBJ        TAM(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     ELEMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C                   SUB       ELEWAV        TAM(A)
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C                   ENDIF                                                  ELFAF IFEQ
      *
     C     A             SUB       1             B                 2 0
     C                   ADD       22            B
     C     B             IFGT      39
     C                   ADD       6             B
     C                   ENDIF                                                  B IFGT 39
     C                   MOVE      *ON           *IN(B)
      *
     C                   ADD       1             A
      *
      *  MANAGED CARE CREDIT
      *
     C     ELASPM        IFNE      *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCM        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCM        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RR1AHD        IFEQ      *BLANKS
     C                   MOVEA     CON(4)        TMP
     C                   ELSE
     C                   MOVEA     RR1AHD        TMP
     C                   ENDIF                                                  RR1AHD IFEQ
      *
     C     ELASPM        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(16)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      ELASPM        XXASP1            5
     C                   MOVEA     XXASP1        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C                   MOVE      X'22'         TMP(16)
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASPM<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  ELASPM IFNE
      *
      *  SAFETY CREDIT
      *
     C     ELASPS        IFNE      *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCS        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCS        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RR1BHD        IFEQ      *BLANKS
     C                   MOVEA     CON(5)        TMP
     C                   ELSE
     C                   MOVEA     RR1BHD        TMP
     C                   ENDIF                                                  RR1BHD IFEQ
      *
     C     ELASPS        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(16)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      ELASPS        XXASP1            5
     C                   MOVEA     XXASP1        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C                   MOVE      X'22'         TMP(16)
      *
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASPS<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  F2SCHD<>*BLANKS
      *
      *
      *
      *  LOAD DEVIATION IF APPLIED TO SUBJECT PREMIUM
      *
     C     RRDV1L        IFEQ      'J'
     C     ELASP2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADC2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDC2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELASP2    IFLT *ZERO
     C*********            MOVEACON,23    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELASP2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
      ***** BEG ADD ***** 04/02/07 ************************************
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
      *
     C     ELASBJ        SUB       ELADCM        XXPREM           15 4
     C     XXPREM        SUB       ELADCS        XXPREM
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELADC2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELASP2
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C                   ENDIF                                                  CLTID = 045
      ***** END ADD ***** 04/02/07 ************************************
      *
     C     ELASP2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELASP2        IFLT      *ZERO
     C     ELASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELASP2        XXASP1            5
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELASP2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASP2<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV1L=M
      *
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO SUBJECT PREMIUM
     C     RRDV1L        IFEQ      'J'
     C     ELD1S2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD1A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD1E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
      *
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
     C     ELASBJ        SUB       ELADCM        XXPREM           15 4
     C     XXPREM        SUB       ELADCS        XXPREM
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELD1A2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELD1S2
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C                   ENDIF                                                  CLTID = 045
      *
     C     ELD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELD1S2        IFLT      *ZERO
     C     ELD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP1            5
     C                   ENDIF                                                  ELD1S2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
     C     ELD1S2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELD1S2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELD1S2<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV1L=M
     C*** END   ADD ***  06/21/10  *********************************************
      *  LOAD 2nd DEVIATION IF APPLIED TO SUBJECT PREMIUM
      *
     C     RRDV2L        IFEQ      'J'
     C     ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'J'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*** END ADD ******** 02/25/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADV2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDV2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELDEV2    IFLT *ZERO
     C*********            MOVEACON,24    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 02/25/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELDEV2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
     C     ELDEV2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELDEV2        IFLT      *ZERO
     C     ELDEV2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP1            5
     C                   ENDIF                                                  ELdev2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELDEV2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELDEV2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDEV2<>*ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C*
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      ELSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     ELSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 02/25/10 ************************************
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV2L=J
     C*** BEGIN ADD ***  06/01/99  ************************************
      *
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD 2nd DEVIATION 2ND PERIOD IF APPLIED TO SUBJECT PREMIUM
     C     RRDV2L        IFEQ      'J'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'J'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD2A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD2E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
      *
     C     ELD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELD2S2        IFLT      *ZERO
     C     ELD2S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP1            5
     C                   ENDIF                                                  ELD2S2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
     C     ELD2S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELD2S2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF                                                  ELD2S2<>*ZERO
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      ELSTFC        XXASP8            8
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C     ELSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV2L=J
     C*** END   ADD ***  06/21/10  *********************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
      *  WAIVER SURCHARGE
      *
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'J'
     C     ELWVFL        IFEQ      'J'
     C*** END CHG *****  09/29/99  ************************************
     C     ELWVTP        ANDNE     *BLANK
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(25)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRWVFL IFEQ
      *
      * ADDITIONAL LOAD
      *
     C     RRASFL        IFEQ      'J'
      *
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2
      *
     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EPSTAT        IFNE      'I'
      *
      *** BEG ADD ***** 04/26/04 ***********************************
     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF
      *** END ADD ***** 04/26/04 ***********************************
      *
     C*** BEGIN CHG ***  11/14/02  ************************************
     C*********            ADD  EPADLD    XTADLD
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     PXCODE        IFEQ      *ZERO
      *** BEG ADD ***** 04/26/04 ***********************************
     C     XXOCCD        ANDEQ     *BLANK
      *** END ADD ***** 04/26/04 ***********************************
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C*** END CHG *****  11/14/02  ************************************
      *
     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     XTADLD        IFGT      *ZERO
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFGT
      *
     C                   ENDIF                                                  ELAMNP<>Y
      *
     C                   ENDIF                                                  RRASFL IFEQ
     C*** END ADD *****  06/01/99  ************************************
     C*** BEGIN ADD ***  04/06/07  ************************************
     C*
     C* ADD SHORT RATE PENALTY PREMIUM TO SUBJECT PREMIUM.
     C*
     C     RRSRAF        IFEQ      'J'
     C     ELSRAM        ANDNE     *ZERO
     C*
     C                   MOVEL     CON(45)       HDG(A)
     C                   Z-ADD     ELSRAM        TAM(A)
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*** END ADD *****  04/06/07  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
      *  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO SUBJECT PREMIUMS
      *
     C     LDPFLG        IFEQ      'Y'
      *
     C     LDPLV1        IFEQ      'J'
     C     LDPCT1        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCT        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(3)        TMP
      *
     C     LDPCT1        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   ENDIF                                                  ELDDCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPLV2= 'J' O
      *
     C     LDPLV2        IFEQ      'J'
     C     LDPCT2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADD2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(41)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPCT2<>*ZERO
      *
     C                   ENDIF                                                  LDPFLG IFEQ
     C*** END ADD ***** 12/30/09 *************************************
      *
      *  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
      *
     C     ELFAF         IFEQ      'A'
     C     ELAXP1        ADD       ELAXP2        TAM(A)
     C                   ADD       ELAXP3        TAM(A)
     C                   ELSE
     C     ELEXP1        ADD       ELEXP2        TAM(A)
     C                   ADD       ELEXP3        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(8)        TMP
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '.'           TMP(13)
     C                   CLEAR                   XXASP1
     C                   MOVE      ELEXM1        XXASP1
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(12)
     C                   MOVE      NUM(2)        TMP(14)
     C                   MOVE      NUM(3)        TMP(15)
     C                   MOVE      NUM(4)        TMP(16)
     C                   MOVE      NUM(5)        TMP(17)
      *
     C     ELFAF         IFEQ      'A'
     C     ELAXP1        ANDNE     TAM(A)
     C     ELFAF         ORNE      'A'
     C     ELEXP1        ANDNE     TAM(A)
     C                   MOVE      '*'           TMP(19)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
      *
     C     A             SUB       1             B                 2 0
     C                   ADD       22            B
     C     B             IFGT      39
     C                   ADD       6             B
     C                   ENDIF                                                  B IFGT 39
     C                   MOVE      *ON           *IN(B)
      *
     C                   ADD       1             A
     C*** BEGIN ADD ***  10/04/01  ************************************
      *
      *  CONTRACTOR CREDIT
      *
     C     ELASP1        IFNE      *ZERO
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADC1        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDC1        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
      ***** BEG CHG ******* 03/16/11 **********************************
     C*************        MOVEACON,10    TMP
     C     RRCCHD        IFNE      *BLANKS
     C                   MOVEL     RRCCHD        CON(10)
     C                   MOVEA     CON(10)       TMP
     C                   ELSE
     C                   MOVEA     CON(10)       TMP
     C                   ENDIF
      ***** END CHG ******* 03/16/11 **********************************
      *
     C     ELASP1        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      ELASP1        XXASP1            5
     C                   MOVEA     XXASP1        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASP1<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  ELASP1 IFNE
     C*** END ADD *****  10/04/01  ************************************
      *
      *  SCHEDULE DB/CR
      *
      *** BEG CHG ***** 12/17/07 **********************************
     C******     ELFAF     IFNE 'A'
     C******     ELEMNP    ANDNE'Y'
     C******     ELFAF     OREQ 'A'
     C******     ELAMNP    ANDNE'Y'
     C******     ELDBCR    IFNE *ZERO
     C*** BEGIN ADD ***  07/15/99  ************************************
     C******     ELDBCR    IFLT 1
     C******               MOVEARRCRHD    TMP
     C******               ELSE
     C******     ELDBCR    IFGT 1
     C******               MOVEARRDBHD    TMP
     C******               ELSE
     C*** END ADD *****  07/15/99  ************************************
     C******               MOVEACON,19    TMP
     C*** BEGIN ADD ***  07/15/99  ************************************
     C******               ENDIF                           ELDBCR IFGT
     C******               ENDIF                           ELDBCR IFLT
     C*** END ADD *****  07/15/99  ************************************
      *
     C*** BEG ADD *****  12/18/05  ************************************
     C******     CLTID     IFEQ '136'
     C******               MOVEACON,38    TMP
     C******               ENDIF
     C*** END ADD *****  12/18/05  ************************************
      *
     C******               MOVEA*BLANKS   TMP,15
     C******               MOVE X'22'     TMP,15
     C******               MOVE '.'       TMP,17
     C******               CLEARXXASP1
     C*** BEG ADD *****  12/18/05  ************************************
     C******               SELEC
     C******     CLTID     WHEQ '136'
     C******     ELDBCR    MULT ELCMUL    XXWK54  54H
     C******               MOVE XXWK54    XXASP1  5
     C******               OTHER
     C*** END ADD *****  12/18/05  ************************************
     C******               MOVE ELDBCR    XXASP1  5
     C*** BEG ADD *****  12/18/05  ************************************
     C******               ENDSL
     C*** END ADD *****  12/18/05  ************************************
     C******               MOVEAXXASP1    NUM
     C******               MOVE NUM,1     TMP,16
     C******               MOVE NUM,2     TMP,18
     C******               MOVE NUM,3     TMP,19
     C******               MOVE NUM,4     TMP,20
     C******               MOVE NUM,5     TMP,21
      *
      *
     C******     ELFAF     IFEQ 'A'
     C*** BEGIN CHG ***  07/22/99  ************************************
     C******               Z-ADDELAFRS    TAM,A
     C******               Z-ADDELADC     TAM,A
     C*** END CHG *****  07/22/99  ************************************
     C******               ELSE
     C*** BEGIN CHG ***  07/22/99  ************************************
     C******               Z-ADDELEFRS    TAM,A
     C******               Z-ADDELEDC     TAM,A
     C*** END CHG *****  07/22/99  ************************************
     C******               ENDIF                           ELFAF=A
      *
     C******               MOVEATMP       HDG,A
     C******               CLEARTMP
     C******               ADD  1         A
     C******               ENDIF                           ELDBCR IFNE
     C******               ENDIF                           ELFAF IFNE
     C     RRDCAP        IFEQ      ' '
     C                   EXSR      CALDBR
     C                   ENDIF                                                  ELDBCR IFNE
      *** END CHG ***** 12/17/07 **********************************
      *
      *
      *  LOAD DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
      *
     C     RRDV1L        IFEQ      'E'
     C     ELASP2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADC2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDC2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      ***** BEG ADD ***** 04/02/07 ************************************
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAFRS        XXPREM
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELADC2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELASP2
     C                   ENDIF                                                  XXPREM IFNE
      *
     C                   ENDIF                                                  ELFAF IFEQ
      *
     C                   ENDIF                                                  CLTID = 045
      ***** END ADD ***** 04/02/07 ************************************
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELASP2    IFLT *ZERO
     C*********            MOVEACON,23    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELASP2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
     C     ELASP2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELASP2        IFLT      *ZERO
     C     ELASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELASP2        XXASP1            5
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELASP2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASP2<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV1L=E
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV1L        IFEQ      'E'
     C     ELD1S2        ANDNE     *ZERO
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD1A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD1E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
     C*
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAFRS        XXPREM
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELD1A2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELD1S2
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C                   ENDIF                                                  CLTID = 045
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*
     C     ELD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     ELD1S2        IFLT      *ZERO
     C     ELD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP1            5
     C                   ENDIF                                                  ELD1S2<*ZERO
     C*
     C                   MOVEA     XXASP1        NUM
     C     ELD1S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
     C*
     C                   ENDIF                                                  ELD1S2<*ZERO
     C*
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
     C*
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF                                                  ELD1S2<>*ZERO
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV1L=E
     C*** END   ADD ***  06/21/10  *********************************************
      *  LOAD 2nd DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
      *
     C     RRDV2L        IFEQ      'E'
     C     ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'E'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*** END ADD ******** 02/25/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADV2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDV2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELDEV2    IFLT *ZERO
     C*********            MOVEACON,24    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 02/25/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELDEV2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
     C     ELDEV2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELDEV2        IFLT      *ZERO
     C     ELDEV2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP1            5
     C                   ENDIF                                                  ELdev2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELDEV2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELDEV2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDEV2<>*ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C*
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      ELSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     ELSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 02/25/10 ************************************
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV2L=E
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD 2nd DEVIATION 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV2L        IFEQ      'E'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'E'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD2A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD2E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
      *
     C     ELD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELD2S2        IFLT      *ZERO
     C     ELD2S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP1            5
     C                   ENDIF                                                  ELD2S2<*ZERO
     C*
     C                   MOVEA     XXASP1        NUM
     C     ELD2S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELD2S2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF                                                  ELD2S2<>*ZERO
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      ELSTFC        XXASP8            8
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     ELSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV2L=E
     C*** END   ADD ***  06/21/10  *********************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
      ***** BEG ADD ***** 12/17/07 ***********************************
     C     RRDCAP        IFEQ      'Y'
     C                   EXSR      CALDBR
     C                   ENDIF
      ***** END ADD ***** 12/17/07 ***********************************
      *
      *  WAIVER SURCHARGE
      *
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'E'
     C     ELWVFL        IFEQ      'E'
     C*** END CHG *****  09/29/99  ************************************
     C     ELWVTP        ANDNE     *BLANK
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(25)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRWVFL IFEQ
     C*** END ADD *****  06/01/99  ************************************
      *
      *  ADDITIONAL LOAD
      *
     C*** BEGIN ADD ***  06/01/99  ************************************
     C     RRASFL        IFEQ      'E'
     C*** END ADD *****  06/01/99  ************************************
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2
      *
     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
     C*** BEGIN CHG ***  11/14/02  ************************************
     C*********            ADD  EPADLD    XTADLD
      *
      *** BEG ADD ***** 04/26/04 ***********************************
     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF
      *** END ADD ***** 04/26/04 ***********************************
      *
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
      *** BEG ADD ***** 04/26/04 ***********************************
     C     XXOCCD        ANDEQ     *BLANK
      *** END ADD ***** 04/26/04 ***********************************
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C*** END CHG *****  11/14/02  ************************************
     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     XTADLD        IFGT      *ZERO
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFGT
      *
     C                   ENDIF                                                  ELAMNP<>Y
     C*** BEGIN ADD ***  06/01/99  ************************************
     C                   ENDIF                                                  RRASFL IFEQ
     C*** END ADD *****  06/01/99  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
      *  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO EXPERIENCE MODIFIER
      *
     C     LDPFLG        IFEQ      'Y'
      *
     C     LDPLV1        IFEQ      'E'
     C     LDPCT1        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCT        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(3)        TMP
      *
     C     LDPCT1        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   ENDIF                                                  ELDDCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPLV1= 'E' O
      *
     C     LDPLV2        IFEQ      'E'
     C     LDPCT2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADD2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(41)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPCT2<>*ZERO
      *
     C                   ENDIF                                                  LDPFLG IFEQ
     C*** END ADD ***** 12/30/09 *************************************
     C*** BEGIN DEL ***  10/04/01  ************************************
      *
      *  CONTRACTOR CREDIT
      *
     C*******    ELASP1    IFNE *ZERO
     C*******    ELFAF     IFEQ 'A'
     C*******              Z-SUBELADC1    TAM,A
     C*******              ELSE
     C*******              Z-SUBELEDC1    TAM,A
     C*******              ENDIF                           ELFAF=A
      *******
     C*******              MOVEACON,10    TMP
      *******
     C*******    ELASP1    IFNE *ZERO
      *******
     C*******              MOVEA*BLANKS   TMP,15
     C*******              MOVE X'22'     TMP,15
     C*******              MOVE '%'       TMP,22
     C*******              MOVE '.'       TMP,19
     C*******              MOVE ELASP1    XXASP1  5
     C*******              MOVEAXXASP1    NUM
      *******
     C*******    NUM,1     IFNE *ZERO
     C*******              MOVE NUM,1     TMP,16
     C*******              ENDIF                           NUM,1<>*ZERO
      *******
     C*******    NUM,2     IFNE *ZERO
     C*******    NUM,1     ORGT *ZERO
     C*******              MOVE NUM,2     TMP,17
     C*******              ENDIF                           NUM,1>*ZERO<>NUM,2
      *******
     C*******              MOVE NUM,3     TMP,18
     C*******              MOVE NUM,4     TMP,20
     C*******              MOVE NUM,5     TMP,21
      *******
     C*******              ENDIF                           ELASP1<>*ZERO
      *******
     C*******              MOVEATMP       HDG,A
     C*******              ADD  1         A
     C*******              ENDIF                           ELASP1 IFNE
     C*** END DEL *****  10/04/01  ************************************
      *
      *  DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
      *
     C     RRRDPR        IFEQ      'S'
     C     ELDDCT        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCT        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(3)        TMP
      *
     C     ELDDCT        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDDCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C     ELDD2P        IFNE      *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADD2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(41)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDD2P<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C***  END   ADD  ***  04/07/05 *************************************
      *
     C                   ENDIF                                                  RRRDPR=S
      *
      *
     C*** BEGIN ADD ***  10/01/01  ************************************
     C     ELLOSS        IFNE      *ZERO
     C                   Z-ADD     ELLOSS        TAM(A)
     C                   MOVEL     CON(26)       HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFGT
     C*** END ADD *****  10/01/01  ************************************
      *
      *** BEG ADD ***** 04/26/04 ***********************************
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2
     C                   MOVE      *BLANKS       HDG(A)
      *
     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EPSTAT        IFNE      'I'
      *
     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF
      *
     C     XXOCCD        IFEQ      'Y'
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     XTADLD        IFGT      *ZERO
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(33)       HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFGT
      *
     C                   ENDIF                                                  ELAMNP<>Y
      *** END ADD ***** 04/26/04 ***********************************
     C*** BEGIN ADD ***  04/06/07  ************************************
     C*
     C* ADD SHORT RATE PENALTY PREMIUM TO STANDARD PREMIUM.
     C*
     C     RRSRAF        IFEQ      'S'
     C     ELSRAM        ANDNE     *ZERO
     C*
     C                   MOVEL     CON(45)       HDG(A)
     C                   Z-ADD     ELSRAM        TAM(A)
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*** END ADD *****  04/06/07  ************************************
      *
      *  STANDARD PREMIUM
      *
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(11)       HDG(A)                         STD PREM
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(47)       HDG(A)                         STD PREM
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELASPR        TAM(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     ELAMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C     ELWVFL        ANDNE     'J'
     C     ELWVFL        ANDNE     'E'
     C                   SUB       ELAWAV        TAM(A)
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C                   ELSE
     C                   Z-ADD     ELSTPR        TAM(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     ELEMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C     ELWVFL        ANDNE     'J'
     C     ELWVFL        ANDNE     'E'
     C                   SUB       ELEWAV        TAM(A)
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C                   ENDIF                                                  ELFAF=A
      *
     C     A             SUB       1             B                 2 0
     C                   ADD       22            B
     C     B             IFGT      39
     C                   ADD       6             B
     C                   ENDIF                                                  B IFGT 39
     C                   MOVE      *ON           *IN(B)
      *
     C                   ADD       1             A
      *
      *  LOAD DEVIATION IF APPLIED TO STANDARD PREMIUM
      *
     C     RRDV1L        IFEQ      'S'
     C     ELASP2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADC2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDC2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
      ***** BEG ADD ***** 04/02/07 ************************************
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELASPR        XXPREM
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELADC2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELASP2
     C                   ENDIF                                                  XXPREM IFNE
      *
     C                   ENDIF                                                  ELFAF IFEQ
     C                   ENDIF                                                  CLTID = 045
      ***** END ADD ***** 04/02/07 ************************************
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELASP2    IFLT *ZERO
     C*********            MOVEACON,23    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELASP2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
     C     ELASP2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELASP2        IFLT      *ZERO
     C     ELASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELASP2        XXASP1            5
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELASP2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASP2<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV1L=S
     C*** BEG   ADD ***  06/21/10  *********************************************
      *  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     C     RRDV1L        IFEQ      'S'
     C     ELD1S2        ANDNE     *ZERO
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD1A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD1E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
     C*
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELASPR        XXPREM
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELD1A2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELD1S2
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C                   ENDIF                                                  CLTID = 045
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*
     C     ELD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     ELD1S2        IFLT      *ZERO
     C     ELD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP1            5
     C                   ENDIF                                                  ELD1S2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
     C     ELD1S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF                                                  ELD1S2<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV1L=S
     C*** END   ADD ***  06/21/10  *********************************************
      *  LOAD 2nd DEVIATION IF APPLIED TO STANDARD PREMIUM
      *
     C     RRDV2L        IFEQ      'S'
     C     ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'S'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*** END ADD ******** 02/25/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADV2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDV2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELDEV2    IFLT *ZERO
     C*********            MOVEACON,24    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 02/25/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELDEV2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
     C     ELDEV2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELDEV2        IFLT      *ZERO
     C     ELDEV2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP1            5
     C                   ENDIF                                                  ELdev2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELDEV2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELDEV2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDEV2<>*ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C*
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      ELSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     ELSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 02/25/10 ************************************
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV2L=S
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD 2nd DEVIATION 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     C     RRDV2L        IFEQ      'S'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'S'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD2A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD2E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
      *
     C     ELD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     ELD2S2        IFLT      *ZERO
     C     ELD2S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP1            5
     C                   ENDIF                                                  ELD2S2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
     C     ELD2S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELD2S2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF                                                  ELD2S2<>*ZERO
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      ELSTFC        XXASP8            8
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C     ELSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV2L=S
     C*** END   ADD ***  06/21/10  *********************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
      *
      *  WAIVER SURCHARGE
      *
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'S'
     C     ELWVFL        IFEQ      'S'
     C*** END CHG *****  09/29/99  ************************************
     C     ELWVTP        ANDNE     *BLANK
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(25)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRWVFL IFEQ
      *
      * ADDITIONAL LOAD
      *
     C     RRASFL        IFEQ      'S'
      *
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2
      *
     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
      *
      *** BEG ADD ***** 04/26/04 ***********************************
     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF
      *** END ADD ***** 04/26/04 ***********************************
      *
     C*** BEGIN CHG ***  11/14/02  ************************************
     C*********            ADD  EPADLD    XTADLD
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
      *** BEG ADD ***** 04/26/04 ***********************************
     C     XXOCCD        ANDEQ     *BLANK
      *** END ADD ***** 04/26/04 ***********************************
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C*** END CHG *****  11/14/02  ************************************
     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     XTADLD        IFGT      *ZERO
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFGT
      *
     C                   ENDIF                                                  ELAMNP<>Y
      *
     C                   ENDIF                                                  RRASFL IFEQ
     C*** END ADD *****  06/01/99  ************************************
     C*** BEG ADD ***** 12/30/09 **************************************
      *
      *  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
      *
     C     LDPFLG        IFEQ      'Y'
      *
     C     LDPLV1        IFEQ      'S'
     C     LDPCT1        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCT        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(3)        TMP
      *
     C     LDPCT1        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      *ZERO
     C     NUM1(1)       ORGT      *ZERO
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   ENDIF                                                  LDPCT1<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPLV1= 'S' O
      *
     C     LDPLV2        IFEQ      'S'
     C     LDPCT2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADD2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(41)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPCT2<>*ZERO
      *
     C                   ENDIF                                                  LDPFLG=Y
     C*** BEG ADD ***** 12/30/09 **************************************
      *
      *  STOCK DISCOUNT
      *
     C     ELEDC3        IFNE      *ZERO
     C     ELFAF         ANDNE     'A'
     C     ELADC3        ORNE      *ZERO
     C     ELFAF         ANDEQ     'A'
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADC3        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDC3        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
      *** BEGIN CHG ***  02/09/98  ************************************
      *********            MOVEACON,12    TMP
     C     RRDSHD        IFEQ      *BLANKS
     C                   MOVEA     CON(12)       TMP
     C                   ELSE
     C                   MOVEA     RRDSHD        TMP
     C                   ENDIF                                                  RRDSHD IFEQ
      *** END CHG *****  02/09/98  ************************************
      *
     C     ELASP3        IFNE      *ZERO
     C*** BEGIN ADD ***  11/12/98  ************************************
     C     ELDISP        ORNE      *ZERO
     C*** END ADD *****  11/12/98  ************************************
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*** BEGIN CHG ***  11/12/98  ************************************
     C*****                MOVE ELASP3    XXASP3  5
     C     ELASP3        IFNE      *ZERO
     C                   MOVE      ELASP3        XXASP3            5
     C                   ELSE
     C                   MOVE      ELDISP        XXASP3
     C                   ENDIF
     C*** END CHG *****  11/12/98  ************************************
     C                   MOVEA     XXASP3        NUM
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASP3<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  ELASP3 IFNE
      *
      *
      *  LOAD DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
      *
     C     RRDV1L        IFEQ      'D'
     C     ELASP2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADC2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDC2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
      ***** BEG ADD ***** 04/02/07 ************************************
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
     C     ELASPR        SUB       ELADC3        XXPREM
     C     RRDV2L        IFEQ      'S'
     C                   SUB       ELADV2        XXPREM
     C                   ENDIF
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELADC2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELASP2
     C                   ENDIF                                                  XXPREM IFNE
      *
     C                   ENDIF                                                  ELFAF IFEQ
     C                   ENDIF                                                  CLTID = 045
      ***** END ADD ***** 04/02/07 ************************************
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELASP2    IFLT *ZERO
     C*********            MOVEACON,23    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELASP2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
     C     ELASP2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELASP2        IFLT      *ZERO
     C     ELASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELASP2        XXASP1            5
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELASP2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELASP2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELASP2<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV1L=D
      *
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO DISCOUNTED PREMIUM
     C     RRDV1L        IFEQ      'D'
     C     ELD1S2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD1A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD1E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     CLTID         IFEQ      '045'
     C     ELFAF         IFEQ      'A'
     C     ELASPR        SUB       ELADC3        XXPREM
     C     RRDV2L        IFEQ      'S'
     C                   SUB       ELD2A2        XXPREM
     C                   ENDIF
     C*
     C     XXPREM        IFNE      *ZERO
     C     ELD1A2        DIV(H)    XXPREM        XXDISP            5 4
     C     100           MULT(H)   XXDISP        ELD1S2
     C                   ENDIF                                                  XXPREM IFNE
      *
     C                   ENDIF                                                  ELFAF IFEQ
     C                   ENDIF                                                  CLTID = 045
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*
     C     ELD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     ELD1S2        IFLT      *ZERO
     C     ELD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP1            5
     C                   ENDIF                                                  ELD1S2<*ZERO
     C*
     C                   MOVEA     XXASP1        NUM
     C     ELD1S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
     C*
     C                   ENDIF                                                  ELD1S2<*ZERO
     C*
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
     C*
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF                                                  ELD1S2<>*ZERO
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV1L=D
     C*** END   ADD ***  06/21/10  *********************************************
      *  LOAD 2nd DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
      *
     C     RRDV2L        IFEQ      'D'
     C     ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'D'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*** END ADD ******** 02/25/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADV2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDV2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********  ELDEV2    IFLT *ZERO
     C*********            MOVEACON,24    TMP
     C*********            ELSE
     C*** END DEL *****  04/10/02  ************************************
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 02/25/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
     C*** BEGIN DEL ***  04/10/02  ************************************
     C*********            ENDIF                           ELDEV2 IFLT
     C*** END DEL *****  04/10/02  ************************************
      *
     C     ELDEV2        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELDEV2        IFLT      *ZERO
     C     ELDEV2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP1            5
     C                   ENDIF                                                  ELdev2<*ZERO
      *
     C                   MOVEA     XXASP1        NUM
      *
     C     ELDEV2        IFLT      *ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
      *
     C                   ENDIF                                                  ELDEV2<*ZERO
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
     C                   ENDIF                                                  ELDEV2<>*ZERO
     C*** BEGIN ADD ****** 02/25/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C*
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      ELSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     ELSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 02/25/10 ************************************
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRDV2L=D
     C*** BEG   ADD ***  06/21/10  *********************************************
     C*  LOAD 2nd DEVIATION 2ND PERIOD IF APPLIED TO DISCOUNTED PREMIUM
     C     RRDV2L        IFEQ      'D'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     CLTID         ANDNE     '025'
     C     RRDV2L        OREQ      'D'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELD2A2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELD2E2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV2H=*BLANKS
      *
     C     ELD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
      *
     C     ELD2S2        IFLT      *ZERO
     C     ELD2S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP1            5
     C                   ENDIF                                                  ELD2S2<*ZERO
     C*
     C                   MOVEA     XXASP1        NUM
     C     ELD2S2        IFLT      *ZERO
     C     NUM(2)        IFNE      *ZERO
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2<>*ZERO
     C*
     C                   ENDIF                                                  ELD2S2<*ZERO
     C*
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF                                                  NUM,1<>*ZERO
     C*
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF                                                  ELD2S2<>*ZERO
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      ELSTFC        XXASP8            8
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C     ELSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV2L=D
     C*** END   ADD ***  06/21/10  *********************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
      *
      *  WAIVER SURCHARGE
      *
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'D'
     C     ELWVFL        IFEQ      'D'
     C*** END CHG *****  09/29/99  ************************************
     C     ELWVTP        ANDNE     *BLANK
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(25)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  RRWVFL IFEQ
      *
      * ADDITIONAL LOAD
      *
     C     RRASFL        IFEQ      'D'
      *
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2
      *
     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
      *
      *** BEG ADD ***** 04/26/04 ***********************************
     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF
      *** END ADD ***** 04/26/04 ***********************************
      *
     C*** BEGIN CHG ***  11/14/02  ************************************
     C*********            ADD  EPADLD    XTADLD
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
      *** BEG ADD ***** 04/26/04 ***********************************
     C     XXOCCD        ANDEQ     *BLANK
      *** END ADD ***** 04/26/04 ***********************************
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C*** END CHG *****  11/14/02  ************************************
     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     XTADLD        IFGT      *ZERO
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFGT
      *
     C                   ENDIF                                                  ELAMNP<>Y
      *
     C                   ENDIF                                                  RRASFL IFEQ
     C*** END ADD *****  06/01/99  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
      *  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO DISCOUNTED PREMIUM
      *
     C     LDPFLG        IFEQ      'Y'
      *
     C     LDPLV1        IFEQ      'D'
     C     LDPCT1        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADCT        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(3)        TMP
      *
     C     LDPCT1        IFNE      *ZERO
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM1,1>*ZERO<>NUM1,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   ENDIF                                                  ELDDCT<>*ZERO
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPLV1='D'  O
      *
     C     LDPLV2        IFEQ      'D'
     C     LDPCT2        ANDNE     *ZERO
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-SUB     ELADD2        TAM(A)
     C                   ELSE
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     CON(41)       TMP
      *
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     LDPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
      *
     C     NUM1(1)       IFNE      *ZERO
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF                                                  NUM1,1<>*ZERO
      *
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *
     C                   ENDIF                                                  LDPCT2<>*ZERO
      *
     C                   ENDIF                                                  LDPFLG IFEQ
     C*** END ADD ***** 12/30/09 *************************************
     C*** BEG ADD *****  01/27/03  ************************************
      *
      *  MASS ARAP SURCHARGE
      *
     C*** BEGIN CHG ***  01/26/04  ************************************
     C*********  ELFAF     IFEQ 'E'
     C     ELFAF         IFNE      'A'
     C*** END CHG *****  01/26/04  ************************************
     C     ELRPE1        ADD       ELRPE2        XXARAP           13 2
     C                   ADD       ELRPE3        XXARAP
      *
     C     XXARAP        IFNE      *ZERO
     C                   MOVEL     CON(32)       HDG(A)
     C                   Z-ADD     XXARAP        TAM(A)
     C                   ADD       1             A
     C                   ENDIF
      *
     C                   ELSE
     C     ELRPA1        ADD       ELRPA2        XXARAP
     C                   ADD       ELRPA3        XXARAP
      *
     C     XXARAP        IFNE      *ZERO
     C                   MOVEL     CON(32)       HDG(A)
     C                   Z-ADD     XXARAP        TAM(A)
     C                   ADD       1             A
     C                   ENDIF
      *
     C                   ENDIF
     C*** END ADD *****  01/27/03  ************************************
      *
      *  NORMAL PREMIUM
      *
     C                   MOVEL     CON(13)       HDG(A)
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELANMP        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELENMP        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C     A             SUB       1             B                 2 0
     C                   ADD       22            B
     C     B             IFGT      39
     C                   ADD       6             B
     C                   ENDIF                                                  B IFGT 39
     C                   MOVE      *ON           *IN(B)
      *
     C                   ADD       1             A
      ***** BEG ADD ******* 06/19/06 **********************************
      *
      *  NORMAL PREMIUM
      *
     C     ELHXMN        IFGT      *ZERO
      ***** BEG CHG ******* 08/16/07 **********************************
     C*********            MOVELCON,44    HDG,A
     C                   MOVEL     CON(46)       HDG(A)
      ***** END CHG ******* 08/16/07 **********************************
     C                   Z-ADD     ELHXMN        TAM(A)
     C                   ADD       1             A
     C                   ENDIF                                                  ELHXMN
      *
      ***** END ADD ******* 06/19/06 **********************************
     C*** BEG ADD *****  07/06/06  ************************************
      *
      * Reinsurance Charge
      *
     C     ELEARC        IFNE      *ZEROS
     C     ELFAF         ANDNE     'A'
     C     ELAARC        ORNE      *ZEROS
     C     ELFAF         ANDEQ     'A'
     C                   MOVEL     CON(44)       HDG(A)
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAARC        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEARC        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   ADD       1             A
     C                   ENDIF
      *
     C*** END ADD *****  07/06/06  ************************************
      *
      *
      *  EXPENSE CONSTANT
      *
     C                   MOVEL     CON(14)       HDG(A)                         EXP CONSTANT
      *
     C     ELFAF         IFEQ      'A'
     C*+* BEGIN ADD *+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C     ELFFL9        OREQ      'Y'
     C*+* END   ADD *+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
     C                   Z-ADD     ELPRTD        TAM(A)
     C*+* BEGIN ADD *+*  04/13/07  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   Z-ADD     ELPRTD        XXEXCO
     C*+* END   ADD *+*  04/13/07  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
     C                   ELSE
      *
     C     ELEMNP        IFEQ      'Y'
     C     ELPRTD        OREQ      *ZERO
      *
     C                   Z-ADD     ELPRTD        TAM(A)
     C*+* BEGIN ADD *+*  04/13/07  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   Z-ADD     ELPRTD        XXEXCO
     C*+* END   ADD *+*  04/13/07  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
     C                   ELSE
      *
     C*** BEGIN CHG ***  01/29/99  ************************************
     C*****                MOVE ELPFDT    XXEFDT
      *****
     C*****      ECKY      SETGTPEC01
     C*****      ECKY1     REDPEPEC01                    60
      *****
     C*****      *IN60     IFEQ *OFF
     C*****                Z-ADDECEXCO    TAM,A
     C*****                Z-ADDECEXCO    XXEXCO
     C*****                ELSE
     C*****                Z-ADD0         TAM,A
     C*****                Z-ADD0         XXEXCO
     C*****                ENDIF                           *IN60=*OFF
      *****
     C*** BEGIN CHG ***  03/31/99  ************************************
     C*******    ELPROR    IFNE *ZERO
     C*******    ELPRTD    DIV  ELPROR    XXWK10 100H
     C*******              Z-ADDXXWK10    XXEXCO
     C*******              Z-ADDXXWK10    TAM,A
     C*******              ELSE
     C*******              Z-ADDELPRTD    TAM,A
     C*******              Z-ADDELPRTD    XXEXCO
     C*******              ENDIF                           ELPROR IFNE
     C*
     C*** BEGIN CHG ***  11/27/01  ************************************
     C*********            Z-ADDELCO#     XXCO#
     C*********            Z-ADDELFND     XXFND
     C*********            Z-ADDELFYR     XXFYR
     C*
     C*********            Z-ADD0         XXEXCO
     C*********            MOVE ELPFDT    XXEFDT
     C*********  ELKY2     CHAINPEL02                60
     C*********  *IN60     DOWEQ*OFF
      *
     C*********  RSKY      CHAINPRS01                60
     C*********            MOVE RSJRST    XXST
     C*********  ECKY      SETGTPEC01
     C*********  ECKY1     REDPEPEC01                    60
      *
     C*********  *IN60     IFEQ *OFF
     C*********  ECEXCO    ANDGTXXEXCO
     C*** BEGIN ADD ***  10/13/99  ************************************
     C*********  ELPFDT    ANDNEELPTDT
     C*** END ADD *****  10/13/99  ************************************
     C*********            Z-ADDECEXCO    XXEXCO
     C*********            ENDIF                           *IN60 IFEQ
     C*********  ELKY2     READEPEL02                    60
     C*********            ENDDO                           *IN60 DOWEQ
     C*
     C*********            Z-ADDXXEXCO    TAM,A
     C*
     C*********  ELKEY     CHAINPEL02                60
     C*********  RSKY      CHAINPRS01                60
     C*********            MOVE RSJRST    XXST
     C                   Z-ADD     ELPRTF        XXEXCO
     C                   Z-ADD     XXEXCO        TAM(A)
     C*** END CHG *****  11/27/01  ************************************
     C*** END CHG *****  03/31/99  ************************************
     C*** END CHG *****  01/29/99  ************************************
     C                   ENDIF                                                  ELEMNP=Y
      *
     C                   ENDIF                                                  ELFAF=A
      *
     C                   ADD       1             A
      ***** BEG ADD ******* 07/13/11 **********************************
      *
      *  Unearned Premium Surcharge
      *
     C     ELUPSA        IFGT      *ZERO
     C                   MOVEL     CON(54)       HDG(A)
     C                   Z-ADD     ELUPSA        TAM(A)
     C                   ADD       1             A
     C                   ENDIF                                                  ELHXMN
      *
      ***** END ADD ******* 07/13/11 **********************************
     C*** BEGIN ADD ***  01/17/03  ************************************
      *
      * Terrorism Risk
      *
     C     ELETER        IFNE      *ZERO
     C     ELFAF         ANDNE     'A'
     C     ELATER        ORNE      *ZERO
     C     ELFAF         ANDEQ     'A'
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELATER        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELETER        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEL     CON(31)       HDG(A)
     C*** begin add ***  03/02/09  ************************************
     C*
     C* Set the terrorism heading fields to optional or default heading.
     C*
     C     RRTERH        IFNE      *BLANKS
     C                   MOVEL     RRTERH        HDG(A)
     C                   ENDIF
     C*** end add *****  03/02/09  ************************************
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  ELETER IFEQ
     C*** END ADD *****  01/17/03  ************************************
      *
     C*** BEGIN ADD ***  01/07/05  ************************************
     C     ELEDTR        IFNE      *ZERO
     C     ELFAF         ANDNE     'A'
     C     ELADTR        ORNE      *ZERO
     C     ELFAF         ANDEQ     'A'
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELADTR        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELEDTR        TAM(A)
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEL     CON(37)       HDG(A)
     C*** begin add ***  03/02/09  ************************************
     C*
     C* Set the terrorism heading fields to optional or default heading.
     C*
     C     RRDTRH        IFNE      *BLANKS
     C                   MOVEL     RRDTRH        HDG(A)
     C                   ENDIF
     C*** end add *****  03/02/09  ************************************
      *
     C                   ADD       1             A
      *
     C                   ENDIF                                                  ELETER IFEQ
     C*** END ADD *****  01/07/05  ************************************
      *
      *  ANNUAL PREMIUM
      *
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(15)       HDG(A)                         EST PREM
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(48)       HDG(A)                         EST PREM
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
      *
     C     ELFAF         IFEQ      'A'
      *
     C                   Z-ADD     ELAAPR        TAM(A)
      *
     C                   ELSE
      *
     C     ELEMNP        IFEQ      'Y'
     C     ELPRTD        OREQ      *ZERO
      *
     C                   Z-ADD     ELENMP        TAM(A)
     C*** BEGIN ADD ***  01/17/03  ************************************
     C                   ADD       ELETER        TAM(A)
     C*** END ADD *****  01/17/03  ************************************
     C*** BEGIN ADD ***  01/07/05  ************************************
     C                   ADD       ELEDTR        TAM(A)
     C*** END ADD *****  01/07/05  ************************************
     C*** BEGIN ADD ***  07/06/06  ************************************
     C                   ADD       ELEARC        TAM(A)
     C*** END ADD *****  07/06/06  ************************************
     C*** BEGIN ADD ***  02/07/02  ************************************
     C     ELEMNP        IFEQ      'Y'
     C                   Z-ADD     ELANPR        TAM(A)
     C                   ENDIF
     C*** END ADD *****  02/07/02  ************************************
      *
     C                   ELSE
      *
     C     ELENMP        ADD       XXEXCO        TAM(A)
     C*** BEGIN ADD ***  01/17/03  ************************************
     C                   ADD       ELETER        TAM(A)
     C*** END ADD *****  01/17/03  ************************************
     C*** BEGIN ADD ***  01/07/05  ************************************
     C                   ADD       ELEDTR        TAM(A)
     C*** END ADD *****  01/07/05  ************************************
     C*** BEGIN ADD ***  07/06/06  ************************************
     C                   ADD       ELEARC        TAM(A)
     C*** END ADD *****  07/06/06  ************************************
      *
     C*+* BEGIN ADD *+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*+* BEGIN DEL *+*  04/13/07  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*********  ELFFL9    IFEQ 'Y'
     C*********  ELENMP    ADD  ELPRTD    TAM,A
     C*********            ENDIF
     C*+* END   DEL *+*  04/13/07  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*+* END   ADD *+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   ENDIF                                                  ELEMNP=Y
      *
     C                   ENDIF                                                  ELFAF=A
      *
     C     A             SUB       1             B                 2 0
     C                   ADD       22            B
     C     B             IFGT      39
     C                   ADD       6             B
     C                   ENDIF                                                  B IFGT 39
     C                   MOVE      *ON           *IN(B)
      *
     C                   ADD       1             A
      *
      *  LOAD PREMIUM INTO ARRAY IF NOT EQUAL 1
      *
     C                   Z-ADD     *ZERO         TAM(A)
     C                   MOVEA     CON(16)       TMP
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '.'           TMP(17)
     C                   MOVE      ELPROR        XXPROR            5
     C                   MOVEA     XXPROR        NUM
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELANPR        TAM(A)
     C                   ENDIF
      *
     C     NUM(1)        IFNE      *ZERO
     C                   MOVE      NUM(1)        TMP(15)
     C                   ENDIF                                                  NUM,1<>*ZERO
      *
     C     NUM(2)        IFNE      *ZERO
     C     NUM(1)        ORGT      *ZERO
     C                   MOVE      NUM(2)        TMP(16)
     C                   ENDIF                                                  NUM,1>*ZERO<>NUM,2
      *
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(19)
     C                   MOVE      NUM(5)        TMP(20)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*** BEGIN ADD ***  01/24/06  ************************************
      *
      * LOAD RETROSPECTIVE PREMIUM VALUES.
      *
     C                   Z-ADD     0             XXRETA           11 2
     C*
     C     ELSTAM        IFNE      *ZERO
     C     A             ANDLT     25
     C*
     C                   MOVEL     CON(42)       HDG(A)
     C*
     C     ELFAF         IFEQ      'A'
     C     ELSTAM        SUB       ELAAPR        TAM(A)
     C                   ELSE
     C     ELSTAM        SUB       ELANPR        TAM(A)
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C                   Z-ADD     TAM(A)        XXRETA
     C*
     C                   ADD       1             A
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(43)       HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(52)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     ELSTAM        TAM(A)
      *
     C     A             SUB       1             B                 2 0
     C                   ADD       22            B
     C     B             IFGT      39
     C                   ADD       6             B
     C                   ENDIF                                                  B IFGT 39
     C                   MOVE      *ON           *IN(B)
      *
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  ELSTAM IFNE
     C*
     C     A             IFLT      25
     C*** END ADD *****  01/24/06  ************************************
      *
      * LOAD STATE TAX FIELD
      *
     C     ELTXDU        IFNE      *ZERO
      *** BEGIN ADD ***  06/04/98  ************************************
     C     RRTXHD        IFNE      *BLANKS
     C                   MOVEL     RRTXHD        HDG(A)
     C                   ELSE
      *** END ADD *****  06/04/98  ************************************
     C                   MOVEL     CON(17)       HDG(A)
      *** BEGIN ADD ***  06/04/98  ************************************
     C                   ENDIF                                                  RRTXHD IFNE
      *** END ADD *****  06/04/98  ************************************
     C                   Z-ADD     ELTXDU        TAM(A)
     C                   ADD       1             A
     C                   ENDIF
      *
      *
      * LOAD TOTAL PREMIUM FIELD
      *
     C     ELTXDU        IFNE      *ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(21)       HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(50)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C     ELFAF         IFEQ      'A'
     C     ELAAPR        ADD       ELTXDU        TAM(A)
     C                   ELSE
     C     ELANPR        ADD       ELTXDU        TAM(A)
     C                   ENDIF                                                  ELFAF IFEQ
     C*** BEGIN ADD ***  01/24/06  ************************************
     C                   ADD       XXRETA        TAM(A)
     C*** END ADD *****  01/24/06  ************************************
      *
     C     A             SUB       1             B                 2 0
     C                   ADD       22            B
     C     B             IFGT      39
     C                   ADD       6             B
     C                   ENDIF                                                  B IFGT 39
     C                   MOVE      *ON           *IN(B)
      *
     C                   ADD       1             A
     C                   ENDIF                                                  ELTXDU IFEQ
      *
     C*** BEGIN ADD ***  01/24/06  ************************************
     C                   ENDIF                                                  A IFLT
     C*** END ADD *****  01/24/06  ************************************
      *
     C                   Z-ADD     ELTCLP        SCTCLP
     C                   Z-ADD     ELOUTS        SCOUTS
     C                   Z-ADD     ELTCLC        SCTCLC
     C                   Z-ADD     ELT#CL        SCT#CL
     C*** BEGIN ADD ***  01/04/10  ************************************
     C                   Z-ADD     ELT#CC        SCT#CC
     C*** END   ADD ***  01/04/10  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   Z-ADD     ELMFAM        SCMFAM
     C                   Z-ADD     ELSFAM        SCSFAM
     C                   Z-ADD     EL1AAM        SC1AAM
     C                   Z-ADD     EL2AAM        SC2AAM
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  01/02/01  ************************************
     C                   Z-ADD     EL3AAM        SC3AAM
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
     C                   Z-ADD     EL4AAM        SC4AAM
     C                   Z-ADD     EL5AAM        SC5AAM
     C*** END ADD *****  07/02/04  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C                   Z-ADD     EL6AAM        SC6AAM
     C                   Z-ADD     EL7AAM        SC7AAM
     C***  End   Add  ***  12/11/08  **************************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *
      *
     C                   ENDSR                                                  MVSCDB
      *
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     C     OTHERR        BEGSR
      *
     C     *INKC         CASEQ     *ON           FINISH
     C                   ENDCS
      *
     C                   ENDSR                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C***  BEG   ADD  ***  04/20/09  *********************************
     C     *INKK         CASEQ     *ON           KKDO                     90
     C***  END   ADD  ***  04/20/09  *********************************
     C     *INKN         CASEQ     *ON           KNDO                     90
     C     *INKP         CASEQ     *ON           KPDO                     90
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     *INKQ         CASEQ     *ON           KQDO                     90
     C*** END ADD *****  11/15/99  ************************************
     C     *INKY         CASEQ     *ON           KYDO                     90
     C     *IN18         CASEQ     *ON           PAGEDN                   90
     C     *IN19         CASEQ     *ON           PAGEUP                   90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * PAGEDN - SUBROUTINE FOR PAGE DOWN
      *===============================================================
      *
     C     PAGEDN        BEGSR
      *
     C     XXDOWN        IFEQ      *OFF
      *
     C                   SETON                                        9022
      *
     C                   ELSE
      *
     C                   Z-ADD     KEYF(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFD1
      *
     C                   Z-ADD     KEYM(2)       XXCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   MOVEL     KEYS(2)       XXSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVEL     KEYE(2)       XXEXT
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C     EPKY2         CHAIN     PEP01                              60
     C     EPKY1         READE     PEP01                                  60
      *
     C                   EXSR      LOAD
      *
     C                   MOVE      *ON           XXUP
     C                   MOVE      *ON           *IN90
      *
     C                   ENDIF                                                  XXDOWN<>*OFF
      *
     C                   ENDSR                                                  PAGEDN
      *
      *===============================================================
      * PAGEUP - SUBROUTINE FOR PAGE UP
      *===============================================================
      *
     C     PAGEUP        BEGSR
      *
     C     XXUP          IFEQ      *OFF
      *
     C                   SETON                                        9022
      *
     C                   ELSE
      *
     C                   Z-ADD     KEYF(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFD1
      *
     C                   Z-ADD     KEYM(1)       XXCLAS
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   MOVEL     KEYS(1)       XXSPEC
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVEL     KEYE(1)       XXEXT
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C     EPKY2         CHAIN     PEP01                              60
     C     EPKY1         READPE    PEP01                                  60
      *
     C                   EXSR      LOADP
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDIF                                                  XXUP<>*OFF
      *
     C                   ENDSR                                                  PAGEUP
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
     C                   CLOSE     WDELP
     C*** BEG   CHG ***  04/20/09  ************************************
     C*********            CLOSEWMEPL
     C                   CLOSE     WMEPLE
     C*** END CHG *****  04/20/09  ************************************
     C*** BEGIN ADD ***  08/10/99  ************************************
     C                   CLOSE     WDELL1
     C*** END ADD *****  08/10/99  ************************************
     C*** BEG ADD ***** 08/30/12 **************************************
     C                   CLOSE     WMLDL1
     C*** END ADD ***** 08/30/12 **************************************
      *
     C     Q1PRMS        IFGT      5
     C                   MOVE      *INKF         @@KF
     C                   MOVE      *INKG         @@KG
     C                   MOVE      *INKI         @@KI
     C                   ENDIF                                                  Q1PRMG>5
     C***  Begin Add  ***  11/10/10  **************************************
     C     Q1PRMS        IFGE      10
     C                   MOVE      *INKS         @@KS
     C                   ENDIF
     C***  End   Add  ***  11/10/10  **************************************
      *
     C                   ENDSR                                                  PGMEND
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
     C*
      *** BEG ADD ***** 12/18/05 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *** END ADD ***** 12/18/05 **************************************
     C*** BEGIN ADD ***  02/05/10  ************************************
     C                   MOVE      *OFF          *IN64
     C*** END ADD *****  02/05/10  ************************************
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   MOVE      '0'           *IN43
     C     CLTID         IFEQ      '025'
     C                   MOVE      '1'           *IN43
     C*** BEGIN ADD ***  02/05/10  ************************************
     C                   MOVE      *ON           *IN64
     C*** END ADD *****  02/05/10  ************************************
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C*
     C***  BEGIN ADD  ***  11/16/98  *********************************
     C                   MOVE      *OFF          *IN92
     C***  END   ADD  ***  11/16/98  *********************************
      *
     C                   OPEN      WDELP
     C*** BEG   CHG ***  04/20/09  ************************************
     C********             OPEN WMEPL
     C                   OPEN      WMEPLE
     C*** END CHG *****  04/20/09  ************************************
     C*** BEGIN ADD ***  08/10/99  ************************************
     C                   OPEN      WDELL1
     C*** END ADD *****  08/10/99  ************************************
     C*** BEG ADD ***** 08/30/12 **************************************
     C                   OPEN      WMLDL1
     C*** END ADD ***** 08/30/12 **************************************
      *
      *** BEGIN ADD *** 11/04/97 **************************************
      *
      *  DEFINE DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
      *
      *  INITIALIZE VARIABLES
      *
     C                   Z-ADD     0             SCCXDT                         CANCEL DATE
     C                   MOVE      *OFF          *IN70                          NOT CANCELLED
      *
      *** END   ADD *** 11/04/97 **************************************
      *
      *
     C                   MOVEA     '00'          *IN(41)
     C                   MOVE      *ON           *IN59
      *
     C     @@MODE        IFEQ      'R'
     C                   MOVE      *ON           *IN41
     C                   MOVE      'D'           @@MODE
     C                   ENDIF                                                  @@MODE
     C     @@MODE        IFEQ      'U'
     C                   MOVE      *ON           *IN41
     C                   MOVE      *OFF          *IN59
     C                   MOVE      'D'           @@MODE
     C                   ENDIF                                                  @@MODE
      *
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
      ********* BEGIN ADD ******* 08/07/98 ****************************
     C***  BEGIN CHG  ***  09/01/98  *********************************
     C********** Q1PRMS    IFGT 5
     C     Q1PRMS        IFGT      9
     C***  END   CHG  ***  09/01/98  *********************************
     C                   MOVE      @@QTE         XXQTE             1
     C                   ELSE
     C                   MOVE      *BLANKS       XXQTE             1
     C                   ENDIF                                                  Q1PRMS>5
      ********* END   ADD ******* 08/07/98 ****************************
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
     C   41              MOVEL(P)  @@PRDS        SCPRDS
      *
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL07        COMP      'Y'                                    67
      *
      *  GET SECURITY LEVEL OF THE USER
      *
     C     Q1USER        CHAIN     PUP01                              60
     C     WCSL04        COMP      UPLVL                              67  67
      *
      *  LOAD SCREEN HEADINGS
      *
     C     @@RRN         CHAIN     DBRCDF                             60
      *
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELFYR         SCFYR
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
      *** BEGIN ADD ***  02/02/98  ************************************
     C     ELFAF         IFEQ      'A'
     C                   MOVE      *ON           *IN45
     C                   ELSE
     C                   MOVE      *OFF          *IN45
     C                   ENDIF
      *** END ADD *****  02/02/98  ************************************
      *
      *** BEGIN ADD *** 11/04/97 **************************************
      *
      *  BEGIN CALCULATION FOR DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
      *
     C     ELKY1         CHAIN     PEL02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 = OFF
      *
      *  END CALCULATION FOR DIVISION COUNT
      *
      *** END   ADD *** 11/04/97 **************************************
      ***  BEGIN ADD  ***  02/04/98  *********************************
      *
      *  MAKE SURE THE CORRECT RECORD IS BEING USED FOR REST OF PROGRAM
      *
     C     @@RRN         CHAIN     DBRCDF                             60
      ***  END   ADD  ***  02/04/98  *********************************
      *
      *  GET EMPLOYER MASTER RECORD
      *
     C     EMKY          CHAIN     PEM01                              60
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
      **** BEGIN DEL **** 02/12/98 *********************************************
      ******************** DO   5         I1      30
      ********************
      ********** CX,I1     IFNE *ZERO
      ******************** Z-ADDCX,I1     SRDTI
      ******************** CALL 'SRCVTDAT'DTMDYY
      ******************** MOVELSRDTO     SCCXDT
      ******************** ELSE
      ******************** LEAVE
      ******************** ENDIF                           CX,I1<>*ZERO
      ********************
      ******************** ENDDO                           I1->5
      *** BEGIN ADD *** 11/04/97 **************************************
      ********************
      ** BEGIN TEST FOR CANCEL DATE <> ZERO
      ********************
      ********** SCCXDT    IFNE *ZERO
      ******************** MOVE *ON       *IN70            CANCELLED
      ******************** ELSE
      ******************** MOVE *OFF      *IN70            NOT CANCELLED
      ******************** ENDIF                           SCCXDT<>*ZERO
      ********************
      ** END TEST FOR CANCEL DATE <> ZERO
      ********************
      *** END   ADD *** 11/04/97 **************************************
      **** END   DEL **** 02/12/98 *********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   MOVE      *OFF          *IN70
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN70
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 02/12/98 *********************************************
      *
      *
      *  GET GROUP MASTER/AUXILARY/DETAIL RECORDS
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C                   MOVEL     FNNAME        SCNAME
      *
     C     FNKY          CHAIN     PF201                              60
     C*** BEGIN DEL ***  10/25/99  ************************************
     C*********  F1KY      CHAINPF101                60
     C*** END DEL *****  10/25/99  ************************************
      *
      *  GET AGENT RECORDS
      *
     C                   CALL      AGTAPI        AGTPL
      *
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      *
     C                   CLEAR                   XXST
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL     RSJRST        XXST
      *
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
      *
      *** BEGIN ADD ***  02/13/98  ************************************
     C     RRTXPC        IFGT      *ZERO
     C                   MOVE      *ON           *IN85
     C                   ELSE
     C                   MOVE      *OFF          *IN85
     C                   ENDIF                                                  RRTXPC IFGT
      *** END ADD *****  02/13/98  ************************************
     C*** BEGIN ADD ***  11/20/01  ************************************
     C     RRSFHD        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(27)       SCSFHD
     C                   ELSE
     C                   MOVEL(P)  RRSFHD        SCSFHD
     C                   ENDIF
     C*
     C     RR1STH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(28)       SC1STH
     C                   ELSE
     C                   MOVEL(P)  RR1STH        SC1STH
     C                   ENDIF
     C*
     C     RR2NDH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(29)       SC2NDH
     C                   ELSE
     C                   MOVEL(P)  RR2NDH        SC2NDH
     C                   ENDIF
     C*
     C     RR3RDH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(30)       SC3RDH
     C                   ELSE
     C                   MOVEL(P)  RR3RDH        SC3RDH
     C                   ENDIF
     C*
     C*** END ADD *****  11/20/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
     C     RR4THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(34)       SC4THH
     C                   ELSE
     C                   MOVEL(P)  RR4THH        SC4THH
     C                   ENDIF
     C*
     C     RR5THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(35)       SC5THH
     C                   ELSE
     C                   MOVEL(P)  RR5THH        SC5THH
     C                   ENDIF
     C*** END ADD *****  07/02/04  ************************************
     C*
     C***  Begin Add  ***  12/11/08  **************************************
     C     RR6THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(39)       SC6THH
     C                   ELSE
     C                   MOVEL(P)  RR6THH        SC6THH
     C                   ENDIF
     C*
     C     RR7THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(40)       SC7THH
     C                   ELSE
     C                   MOVEL(P)  RR7THH        SC7THH
     C                   ENDIF
     C***  End   Add  ***  12/11/08  **************************************
      *
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     EXKY          CHAIN     PEX01                              63
      ***** BEG ADD ******* 06/24/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/24/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      ***** END ADD ******* 08/10/98 **********************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C                   MOVE      'N'           LDPFLG            1
     C     WCFL43        IFEQ      'Y'
     C                   MOVEL     ELBSCD        XXKEY1           10
     C     TBKEY         CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFEQ      'L'
      *
     C     LDKY          CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      'Y'           LDPFLG
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*** BEGIN ADD ***  02/05/10  ************************************
     C                   Z-ADD     0             SCCLAM
     C     LDKY          CHAIN     PDA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       DAAMTB        SCCLAM
     C     LDKY          READE     PDA01                                  60
     C                   ENDDO
     C*** END ADD *****  02/05/10  ************************************
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C* Check for HCN availablity
     C                   EXSR      CHKHCN
     C*
     C                   EXSR      SETFKS
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
      *
     C***  Begin Add  ***  11/10/10  **************************************
      *===============================================================
      * SETFKS - Set key values
      *===============================================================
     C     SETFKS        BEGSR
      *
     C                   Z-ADD     1             #FKL1
      *
     C                   MOVE      *BLANKS       XXSCRN            1
     C                   MOVE      *BLANKS       XXNMBR            2
     C                   MOVE      *BLANKS       XXCDA             3
      *
     C     *IN41         IFEQ      *ON
     C*
     C                   MOVE      'YYY'         XXCDA
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      '1'           XXSCRN
     C                   EXSR      CHGCDA
     C*
     C                   MOVE      'YYY'         XXCDA
     C                   MOVE      '07'          XXNMBR
     C                   MOVE      '1'           XXSCRN
     C                   EXSR      CHGCDA
     C*
     C                   MOVE      'YYY'         XXCDA
     C                   MOVE      '09'          XXNMBR
     C                   MOVE      '1'           XXSCRN
     C                   EXSR      CHGCDA
     C*
     C     HCN           IFEQ      *ON
     C                   MOVE      'YYY'         XXCDA
     C                   MOVE      '18'          XXNMBR
     C                   MOVE      '1'           XXSCRN
     C                   EXSR      CHGCDA
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
     C***  End   Add  ***  11/10/10  **************************************
      *
      *===============================================================
      *  SETKEY - SUBROUTINE TO SET THE KEYS FOR THE NEXT PAGE
      *===============================================================
      *
     C     SETKEY        BEGSR
      *
     C                   Z-ADD     DT(1)         KEYF(1)
     C                   Z-ADD     DT(6)         KEYF(2)
     C                   MOVEL     CL(1)         KEYM(1)
     C                   MOVEL     CL(6)         KEYM(2)
     C***  BEGIN ADD  ***  11/04/98  *********************************
     C                   MOVE      CL(1)         KEYS(1)
     C                   MOVE      CL(6)         KEYS(2)
     C***  END   ADD  ***  11/04/98  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVE      EX(1)         KEYE(1)
     C                   MOVE      EX(6)         KEYE(2)
     C***  END   ADD  ***  04/20/09  *********************************
      *
     C                   ENDSR                                                  SETKEY
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  UPDATR
      *
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
      *===============================================================
      *
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
      *
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  WRITER
      *
      ***** BEG ADD ***** 12/17/07 ***********************************
     C     CALDBR        BEGSR
      *  SCHEDULE DB/CR
      *
     C     ELFAF         IFNE      'A'
     C     ELEMNP        ANDNE     'Y'
     C     ELFAF         OREQ      'A'
     C     ELAMNP        ANDNE     'Y'
     C     ELDBCR        IFNE      *ZERO
     C*** BEGIN ADD ***  07/15/99  ************************************
     C     ELDBCR        IFLT      1
     C                   MOVEA     RRCRHD        TMP
     C                   ELSE
     C     ELDBCR        IFGT      1
     C                   MOVEA     RRDBHD        TMP
     C                   ELSE
     C*** END ADD *****  07/15/99  ************************************
     C                   MOVEA     CON(19)       TMP
     C*** BEGIN ADD ***  07/15/99  ************************************
     C                   ENDIF                                                  ELDBCR IFGT
     C                   ENDIF                                                  ELDBCR IFLT
     C*** END ADD *****  07/15/99  ************************************
      *
     C*** BEG ADD *****  12/18/05  ************************************
     C     CLTID         IFEQ      '136'
     C                   MOVEA     CON(38)       TMP
     C                   ENDIF
     C*** END ADD *****  12/18/05  ************************************
      *
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '.'           TMP(17)
     C                   CLEAR                   XXASP1
     C*** BEG ADD *****  12/18/05  ************************************
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C     ELDBCR        MULT(H)   ELCMUL        XXWK54            5 4
     C                   MOVE      XXWK54        XXASP1            5
     C                   OTHER
     C*** END ADD *****  12/18/05  ************************************
     C                   MOVE      ELDBCR        XXASP1            5
     C*** BEG ADD *****  12/18/05  ************************************
     C                   ENDSL
     C*** END ADD *****  12/18/05  ************************************
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(16)
     C                   MOVE      NUM(2)        TMP(18)
     C                   MOVE      NUM(3)        TMP(19)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
      *
      *
     C     ELFAF         IFEQ      'A'
     C*** BEGIN CHG ***  07/22/99  ************************************
     C******               Z-ADDELAFRS    TAM,A
     C                   Z-ADD     ELADC         TAM(A)
     C*** END CHG *****  07/22/99  ************************************
     C                   ELSE
     C*** BEGIN CHG ***  07/22/99  ************************************
     C******               Z-ADDELEFRS    TAM,A
     C                   Z-ADD     ELEDC         TAM(A)
     C*** END CHG *****  07/22/99  ************************************
     C                   ENDIF                                                  ELFAF=A
      *
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  ELDBCR IFNE
     C                   ENDIF                                                  ELFAF IFNE
      *
     C                   ENDSR                                                  CALDBR
      ***** END ADD ***** 12/17/07 ***********************************
      *================================================================
**  CON
Manual premium
Incr limit
Deductible
MC credit
Safety credit
Deviation
Subtotal
Exp mod
Addl load
Contr credit
Standard prem
Discount
Total normal
Expense const
Annual prem
Prorata factor
State tax
2nd deviation
Schedule db/cr
2nd Inc limits
Total premium
Subject prem
Surcharge
2nd surcharge
Waiver surcharge
Loss Constant
Safety Fee
1st Assessment
2nd Assessment
3rd Assessment
Foreign Terrorism
Mass ARAP Surcharge
Occ Disease Cvrge
4th Assessment
5th Assessment
Vol Labor Chg
Domestic Terr Chg
Company Factor
6th Assessment                                              12/11/08
7th Assessment                                              12/11/08
2nd Deductible                                              01/08/04
Retro adjustment
Retrospective premium
Facultative Reinsurance                                     07/06/06
Short rate penalty
Harbor Exposure Fee
Standard contribution                                        01/14/10
Annual contribution                                          01/14/10
Manual contribution                                          01/14/10
Total contribution                                           01/14/10
Subject cont                                                 01/14/10
Retrospective cont                                           01/14/10
HCN Credit                                                   11/10/10
Unearn prem surcharge                                        07/13/11
CON,55 future use
CON,56 future use
CON,57 future use
CON,58 future use
CON,59 future use
CON,60 future use
** Description--------------CDAN                           11/10/10
103F3=Exit                  YYN
106F6=Emplr detail          NNN
107F7=Bill revision         NNN
109F9=Payrolls              NNN
111F11=Change info          YYY
112F12=Cancel               YYY
114F14=DARTS                YYY
115F15=Loss summary         YYY
116F16=Fees                 YYY
118F18=HCN Program          NNN
203F3=Exit                  YYN
303F3=Exit                  YYN
*
