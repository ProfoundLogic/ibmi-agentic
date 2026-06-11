      * (c) 2025 Sapiens. All Rights Reserved.                             *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     h bnddir('WTBNDDIR')
     H/define CONVERT
     H***************************************************************
     H/TITLE W7167R - REINSTATEMENT NOTICE
     H***************************************************************
     H*
     H*   PROGRAM DESCRIPTION:
     H*      THIS PROGRAM PRINTS THE REINSTATEMENT NOTICE FOR NCCI
     H*      REPORTING.
     H*
     H*   CALLED FROM:
     H*      W7168C
     H*
     H*   CALLS TO:
     H*      NONE
     H*
     H*   CREATED: 02/23/95      PROGRAMMER: 11       PROJECT: WC396
     H***************************************************************
     H*
     H*     INDICATOR   FUNCTION DESCIPTION
     H*        20       REVISED REINSTATEMENT
     H*        60       FILE CHAIN
     H*        70       EMPLOYER ADDRESS 2 NOT BLANK
     H*        80       FUND ADDRESS 2 NOT BLANK
     H*        90       DBA NAME IS NOT EQUAL TO BLANKS
     H*
     H*****************************************************************
     H* REVISIONS:
      *                                                               *
      * 07/28/25 C:062 Project: 153WBJ                                *
      *                Write A/R record for reinstatement.            *
      *                                                               *
      * 07/11/25 C:300 Support: 119359 (SUNZ)                         *
      *                Master policy canceled in error, unable to reinstate
      *                Fix: Move WC2956 changes outside of TRANS subroutine
      *                                                                                            *
      * 02/26/24 C:340 Support: WC2956                                                             *
      *                Reset Flag - Cancellation/Reinstatement Process                             *
      *                             Control                                                        *
     H*                                                               *
     H* 01/24/24 C:300 Support: 114919 (AmWins)                       *
     H*                Deletion of division being overridden by Cx/Rn *
     H*                Fix: Before clearing Cancellation Effective date
     H*                in WMEMP, check WDCXP file for any cancellation*
     H*                record exists for that division                *
     H*                Also update Policy Status in QUOTE Tracking File
     H*
     H* 09/19/23 C:079 Support: 112968 (148)                                                       *
     H*                Specifically update LDA using the name svLda                                *
     H*
     H* 08/15/23 C:062 Support: 113384 (129)                                                       *
     H*                Pass "billing" for invoice type.                                            *
     H*                                                               *
     H* 06/29/22 C:340 Support: WC2854                                                             *
     H*                Invoice Cloud:  Retain Payment Ref#                                         *
     H*                                                                                            *
     H* 02/22/22 C:62  Project: 150WFN                                *
     H*                Use new GetNameAddress.                        *
     H*                                                               *
     H* 09/09/21 C:079 Project: WC2663                                *
     H*                Invoice Cloud Changes if AR was not deleted and*
     H*                therefore not in WMXAP                         *
     H*                                                               *
     H* 03/25/21 C:079 Project: WC2663                                *
     H*                Invoice Cloud Changes                          *
     H*                                                               *
     H* 02/10/20 C:298 Project: WC2704                                *
     H*                Cancellation Output Configuration - Print      *
     H*                for All Divisions or State Report Only.        *
     H*                                                               *
     H* 09/16/19 C:300 Support: 83101                                 *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Adding week ending date into WXC7P/C7WEDT      *
     H*
     H* 02/16/18 C:136 Support: 64432 (FCC)
     H*                Save Output from OutputWindow in SPILETTER
     H*                uses position 190 of LDA. Adjusted ldMassFile
     H*                to use different positions of the LDA even
     H*                though mass reinstatements isn't used yet.
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*
     H* 03/28/17 C:65  PROJECT: SY371                                 *
     H*                Populate fein into scriptura workfile.         *
     H*                                                               *
     H* 03/17/17 C:65  PROJECT: SY371                                 *
     H*                Populate formatted name/address fields for     *
     H*                Scriptura output.                              *
     H*                                                               *
     H* 02/28/17 C:65  PROJECT: SY371                                 *
     H*                Update category on master record for mass      *
     H*                run to allow proper work file data program     *
     H*                to be called.                                  *
     H*                                                               *
     H* 01/26/17 C:65  Project: SY371                                 *
     H*                Pass XML Flow file name out to call S4401C     *
     H*                later in the process.                          *
     H*                                                               *
     H* 01/13/17 C:136 Project: SY371                                 *
     H*                Don't abend if Scriptura fails on PDSDEV       *
     H*                                                               *
     H* 01/12/17 C:65  Project: SY371                                 *
     H*                Populate CX reason fields in same manner as    *
     H*                the cancellation program.                      *
     H*                                                               *
     H* 12/19/16 C:65  Project: SY371                                 *
     H*                Check WSCRP for "All Groups" and "All States"  *
     H*                records when not finding setup records for     *
     H*                the specific group or state.                   *
     H*                                                               *
     H* 11/04/16 C:136 Project: SY371                                 *
     H*                Disable spool and DTM output if processing     *
     H*                Scriptura only                                 *
     H*                                                               *
     H* 10/06/16 C:65  Project: SY371                                 *
     H*                Request XMLPrep record for Scriptura if needed *
     H*                and update into new work file.  Populate mass  *
     H*                request XML file name into lda to allow        *
     H*                different programs to link documents together. *
     H*                                                               *
     H* 12/03/15 C:65  Project: 45WKP                                 *
     H*                If processing a policy effective prior to      *
     H*                1/1/2016 for LCTA then print/use old carrier#. *
     H*                                                               *
     H* 02/15/13 C:136 SUPPORT: WA045 81640                           *
     H*                CORRECT LOGIC FOR WRITING TO SEQ# FOR          *
     H*                WCPOLS REPORT                                  *
     H*                                                               *
     H* 01/22/13 C:287 Seq#: SY266_OVR                                *
     H*                Added DFTACTGRP and ACTGRP compile options     *
     H*                                                               *
     H* 06/21/12 C:282 Project: WC2086                                *
     H*                Reinstatement Reason Code added in system      *
     H*                table value.                                   *
     H*                                                               *
     H* 10/17/11 C:278 Project: SY195                                 *
     H*                QDLS migration to IFS                          *
     H*                Add Two Parameters with command.               *
     H*                                                               *
     H* 09/29/10 C:220 PROJECT:WC1775                                 *
     H*                - Added new functinality for A/R calculations  *
     H*                per new group level control setting            *
     H*                                                               *
     H* 08/09/10 C:191 Project: 71WKU                                 *
     H*                Restore payments future of the cancellation    *
     H*                date from the cancellation A/R hold file       *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -A/R records restored with holding records      *
     H*                 written on the same day will delete holding   *
     H*                 records.                                      *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records with new posting dates.       *
     H*                                                               *
     H* 09/17/09 C:220 Seq#: 69067                                    *
     H*                 Duplicate audit tracking record.              *
     H*                                                               *
     H* 04/07/09 C:220 SUPPORT: 66684                                 *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP             *
     H*                                                               *
     H* 02/19/09 C:220 Project: WC1664                                *
     H*                Fixed for the runnig Reinst properly           *
     H*                                                               *
     H* 02/09/09 C:33  Project: 136WDQ                                *
     H*                Add agent phone #.                             *
     H*                                                               *
     H* 01/21/09 C:220 Project: WC1664                                *
     H*                AUDIT TRACKING - TRACKING FROM EXPIRATION AUDIT*
     H*                 TO CANCELLATION AUDIT                         *
     H*                                                               *
     H* 07/16/07 C:190 Project: 93WGX                                 *
     H*                WHEN CREATING A WMSHP RECORD, WRITE A 'XX'     *
     H*                TO THE STATE ARRAY IF THE EXPERIENCE RATING IS *
     H*                INTERSTATE  ('1') OR INTERSTATE AND INTRASTATE *
     H*                ('2')  FOR NCCI SUBMISSION.                    *
     H*                                                               *
     H* 06/06/07 C:132 Project: 102WEF  (REVISED)                     *
     H*                Remove the client specific code that loaded    *
     H*                CRRCNL date as per revised specifications.     *
     H*                                                               *
     H* 05/29/07 C:136 SUPPORT: WA114 58478                           *
     H*                ALLOW REINSTATEMENT OF RENEWAL YEAR TO CLEAR   *
     H*                CX DATE IN EMPLOYER MASTER.                    *
     H*                                                               *
     H* 05/03/07 C:132 Project: 102WEF                                *
     H*                Cancellation/Reinstatement Back to Base-to     *
     H*                display PRODUCER # on Cancellation Notice.     *
     H*                                                               *
     H* 01/22/07 C:33  Seq#: WA109/55378                              *
     H*                Assign the PDF name in this program, prior to  *
     H*                 resolving to DARTS.                           *
     H*                                                               *
     H* 11/15/06 C:61  PROJECT 139WCI                                 *
     H*                If PEO group, print PEO name / address info.   *
     H*                                                               *
     H* 11/03/06 C:136 SEQ#: WA071 55489 (FOR 71WHE)                  *
     H*                CLIENT SPECIFIC - POPULATE GROUP SELF INSURED# *
     H*                                                               *
     F* 09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE)                  *
     H*               -CLIENT SPECIFIC - CHG 'POLICY#' TO 'MEMBER #'  *
     H*               -CLIENT SPECIFIC - SPECIAL FORMAT FOR POLICY #  *
     H*                                                               *
     H* 06/23/06 C:168 PROJECT 129WNN (PART 2)                        *
     H*              - remembers the basic spool file property        *
     H*                                                               *
     H* 05/23/06 C:168 PROJECT 129WNN                                 *
     H*              - Retrieved the client id to either use the value*
     H*                of field SVAOUT or EXAOUT, if clt=129 and agt#=1
     H*                SVAOUT = *blanks                               *
     H*                                                               *
     H* 05/22/06 C:65  PROJECT: WC1419                                *
     H*                DO NOT WRITE WCPOLS WMSHP RECORD IF NO WMSHP   *
     H*                POLICY RECORD HAS BEEN WRITTEN YET. DELETE     *
     H*                OTHER 'C' AND 'R' TYPE RECORDS AS WELL.        *
     H*                                                               *
     H* 02/22/06 C:136 SUPPORT WA099 51246                            *
     H*                POPULATE ORIGINATING DIVISION FIELD IN         *
     H*                WMXAP (CANCELLED A/R HOLDING FILE)             *
     H*                                                               *
     H* 01/09/06 C:168 PROJECT 129WNI                                 *
     H*              Added code to fill the following fields for      *
     H*              document printing                                *
     H*              - Agent mailing address                          *
     H*              - Agent mailing city/state/zip                   *
     H*              - Employer contact #                             *
     H*                                                               *
     H* 12/20/05 C:61  SUPPORT WC1359 129WLT WA132 50423              *
     H*                Record name of resolved document for REINFRM   *
     H*                in DARTS remark correctly as .PDF.             *
     H*                                                               *
     H* 09/02/05 C:61  PROJECT WC1359 129WLT                          *
     H*              - Resolve state documents to DARTS in            *
     H*                .PDF format.                                   *
     H*              - Print all documents using W7167C3.             *
     H*              - Route printed documents to Output queues       *
     H*                (Member, Agent, State) if specified.           *
     H*                                                               *
     H* 03/23/05 C:53  PROJECT: WC1196                                *
     H*                CLEAR MASS AUDIT REQUEST FLAG                  *
     H*                                                               *
     H* 11/30/04 C:65  PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*                                                               *
     H* 08/03/04 C:61  PROJECT WC966                                  *
     H*                Do not clear CX date from employer master      *
     H*                record if processing reinstatement that was    *
     H*                and automatic flat CX for renewal year.        *
     H*                                                               *
     H* 05/14/04 C:65 PROJECT: WC1202                                 *
     H*               STORE EXTENDED EMPLOYER NAMES FOR REPORTING.    *
     H*                                                               *
     H* 05/14/04  C:61 SUPPORT WA081 42370                            *
     H*                Do not save reprinted documents.               *
     H*                                                               *
     H* 02/12/04  C:35   PROJECT: 114WGH                              *
     H*                  INCLUDE 1 BLANK BETWEEN ADDRESS AND CITY ON  *
     H*                  CANCELLATION TRANSACTION TO NCCI.            *
     H*                                                               *
     H* 01/17/04 C:61  PROJECT WC577                                  *
     H*                Clear WX@#P before use by query W7167Q.        *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP   *
     H*                and WMXAP.                                     *
     H*                                                               *
     H* 07/02/03 C:61  PROJECT WC577                                  *
     H*                Load CRLSID with Employer Leasing ID (EMLSID)  *
     H*                                                               *
     H* 02/11/03 C:61  PROJECT WC1109                                 *
     H*              - Load Unemployment Number.                      *
     H*              - Load Comment fields.                           *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                                                               *
     H* 07/03/02 C:61  PROJECT WC1081                                 *
     H*                Print the Federal Name on reinstatement notice.*
     H*                                                               *
     H* 07/01/02 C:61  PROJECT 91WGS WC1086                           *
     H*                If audit tracking record is deleted, then      *
     H*                attach DARTS remark to employer.               *
     H*                                                               *
     H* 08/20/01 C:65  PROJECT: SUPP#28588                            *
     H*                WRITE A/R RECORDS BACK WITH A UNIQUE TIME      *
     H*                STAMP.                                         *
     H*                                                               *
     H* 04/30/01 C:65  PROJECT: 114WAK                                *
     H*                COPY THE STATE LIST FROM THE WMSHP POLICY      *
     H*                RECORD INTO THE WMSHP CX RECORD.               *
     H*                                                               *
     H* 06/13/01 C:61  PROJECT WC1038                                 *
     H*                Add seq number WXC#P.                          *
     H*                                                               *
     H* 11/15/00  C:35   PROJECT: SUPPORT WA090 #25043                *
     H*                  DO NOT WRITE WCPOLS RECORDS WHEN PROCESSING  *
     H*                  A REPRINT.                                   *
     H*                                                               *
     H*  09/08/00 C:61  PROJECT 109BL
     H*                 Remove incorrect code.
     H*
     H*  08/04/00 C:61  PROJECT WC1002
     H*               - Load CRLAPS with 'Y' to indicate a lapse in
     H*                 coverage if reinstatement date is not equal
     H*                 to cancel date.
     H*               - Load NCCI CX reason code to CRCRSN.
     H*
     H*  06/12/00 C:61  PROJECT WC984
     H*               - If this is a renewal policy then load the
     H*                 original policy number and effective date.
     H*               - Load the effective date (CXCXDT) of the
     H*                 cancellation being reinstated into CRRCNL in
     H*                 the format MMDDCCYY.
     H*
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 04/24/00  C:35   PROJECT: SUPPORT WA096 #22613                *
     H*                  ONLY WRITE WCPOLS RECORDS IF PROCESSING THE  *
     H*                  STATE REPORTING DIVISION.                    *
     H*                                                               *
     H*                                                               *
     H* 12/06/99 C:118 SUPPORT: WA048/20840                           *
     H*                Modify the test of the carrier policy number   *
     H*                to see if it is an old-format PDS policy       *
     H*                number.                                        *
     H*                                                               *
     H* 09/02/99 C:61  PROJECT WC920                                  *
     H*                Concatenate comments CXRCM1 and CXRCM2 from    *
     H*                WDCXP onto DARTS comment CMCOMM after it is    *
     H*                loaded with DACOMM.                            *
     H*                                                              *
     H* 07/12/99  C:87   PROJECT: WC900                              *
     H*                  If records exist in WMXAP for the policy,   *
     H*                  determine write transactions back into A/R  *
     H*                  Delete records processed from WMXAP.        *
     H*                                                              *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 04/29/99  C:35   PROJECT: SUPPORT WA096 #17258               *
     H*                  ALWAYS WRITE A NEW RECORD TO THE NCCI       *
     H*                  CANCELLATION TRANSACTION FILE.              *
     H*                                                              *
     H* 10/12/98  C:35   PROJECT: WC839                              *
     H*                  LOAD THE CANCELLATION ISSUE DATE FOR STATE  *
     H*                  FORM.                                       *
     H*                                                              *
     H* 09/30/98  C:35   PROJECT: SUPPORT CLT 077                    *
     H*                  PROGRAM WAS GENERATING DOCUMENTS ONLY IF    *
     H*                  BOTH THE FLAG FOR OFFICEVISION (SYOFIF) AND *
     H*                  THE FLAG FOR WCPOLS (WCFL03) WERE = 'Y'.    *
     H*                  THIS HAS BEEN CHANGED TO USE SYOFIF TO      *
     H*                  CONTROL PRINTING OF DOCUMENTS ONLY.  WCFL03 *
     H*                  WILL CONTROL WCPOLS RECORD GENERATION ONLY. *
     H*                                                              *
     H* 09/13/98  C:35   PROJECT: WC839                              *
     H*                  READ STATE CX/REIN DEFINITION FILE USING    *
     H*                  JURISDICTION STATE AND POLICY EFFECTIVE     *
     H*                  DATE TO GET THE STATE REIN FORM FOLDER AND  *
     H*                  DOCUMENT.  PASS THIS FOLDER AND DOCUMENT    *
     H*                  TO W7167C2 SO IT WILL PRINT WITH THE NCCI   *
     H*                  REINSTATEMENT DOCUMENT.                     *
     H*                  ALSO ADD CODE TO LOAD THE GROUP ADDRESS AND *
     H*                  PHONE NUMBER TO THE CANCELLATION/           *
     H*                  REINSTATEMENT TRANSACTION FILE.             *
     H*                                                              *
     H* 08/29/98  C:35   PROJECT: WC862                              *
     H*                  USE CARRIER POLICY NUMBER IN PLACE OF PDS   *
     H*                  POLICY NUMBER IF RECORD EXISTS AND IT IS    *
     H*                  NOT BLANK.  IF CARRIER POLICY NUMBER IS     *
     H*                  USED, ALSO LOAD PDS POLICY NUMBER IN CX     *
     H*                  TRANSACTION/DOCUMENT RECORD.                *
     H*                                                              *
     H* 07/07/98  C:35  PROJECT: SUPPORT WA095 12536                 *
     H*                 USE ALTERNATE COMPANY NAME FROM THE FUND     *
     H*                 MASTER FILE (IF IT IS NOT BLANK) IN PLACE    *
     H*                 OF COMPANY NAME FOR ISSUING OFFICE.          *
     H*                                                              *
     H* 06/25/98  C:87  PROJECT: WC630                               *
     H*                 REMOVE SYSTEM CONTROL FLAG PROCESSING FOR    *
     H*                 OPTION 41=AUDIT TRACKING. IT IS A PART OF    *
     H*                 BASE PRODUCT, NO LONGER A SEPARATE MODULE.   *
     H*                                                              *
     H* 03/04/98  C:87  PROJECT: WC630                               *
     H*                 ADDED PROCESSING FOR AUDIT TRACKING BASED ON *
     H*                 SYSTEM CONTROL FLAG WCFL12                   *
     H*                                                              *
     H* 02/23/98  C:85  Project: WC577                               *
     H*                 ADDED FIELD CMSMCD, LOADED FROM CTSMCD.      *
     H*                                                              *
     H* 02/10/98  C:35  PROJECT: WC735                               *
     H*                 CHANGED TO USE ADMIN DARTS CONTROL FILE FOR  *
     H*                 RESOLVE DARTS REMARK CODES.                  *
     H*                 ALSO CORRECTED 8-DIGIT DATE IN DARTS REMARK  *
     H*                 TEXT.                                        *
     H*                                                              *
     H* 01/06/98  C:65  PROJECT: WC808                               *
     H*                 ADD AUTO-FOLDER SWITCHING FOR MAX DOCS.      *
     H*                                                              *
     H* PLEASE ADD TO TOP OF REVISIONS                               *
     H****************************************************************
      *
      *
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     UF   E           K DISK
     FWDELL     UF   E           K DISK
     F                                     infds(dbfds)
     FWSCRL     IF   E           K DISK    rename(pcr01:pcr00)
     F                                     prefix(ST)
     FWXC7P     O    E             DISK    USROPN
     FXMLPREL1  UF   E           K DISK
     FXMLTEML1  UF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     F*   CANCELLATION FILES
     C*** Beg Chg *** 02/26/24 *** WC2956 **********************************
     F*WDCXL     IF   E           K DISK
     FWDCXL     UF   E           K DISK
     C*** Beg Chg *** 02/26/24 *** WC2956 **********************************
     FWDXHL     IF   E           K DISK
      *** Beg Add ***** 01/24/24 ************************************
     fwdcxl10   if   e           k disk    rename(pcx01:pcx10)
     f                                     prefix(p10_)
      * Quote Tracking File
     FWAQTL12   IF A E           K DISK    rename(pqt01:pqt12)
      *** End Add ***** 01/24/24 ************************************
     F*   DARTS FILES
     FWMEXL     IF   E           K DISK
     F*                                                                *********
     F*
     FWMAUL2    UF   E           K DISK
     FWMAUL6    UF   E           K DISK
     F                                     RENAME(PAU01:PAU02)
     F*                                                                *********
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
      *
      *  CX Reason Table
     FWTCRL     IF   E           K DISK
     F*   OUTPUT FILES
      *
     FWXC#L3    IF A E           K DISK
     F                                     RENAME(PCR01:PC#01)
      *
     FWMSHP     O    E             DISK
     FWMSHL1    UF   E           K DISK
     F                                     RENAME(PSH01:PSH02)
     FWTGTL     IF   E           K DISK
     FWX#@P     O    E             DISK    USROPN
     FWMRPL     IF   E           K DISK
     FWTXCL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F*
     FWMEAL14   UF A E           K DISK
     FWMXAL     UF   E           K DISK
     FWMA#L1    UF   E           K DISK
     F                                     RENAME(PEA01:PEA0#)
      *
      *  Audit Company Table
     FWTACL     IF   E           K DISK
      *
      *  User parameter
     FSMUPL1    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWXAGP     IF A E           K DISK    USROPN
     F                                     RENAME(PAG01:PAG01X)
      *
      *  Output Routing
     FSMORL     IF   E           K DISK
      *
     FWMECL1    IF   E           K DISK
     F*
     FWMAPL     IF   E           K DISK
     FWMAHL1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMXAL2    IF   E           K DISK
     F                                     RENAME(PXA01:PXA02)
     FWDRRL     IF   E           K DISK    RENAME(PRR01:PRR00)
     F*** Begin Add *** 07/28/25 *******************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
     F*** End   Add *** 07/28/25 *******************************************
     F*
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D DA              S             63    DIM(2) CTDATA PERRCD(1)              DARTS COMMENT
      *
      *
      *
     D D1              C                   CONST('Audit Tracking reco-
     D                                     rd updated by')
     D D2              C                   CONST('due to reinstatemen-
     D                                     t.')
     D D3              C                   CONST('Audit Company:')
     D D4              C                   CONST('Audit Rep:')
     D D5              C                   CONST('Audit Req Date:')
     D CHGNUM          C                   CONST('WA003R')
     D RECOV           C                   CONST('Reinstatement Cover-
     D                                     Letter attached')
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     d outputFile      s                   like(XPFILE)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D isUseScript     S              1n   inz
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('RR')
     d order           s              5  0
     d templateFile    s                   like(XTXMLT)
     d xxTemplate      s             60
     d xxPath          s            250
     d processType     c                   const('Reinstatements')
     d xxstRptDv       s              5  0
     d*** Beg Add *** 03/25/21 **************************************
     d inJson          s          32000    Varying
     d n               s              3  0
     d isInvoiceCloud  s              1n   inz
     d o               s              3  0
     d isDupInv#       s              1
     d inKey           s             20
     d*** End Add *** 03/25/21 **************************************
     d*** Beg Add *** 09/09/21 **************************************
     d cnt2            s              3  0
     d intInvDlt       s             10  0
     d svXAnInv        s             10  0
     d*** End Add *** 09/09/21 **************************************
     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x#RRN           s              9  0 inz(0)
     d*
     D/copy sPrXML
     D/copy wPrEmplr
     d*** Beg Add *** 03/25/21 *******************************************
     d/Copy sPrModule
     d/Copy wPrPmtSvc
     d/Copy wPrAr
     d*** End Add *** 03/25/21 *******************************************
     D*
     D svlda           ds          1024    dtaara(*lda)
     D ldMassFile                    60    Overlay(svlda:129)
     D ldOutputFile                  60    Overlay(svlda:191)
     D S4401C          pr                  ExtPgm('S4401C')
     D   xmlName                     61    const
     D ObjExists       pr             1n
     D  object                       10    const
     D  objType                       7    const
     D                 ds
     D  amn                    1    540    dim(9)
     D  ##amn1               001    060
     D  ##amn2               061    120
     D  ##amn3               121    180
     D  ##amn4               181    240
     D  ##amn5               241    300
     D  ##amn6               301    360
     D  ##amn7               361    420
     D  ##amn8               421    480
     D  ##amn9               481    540
     D*** Begin Add *** 02/22/22 *******************************************
     d #1co#           s              3
     d #1fnd           s              3
     d #1emp#          s              9
     d #1div           s              5
     d ##odr           s              1
     d ##aodr          s              1
     d ##nodr          s              1
     d ##emn1          s             50
     d ##emn2          s             50
     d ##emn3          s             50
     d ##emn4          s             50
     d ##emn5          s             50
     d ##emn6          s             50
     d ##emn7          s             50
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d ##dbaf          s              1
     d ##peol          s              3  0
     d ##al2           s              3  0
     d ##nl2           s              3  0
     d ##fl2           s              3  0
     d ##pl2           s              3  0
     d ##cl2           s              3  0
     d QQname1         s            120
     d QQname2         s            120
     d QQname3         s            120
     d QQname4         s            120
     d QQPEOExt        s             10
     d QQfein          s              9  0
     D*** End   Add *** 02/22/22 *******************************************
      *
     D*** DATA STRUCTURE FOR DARTS RECORD
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D                 DS
     D  DACOMM                 1     35
     D  XXMM                  26     27
     D  XXSL1                 28     28
     D  XXDD                  29     30
     D  XXSL2                 31     31
     D  XXCC                  32     33
     D  XXYY                  34     35
     D                 DS
     D  X1DATE                 1      8  0
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1CC                   5      6
     D  X1YY                   7      8
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5)                               CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  DD                     1     20
     D                                     DIM(20) ASCEND                       DATE DESC
     D  XXDTDS                 1     20
     D                 DS
     D  XXPOL                  1     18
     D  XXOPOL                 1     11
     D  XXOFND                 1      3
     D  XXOEMP                 4      8
     D  XXOFYR                 9     11
     D  XOBLNK                12     18
     D                 DS
     D  SST                    1     30
     D                                     DIM(15)
     D  SHST01                 1      2
     D  SHST02                 3      4
     D  SHST03                 5      6
     D  SHST04                 7      8
     D  SHST05                 9     10
     D  SHST06                11     12
     D  SHST07                13     14
     D  SHST08                15     16
     D  SHST09                17     18
     D  SHST10                19     20
     D  SHST11                21     22
     D  SHST12                23     24
     D  SHST13                25     26
     D  SHST14                27     28
     D  SHST15                29     30
     D                 DS
     D  SSS                    1     15
     D                                     DIM(15)
     D  SHSS01                 1      1
     D  SHSS02                 2      2
     D  SHSS03                 3      3
     D  SHSS04                 4      4
     D  SHSS05                 5      5
     D  SHSS06                 6      6
     D  SHSS07                 7      7
     D  SHSS08                 8      8
     D  SHSS09                 9      9
     D  SHSS10                10     10
     D  SHSS11                11     11
     D  SHSS12                12     12
     D  SHSS13                13     13
     D  SHSS14                14     14
     D  SHSS15                15     15
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  WXAGPX                 1    256
     D  WXCO#                  1      3
     D  WXAGT#                 4      8
     D  WXJOBN                 9     18
     D  WXJOBU                19     28
     D  WXJOB#                29     34
     D  WXSPLN                35     44
     D  WXSPL#                45     48
     D  WXOUTQ                49     58
     D  WXSTAT                59     59
     D  WXHDR                 60     60
     D  WXSPST                61     70
     D  WXSPPG                71     74
     D  WXSPCY                75     78
     D  WXSPFR                79     88
     D  WXPGM                 89     98
     d*** Beg Add *** 03/25/21 **************************************
     d  arrInvoice#                  10  0 Dim(999)
     d*** End Add *** 03/25/21 **************************************
     d*** Beg Add *** 09/09/21 **************************************
     d  arrInv#Dlt                   10  0 Dim(999)
     d*** End Add *** 09/09/21 **************************************
     I*
      *
     IPCR01         01
     I              CRCO#                       $CRCO#
     I              CRKEY                       $CRKEY
     I              CRDESC                      $CRDSC
     I              CRSTAT                      $CRSTA
     I              CRUSER                      $CRUSR                           =
     I              CRPGM                       $CRPGM
     I              CRTIME                      $CRTIM
     I              CRDT                        $CRDT
     I              CRNCCI                      $CRNCC
     I              CRST                        $CRST
     I              CRNOTC                      $CRNTC
     I              CRADDM                      $CRADM
     I              CRRFEE                      $CRFEE
     I              CREFDT                      $CREDT
     I              CRPRTC                      $CRPRT
     I              CRCERT                      $CRCRT
     I              CRAREI                      $CRARE
     I              CRRDAY                      $CRRDY
     I              CRPCNT                      $CRPCT
      *
      *
      *
      *================================================================
      *  KLISTS
      *================================================================
      *
      *  Output Routing
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
      *
      *  Audit Company Table
      *
     C     ACKY          KLIST
     C                   KFLD                    AUCO#
     C                   KFLD                    AUACO
     C*
     C*  COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     EMKY1         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *
      * CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     RPKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C*
     C*  CANCELLATION DETAIL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXREQ#
     C*
     C     CXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C* CANCEL HISTORY FILE
     C*
     C     XHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  CX Reason Table
      *
     C     $CRKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
      *
     C     $CRKY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    KKKEY
      *
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C* WORK COMP CONTROL FILE
     C     WCKY          KLIST
     C                   KFLD                    XCKEY             2
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
      ********
     C**DARTS*FILE
     C*
     C*  CNCL/REIN TRANS FILE.
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    ##TYP             1
      *
     C     CRKY1         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXFYR
     C                   KFLD                    CXRDAT
      *
      *
      * ADMIN DARTS CONTROL FILE
      *
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     EXKY1         KLIST
     C                   KFLD                    AUCO#
     C                   KFLD                    AUFND
      *
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     AUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * KEY LISTS FOR THE CANCELLATION/REINSTATEMENT STATE FILE
      *
     C     XCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFFD
      *
     C     XCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
     C     XCKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
      *
     C     XCKY4         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
      *
      * KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C* Key list for A/R detail file
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARCD
     C                   KFLD                    XXPODT
     C*
     C*
     C     EXKY2         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1EMP#
     C                   KFLD                    X1DIV
     C                   KFLD                    X1PODT
     C*
     C     EXKY3         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1EMP#
     C                   KFLD                    X1DIV
     C*
     C*
     C* Key list for Cancellation A/R detail file
     C     XAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C*
     C* Key list for Employer Contacts
     C*
     C     MEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    ##TYP1            1
      *
      *
      * SUBMISSION HISTORY FILE
      *
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
      *
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
      *
      * GENERIC TABLE FILE KEY
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    AAAGT#
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    AHPROD
      *
      *===============================================================
      *  *LIKE DEFNS
      *===============================================================
     C     *LIKE         DEFINE    CRLSID        XXLSID
     C     *LIKE         DEFINE    AUCO#         KKCO#
     C     *LIKE         DEFINE    CRSEQ#        SVSEQ#
     C     *LIKE         DEFINE    $CRKEY        KKKEY
     C     *LIKE         DEFINE    $CREDT        KKEFDT
     C     *LIKE         DEFINE    CXCO#         XXCO#
     C     *LIKE         DEFINE    CXFND         XXFND
     C     *LIKE         DEFINE    CXFYR         XXFYR
     C     *LIKE         DEFINE    CXEMP#        XXEMP#
     C     *LIKE         DEFINE    CXDIV         XXDIV
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C     *LIKE         DEFINE    CXRDAT        XXRDAT
     C     *LIKE         DEFINE    XCEFDT        XXEFFD
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    CXDIV         KKDIV
     C     *LIKE         DEFINE    EAARCD        XXARCD
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EAPODT        SYSDT
     C     *LIKE         DEFINE    EAPODT        ZZPODT
     C     *LIKE         DEFINE    EAPODT        SVCXDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    XACO#         X1CO#
     C     *LIKE         DEFINE    XAFND         X1FND
     C     *LIKE         DEFINE    XAFYR         X1FYR
     C     *LIKE         DEFINE    XAEMP#        X1EMP#
     C     *LIKE         DEFINE    XADIV         X1DIV
     C     *LIKE         DEFINE    XAPODT        X1PODT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQREQ#            5
     C                   PARM                    QQREPR            1            REPRINT? (Y/N)
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQFLOW           61
     C*
     C                   Clear                   QQFlow
      /free
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);

       Clear xmlLink;
       Clear xmlFileName;

       isMassReq = *off;
       massFlag = ' ';

      /end-free
       //*** Beg Add *** 03/25/21 ****************************************
       isInvoiceCloud = IsModEnabled('Invoice Cloud');
       //*** End Add *** 03/25/21 ****************************************
      *
      *  Get the client id
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     Q1USER        CHAIN     PUP02                              60
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      * EDIT MASK API PARAMETER LIST
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *
      *  LINE FORMAT
      *
     C     LINEPL        PLIST
     C                   PARM                    ##LINE          256
     C                   PARM                    ##LEN             3 0
     C                   PARM                    ##REM           256
      *
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQREQ#        XXREQ#
     C                   MOVE      'P'           XXTYPE
     C*
     C                   TIME                    DSDTTM
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C                   Z-ADD     *HIVAL        ##SEQ#
     C                   MOVE      'R'           ##TYP
     C                   MOVE      Q1JOBN        $$JOB#            6
     C                   CLEAR                   P#@01
     C*
     C                   MOVEL     'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              60
     C                   MOVEL     'SY'          ##KEY
     C     SYKY          CHAIN     SSY01                              60
     C*
     C*   GET CANCELLATION DETAIL RECORD
     C*
     C*** Beg Chg *** 02/26/24 *** WC2956 **********************************
     C*    CXKY          CHAIN     PCX01                              60
     C     CXKY          CHAIN(N)  PCX01                              60
     C*** End Chg *** 02/26/24 *** WC2956 **********************************
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*  GET FUND INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        XXFNAM
     C                   ELSE
     C                   MOVEL     FNNAME        XXFNAM           60
     C                   ENDIF
     C*
     C     F1KY          CHAIN     PF101                              60
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         XXCARC
     C                   Z-ADD     *ZERO         XXFEIN
     C                   ELSE
     C                   MOVE      F2CAR#        XXCARC            5 0
     C                   Z-ADD     F2FEIN        XXFEIN            9 0
     C                   ENDIF
      *
      * GET ADMIN DARTS CONTROL RECORD
      *
     C     EXKY          CHAIN     PEX01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PEX01
     C                   ENDIF                                                  EXKY
      /Free
       xxstRptDv = GetStRptDv(xxco#:xxfnd:xxemp#);
       If xxdiv <> xxstRptDv and EXcxstrp = 'Y';
          isUseXml = *off;
          In *Lock *dtaara;
          Clear ldOutputFile;
          Out *dtaara;
       Endif;
      /End-free
      *
      * Don't allow the printing for the first agent.
      *
     C     *LIKE         DEFINE    EXAOUT        SVAOUT
      *
     C     CLTID         IFEQ      '129'
     C     CXAGT#        ANDEQ     1
     C                   MOVEL     *BLANKS       SVAOUT
     C                   ELSE
     C                   MOVEL     EXAOUT        SVAOUT
     C                   ENDIF
      *
      *  Determine Member Output Queue
      *
     C     EXMOUT        IFNE      *BLANKS
     C                   MOVE      EXMOUT        QQOUTQ
     C                   ENDIF                                                  EXMOUT
      *
      *  else User
     C     QQOUTQ        IFEQ      *BLANKS
     C     Q1USER        CHAIN     PUP02                              60
     C  N60              MOVE      UPOUTQ        QQOUTQ
     C                   ENDIF
      *
      *  else Output Routing
     C     QQOUTQ        IFEQ      *BLANKS
     C                   MOVE      '0'           XXPRT             1
     C     ORKY          CHAIN     POR01                              60
     C  N60              MOVE      OROUTQ        QQOUTQ
     C                   ENDIF
      *
      *  else QPRINT
     C     QQOUTQ        IFEQ      *BLANKS
     C                   MOVEL(P)  'QPRINT'      QQOUTQ
     C                   ENDIF
      *
     C*
     C*   GET AGENT NAME
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
     C     AGKY          CHAIN     PAG01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AGNAME        XXANAM
     C     *LIKE         DEFINE    AGARE1        XXAARE
     C     *LIKE         DEFINE    AGPHN1        XXAPHN
     C                   Z-ADD     AGARE1        XXAARE
     C                   Z-ADD     AGPHN1        XXAPHN
      *
      *
     C                   MOVEL(P)  AGNAME        CRANAM
     C                   MOVEL(P)  AGMAD1        CRAMAD
     C     AGMCTY        CAT(P)    ',':0         CRAMCZ
     C                   CAT(P)    AGMST:1       CRAMCZ
     C                   MOVEL(P)  AGMZIP        @@MZIP            5
     C                   CAT(P)    @@MZIP:1      CRAMCZ
      *
      * Get the name for the employer
      *
     C                   MOVEL     '1'           ##TYP1
     C     MEKY          CHAIN     PEC01                              80
     C     *IN(80)       IFEQ      *OFF
     C     ECTITL        IFNE      *BLANKS
     C                   MOVEL(P)  ECTITL        CRECN1
     C                   CAT(P)    ECFSN:1       CRECN1
     C                   ELSE
     C                   MOVEL(P)  ECFSN         CRECN1
     C                   ENDIF
      *
     C     ECMID         IFNE      *BLANKS
     C                   CAT(P)    ECMID:1       CRECN1
     C                   ENDIF
     C                   CAT(P)    ECLSN:1       CRECN1
     C                   ENDIF
      *
      *
     C                   ELSE
     C                   MOVE      *BLANKS       XXANAM           40
     C                   CLEAR                   XXAARE
     C                   CLEAR                   XXAPHN
     C                   ENDIF
      *
     C     AHKY          CHAIN     PAH01                              50
     C     APKY          CHAIN     PAP01                              50
     C                   MOVEL     APPROD        XXPROD            5
     C*
     C*  GET THE EMPLOYER NAME, ADDRESS, CITY, STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXEAD1           40
     C                   MOVE      *BLANKS       XXEAD2           40
     C                   MOVE      *BLANKS       XXECSZ           40
     C                   MOVE      *BLANK        XXLSID
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      EMLSID        XXLSID
     C                   MOVEL     EMFDNM        XXENAM
     C     EMMPFL        IFEQ      'P'
     C                   MOVEL     EMPAD1        XXEAD1
     C                   MOVEL     EMPAD2        XXEAD2
     C                   MOVEL     EMPCTY        XXCITY           25
     C                   MOVEL     EMPST         XXST              2
     C                   MOVEL     EMPZIP        XXZIP5            5
     C                   MOVE      EMPZIP        XXZIP4            4
     C                   ELSE
     C                   MOVEL     EMMAD1        XXEAD1
     C                   MOVEL     EMMAD2        XXEAD2
     C                   MOVEL     EMMCTY        XXCITY
     C                   MOVEL     EMMST         XXST
     C                   MOVEL     EMMZIP        XXZIP5
     C                   MOVE      EMMZIP        XXZIP4
     C                   ENDIF
     C*
     C     XXCITY        CAT(P)    ',':0         XXECSZ
     C                   CAT       XXST:1        XXECSZ
     C                   CAT       XXZIP5:1      XXECSZ
     C     XXZIP4        IFNE      *ZERO
     C                   CAT(P)    '-':0         XXECSZ
     C                   CAT(P)    XXZIP4:0      XXECSZ
     C                   ENDIF
     C*
     C*  GET EMPLOYER EFFECTIVE DATE
     C*
     C     ELKY          CHAIN     PEL01                              60
     C                   eval      x#RRN = dbRRN
     C                   Z-ADD     CXREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##REDT            6 0
     C*
     C                   Z-ADD     CXRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRDAT
     C                   MOVE      *BLANK        ELMRFL
     C                   UPDATE    PEL01
     C*
     C* Use old NCCI Carrier # for LCTA if processing policy prior to 1/1/2016.
     C*
     C                   If        cltid = '045' and ELPfdt < 1160101
     C                   Z-add     33057         F2Car#
     C                   Z-add     33057         xxCarc
     C                   Endif
     C*
     C*** GET THE ISSUING OFFICE (COMPANY FILE) FOR NCCI FORM
     C*
      *
      * IF THE ALTERNATE COMPANY NAME IS NOT BLANK, USE IT INSTEAD OF THE
      * COMPANY NAME.
      *
     C     FNANAM        IFNE      *BLANKS
      *
     C                   MOVEL     FNAZIP        XXZIP5
     C                   MOVE      FNAZIP        XXZIP4
     C     FNANAM        CAT(P)    ',':0         XXISUO           90
     C                   CAT       FNAAD1:1      XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       FNACTY:1      XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       FNAST:1       XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       XXZIP5:1      XXISUO
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXISUO
     C                   CAT       XXZIP4:0      XXISUO
     C                   ENDIF
      *
     C                   ELSE
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL     COZIP         XXZIP5
     C                   MOVE      COZIP         XXZIP4
     C     CONAME        CAT(P)    ',':0         XXISUO           90
     C                   CAT       COADR1:1      XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       COCTY:1       XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       COST:1        XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       XXZIP5:1      XXISUO
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXISUO
     C                   CAT       XXZIP4:0      XXISUO
     C                   ENDIF
     C                   ENDIF                                                  FNANAM IFNE *BLANKS
     C*
     C                   MOVEL     XXANAM        XXAGNM           30
     C                   MOVEL     CXAGT#        XXA#C             5
     C     XXAGNM        CAT(P)    '/':1         XXAOUT           60
     C                   CAT       XXA#C:1       XXAOUT
     C     CLTID         IFEQ      '102'
     C                   CAT       '- Prod':1    XXAOUT
     C                   CAT       ' #':0        XXAOUT
     C                   CAT       XXPROD:1      XXAOUT
     C                   ENDIF                                                  CLTID = 102
     C*
     C*  UPDATE EMPLOYER MASTER AND REMOVE CANCELLATION DATE IF
     C*  REINSTATED
     C*
     C                   Z-ADD     1             X                 2 0
     C     CXCXDT        LOOKUP    CD(X)                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZEROS        CD(X)
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   Z-ADD     DSTIME        EMTIME
      *** Beg Add ***** 01/24/24 ************************************
     C                   MOVE      *blanks       rcdfound          1
      * Set the Cancellation date to previous delete/cancel date
      /Free
       Setgt (EMco#:EMfnd:EMemp#:EMdiv) pcx10;
       Readpe (EMco#:EMfnd:EMemp#:EMdiv) pcx10;
       Dow not %eof(wdcxl10);
          If p10_CXrein <> 'Y';
             CD(X) = p10_CXcxdt;
             rcdfound = 'Y';
          Endif;
          Readpe (EMco#:EMfnd:EMemp#:EMdiv) pcx10;
       Enddo;
       If rcdfound = 'Y';
          Exsr UpdQT;
       Endif;
      /End-Free
      *** End Add ***** 01/24/24 ************************************
     C                   UPDATE    PEM01
     C                   ENDIF
      *
     C*
     C                   ENDIF                                                  EMKY
      *
     C                   UNLOCK    WMEML
     C*
     C* GET LAST CANCELLATION HISTORY RECORD.
     C*
     C*
     C     XHKY          SETGT     PXH01
     C     XHKY          READPE    PXH01                                  61
     C     *IN61         IFEQ      *ON
     C                   MOVE      'N'           XHRVFL
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C* WRITE A DARTS REMARK IF 1)THE NOTICE IS A NOT A REPRINT
     C* & 2) ORIGINAL/REVISED DARTS CODE IS NOT BLANK.
     C*
     C                   MOVE      'N'           XXDRT3            1
     C     QQREPR        IFNE      'Y'
     C     XHRVFL        ANDNE     'R'
     C     EXRECD        ANDNE     *BLANK
     C     QQREPR        ORNE      'Y'
     C     XHRVFL        ANDEQ     'R'
     C     EXRRCD        ANDNE     *BLANK
     C                   MOVE      'Y'           XXDRT3            1
      *
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C  N60RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFFD
     C     XCKY          SETGT     PXC01
     C  N60XCKY2         READPE    PXC01                                  60
      *
     C                   ENDIF
     C*
     C*** ISSUE NCCI REINSTATEMENT NOTICE FORM
     C*
     C     WCFL03        IFEQ      'Y'
     C     SYOFIF        OREQ      'Y'
     C                   EXSR      TRANS
     C                   ENDIF
     C*
     C*** Beg Add *** 07/11/25 *************************************
      * Reset Pending/Processing Flag
       Chain (XXCO#:XXFND:XXFYR:XXEMP#:XXDIV:XXREQ#) PCX01;
       If %Found(wdcxl);
          CXpdPr = ' ';
          Update Pcx01;
       EndIf;
     C*** End Add *** 07/11/25 *************************************

     C     AUKY          CHAIN     PAU01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  If a cancellation audit tracking record is found update AUSTAT
      *
     C                   MOVE      'I'           AUSTAT
     C     AUCXFL        IFEQ      'Y'
      *
      * Attach DARTS remark to Employer if audit tracking record deleted
      *
     C                   EXSR      DRT2
      *
     C                   UPDATE    PAU01
     C     AUKY          CHAIN     PAU02                              60
     C     *IN60         IFEQ      *OFF
     C     *IN60         DOWEQ     *OFF
     C     AUSTAT        IFEQ      'I'
     C     AUCXFL        ANDNE     'Y'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C                   Z-ADD     DSTIME        AUTIME
     C                   MOVE      Q1PGM         AUUSER
     C                   MOVE      Q1PGM         AUPGM
     C                   MOVE      'A'           AUSTAT
     C                   UPDATE    PAU02
     C                   ENDIF                                                  AUCXFL<>'Y'
     C     AUKY          READE     PAU02                                  60
     C                   ENDDO                                                  ENDDO
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  AUCXFL='Y'
     C                   ENDIF                                                  *IN60=*OFF
     C                   Z-ADD     0             XXACDT            8 0
     C*
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     XXEMP#        @@EMP#
     C                   Z-ADD     XXDIV         @@DIV
     C                   Z-ADD     XXFYR         @@FYR
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
     C*
     C     @@PLAN        IFEQ      'A'
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     EXAOID        XXDAYS            5 0
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      XXDAYS        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          XXACDT            8 0
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C                   Z-ADD     @@AREA        CRAARE
     C                   Z-ADD     @@PHON        CRAPHN
     C*
     C*
     C                   SELECT
     C     EXARRI        WHENEQ    'A'
     C     EXARRI        OREQ      ' '
     C*    Determine posting date for a/r (revised interim days + system dt)
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT
     C                   ENDIF
     C*
     C     EXARRI        WHENEQ    'B'
     C*    Determine posting date for a/r (system dt)
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT
     C                   ENDIF
     C*
     C     EXARRI        WHENEQ    'C'
     C*    Determine posting date for a/r (next Installment Date)
     C*
     C*    Process installment date
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTPDT            7 0
     C*    Get the next valid installment date
     C                   Z-ADD     XXTPDT        X1PODT
     C*
     C                   MOVEL     XXCO#         X1CO#
     C                   MOVEL     XXFND         X1FND
     C                   MOVEL     XXFYR         X1FYR
     C                   MOVEL     XXEMP#        X1EMP#
     C                   MOVEL     XXDIV         X1DIV
     C*
     C     EXKY2         SETGT     PXA02
     C     *IN60         DOUEQ     *ON
     C     XAARTY        OREQ      'B'
     C     XAARTY        OREQ      'A'
     C     XAARTY        OREQ      'D'
     C     EXKY3         READE     PXA02                                  60
     C                   ENDDO                                                  *IN60=*ON
     C*
     C     XAPODT        IFNE      0
     C* IF INSTALLMENT DATE EXISTS
     C                   Z-ADD     XAPODT        ZZPODT
     C*
     C                   ELSE
     C* IF INSTALLMENT DATE NOT EXISTS, PROCESS LIKE 'A' MODE
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C     XXACDT        IFNE      *ZERO
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C*    If this is not a reprint then transfer any previously deleted
     C*    a/r records to WMEAP determining new posting dates if necessary.
     C     QQREPR        IFNE      'Y'
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
       //*** Beg Add *** 03/25/21 ****************************************************
       n = 0;
       Clear arrInvoice#;
       //*** End Add *** 03/25/21 ****************************************************
       //*** Beg Add *** 03/25/21 ****************************************************
       cnt2 = 0;
       Clear arrInv#Dlt;
       //*** End Add *** 03/25/21 ****************************************************
     C*
     C*      Read cancellation a/r detail file
     C     XAKY          CHAIN     PXA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C*      If posting date is greater than calc date then write record as
     C*      is, otherwise write/update record with calc date as posting date
     C     XAPODT        IFGT      ZZPODT
     C                   EXSR      HOLDAR
     C                   EXSR      WRTREC
     C                   DELETE    PXA01
     C                   ELSE
     C*
     C                   MOVE      XAARCD        XXARCD
     C                   Z-ADD     ZZPODT        XXPODT
     C     EAKY          CHAIN     PEA01                              61
     C     *IN61         IFEQ      *OFF
     C     EAARCD        ANDNE     F1RCCD
     C                   ADD       XAAMT         EAAMT
     C                   MOVE(P)   Q1PGM         EAPGM
     C                   Z-ADD     SYSDT         EATRDT
     C                   UPDATE    PEA01
     C                   DELETE    PXA01
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ZZPODT        XAPODT
     C                   EXSR      GETINV
     C                   EXSR      WRTREC
     C                   DELETE    PXA01
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   ENDIF                                                  XAPODT>ZZPODT
     C*
      //*** Beg Add *** 03/25/21 ****************************************
      // Track Invoice Numbers
       o = 1;
       isDupInv# = 'N';
       Dow o <= n;
         If arrInvoice#(o) = EAnInv;
            isDupInv# = 'Y';
            Leave;
         Endif;
         o = o + 1;
       Enddo;
       If isDupInv# = 'N';
          n = n + 1;
          arrInvoice#(n) = EAnInv;
       Endif;
      //*** End Add *** 03/25/21 ****************************************
      //*** Beg Add *** 09/09/21 ****************************************
      // See if invoice number was reassigned and load to array for future
      // deletion at IC
       If XANinv <> EANinv;
          If %lookup(XANinv:arrInv#Dlt) = 0;
             cnt2 = cnt2 + 1;
             arrInv#Dlt(cnt2) = XAnInv;
          Endif;
       Endif;
      //*** End Add *** 09/09/21 ****************************************
     C     XAKY          READE     PXA01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      //*** Beg Add *** 09/09/21 ****************************************
      // If there were no records in WMXAP then read through WMEAP
       If n = 0;
          Setll (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pea01;
          Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pea01;
          Dow not %Eof(WMEAL14);
             If eapodt >= CXreDt;
                // Track Invoice Numbers
                o = 1;
                isDupInv# = 'N';
                Dow o <= n;
                   If arrInvoice#(o) = EAnInv;
                      isDupInv# = 'Y';
                      Leave;
                   Endif;
                   o = o + 1;
                Enddo;
                If isDupInv# = 'N';
                   n = n + 1;
                   arrInvoice#(n) = EAnInv;
                Endif;
             Endif;
             Reade (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pea01;
          Enddo;
       Endif;
      //*** End Add *** 09/09/21 ****************************************
     C*
     C                   ENDIF                                                  QQREPR<>'Y'
       //*** Beg Add *** 03/25/21 ***********************************
       // Communicate with Third Party Invoicing to activate if group was set to inactivate
       If isInvoiceCloud and IcGroupOpt(EAco#:EAfnd) and
        (EXicCxInv = 'Y' or EXdlAr = 'Y');
          n = 1;
          Dow arrInvoice#(n) <> 0;
            //*** Begin Chg *** 08/15/23 **********************************
            //*** inJson =
            //***    '{"invoice":"'  + %Editc(arrInvoice#(n):'X') + '",' +
            //***    '"company":"' + %Editc(EAco#:'X') + '",' +
            //***    '"group":"' + %Editc(EAfnd:'X') + '",' +
            //***    '"employer":"' + %Editc(EAemp#:'X') + '",' +
            //***    '"division":"' + %Editc(EAdiv:'X') + '",' +
            //***    '"active":' + '"true"' + ',' +
            //***    '"delete":' + '"false"}';
            inJson = '{"type":"billing",' +
               '"invoice":"'  + %Editc(arrInvoice#(n):'X') + '",' +
               '"company":"' + %Editc(EAco#:'X') + '",' +
               '"group":"' + %Editc(EAfnd:'X') + '",' +
               '"employer":"' + %Editc(EAemp#:'X') + '",' +
               '"division":"' + %Editc(EAdiv:'X') + '",' +
               '"active":' + '"true"' + ',' +
               '"delete":' + '"false"}';
            //*** End   Chg *** 08/15/23 **********************************
            ThirdPartyInvoicing(inJson);
            n = n + 1;
          Enddo;
          FinalizeThirdPartyInvoicing(EAco#:EAfnd);
          Clear arrInvoice#;
       Endif;
       //*** End Add *** 03/25/21 ***********************************
       //*** Beg Add *** 09/09/21 ***********************************
       // Communicate with Third Party Invoicing to activate if group was set to inactivate
       // Delete "Orphan" invoices from IC
       If isInvoiceCloud and IcGroupOpt(EAco#:EAfnd) and
        (EXicCxInv = 'Y' and EXdlAr = 'Y');
          cnt2 = 1;
          Dow arrInv#Dlt(cnt2) <> 0;
            inJson =
               '{"invoice":"'  + %Editc(arrInv#Dlt(cnt2):'X') + '",' +
               '"company":"' + %Editc(EAco#:'X') + '",' +
               '"group":"' + %Editc(EAfnd:'X') + '",' +
               '"employer":"' + %Editc(EAemp#:'X') + '",' +
               '"division":"' + %Editc(EAdiv:'X') + '",' +
               '"active":' + '"false"' + ',' +
               '"delete":' + '"true"}';
             ThirdPartyInvoicing(inJson);
            cnt2  = cnt2 + 1;
          Enddo;
          FinalizeThirdPartyInvoicing(EAco#:EAfnd);
          Clear arrInv#Dlt;
       Endif;
       //*** End Add *** 09/09/21 ***********************************
     C* Call process to generate Scriptura output/Darts if requested.
      /Free
      /End-Free
     C                   If        xmlFileName <> ' ' and massFlag <> 'Y'
     C                   Eval      QQFlow = xmlFileName + '$'
     C                   Endif
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C/EJECT
      *
      *
      *====================================================================
      * Check to see if test document should be used
      *====================================================================
     C     CHKTST        BEGSR
     C*
     C                   MOVE      *OFF          USETST            1
     C*
     C     *DTAARA       DEFINE    WDAILYA       DLYA             80
     C     *LOCK         IN        DLYA
     C                   MOVEL     DLYA          XXENV             3
     C                   OUT       DLYA
     C*
     C     XXENV         IFEQ      'TWC'
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM      'W7167TST'    XXOBJ            10
     C                   PARM      '*DTAARA'     XOTYPE            7
     C                   PARM                    XXERR             1
     C*
     C     XXERR         IFEQ      *OFF
     C     *DTAARA       DEFINE    W7167TST      TSTFLR           60
     C     *LOCK         IN        TSTFLR
     C                   OUT       TSTFLR
     C                   ENDIF
     C*
     C                   CALL      'SRCHKDLO'
     C                   PARM      'REINFRM'     XXDLO            12
     C                   PARM      TSTFLR        XXFLR            32
     C                   PARM      '*DOC'        XDTYPE            4
     C                   PARM                    XXERR             1
     C                   PARM      *BLANKS       XXUNCN          255
     C                   PARM      *BLANKS       XXFSF             1
     C*
     C     XXERR         IFEQ      *OFF
     C                   MOVE      *ON           USETST
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      *
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    XACO#         #ICO#
     C     *LIKE         DEFINE    XAFND         #IFND
     C     *LIKE         DEFINE    XAEMP#        #IEMP#
     C     *LIKE         DEFINE    XADIV         #IDIV
     C     *LIKE         DEFINE    XAFYR         #IFYR
     C     *LIKE         DEFINE    XAPODT        #IDT
     C     *LIKE         DEFINE    XANINV        #IINV#
     C*
     C     CLTID         IFEQ      '025'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      XACO#         #ICO#
     C                   PARM      XAFND         #IFND
     C                   PARM      XAEMP#        #IEMP#
     C                   PARM      XADIV         #IDIV
     C                   PARM      XAFYR         #IFYR
     C                   PARM      XAPODT        #IDT
     C     XANINV        PARM                    #IINV#
     C*
     C                   ENDIF
       //*** Beg Add *** 09/09/21 ****************************************
       Else;
          // Get invoice#
          If isInvoiceCloud and IcGroupOpt(XAco#:XAfnd);
             svXAnInv = XAnInv;
             XAnInv = 0;
             Exec Sql Select EAninv Into :XAninv From WMEAP
              Where EAco#=:XAco# And EAfnd=:XAfnd And
              EAemp#=:XAemp# And EAdiv=:XAdiv And EAfyr=:XAfyr
              And EACTyp<> 'C' And EACTyp <> 'R' And
              EApoDt=:XApoDt And EAarTy in ('A','B','D','F','H');

              If XAninv = 0;
                 inKey =  %EditC(XAco#:'X') + %EditC(XAfnd:'X') +
                  %EditC(XAemp#:'X') + %EditC(XAdiv:'X');
                 XAnInv = GetNext3rdPartySeq#(inKey);
              Endif;
           Endif;
       //*** End Add *** 09/09/21 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Delete from A/R holding file when holding record was written today
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C     A#KEY         KLIST
     C                   KFLD                    XACO#
     C                   KFLD                    XAFND
     C                   KFLD                    XAEMP#
     C                   KFLD                    XADIV
     C                   KFLD                    XAFYR
     C                   KFLD                    SYSDT
     C*
     C     A#KEY         CHAIN     PEA0#                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     XATRDT        IFEQ      EATRDT
     C     XAPODT        ANDEQ     EAPODT
     C     XAAMT         ANDEQ     EAAMT
     C     XANINV        ANDEQ     EANINV
     C                   DELETE    PEA0#
     C                   LEAVE
     C                   ENDIF
     C*
     C     A#KEY         READE     PEA0#                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
      *
      *===============================================================
      * PRTNCC - Print NCCI Form (If state form not printed)
      *===============================================================
     C     PRTNCC        BEGSR
      *
     c                   If        xcReDc = *blanks and isUseSpoolOut
      *
      *  Print NCCI Reinstatement Form - Member Copy
      *
     C                   EXSR      CHKTST
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   MOVEL(P)  'CLT071'      ##FFLR
     C     USETST        WHENEQ    *ON
     C                   MOVEL(P)  TSTFLR        ##FFLR
     C                   OTHER
     C                   MOVEL(P)  'WCDOC'       ##FFLR
     C                   ENDSL
     C                   MOVEL(P)  'REINFRM'     ##FDOC
     C                   MOVE(P)   *BLANKS       ##TFLR
     C                   MOVE(P)   *BLANKS       ##TDOC
      *
     C     XXDRT3        IFEQ      'Y'
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##TFLR           80
     C                   PARM      *BLANKS       ##TDOC           12
     C                   PARM      'RR'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##FND             3
     C                   PARM      'P'           ##RTCD            1
     C                   ENDIF
      *
      *
     C                   CALL      'W7167C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM                    ##TFLR           80
     C                   PARM                    ##TDOC           12
     C                   PARM      'RR'          ##PRE             2
     C                   PARM      QQOUTQ        ##OUTQ           10
     C                   PARM      'Y'           ##PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'M'           XXRUN             1
     C                   EXSR      WRTAGP
      *
      ** Print DARTS remark
      *
     C     XXDRT3        IFEQ      'Y'
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C                   MOVEL(P)  QCKEY         Q#KEY            40
      *
     C     XHRVFL        COMP      'R'                                    20
     C                   MOVE      *BLANKS       Q#CSID            2
      *
     C     *IN20         IFEQ      *ON
     C                   MOVEL     DA(2)         DACOMM
     C                   ELSE                                                   *IN20 = On
     C                   MOVEL     DA(1)         DACOMM
     C                   ENDIF                                                  *IN20 = On
      *
     C                   Z-ADD     CXREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1DATE
      *
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CC          XXCC
     C                   MOVE      X1YY          XXYY
     C                   MOVE      '/'           XXSL1
     C                   MOVE      '/'           XXSL2
     C                   MOVEL(P)  DACOMM        Q#COMM          256
     C                   CAT(P)    CXRCM1:2      Q#COMM
     C                   CAT(P)    CXRCM2:2      Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
      *
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
      *
     C                   MOVEL(P)  ##TDOC        Q#DCID           12
     C                   MOVEL(P)  ##TFLR        Q#FOLD           80
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  XHUSER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
      *
     C     *IN20         IFEQ      *ON
     C                   MOVEL(P)  EXRRCD        Q#RMCD            5
     C                   ELSE                                                   *IN20 = On
     C                   MOVEL     EXRECD        Q#RMCD
     C                   ENDIF                                                  *IN20 = On
      *
     C                   MOVEL     *BLANK        Q#FL01            1
     C                   MOVEL     *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL     *BLANKS       Q#APID            2
     C                   MOVEL     *BLANKS       Q#IDOC           12
     C                   MOVEL     *BLANKS       Q#IQUE           10
     C                   MOVEL     'O'           Q#IMGF            1
     C                   MOVEL     *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL     *BLANK        Q#QPTY            1
     C                   MOVEL     *BLANKS       Q#DCTP            5
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
     C                   MOVE      Q#SEQ#        ##SEQ#            5 0
     C                   ENDIF                                                  XXDRT3
      *
      *
      *  Print NCCI Reinstatement Form - Agent Copy
      *
     C     SVAOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7167C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM      *BLANKS       ##TFLR           80
     C                   PARM      *BLANKS       ##TDOC           12
     C                   PARM      'RR'          ##PRE             2
     C                   PARM      SVAOUT        ##OUTQ           10
     C                   PARM      'N'           ##PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'A'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
      *  Print NCCI Reinstatement Form - State Copy
      *
     C     EXSOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7167C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM      *BLANKS       ##TFLR           80
     C                   PARM      *BLANKS       ##TDOC           12
     C                   PARM      'RR'          ##PRE             2
     C                   PARM      EXSOUT        ##OUTQ           10
     C                   PARM      'N'           ##PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'S'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
     C                   ENDIF                                                  XCCXDC
      *
     C                   ENDSR                                                  PRTNCC
      *
      *
      *
      *
      *
      *================================================================
      * DRT2 - Write DARTS remark on deletion of emp Audit Trk record
      *================================================================
     C     DRT2          BEGSR
      *
      *  If Audit Tracking Deletion Remark Code Specified
      *  (WW Grp, opt16, scr03)
      *
     C     EXKY1         CHAIN     PEX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     EXAUDL        ANDNE     *BLANKS
      *
      *  User Name
      *
     C                   MOVEL(P)  D1            Q#COMM
     C                   CAT(P)    UPUSNM:1      Q#COMM          256
     C                   CAT(P)    D2:1          Q#COMM
      *
      *  Audit company name
      *
     C     AUACO         IFNE      *BLANKS
     C     ACKY          CHAIN     PAC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     ACNAME        ANDNE     *BLANKS
     C                   CAT(P)    D3:2          Q#COMM
     C                   CAT(P)    ACNAME:1      Q#COMM
     C                   ENDIF
      *
     C                   ENDIF                                                  AUACO
      *
      *  Audit Representative Name
      *
     C     AUREP         IFNE      *BLANKS
     C     AUREP         CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   CAT(P)    D4:2          Q#COMM
     C                   CAT(P)    UPUSNM:1      Q#COMM
     C                   ENDIF
      *
     C                   ENDIF                                                  AUREP
      *
      *  Audit Request Date
      *
     C     AURQDT        IFNE      *ZERO
     C                   Z-ADD     AURQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C     SRERR         IFEQ      *OFF
     C                   CAT(P)    D5:2          Q#COMM
     C                   MOVEL(P)  SRDTO         ##NUM
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK           10
      *
     C                   CAT(P)    ##NUM:1       Q#COMM
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  AURQDT
      *
      *          Call API to write a DARTS Remark
      *          ------------------------------------------------------
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      AUCO#         QCCO#
     C                   MOVE      AUFND         QCFND
     C                   MOVE      AUEMP#        QCEMP#
     C                   MOVE      AUDIV         QCDIV
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  EXAUDL        Q#RMCD            5
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
      *
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  DRT2
      *
      *
      *===================================================================
      * SRORIG - Get Orig Pol Num, Orig Pol Eff Dt, Prior CX Date
      *===================================================================
     C     SRORIG        BEGSR
      *
     C     ELKY          CHAIN(N)  PEL01                              60
     C  N60ELKY3         READPE(N) PEL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     ELKY3         CHAIN(N)  PEL01                              60
     C     RPKY1         CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL(P)  RPCAP#        CROPOL
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ELFND         XXTMP            10
     C                   CAT(P)    XXTMP:0       CROPOL
     C                   MOVEL(P)  ELEMP#        XXTMP            10
     C                   CAT(P)    XXTMP:0       CROPOL
     C                   MOVEL(P)  ELFYR         XXTMP            10
     C                   CAT(P)    XXTMP:0       CROPOL
     C                   ENDIF                                                  RPCAP#
      *
     C                   MOVE      ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CROPED
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       CROPOL
     C                   Z-ADD     *ZERO         CROPED
     C                   ENDIF                                                  *IN60
      *
     C     ELKY          CHAIN(N)  PEL01                              60
      *
      *   Get NCCI CX reason code
      *
     C                   MOVE(P)   *BLANKS       CRCRSN
     C                   Clear                   CRRESN
     C                   Clear                   CRDESC
     C                   Eval      CRCXRN = CXRSN
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   MOVEL(P)  CXRSN         KKKEY
      *
      *
     C                   Z-ADD     CXCDAT        KKEFDT
      *
      *
     C     $CRKY         SETGT     PCR01
     C     $CRKY1        READPE    PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     $CRSTA        ANDEQ     'I'
     C     $CRKY1        READPE    PCR01                                  60
     C                   ENDDO
       //*** Begin Add *** 07/28/25 ****************************************
       //
       // Get Bill Schedule Code Information
       //
       Chain (ELco#:%EditC(ELbsCd:'X')) pbs01;

       // Create Late Fee A/R Record
       If cltID = '153' And FNpeoF = ' ' And
          GetStRptDv(ELco#:ELfnd:ELemp#) = ELdiv;
          //
          // There must be a late fee and a late fee code
          If $CrFee <> 0 And CRrfCd <> ' ' And sysDt > CXcxDt;

             // Write A/R Record
             Clear pea01;

             EAco#  = ELco#;
             EAfnd  = ELfnd;
             EAfyr  = ELfyr;
             EAemp# = ELemp#;
             EAdiv  = ELdiv;

             Chain (EAco#:CRrfCd) ptt01;
             EAarCd = CRrfCd;
             EAarTy = TTtype;
             EAdesc = TTdesc;
             EAcTyp = TTcTyp;
             EAamt  = $CrFee;

             EApoDt = CXreDt;

             // Get the invoice#
             Exec Sql Select EAninv Into :EAninv From WMEAP
              Where EAco#=:EAco# And EAfnd=:EAfnd And
              EAemp#=:EAemp# And EAdiv=:XAdiv And EAfyr=:XAfyr
              And EACTyp<> 'C' And EACTyp <> 'R' And
              EApoDt=:EApoDt And EAarTy in ('A','B','D','F','H');

             If EAninv = 0;
                Exec Sql Select XAninv Into :EAninv From WMXAP
                 Where XAco#=:EAco# And XAfnd=:EAfnd And
                 XAemp#=:EAemp# And XAdiv=:XAdiv And XAfyr=:XAfyr
                 And XACTyp<> 'C' And XACTyp <> 'R' And
                 XApoDt=:EApoDt And XAarTy in ('A','B','D','F','H');
             Endif;

             If EAninv = 0;
                inKey =  %EditC(EAco#:'X') + %EditC(EAfnd:'X') +
                 %EditC(EAemp#:'X') + %EditC(EAdiv:'X');
                EAnInv = GetNext3rdPartySeq#(inKey);
             Endif;

             EAstat = 'A';

             // Update these fields with reinstatement info
             EAtrDt = sysDt;
             EAuser = Q1user;
             EApgm  = Q1pgm;
             DStime += 1;
             EAtime = DStime;
             EAdt   = sysDt;

             Write pea01;
          EndIf;
       EndIf;
       //*** End   Add *** 07/28/25 ****************************************
      *
     C  N60              MOVEL(P)  $CRNCC        CRCRSN
     C                   Eval      CRDESC = $CRDSC
     C                   MOVE      $CRNCC        XXCD              2
     C                   MOVE      'CXCD'        XXTBID
     C                   MOVEL(P)  XXCD          XXKEY
     C     GTKY          CHAIN     PGT01                              61
      *
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     GTDESC        CRRESN
     C                   ENDIF                                                  *IN61 IFEQ
     C                   ENDIF                                                  RSKY
      *
     C                   ENDIF                                                  ELKY
      *
      *
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRRCNL
      *
     C                   Z-ADD     XXFEIN        CRGFIN
      *
      *  LAPSE IN COVERAGE
      *
     C     CXCXDT        IFNE      CXREDT
     C                   MOVE      'Y'           CRLAPS
      *
     C                   ELSE
      *
     C                   MOVE      'N'           CRLAPS
     C                   ENDIF                                                  CXDT NE REDT
      *
      *
     C                   ENDSR                                                  SRORIG
      *
      *
      *
      *
     C****SUBROUTINE DARTS - TO WRITE DARTS REMARK RECORD             *
     C*---------------------------------------------------------------*
     C**ESTABLISH DATE FOR DARTS' COMMENT
      *
      *
      *
      *===================================================================
      *  DART3 - Resolve state forms to DARTS as .PDF
      *===================================================================
      *
     C     DART3         BEGSR

     c                   If        qqReFl <> *blanks and
     c                             qqReDc <> *blanks and
     c                             isUseSpoolOut
     C                   MOVE      *BLANKS       #3STFL
     C                   MOVE      *BLANKS       #3STDC
      *
     C     XXDRT3        IFEQ      'Y'
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'RR'          #3PRE             2
     C                   PARM      QQCO#         #3CO#             3
     C                   PARM      QQFND         #3FND             3
     C                   PARM      'P'           #3RTCD            1
     C                   ENDIF
      *
      *  Print State CX form - Member Copy
      *
     C                   CALL      'W7167C3'
     C                   PARM      QQREFL        #3CXFL           60
     C                   PARM      QQREDC        #3CXDC           12
     C                   PARM                    #3STFL           80
     C                   PARM                    #3STDC           12
     C                   PARM      'RR'          #3PRE             2
     C                   PARM      QQOUTQ        #3OUTQ           10
     C                   PARM      'Y'           #3PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'M'           XXRUN
     C                   EXSR      WRTAGP
      *
     C     XXDRT3        IFEQ      'Y'
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C                   MOVEL(P)  QCKEY         Q#KEY            40
      *
     C     XHRVFL        IFEQ      'R'
     C                   MOVEL(P)  EXRRCD        Q#RMCD            5
     C                   ELSE
     C                   MOVEL     EXRECD        Q#RMCD
     C                   ENDIF
      *
      ****
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
      *
     C                   MOVEL(P)  #3STFL        Q#FOLD
     C                   MOVEL(P)  #3STDC        Q#DCID
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
      *
      *  Comment
      *
     C                   MOVE      *BLANKS       Q#CSID            2
      *
     C     XHRVFL        IFEQ      'R'
     C                   MOVEL     DA(2)         DACOMM
     C                   ELSE                                                   *IN20 = On
     C                   MOVEL     DA(1)         DACOMM
     C                   ENDIF                                                  *IN20 = On
      *
     C                   Z-ADD     CXREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1DATE
      *
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CC          XXCC
     C                   MOVE      X1YY          XXYY
     C                   MOVE      '/'           XXSL1
     C                   MOVE      '/'           XXSL2
     C     XXST          CAT(P)    'State':1     XXTMP3           10
     C     XXTMP3        CAT(P)    DACOMM:1      Q#COMM          256
     C                   CAT(P)    CXRCM1:2      Q#COMM
     C                   CAT(P)    CXRCM2:2      Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID           12
     C                   PARM                    Q#PNAM
     C                   PARM      *ZEROS        Q#SECL            3
     C                   PARM      Q1USER        Q#USER           10
     C                   PARM      Q1PGM         Q#PGM            10
     C                   PARM                    Q#FOLD           80
     C                   PARM      'N'           Q#DIFL            1
     C                   PARM      Q1PGM         Q#IPGM           10
     C                   PARM      *BLANK        Q#FL01            1
     C                   PARM      *BLANK        Q#FL02            1
     C                   PARM                    Q#RMCD
     C                   PARM      '00000'       Q#SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      'O'           Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *ZEROS        Q#PIDX            5
     C                   PARM      *BLANK        Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
      *
     C                   ENDIF                                                   XXDRT3
      *
      *  Print State CX form - Agent Copy
      *
     C     SVAOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7167C3'
     C                   PARM      QQREFL        #3CXFL           60
     C                   PARM      QQREDC        #3CXDC           12
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'RR'          #3PRE             2
     C                   PARM      SVAOUT        ##OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'A'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
      *  Print State CX form - State Copy
      *
     C     EXSOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7167C3'
     C                   PARM      QQREFL        #3CXFL           60
     C                   PARM      QQREDC        #3CXDC           12
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'RR'          #3PRE             2
     C                   PARM      EXSOUT        #3OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'S'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXSOUT
      *
     C                   ENDIF                                                   REFL REDC
      *
     C                   ENDSR                                                  DART3
      *
      *
      *
      *
     C********************************************************************
     C*
     C*-------------------------------------------------------------------
     C*  TRANS SUBROUTINE  -  CREATE TRANSACTION FOR NCCI REPORTING
     C*-------------------------------------------------------------------
     C     TRANS         BEGSR
     C*
      *
     C     CRKY1         SETGT     PC#01
     C     CRKY1         READPE    PC#01                                  60
     C  N60              Z-ADD     CRSEQ#        SVSEQ#
     C   60              Z-ADD     *ZERO         SVSEQ#
     C     SVSEQ#        ADD       1             CRSEQ#
      *
     C                   MOVE      XXCO#         CRCO#
     C                   MOVE      XXFND         CRFND
     C                   MOVE      EMEMP#        CREMP#
     C                   MOVE      XXFYR         CRFYR
     C                   TIME                    CRUNID
     C                   MOVEL     'R'           CRCORR
     C*
     C                   MOVE      *BLANKS       CRFLAT
     C                   MOVE      *BLANKS       CRPROR
     C                   MOVE      *BLANKS       CRSHRT
     C                   MOVE      *BLANK        CRNRNW
     C*
     C                   MOVEL(P)  EMNAME        CRENAM
     C                   MOVEL     EMNAME        CRNAM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        CRNAM1
     C                   MOVE      EMNAM2        CRNAM2
     C                   MOVEL     EMFDNM        CRFDN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        CRFDN1
     C                   MOVE      EMFDN2        CRFDN2
      *
     C                   MOVEL(P)  CRNAM1        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRNAM1
      *
     C                   MOVE(P)   CRNAM2        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRNAM2
      *
     C                   MOVEL(P)  CRFDN1        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN1
      *
     C                   MOVE(P)   CRFDN2        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN2
      *
     C     XXEAD1        CAT(P)    ', ':0        CREADR
     C                   CAT(P)    XXECSZ:1      CREADR
      *
     C                   MOVE      CXREDT        CREFDT
     C                   MOVE      CXRDAT        CRMLDT
     C*
     C                   MOVE      XXCARC        CRCARR
     C                   MOVE      ELPFDT        CRPFDT
     C                   MOVE      *BLANKS       CRCRSN
     C                   MOVE      CXREVL        CRTORE
     C     CLTID         IFEQ      '071'
     C                   MOVEL     EMDBA         CRGSI#
     C                   ENDIF
     C                   MOVEL     XXFNAM        CRFNAM
     C                   MOVE      EMFEIN        CRFEIN
     C                   MOVEL     XXEAD1        CREAD1
      *
     C                   MOVEL     XXEAD2        CREAD2
     C                   MOVEL     XXECSZ        CRECSZ
      *
      * Get PEO Information if PEO Group
      *
     C     FNPEOF        IFNE      *BLANK
     C                   MOVE      QQCO#         #5CO#
     C                   MOVE      QQFND         #5FND
     C                   MOVE      QQEMP#        #5EMP#
     C                   MOVE      QQDIV         #5DIV
     C                   MOVE      QQFYR         #5FYR
      *
     C                   CALL      'SRADD1'
     C                   PARM                    #5CO#             3 0
     C                   PARM                    #5FND             3 0
     C                   PARM                    #5EMP#            9 0
     C                   PARM                    #5DIV             5 0
     C                   PARM                    #5FYR             3 0
     C                   PARM                    #5NAME          100
     C                   PARM                    #5FNAM          100
     C     CREAD1        PARM                    #5EAD1           40
     C     CREAD2        PARM                    #5EAD2           40
     C     CRECSZ        PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C     CRFEIN        PARM                    #5FEIN            9 0
     C     CREADR        PARM                    #5EADR           90
      *
     C                   MOVEL(P)  #5NAME        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRNAM1
      *
     C                   MOVE(P)   #5NAME        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRNAM2
      *
     C                   MOVEL(P)  #5FNAM        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN1
      *
     C                   MOVE(P)   #5FNAM        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN2
      *
     C                   MOVEL(P)  #5NAME        CRENAM
     C                   MOVEL(P)  #5FNAM        CREDBA
     C                   ENDIF                                                  FNPEOF
      *
     C     CREAD2        IFEQ      *BLANK
     C                   MOVEL(P)  CRECSZ        CREAD2
     C                   MOVEL(P)  *BLANKS       CRECSZ
     C                   ENDIF                                                  CREAD2
      *
     C*
     C                   MOVE      XXFND         #@FND             3
     C                   MOVE      XXEMP#        #@EMP#            9
     C                   MOVE      XXFYR         #@FYR             3
     C*
     C     #@FND         CAT(P)    #@EMP#:0      CRPOL#
     C                   CAT       #@FYR:0       CRPOL#
     C                   MOVEL(P)  CRPOL#        CRCAP#
      *
     C     CLTID         IFEQ      '071'
     C                   EXSR      SRP71#
     C                   ENDIF
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
      *
     C                   MOVEL(P)  RPCAP#        CRCAP#
     C                   MOVEL(P)  RPCAP#        XXPOL
      *
      * IF CARRIER POLICY NUMBER IS NUMERIC, CHECK TO SEE IF IT IS A
      * PDS OLD-FORMAT POLICY NUMBER
      *
     C                   TESTN                   XXOPOL               61
     C     *IN61         IFEQ      *ON
     C     XOBLNK        IFEQ      *BLANKS
     C                   Z-ADD     *ZEROS        XXEMP#
     C                   MOVE      XXOFND        XXFND
     C                   MOVE      XXOEMP        XXEMP#
     C                   MOVE      XXOFYR        XXFYR
      *
     C     ELKY2         CHAIN     PEL02                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELFYR         XXFYR
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       CRPOL#
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  XOBLNK=BLKS
     C*
     C                   ENDIF                                                  *IN61 IFEQ *OFF
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       CRPOL#
      *
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       CRPOL#
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   MOVE      ELPTDT        CRPTDT
     C                   MOVEL     *BLANKS       CRRESN
     C                   MOVEL     XXISUO        CRISUO
     C                   MOVEL     XXAOUT        CRAOUT
     C                   Z-ADD     XXAARE        CRAARE
     C                   Z-ADD     XXAPHN        CRAPHN
     C*
     C                   Z-ADD     CXRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRISUD
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRCISD
      *
     C                   MOVE      ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRPTOD
      *
     C                   MOVE      ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRPFRD
      *
     C                   Z-ADD     CXREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRCAND
      *
      *
      *
      *  Get Employer Federal Name
      *
     C                   MOVEL(P)  *BLANKS       CREDBA
     C     EMKY1         CHAIN(N)  PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMFDNM        CREDBA
     C     EMFDN2        IFNE      *BLANKS
     C     EMFDNM        CAT(P)    EMFDN2:0      #WRK10          100
     C                   MOVEL(P)  #WRK10        CREDBA
     C                   ENDIF                                                  EMFDN2 IFNE
     C                   MOVEL(P)  EMUE#         CRUE#
     C                   ENDIF
      *
      *
     C                   MOVEL(P)  CXRCM1        CRCOM1
     C                   MOVEL(P)  CXRCM2        CRCOM2
     C                   MOVE      CXAMTD        CRAMTD
     C                   MOVE      CXRFEE        CRRFEE
      *
     C                   Z-ADD     CXRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         CRRPMY
      *
     C                   MOVE      CXERR         CRERR
     C*
     C     FNMAD1        IFNE      *BLANKS
      *
     C                   MOVEL(P)  FNMAD1        CRGAD1
     C                   MOVEL(P)  FNMAD2        CRGAD2
     C                   MOVEL     FNMCTY        XXCITY
     C                   MOVEL     FNMST         XXST
     C                   MOVEL     FNMZIP        XXZIP5
     C                   MOVE      FNMZIP        XXZIP4
      *
     C                   ELSE
      *
     C                   MOVEL(P)  FNPAD1        CRGAD1
     C                   MOVEL(P)  FNPAD2        CRGAD2
     C                   MOVEL     FNPCTY        XXCITY
     C                   MOVEL     FNPST         XXST
     C                   MOVEL     FNPZIP        XXZIP5
     C                   MOVE      FNPZIP        XXZIP4
      *
     C                   ENDIF                                                  FNMAD1 IFNE *BLANKS
      *
     C     XXCITY        CAT(P)    ',':0         CRGCSZ
     C                   CAT(P)    XXST:1        CRGCSZ
     C                   CAT(P)    XXZIP5:1      CRGCSZ
      *
     C     XXZIP4        IFNE      *ZEROS
     C                   CAT(P)    '-':0         CRGCSZ
     C                   CAT(P)    XXZIP4:0      CRGCSZ
     C                   ENDIF                                                  XXZIP4 IFNE *ZEROS
      *
     C     CRGAD2        IFEQ      *BLANKS
     C                   MOVEL(P)  CRGCSZ        CRGAD2
     C                   MOVEL(P)  *BLANKS       CRGCSZ
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       CRGPHN
     C                   MOVE      *BLANKS       XXAREA
      *
     C     FNPH1A        IFNE      0
      *
     C                   MOVE      FNPH1A        XXAREA            3
     C                   MOVEL     FNPH1         XXEXCH            3
     C                   MOVE      FNPH1         XXPHN#            4
      *
     C                   ELSE
      *
     C                   MOVE      FNAREA        XXAREA            3
     C                   MOVEL     FNPH2         XXEXCH            3
     C                   MOVE      FNPH2         XXPHN#            4
      *
     C                   ENDIF                                                  FNPH1A IFNE 0
      *
     C     XXAREA        IFNE      *BLANKS
      *
     C     '('           CAT(P)    XXAREA:0      CRGPHN
     C                   CAT(P)    ')':0         CRGPHN
     C                   CAT(P)    XXEXCH:1      CRGPHN
     C                   CAT(P)    '-':0         CRGPHN
     C                   CAT(P)    XXPHN#:0      CRGPHN
      *
     C                   ENDIF                                                  XXAREA IFNE *BLANKS
     C                   MOVEL     'A'           CRSTAT
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C                   Z-ADD     DSTIME        CRTIME
     C*
      *
      * Get original Policy number, Policy effective date, Prior CX date
      *
     C                   EXSR      SRORIG
      *
     C                   MOVE      XXLSID        CRLSID
      *
      *  UPDATE FILE
      *
     C     WCFL03        IFEQ      'Y'
     C     EMSTRP        ANDEQ     'Y'
     C     QQREPR        ANDNE     'Y'
      /free
       CRreCd = CXreCd;   //Load Reinstatement Reason Code
      /End-Free
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRIDT
     C                   MOVEL     Q1USER        CRIUSR
     C                   WRITE     PC#01
     C                   ENDIF                                                  WCFL03 IFEQ 'Y'
     C                   CLEAR                   PXC01
      *
     C     SYOFIF        IFEQ      'Y'
      *
      * READ THE STATE CX/REIN DEFINITION FILE TO GET THE FOLDER AND
      * DOCUMENT FOR THE STATE REINSTATEMENT FORM
      *
     C                   MOVE      *BLANKS       QQREFL           60
     C                   MOVE      *BLANKS       QQREDC           12
     C                   MOVE      ELRTCD        XXRTCD
     C                   CLEAR                   XXST
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      RSJRST        CRJRST
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFFD
      *
     C     XCKY          SETGT     PXC01
     C     XCKY2         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  XCREFL        QQREFL
     C                   MOVEL(P)  XCREDC        QQREDC
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      *     CREATE A DB FILE TO USE IN A DOCUMENT QUERY, THEN EXECUTE THE
      *      CODE TO PRT THE DOC WITH THE DATA
      *
     C                   OPEN      WX#@P
     C                   WRITE     P#@01
     C                   CLOSE     WX#@P
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         saveDate          7 0
     C*
     C                   Z-ADD     CRMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxMLDT            8 0
     C*
     C                   Z-ADD     CREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         x1EFDT            8 0
     C*
     C                   move      xxco#         #1co#
     C                   move      xxfnd         #1fnd
     C                   move      xxemp#        #1emp#
     C                   move      xxdiv         #1div
     C                   move      ' '           ##odr
     C                   move      'F'           ##nodr
     C                   move      'M'           ##aodr
     C                   clear                   ##emn1
     C                   clear                   ##emn2
     C                   clear                   ##emn3
     C                   clear                   ##emn4
     C                   clear                   ##emn5
     C                   clear                   ##emn6
     C                   clear                   ##emn7
     C                   clear                   ##amn1
     C                   clear                   ##amn2
     C                   clear                   ##amn3
     C                   clear                   ##amn4
     C                   clear                   ##amn5
     C                   clear                   ##amn6
     C                   clear                   ##amn7
     C                   clear                   ##amn8
     C                   clear                   ##amn9
     C                   Clear                   QQPEOExt
     C*
     C     XCKY          Setgt     PRR00
     C     XCKY2         Readpe(n) PRR00                                  60
     C*
     C                   select
     C                   when      FNPEOF = *blanks
     C                   clear                   ##pgfl
     C                   when      rrst <> '  '
     C                   eval      ##pgfl = RRPGFL
     c                   if        ##pgfl = *blank
     c                   move      'B'           ##pgfl
     c                   endif
     C                   Eval      QQPEOExt = RRPEOA
     C                   other
     C                   clear                   ##pgfl
     C                   endsl
     C                   Eval      ##dbaf = ' '
     C*
     C*** Begin Chg *** 02/22/22 *******************************************
     C**********         call      'SRADDR'
     C**********         parm                    #1co#             3
     C**********         parm                    #1fnd             3
     C**********         parm                    #1emp#            9
     C**********         parm                    #1div             5
     C**********         parm                    ##odr             1
     C**********         parm                    ##nodr            1
     C**********         parm                    ##aodr            1
     C**********         parm                    ##emn1           50
     C**********         parm                    ##emn2           50
     C**********         parm                    ##emn3           50
     C**********         parm                    ##emn4           50
     C**********         parm                    ##emn5           50
     C**********         parm                    ##emn6           50
     C**********         parm                    ##emn7           50
     C**********         parm      *zero         ##adrl            3 0
     C**********         parm      *zero         ##naml            3 0
     C**********         parm      *zero         ##fedl            3 0
     C**********         parm      *blank        ##peo1           57
     C**********         parm      *blank        ##peo2           57
     C**********         parm                    ##pgfl            1
     C**********         parm                    ##conl            3 0
     C**********         parm                    ##amn1
     C**********         parm                    ##amn2
     C**********         parm                    ##amn3
     C**********         parm                    ##amn4
     C**********         parm                    ##amn5
     C**********         parm                    ##amn6
     C**********         parm                    ##amn7
     C**********         parm                    ##amn8
     C**********         parm                    ##amn9
     C**********         parm      *zero         ##peol            3 0
     C**********         parm                    ##dbaf            1
     C**********         parm      *zero         ##al2             3 0
     C**********         parm      *zero         ##nl2             3 0
     C**********         parm      *zero         ##fl2             3 0
     C**********         parm      *zero         ##pl2             3 0
     C**********         parm      *zero         ##cl2             3 0
     C**********         PARM                    QQPEOExt         10
     C**********         PARM      *blanks       QQname1         120
     C**********         PARM      *blanks       QQname2         120
     C**********         PARM      *blanks       QQname3         120
     C**********         PARM      *blanks       QQname4         120
       ##adrl  = 0;
       ##naml  = 0;
       ##fedl  = 0;
       ##peo1  = ' ';
       ##peo2  = ' ';
       ##peol  = 0;
       ##al2   = 0;
       ##nl2   = 0;
       ##fl2   = 0;
       ##pl2   = 0;
       ##cl2   = 0;
       QQname1 = ' ';
       QQname2 = ' ';
       QQname3 = ' ';
       QQname4 = ' ';
       QQfein  = 0;

       GetNameAddress(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:
              ##emn1:##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:
              ##adrl:##naml:##fedl:##peo1:##peo2:##conl:##amn1:
              ##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:##amn8:##amn9:
              ##peol:##al2:##nl2:##fl2:##pl2:##cl2:QQname1:QQname2:
              QQname3:QQname4:QQfein);
     C*** End   Chg *** 02/22/22 *******************************************
     C*** Beg Add *** 02/26/24 *** WC2956 **********************************
      * Reset Pending/Processing Flag
     C*** Beg Del *** 07/11/25 *************************************
      * Moving these changes outside of TRANS subroutine

        // Chain (XXCo#:XXFnd:XXFyr:XXEmp#:XXDiv:XXReq#) WDCXL;
        // If %Found;
        //    CXpdPr = ' ';
        //    Update Pcx01;
        // EndIf;

     C*** End Del *** 07/11/25 *************************************
     C*** End Add *** 02/26/24 *** WC2956 **********************************
     C*
     C                   If        isUseXml
     C                   Open      wxc7p
     C                   Exsr      ScriptXML
     C                   Close     wxc7p
     C                   Endif
     C*
      *
      *  If Cover Letter, print and resolve to DARTS
      *
     C                   EXSR      PRTCOV
      *
      *  If state form, then resolve to DARTS in .pdf format
      *
     C                   EXSR      DART3
      *
      *  If state form not printed, the print NCCI CX form and resolve
      *  to DARTS in .pdf format
      *
     C                   EXSR      PRTNCC
      *
     C                   Exsr      ScriptOrder
      *
     C                   ENDIF                                                  SYOFIF IFEQ 'Y'
      *
     C     WCFL03        IFEQ      'Y'
     C     EMSTRP        ANDEQ     'Y'
     C     QQREPR        ANDNE     'Y'
     C*
      *
     C                   MOVE      *BLANK        XXPROC            1
      *
     C     SHKY          CHAIN(N)  PSH02                              60
      *
      * ONLY WRITE SUBMISSION HISTORY RECORD IF A POLICY RECORD EXISTS
      * AND HAS GENERATED A POLICY PAGE.
      *
     C     *IN60         IFEQ      *OFF
      *
     C     SHSTAT        IFEQ      '3'
     C     SHSTAT        OREQ      '4'
     C     SHSTAT        OREQ      '5'
     C     SHSTAT        OREQ      'E'
     C                   MOVE      'Y'           XXPROC
     C                   ENDIF                                                  SHSTAT IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * CHECK IF ANY CANCELLATIONS HAVE BEEN SUBMITTED.
      *
     C     XXPROC        IFEQ      *BLANK
      *
     C                   MOVE      'C'           XXTYPE
     C     SHKY          CHAIN(N)  PSH02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     XXPROC        ANDEQ     *BLANK
      *
     C     SHSTAT        IFEQ      '4'
     C                   MOVE      'Y'           XXPROC
     C                   ELSE
      *
     C     1             DO        15            I                 2 0
      *
     C     SST(I)        IFNE      *BLANKS
     C     SSS(I)        ANDEQ     '4'
     C                   MOVE      'Y'           XXPROC
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF                                                  SHSTAT IFEQ
      *
     C     SHKY          READE(N)  PSH02                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  XXPROC IFEQ
      *
      * CHECK IF ANY REINSTATEMENTS HAVE BEEN SUBMITTED.
      *
     C     XXPROC        IFEQ      *BLANK
      *
     C                   MOVE      'R'           XXTYPE
     C     SHKY          CHAIN(N)  PSH02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     XXPROC        ANDEQ     *BLANK
      *
     C     SHSTAT        IFEQ      '4'
     C                   MOVE      'Y'           XXPROC
     C                   ELSE
      *
     C     1             DO        15            I                 2 0
      *
     C     SST(I)        IFNE      *BLANKS
     C     SSS(I)        ANDEQ     '4'
     C                   MOVE      'Y'           XXPROC
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF                                                  SHSTAT IFEQ
      *
     C     SHKY          READE(N)  PSH02                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  XXPROC IFEQ
      *
     C                   MOVE      'P'           XXTYPE
      *
     C     XXPROC        IFEQ      'Y'
     C*
     C                   MOVE      *BLANKS       XXSST            30
     C                   MOVE      *BLANKS       XXSSS            15
     C     SHKY          CHAIN(N)  PSH02                              60
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     1             X                 2 0
      *
     C     ELKY4         CHAIN     PEL02                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL(P)  'NCCI'        XXTBID
     C                   MOVEL(P)  RSJRST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'XX'          XZST              2
     C                   ELSE
     C                   MOVE      RSJRST        XZST              2
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     1             I
     C     XZST          LOOKUP    SST(I)                                 60
     C*
     C     *IN60         IFEQ      *OFF
     C     X             ANDLE     15
     C                   MOVE      XZST          SST(X)
     C                   MOVE      '3'           SSS(X)
     C                   ADD       1             X
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY4         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
      *
      * ADD NCCI STATE RECORD IF POLICY HAS EXPERIENCE RATING OF INTERSTATE ('1'
      * INTERSTATE AND INTRASTATE ('2'),  IF NEEDED
      *
     C     CLTID         IFEQ      '093'
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVE      EMEXRT        XXEXRT            1
      *
     C     XXEXRT        IFEQ      '1'
     C     XXEXRT        OREQ      '2'
      *
      *  CHECK IF 'XX' IS IN ARRAY
      *
     C                   MOVE      'XX'          XZST
     C                   Z-ADD     1             I                 2 0
     C     XZST          LOOKUP    SST(I)                                 60
      *
      *  ADD 'XX' RECORD TO FIRST BLANK POSITION IN ARAY
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     1             X
     C     SST(X)        DOUEQ     *BLANK
      *
     C     X             IFEQ      16
     C     SST(X)        OREQ      *BLANK
     C                   LEAVE
     C                   ENDIF
      *
     C                   ADD       1             X
     C                   ENDDO
      *
     C     X             IFLE      15
     C                   MOVE      XZST          SST(X)
     C                   MOVE      '3'           SSS(X)
     C                   ENDIF                                                  IF X <= 15
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  XXEXRT IFEQ
     C                   ENDIF                                                  CLTID = '093'
     C                   MOVEA     SST           XXSST
     C                   MOVEA     SSS           XXSSS
     C*
     C                   ELSE
     C*
     C     1             DO        15            I                 2 0
     C     SST(I)        IFNE      *BLANKS
     C                   MOVE      '3'           SSS(I)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     SST           XXSST
     C                   MOVEA     SSS           XXSSS
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   CLEAR                   PSH02
     C                   MOVEA     XXSST         SST
     C                   MOVEA     XXSSS         SSS
     C                   MOVE      XXCO#         SHCO#
     C                   MOVE      XXFND         SHFND
     C                   MOVE      EMEMP#        SHEMP#
     C                   MOVE      XXFYR         SHFYR
     C                   MOVE      EMDIV         SHDIV
     C                   MOVE      CRUNID        SHUNID
     C                   MOVE      CRDT          SHTRDT
     C*
     C                   MOVE      *BLANKS       SHSBDT
     C                   MOVE      *BLANKS       SHSBDT
     C                   MOVE      'R'           SHTYPE                               REINSTAT
     C                   MOVE      'O'           SHCODE
     C                   MOVE      '3'           SHSTAT
     C*
     C     ##SEQ#        IFNE      *ZEROS
     C                   MOVE      ##SEQ#        SHSEQ#
     C                   ELSE
     C                   MOVE      *BLANKS       SHSEQ#
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   Z-ADD     DSTIME        SHTIME
     C                   Z-ADD     SHDT          SHIDT
     C                   MOVEL     Q1USER        SHIUSR
     C                   MOVE      ELPFDT        SHPFDT
     C*
     C                   WRITE     PSH01
     C                   ELSE
     C*
     C* REMOVE ANY CANCELLATION/REINSTATEMENT RECORDS FROM SUBMISSION FILE.
     C*
     C                   MOVE      'C'           XXTYPE
     C     SHKY          CHAIN     PSH02                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PSH02
     C     SHKY          READE     PSH02                                  60
     C                   ENDDO
     C*
     C                   MOVE      'R'           XXTYPE
     C     SHKY          CHAIN     PSH02                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PSH02
     C     SHKY          READE     PSH02                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXPROC IFEQ
     C*
     C                   ENDIF                                                  WCFL03 IFEQ 'Y'
     C*
     C     ETRANS        ENDSR
      *
      *
      *
      ********************************************************************
      *  PRTCOV - Print/Resolve CX cover letter
      ********************************************************************
     C     PRTCOV        BEGSR
      *
     C     XCKY3         SETGT     PXC01
     C     XCKY4         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   Exsr      ScriptMCX
      *
      *  If Reinstatement Cover letter specified
      *
     c                   If        xcRcLd <> *blanks and isUseSpoolOut
      *
      *  Get Resolved Document Name
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'RR'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##FND             3
     C                   PARM      *BLANKS       ##RTCD            1
      *
      *  Create DARTS remark
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C                   MOVEL(P)  QCKEY         #1KEY            40
      *
     C     XHRVFL        IFEQ      'R'
     C                   MOVEL(P)  EXRRCD        #1RMCD            5
     C                   ELSE
     C                   MOVEL     EXRECD        #1RMCD
     C                   ENDIF
      *
      ****
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         #1CDAT            8
      *
     C                   MOVEL(P)  #3STDC        #1DCID
     C                   MOVEL(P)  'W0050R'      #1PNAM           10
     C                   MOVEL(P)  #3STFL        #1FOLD
      *
     C                   MOVEL(P)  RECOV         #1COMM
      *
     C                   CALL      'DA700R'
     C                   PARM                    #1KEY            40
     C                   PARM                    #1COMM          256
     C                   PARM                    #1CDAT
     C                   PARM                    #1DCID           12
     C                   PARM                    #1PNAM
     C                   PARM      *ZEROS        #1SECL            3
     C                   PARM      Q1USER        #1USER           10
     C                   PARM      Q1PGM         #1PGM            10
     C                   PARM                    #1FOLD           80
     C                   PARM      'N'           #1DIFL            1
     C                   PARM      Q1PGM         #1IPGM           10
     C                   PARM      *BLANK        #1FL01            1
     C                   PARM      *BLANK        #1FL02            1
     C                   PARM                    #1RMCD
     C                   PARM      '00000'       #1SEQ#            5
     C                   PARM      'N'           #1SBYL            1
     C                   PARM      *BLANKS       #1COM2           19
     C                   PARM      *BLANKS       #1APID            2
     C                   PARM      *BLANKS       #1IDOC           12
     C                   PARM      *BLANKS       #1IQUE           10
     C                   PARM      *BLANK        #1IMGF            1
     C                   PARM      *BLANKS       #1IPTH           10
     C                   PARM      *ZEROS        #1PIDX            5
     C                   PARM      *BLANK        #1QPTY            1
     C                   PARM      *BLANKS       #1DCTP            5
     C                   PARM      *BLANKS       #1CSID            2
     C                   PARM      *BLANKS       #1CODE            2
      *
     C                   MOVEL(P)  XCRCLF        #3CCLF
     C                   MOVEL(P)  XCRCLD        #3CCLD
      *
      *  Print Member Copy - CX Cover Letter
      *
     C                   CALL      'W7167C3'
     C                   PARM                    #3CCLF           60
     C                   PARM                    #3CCLD           12
     C                   PARM                    #3STFL           80
     C                   PARM                    #3STDC           12
     C                   PARM      'RR'          #3PRE             2
     C                   PARM      QQOUTQ        #3OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'M'           XXRUN
     C                   EXSR      WRTAGP
      *
      *  Print Agent Copy - CX Cover Letter
      *
     C     SVAOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7167C3'
     C                   PARM                    #3CCLF           60
     C                   PARM                    #3CCLD           12
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'RR'          #3PRE             2
     C                   PARM      SVAOUT        ##OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      * write the spool file record to the WXAGP file
     C                   MOVEL     'A'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
     C                   ENDIF                                                  XCRCLD
      *
     C                   ENDIF                                                  XCKY
      *
     C                   ENDSR                                                  PRTCOV
      *
      *
      *
      *
     C*================================================================
     C*  WRTREC - writes record to a/r file exactly as they were when
     C*           they were deleted except for the maintenance fields
     C*           and the transaction date field
     C*================================================================
     C     WRTREC        BEGSR
     C*
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XAFND         EAFND
     C                   MOVE      XAFYR         EAFYR
     C                   MOVE      XAEMP#        EAEMP#
     C                   MOVE      XADIV         EADIV
     C                   MOVE      XAARCD        EAARCD
     C                   MOVE      XAARTY        EAARTY
     C                   MOVE      XADESC        EADESC
     C                   MOVE      XAAMT         EAAMT
     C                   MOVE      XAPODT        EAPODT
     C                   MOVE      XAEDIT        EAEDIT
     C                   MOVE      XASTAT        EASTAT
     C                   MOVE      XABUL1        EABUL1
     C                   MOVE      XACHK#        EACHK#
     C                   MOVE      XANINV        EANINV
     C                   MOVE      XALATE        EALATE
     C                   MOVE      XAFL01        EAFL01
     C                   MOVE      XAFL02        EAFL02
     C                   MOVE      XADT01        EADT01
     C                   MOVE      XADT02        EADT02
     C                   MOVE      XACTYP        EACTYP
     C                   MOVE      XARDIV        EARDIV
     C                   MOVE      XACCTP        EACCTP
     C                   MOVE      XACNUM        EACNUM
     C                   MOVE      XACEXP        EACEXP
     C                   MOVE      XACNAM        EACNAM
     C*
     C*    Update these fields with reinstatement info
      *
      *  DO NOT UPDATE TRANSACTION DATE FOR RETRO PAYMENTS UNLESS THE
      *  TRANSACTION DATE IS BEFORE THE NEXT INSTALLMENT DATE (ZZPODT)
      *
     C                   SELECT
     C     XAARCD        WHENEQ    F1RCCD
     C     XATRDT        IFLT      ZZPODT
     C                   Z-ADD     ZZPODT        EATRDT
     C                   ELSE
     C                   MOVE      XATRDT        EATRDT
     C                   ENDIF
     C                   OTHER
     C                   Z-ADD     SYSDT         EATRDT
     C                   ENDSL
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   MOVE      DSTIME        EATIME
     C                   Z-ADD     SYSDT         EADT
     C*
          //*** Beg Add ***** 06/29/22 ***************************
     C                   MOVE      XAref#        EAref#
          //*** End Add ***** 06/29/22 ***************************
     C                   WRITE     PEA01
     C*
     C                   ENDSR                                                  WRTREC
     C*================================================================
     C*  WRTAGP - writes the spool file record to file WXAGP
     C*================================================================
     C     WRTAGP        BEGSR
      *
      *  Check to see if there is a header record (AGHDR=Y) for the
      *  current agent (user)
      *  NOTE: File already overridden to only member for current user
      *
     C                   OPEN      WXAGP
     C                   MOVEL     'N'           XXHDR             1
     C                   READ      PAG01X                                 60
     C     *IN60         DOWEQ     *OFF
     C     AGHDR         IFEQ      'Y'
     C                   MOVEL     'Y'           XXHDR
     C                   LEAVE
     C                   ENDIF
     C                   READ      PAG01X                                 60
     C                   ENDDO
      *
      *  No header record found, create one based on datara WXAGP
     C     XXHDR         IFEQ      'N'
     C                   CLEAR                   PAG01X
     C                   MOVE      XXCO#         AGCO#
     C                   MOVE      CXAGT#        AGAGT#
     C                   MOVEL     *BLANKS       AGSTAT
     C                   MOVEL     'Y'           AGHDR
     C                   WRITE     PAG01X
     C                   ENDIF
      *
      * Write the record to the file
     C     *DTAARA       DEFINE    WXAGP         WXAGPX
     C     *LOCK         IN        WXAGPX
     C                   CLEAR                   PAG01X
     C                   MOVE      XXCO#         AGCO#
     C                   MOVE      CXAGT#        AGAGT#
     C                   MOVE      XXEMP#        AGEMP#
     C                   MOVEL     WXJOBN        AGJOBN
     C                   MOVEL     WXJOBU        AGJOBU
     C                   MOVEL     WXJOB#        AGJOB#
     C                   MOVEL     WXSPLN        AGSPLN
     C                   MOVE      WXSPL#        AGSPL#
     C                   MOVEL     WXOUTQ        AGOUTQ
     C                   MOVEL     WXSPST        AGSPST
     C                   MOVE      WXSPPG        AGSPPG
     C                   MOVE      WXSPCY        AGSPCY
     C                   MOVEL     WXSPFR        AGSPFR
     C                   MOVEL     *BLANKS       AGSTAT
     C                   MOVEL     *BLANKS       AGHDR
     C                   MOVEL     XXRUN         AGRUN
     C                   MOVEL     WXPGM         AGPGM
     C                   WRITE     PAG01X
      *
     C                   CLOSE     WXAGP
     C*
     C                   ENDSR                                                  WRTREC
     C*================================================================
      *================================================================
      *   SRP71# - CONCAT GRP-EMP-DIV WITHOUT LEADING ZEROS
      *================================================================
     C     SRP71#        BEGSR
      *
     C                   CLEAR                   X1PLCY
     C                   MOVE(P)   XXFND         ZXFND             3
     C                   MOVE(P)   XXEMP#        ZXEMP             5
     C                   MOVE(P)   XXFYR         ZXFYR             3
     C     '0'           CHECK     ZXFND         X
     C                   SUBST     ZXFND:X       XXTMP1            3
     C     XXTMP1        CAT       '-':0         X1PLCY           13
     C     X1PLCY        CAT       ZXEMP:0       X1PLCY
     C     X1PLCY        CAT       '-':0         X1PLCY
     C     X1PLCY        CAT       ZXFYR:0       X1PLCY
     C                   MOVEL(P)  X1PLCY        CRCAP#
      *
     C                   ENDSR
      /free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';

       If isMassReq;

          In *Lock *dtaara;
          Out *dtaara;

          If ldMassFile <> ' ';
             xmlFile = %trim(ldMassFile);
          Else;
             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'MassReinst':'000':'000':massFlag:xmlLink);
             In *Lock *dtaara;
             ldMassFile = %trim(xmlFile);
             Out *dtaara;
             Chain (xmlFile) xp100;
             XPCat = 'PS';
             Update xp100;
          Endif;

          isMassReq = *off;
          xmlFileName = xmlFile;
          xmlLink = xmlFile;

       Endif;

       StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
        string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

       If xmlFileName = ' ';
          xmlFileName = xmlFile;
       Endif;

       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       outputFile = XPFile;

       //* Set the output file name into the local area for additional documents to pick up.
       //*** Beg Chg *** 09/19/23 ******************************************
       //***In *Lock *dtaara;
       In svLda;
       //*** End Chg *** 09/19/23 ******************************************
       ldOutputFile = outputFile;
       //*** Beg Chg *** 09/19/23 ******************************************
       //***Out *dtaara;
       Out svLda;
       //*** End Chg *** 09/19/23 ******************************************

       Setgt (xxCO#:xxFND:RSJrSt:ELPfdt) pcr00;
       Readpe (xxCO#:xxFND:RSJrSt) pcr00;
       If not %eof;
          If STCRREDocC <> ' ';
             XPDcid = %trim(STCRREDocC);
             XPPath = %trim(STCRREPathC);
          Else;
             If STCRREDoc <> ' ';
                XPDcid = %trim(STCRREDoc);
                XPPath = %trim(STCRREPath);
             Endif;
          Endif;
       Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pcr00;
          Readpe (xxCO#:xxFnd:'  ') pcr00;
          If not %eof;
             If STCRREDocC <> ' ';
                XPDcid = %trim(STCRREDocC);
                XPPath = %trim(STCRREPathC);
             Else;
                If STCRREDoc <> ' ';
                   XPDcid = %trim(STCRREDoc);
                   XPPath = %trim(STCRREPath);
                Endif;
             Endif;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pcr00;
             Readpe (xxCO#:000:RSJrst) pcr00;
             If not %eof;
                If STCRREDocC <> ' ';
                   XPDcid = %trim(STCRREDocC);
                   XPPath = %trim(STCRREPathC);
                Else;
                   If STCRREDoc <> ' ';
                      XPDcid = %trim(STCRREDoc);
                      XPPath = %trim(STCRREPath);
                   Endif;
                Endif;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pcr00;
                Readpe (xxCO#:000:'  ') pcr00;
                If not %eof;
                   If STCRREDocC <> ' ';
                      XPDcid = %trim(STCRREDocC);
                      XPPath = %trim(STCRREPathC);
                   Else;
                      If STCRREDoc <> ' ';
                         XPDcid = %trim(STCRREDoc);
                         XPPath = %trim(STCRREPath);
                      Endif;
                   Endif;
                Endif;
             Endif;
          Endif;
       Endif;

       order = 1;

       XPData = 'Y';

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DsTime;
       XPDt   = saveDate;

       Update xp100;

       //* Write the main work file record.
       Clear pc701;

       C7XML = xmlFile;

       C7Co#  = xxCo#;
       C7Fnd  = xxFnd;
       C7Emp# = xxEmp#;
       C7Div  = xxDiv;
       C7Fyr  = xxFyr;
       C7UNID = CRUNID;

       C7CARR = CRCARR;
       C7GFIN = CRGFIN;
       C7GSI# = CRGSI#;
       C7POL# = CRPOL#;
       C7CAP# = CRCAP#;
       C7OPOL = CROPOL;
       C7OPED = CROPED;
       C7LSID = CRLSID;

       C7CORR = CRCORR;
       C7TORE = CRTORE;
       C7FLAT = CRFLAT;
       C7PROR = CRPROR;
       C7SHRT = CRSHRT;
       C7NRNW = CRNRNW;
       C7CFFL = CRCFFL;
       C7ERR  = CRERR;

       C7ISUD = CRISUD;
       C7PFRD = CRPFRD;
       C7PTOD = CRPTOD;
       C7CAND = CRCAND;
       C7MLDT = xxMLDT;
       C7EFDT = x1EFDT;
       C7PCXD = CRPCXD;
       C7CISD = CRCISD;
       C7RECD = CXRECD;
       xxTbid = 'RECD';
       xxKey = C7RECD;
       Chain(n) gtky pgt01;
       If %found;
          C7REDS = %trim(GTDESC);
       Endif;

       C7CNC# = CRCNC#;
       C7RPMY = CRRPMY;
       C7WEDT = 0;
       C7CNC# = CRCNC#;
       C7CRSN = CRCRSN;
       C7RESN = CRRESN;
       C7CXRN = CRCXRN;
       C7DESC = CRDESC;

       C7COM1 = CRCOM1;
       C7COM2 = CRCOM2;

       C7AMTD = CRAMTD;
       C7RFEE = CRRFEE;
       C7RFDS = CRRFDS;
       C7TDUE = CRTDUE;
       C7CEMN = CRCEMN;

       C7OCR  = CROCR;

       C7ForNam1 = QQName1;
       C7ForNam2 = QQName2;
       C7ForNam3 = QQName3;
       C7ForNam4 = QQName4;
       C7ForAdr1 = %trim(amn(##adrl));
       C7ForAdr2 = %trim(amn(##adrl+1));
       C7ForAdr3 = %trim(amn(##adrl+2));

       C7Fein = %editc(CRFein:'X');

       C7Pgm  = Q1PGM;
       C7User = Q1USER;
       C7Time = DsTime;
       C7Dt   = saveDate;

       Write pc701;

       Endsr;
       // *=========================================================================
       // *  ScriptMCX - Write record to work file for Scriptura output
       // *=========================================================================
       Begsr ScriptMCX;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear xxTemplate;
       Clear xxPath;

       Setgt (xxCO#:xxFND:RSJrSt:ELPfdt) pcr00;
       Readpe (xxCO#:xxFND:RSJrSt) pcr00;
       If not %eof;
          If STCRRCov <> ' ';
             xxTemplate = %trim(STCRRCov);
             xxPath = %trim(STCRRCovP);
          Endif;
       Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pcr00;
          Readpe (xxCO#:xxFnd:'  ') pcr00;
          If not %eof;
             If STCRRCov <> ' ';
                xxTemplate = %trim(STCRRCov);
                xxPath = %trim(STCRRCovP);
             Endif;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pcr00;
             Readpe (xxCO#:000:RSJrst) pcr00;
             If not %eof;
                If STCRRCov <> ' ';
                   xxTemplate = %trim(STCRRCov);
                   xxPath = %trim(STCRRCovP);
                Endif;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pcr00;
                Readpe (xxCO#:000:'  ') pcr00;
                If not %eof;
                   If STCRRCov <> ' ';
                      xxTemplate = %trim(STCRRCov);
                      xxPath = %trim(STCRRCovP);
                   Endif;
                Endif;
             Endif;
          Endif;
       Endif;

       If xxTemplate = ' ';
          Leavesr;
       Endif;

       message = %trim(processType);

       templateFile = outputFile;
       StLetter('TEMPLRQST':templateFile:message:'E':%editc(x#RRN:'X'):
        xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'));

       Chain (templateFile) xt100;

       XTOrder = order;
       order = order + 1;
       XTType = 'RE Cover Letter';
       XTDcid = xxTemplate;
       XTPath = xxPath;

       Update xt100;

       Endsr;
       // *=========================================================================
       // *  ScriptOrder - Update order field on xmlprep as needed.
       // *=========================================================================
       Begsr ScriptOrder;

       If not isUseXML;
          Leavesr;
       Endif;

       If order <> 1;

          If xmlLink = ' ';
             Chain (xmlFileName) xp100;
          Else;
             Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          Endif;

          XPOrder = order;

          Update xp100;

       Endif;

       Endsr;

      /end-free
      *
      *** Beg Add ***** 01/24/24 ************************************
      *=========================================================================
      * UpdQT - Update QUOTE Tracking Record
      *=========================================================================
      /free
       Begsr UpdQT;
        Setgt (EMco#:EMfnd:EMemp#:EMdiv) pqt12;
        Readpe (EMco#:EMfnd:EMemp#:EMdiv) pqt12;
        If not %eof(waqtl12);
           QTtype = 'C';
           QTwkdt = EMdt;
           QTdt   = EMdt;
           QTtime = EMtime;
           QTuser = Q1user;
           QTpgm  = Q1pgm;
           Write pqt12;
        Endif;
       Endsr;
      /end-free
      *** End Add ***** 01/24/24 ************************************
      *
     P*============================================
     P* Determine if the object exists
     P*============================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
      **  MONTHS
      *JANUARY
      *FEBRUARY
      *MARCH
      *APRIL
      *MAY
      *JUNE
      *JULY
      *AUGUST
      *SEPTEMBER
      *OCTOBER
      *NOVEMBER
      *DECEMBER
**  DA -- DARTS COMMENTS
REINSTATEMENT ISSUED ON
REINSTATEMENT REVISED ON
