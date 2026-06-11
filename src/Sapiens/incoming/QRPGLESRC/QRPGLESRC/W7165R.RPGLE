      * (c) 2025 Sapiens. All Rights Reserved.                             *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     h bnddir('WTBNDDIR')
     H/define CONVERT
     H***************************************************************
     H/TITLE W7165R - CANCELLATION NOTICE
     H***************************************************************
     H*  PROGRAM DESCRIPTION:
     H*     THIS PROGRAM PRINTS AN NCCI NOTICE OF TERMINATION FORM
     H*     AND WRITES A DARTS REMARK FOR THE CANCELLATION.
     H*
     H*  CALLED FROM:
     H*     W7166C
     H*
     H*  CALLS TO:
     H*     NONE
     H***************************************************************
     H*
     H* INDICATORS
     H*
     H* 60       FILE CHAIN
     H*
     H*****************************************************************
      * REVISIONS:
      *
      * 07/28/25 C:062 Project: 153WBJ
      *                Write A/R record for late fee
      *
      * 07/11/25 C:300 Support: 119359 (SUNZ)
      *                Master policy canceled in error, unable to reinstate
      *                Fix: Move WC2956 changes outside of TRANS subroutine
      *                and just before WDCXL file update
      *                                                                                            *
      * 02/26/24 C:340 Support: WC2956                                                             *
      *                Reset Flag - Cancellation/Reinstatement Process                             *
      *                             Control                                                        *
      *                                                               *
      * 01/24/24 C:300 Support: 114919 (AmWins)                       *
      *                Deletion of division being overridden by Cx/Rn *
      *                Fix: Before updating Cancellation Effective date
      *                in WMEMP, check WDCXP file for any cancellation*
      *                record exists with prior CX date for that division
      *
      * 08/15/23 C:062 Support: 113384 (129)                                                       *
      *                Pass "billing" for invoice type.                                            *
      *
      * 09/19/22 C:344 Support: 102988 (WC2663)                                                    *
      *                To correct this ticket change WMEAL6 to be                                  *
      *                non-locking.                                                                *
      *                                                                                            *
      * 07/21/22 C:33  Support: 102988 (WC2663)                                                    *
      *                Delete A/R based on RRN                                                     *
      *                                                                                            *
      * 06/29/22 C:340 Support: WC2854                                                             *
      *                Invoice Cloud:  Retain Payment Ref#                                         *
      *                                                                                            *
     H* 02/22/22 C:62  Project: 150WFN                                *
     H*                Use new GetNameAddress.                        *
     H*                                                               *
     H* 03/25/21 C:79  Project: WC2663                                *
     H*                Invoice Cloud changes                          *
     H*                                                               *
     H* 06/19/20 C:65  Support: 89453 (Sunz)                          *
     H*                Set the non-renew flag to yes value for any    *
     H*                cx codes overriden to report as non-renewal.   *
     H*                                                               *
     H* 02/10/20 C:298 Project: WC2704                                *
     H*                Cancellation Output Configuration - Print      *
     H*                for All Divisions or State Report Only.        *
     H*                                                               *
     H* 09/16/19 C:300 Support: 83101                                 *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Load week ending date into WXC7P/C7WEDT        *
     H*                for Mass batch S/R cancellations.              *
     H*                                                               *
     H* 08/29/19 C:300 Support: 80214 (FMIC)                          *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Load missing reporting period value into       *
     H*                WXC7P/C7RPMY for S/R cancellations.            *
     H*
     H* 10/28/18 C:079 Support: 99/74233
     H*                Clear WXC#P/CROPOL before loading each division
     H*
     H* 02/16/18 C:136 Support: 64432 (FCC)
     H*                Save Output from OutputWindow in SPILETTER
     H*                uses position 190 of LDA.  If set, it
     H*                causes Scriptura to fail when processing a mass
     H*                run. Adjusted ldMassFile in LDA to use
     H*                different positions.
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
     H*                Populate user CX fields for Scriptura.         *
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
     H* 10/30/15 C:307 Support: PHW38167                              *
     H*                -To except the routing apge under specific     *
     H*                 conditions only.                              *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 02/15/13 C:136 SUPPORT: WA045 81640                           *
     H*                CORRECT LOGIC FOR WRITING TO SEQ# FOR          *
     H*                WCPOLS REPORT                                  *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 10/17/11 C:278 Project: SY195                                 *
     H*                QDLS migration to IFS                          *
     H*                Add Two Parameters with command.               *
     H*                                                               *
     H* 10/31/11 C:189 SUPPORT: WA114 76913                           *
     H*                Work with cancellation batch process hang up   *
     H*                due to lock on Daily batch data area           *
     H*                                                               *
     H* 10/13/11 C:255 Support: WA114 77641                           *
     H*                Added change to display Federal Name correctly *
     H*                on report. (Issue related to second line of    *
     H*                display)                                       *
     H*                                                               *
     H* 04/06/11 C:235 PROJECT: 114WYM                                *
     H*                OCR for Cancellation/ Audit Bills              *
     H*                                                               *
     H* 08/09/10 C:191 PROJECT: 71WKU                                 *
     H*                Move Retro payments future of the cancellation *
     H*                date to cancellation A/R hold file             *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/23/09 C:232 Support: WA136/70790                           *
     H*                Issue related to second line of DBA name in the*
     H*                cancellation notice.                           *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Prior to deleting A/R, write the information   *
     H*                 to the A/R Holding file                       *
     H*                                                               *
     H* 05/11/09 C:136 PROJECT: 93WEV 93WHW                           *
     H*          C:61  WHEN CREATING A WMSHP RECORD, WRITE A 'XX'     *
     H*                TO THE STATE ARRAY IF THE EXPERIENCE RATING IS *
     H*                INTERSTATE  ('1') OR INTERSTATE AND INTRASTATE *
     H*                ('2')  FOR NCCI SUBMISSION.                    *
     H*                                                               *
     H* 04/07/09 C:220 SUPPORT: 66684                                 *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP             *
     H*                                                               *
     H* 02/09/09 C:33  Project: 136WDQ                                *
     H*                Add agent phone #.                             *
     H*                                                               *
     H* 12/11/08 C:33  Project: WC1769                                *
     H*                Add 6th and 7th assessments.                   *
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
     H*               -CLIENT SPECIFIC - SPECIAL FORMAT FOR POLICY #  *
     H*               -CLIENT SPECIFIC - PULL CNLFORM FROM FOLDER     *
     H*                'CLT071'                                       *
     H*                                                               *
     H* 06/23/06 C:168 PROJECT 129WNN (PART 2)                        *
     H*              - remembers the basic spool file property        *
     H*                                                               *
     H* 06/02/06 C:61  SUPPORT 133WBV WA133 52980                     *
     H*                Change to use system folder control "CC" for   *
     H*                Multiple CX Letter.                            *
     H*                                                               *
     H* 05/23/06 C:168 PROJECT 129WNN                                 *
     H*              - Retrieved the client id to either use the value*
     H*                of field SVAOUT or EXAOUT, if clt=129 and agt#=1
     H*                SVAOUT = *blanks                               *
     H*                                                               *
     H* 05/22/06 C:65  PROJECT: WC1419                                *
     H*                DO NOT WRITE WCPOLS WMSHP RECORD IF NO WMSHP   *
     H*                POLICY RECORD HAS BEEN WRITTEN YET.            *
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
     H*                Record name of resolved document for CNLFORM   *
     H*                in DARTS remark correctly as .PDF.             *
     H*                                                               *
     H* 09/02/05 C:61  PROJECT WC1359 129WLT                          *
     H*              - Print DOCs using W7165C3.                      *
     H*              - Print and resolve CX forms in .PDF format      *
     H*              - Create DARTS remark for CX cover letter        *
     H*                if specified.                                  *
     H*              - Return Member OUTQ to be used by Cert Holder   *
     H*                letter print process.                          *
     H*              - Output directed to three different OUTQ        *
     H*                (Member, Agent, State), if specified.          *
     H*                                                               *
     H* 07/25/05 C:61  PROJECT 133WBV                                 *
     H*              - Initialize Cancellation Count with value in    *
     H*                Cancelletion Detail WDXCP/CXCNC#.              *
     H*                                                               *
     H* 01/20/05 C:136 SUPPORT: WA129 45302                           *
     H*                If the cx date is revised, bring A/R back      *
     H*                from WMXAP. Use same processing that rein-     *
     H*                statement uses to bring A/R back.              *
     H*                                                               *
     H* 08/03/04 C:61  PROJECT WC966                                  *
     H*                Do not update Employer Master Cancel Date      *
     H*                if processing automatic flat cancellation      *
     H*                for renewal year.                              *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                    *
     H*                Add code to transfer 4th and 5th assessments   *
     H*                to WMXAP from WMEAP.                           *
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
     H*                Clear WX@#P before use by query W7165Q.        *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP   *
     H*                and WMXAP.                                     *
     H*                                                               *
     H* 07/02/03 C:61  PROJECT WC577                                  *
     H*                Load CRLSID with Employer Leasing ID (EMLSID)  *
     H*                                                               *
     H* 02/11/03 C:61  PROJECT WC1109                                 *
     H*              - Set non-renewal flag if state is MA and CX eff *
     H*                date equals policy expiration date.            *
     H*              - Load Unemployment Number                       *
     H*                                                               *
     H*  12/09/02 C:65 PROJECT: WC1064                                *
     H*                DELETE INSTALLMENT FEES IN THE FUTURE.         *
     H*                                                               *
     H* 07/03/02 C:61  PROJECT WC1081                                 *
     H*                Print the Federal Name on CX notice.           *
     H*                                                               *
     H* 04/24/02 C:61  SUPPORT WA112                                  *
     H*                Do not remove A/R records if policy year is    *
     H*                audited.                                       *
     H*                                                               *
     H* 11/27/01 C:61  SUPPORT WA107 29814  107BQ                     *
     H*                Only use active WTCRP records.                 *
     H*                                                               *
     H* 04/30/01 C:65  PROJECT: 114WAK                                *
     H*                COPY THE STATE LIST FROM THE WMSHP POLICY      *
     H*                RECORD INTO THE WMSHP CX RECORD.               *
     H*                                                               *
     H* 06/13/01 C:61  PROJECT WC1038                                 *
     H*              - Add seq number WXC#P.                          *
     H*              - Remove old code.                               *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: 109CO                                 *
     H*                 ADD 3RD ASSESSMENTS FOR A FEE.                *
     H*                                                               *
     H* 11/15/00  C:35   PROJECT: SUPPORT WA090 #25043                *
     H*                  DO NOT WRITE WCPOLS RECORDS WHEN PROCESSING  *
     H*                  A REPRINT.                                   *
     H*                                                               *
     H* 08/21/00 C:61  SUPPORT WA033 24028 WC984                      *
     H*                Correct when CRPCXD is populated.              *
     H*
     H* 06/12/00 C:61  PROJECT WC984
     H*              - If this is a renewal policy then load the
     H*                original policy number and effective date.
     H*              - If this is a revised cancellation, retrieve
     H*                the most recent prior cx history record for the
     H*                current cx sequence.  If the CX eff dt in this
     H*                record does not match the cx eff dt for the cx
     H*                being processed, load shcxdt into CRPCXD in the
     H*                format MMDDCCYY.
     H*              - Load Group FEIN from WDF2P.
     H*              - Load CRGCSZ to CRGAD2 if blank second addr.    *
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
     H* 03/10/00  C:35   PROJECT: WC900/SUPPORT WA096 #22081          *
     H*                  DO NOT DELETE PAST A/R RECORDS.              *
     H*                                                               *
     H* 12/06/99 C:118 SUPPORT: WA048/20840                           *
     H*                Modify the test of the carrier policy number   *
     H*                to see if it is an old-format PDS policy       *
     H*                number.                                        *
     H*                                                               *
     H* 09/16/99 C:65  PROJECT: WC851                                 *
     H*                PULL THE SHORT-RATE/PRORATE FLAG FROM WDELP.   *
     H*                                                              *
     H* 08/12/99 C:87  PROJECT: SUPP WA100 #19041                    *
     H*                Chgd XXFNAM to be length=60 char              *
     H*                                                              *
     H* 07/12/99 C:87  PROJECT: WC900                                *
     H*                After update of WDCXP, read WMEAP for         *
     H*                EMP.  Delete billing records if posting       *
     H*                date >= effective date of cancellation.       *
     H*                Billing transactions deleted include:         *
     H*                 B=Billing                                    *
     H*                 A=Adjustments                                *
     H*                 H=Deposit bills                              *
     H*                 F=Fees if category type is T=state tax       *
     H*                                            M=Member fees     *
     H*                                            S=Safety fees     *
     H*                                            1=1st Assessment  *
     H*                                            2=2nd Assessment  *
     H*                                                              *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*
     H* 04/29/99  C:35   PROJECT: SUPPORT WA096 #17258               *
     H*                  ALWAYS WRITE A NEW RECORD TO THE NCCI       *
     H*                  CANCELLATION TRANSACTION FILE.              *
     H*                                                              *
     H* 02/24/99  C:87   PROJECT: SUPPORT TASB #NONE                  *
     H*                  Change to use the current date as opposed to *
     H*                  the policy eff date when reading the canx    *
     H*                  reason code table file(WTCRP).               *
     H*                                                              *
     H* 12/17/98  C:87   PROJECT: SUPP #14639                        *
     H*                  Loaded confirmation cancelled flag (CXCFFL) *
     H*                  to workfile (CRCFFL).                       *
     H*                                                              *
     H* 11/10/98  C:87   PROJECT: WC852                              *
     H*                  Add parm for the certified notification     *
     H*                  flag. Change to use WTCRL instead of WTABL  *
     H*                  to retrieve canx reason file. If the Cert-  *
     H*                  ified Notification parm = 'Y' and the Cert- *
     H*                  ified Notification flag in the canx reason  *
     H*                  code file = 'Y', then produce additional    *
     H*                  output for certified mailing.               *
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
     H*                  DATE TO GET THE STATE CX FORM FOLDER AND    *
     H*                  DOCUMENT.  PASS THIS FOLDER AND DOCUMENT    *
     H*                  TO W7165C2 SO IT WILL PRINT WITH THE NCCI   *
     H*                  CANCELLATION DOCUMENT.                      *
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
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H****************************************************************
      *
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     UF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     infds(dbfds)
     FWSCRL     IF   E           K DISK    rename(pcr01:pcr00)
     F                                     prefix(ST)
     FWXC7P     O    E             DISK    USROPN
     FXMLPREL1  UF   E           K DISK
     FXMLTEML1  UF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
      *   CANCELLATION FILES
     FWDCXL     UF   E           K DISK
     FWDXHL     IF   E           K DISK
      *** Beg Add ***** 01/24/24 ************************************
     fwdcxl10   if   e           k disk    rename(pcx01:pcx10)
     f                                     prefix(p10_)
      *** End Add ***** 01/24/24 ************************************
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
      *** Beg Del *** 07/11/25 *************************************
      * Not using this file
     F***** WDCXL2    UF   E           K DISK    ReName(pcx01:pcx02)
      *** End Del *** 07/11/25 *************************************
      *** End Add *** 02/26/24 *** WC2956 **********************************
     F*** TABLE FILES
     F*
     FWTCRL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
      ****DARTS FILES
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     FWMEXL     IF   E           K DISK
      *   OUTPUT FILES
      *
     FWXC#L3    IF A E           K DISK
     F                                     RENAME(PCR01:PC#01)
     F*                                                                *********
     F*                                                                *********
     FWMSHP     O    E             DISK
     FWMSHL1    IF   E           K DISK
     F                                     RENAME(PSH01:PSH02)
     FWX#@P     O    E             DISK    USROPN
     FWMRPL     IF   E           K DISK
     FWTXCL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F*  Employer Accounts Receivable
     F***  Begin Chg  ***  09/19/22  *********************
     F****WMEAL6    UF   E           K DISK
     FWMEAL6    IF   E           K DISK
     F***  End Chg ******  09/19/22  *********************
     F*** Begin Add *** 07/28/25 *************************
     FWMEAL7    IF   E           K DISK    Rename(pea01:pea07) Prefix(E2:2)
     F*** End   Add *** 07/28/25 *************************
     F***  Begin Add  ***  07/21/22  *********************
     F                                     infds(eaFds6)
     F***  End   Add  ***  07/21/22  *********************
     FWMEAL14   UF A E           K DISK
     F                                     RENAME(PEA01:PEA014)
     F***  Begin Add  ***  07/21/22  *********************
     Fwmeap     uf   e             disk    rename(pea01:pea0p)
     F                                     infds(eaFdsP)
     F***  End   Add  ***  07/21/22  *********************
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     F*
     F*  Transaction Type Code Table  (WTTTP)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
     F*
     FWMXAL     UF   E           K DISK
     F                                     RENAME(PXA01:PXA0L)
     F*** Begin Add *** 07/28/25 *******************************************
     FWMXAL2    IF   E           K DISK    Rename(pxa01:pxa02)
     F*** End   Add *** 07/28/25 *******************************************
     FWMXAP     O    E             DISK
     FWMIFL     IF   E           K DISK
     FQSYSPRT   O    F  127        PRINTER OFLIND(*INOA)
      *
     FWMECL1    IF   E           K DISK
      *
      *
      * System User
     FSMUPL     IF   E           K DISK
     FWXAGP     O  A E             DISK    USROPN
     F                                     RENAME(PAG01:PAG01X)
     FWXAGL1    UF   E           K DISK    USROPN
     F                                     RENAME(PAG01:XAG01)
     F*
     FWMAPL     IF   E           K DISK
     FWMAHL1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDRRL     IF   E           K DISK    RENAME(PRR01:PRR00)
     F*
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
     D xxPre           s              2    inz('CC')
     d order           s              5  0
     d templateFile    s                   like(XTXMLT)
     d xxTemplate      s             60
     d xxPath          s            250
     d processType     c                   const('Cancellations')
     d xxstRptDv       s              5  0
     d*** Beg Add *** 03/25/21 **************************************
     d inJson          s          32000    Varying
     d n               s              3  0
     d isInvoiceCloud  s              1n   inz
     d isOnlyInactIC   s              1
     d o               s              3  0
     d isDupInv#       s              1
     d*** End Add *** 03/25/21 **************************************

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
     D***  Begin Add  ***  07/21/22  *********************
     D eaFds6          ds
     D  eaRrn6               397    400b 0
     D eaFdsP          ds
     D  eaRrnP               397    400b 0
     D***  End   Add  ***  07/21/22  *********************
     D*
     d x#RRN           s              9  0 inz(0)
     d*
     D/copy sPrXML
     D/copy wPrEmplr
     d*** Beg Add *** 03/25/21 *******************************************
     d/Copy sPrModule
     d/Copy wPrPmtSvc
     d/Copy wPrAR
     d*** End Add *** 03/25/21 *******************************************
     D*** Begin Add *** 07/28/25 *****************************
     D/copy sPrString
     D*** End   Add *** 07/28/25 *****************************
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
     D*** Begin Add *** 07/28/25 *****************************
     D outInvoice#     s                   Like(EAninv)
     D*** End   Add *** 07/28/25 *****************************

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D DA              S             65    DIM(2) CTDATA PERRCD(1)              DARTS COMMENT
     D MN              S              9    DIM(12) CTDATA PERRCD(1)
      *
     D CHGNUM          C                   CONST('WA003R')
     D MASCX           C                   CONST('CX Cover Letter att-
     D                                     ached')
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      *
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      *
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
      *
      *
     D                 DS
     D  DD                     1     20
     D                                     DIM(20) ASCEND                       DATE DESC
     D  XXDTDS                 1     20
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
      *
     D                 DS
     D  X1COMM                 1    256
     D  DCCOMM                 1     65
     D  CXRSN                 67     68
     D  $CRDSC                70    129
     D  CXCOM1               130    189
     D  CXCOM2               190    249
      *
     D                 DS
     D  DACOMM                 1     65
     D  XXMM                  46     47
     D  XXSL1                 48     48
     D  XXDD                  49     50
     D  XXSL2                 51     51
     D  XXCC                  52     53
     D  XXYY                  54     55
      *
     D                 DS
     D  X1DATE                 1      8  0
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1CC                   5      6
     D  X1YY                   7      8
      *
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
      *
     D                 DS
     D  EM                     1    300
     D                                     DIM(5)
     D  CRCM06                 1     60
     D  CRCM07                61    120
     D  CRCM08               121    180
     D  CRCM09               181    240
     D  CRCM10               241    300
      *
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
      *

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
     I              CREFDT                      $CREDT
     I              CRNOTC                      $CRNTC
     I              CRADDM                      $CRADM
     I              CRRFEE                      $CRFEE
     I              CRPRTC                      $CRPRT
     I              CRCERT                      $CRCRT
     I              CRAREI                      $CRAREI
     I              CRRDAY                      $CRRDAY
     I              CRPCNT                      $CRPCNT
     I              CRACNF                      $CRACNF
     I              CRCDAY                      $CRCDAY
     I              CRATRK                      $CRATRK
     I              CRPCHL                      $CRPCHL
     I              CRICE                       $CRICE
     I              CRUPSP                      $CRUPSP
     I              CRUPSC                      $CRUPSC
     I              CRNDNW                      $CRNDNW
     I              CRNTNP                      $CRNTNP
     I              CRMDNP                      $CRMDNP
     I              CRIDOV                      $CRIDOV
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
      *  EMPLOYER MASTER FILE
      *
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
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
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
      *
      *  AGENT ASSIGNMENT FILE
      *
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CXAGT#
      *
      *  AGENT MASTER FILE
      *
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      *  CANCELLATION DETAIL FILE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXREQ#
      *
      *  CANCELLATION HISTORY FILE
      *
     C     XHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     XHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXREQ#
      *
      *
      *  TABLE FILE KEY LIST CANCEL REASON CODE
      **********
     C*
     C     $CRKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXEFDT
     C*
     C     $CRKY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
      ********
      ***DARTS REMARKS FILE
      ********
      *
      *  WORK COMP CONTROL FILE
      *
     C     WCKY          KLIST
     C                   KFLD                    XCKEY             2
      *
      *  SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
      *
      *  CNCL/REIN TRANS FILE.
      *
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
     C                   KFLD                    CXCDAT
      *
      * GENERIC TABLE FILE KEY
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
      * ADMIN DARTS CONTROL FILE
      *
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * KEY LISTS FOR THE CANCELLATION/REINSTATEMENT STATE FILE
      *
     C     XCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
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
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*  Employer Accounts Receivable
     C*
     C     EAKY1         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C                   KFLD                    XXKDAT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C*
     C*  Transaction Type Code Table
     C     TTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
      *
      * SUBMISSION HISTORY FILE
      *
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
     C     IFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
      *
     C     IFKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
     C* Key list for A/R detail file
      *
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARCD
     C                   KFLD                    XXPODT
     C*
     C* Key list for Cancellation A/R detail file
     C*
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
      *
      *
      *================================================================
      * *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    XC#2DR        XXCLDR
     C     *LIKE         DEFINE    CRLSID        XXLSID
     C     *LIKE         DEFINE    CRSEQ#        SVSEQ#
     C     *LIKE         DEFINE    CRCAND        XXDUDT
     C     *LIKE         DEFINE    CROCR         XXSCAN
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    CXCO#         XXCO#
     C     *LIKE         DEFINE    CXFND         XXFND
     C     *LIKE         DEFINE    CXFYR         XXFYR
     C     *LIKE         DEFINE    CXEMP#        XXEMP#
     C     *LIKE         DEFINE    CXDIV         XXDIV
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C     *LIKE         DEFINE    $CRKEY        XXKEY
     C     *LIKE         DEFINE    XCEFDT        XXEFDT
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    EMFDNM        XXFDNM
     C     *LIKE         DEFINE    TTCO#         KKCO#
     C     *LIKE         DEFINE    TTKEY         KKKEY
     C     *LIKE         DEFINE    CXCXDT        XXKDAT
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EAARCD        XXARCD
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQREQ#            5
     C                   PARM                    QQREPR            1
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQCERT            1
     C                   PARM                    QQFLOW           61
     C                   PARM                    QSRPMY            6
     C                   PARM                    QSWEDT            8
      *
      *  Get the client id
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     *LIKE         DEFINE    INSTCK        MASSCX
     C*
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W0164C'      XXPGM            10
     C     MASSCX        PARM                    INSTCK            1
     C                   Clear                   QQFlow
      /free
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);

       Clear xmlLink;
       Clear xmlFileName;

       If MassCX <> *on;
          isMassReq = *off;
          massFlag = ' ';
       Else;
          isMassReq = *on;
          massFlag = 'Y';
       Endif;

      /end-free
      //*** Beg Add *** 03/25/21 ****************************************
       isInvoiceCloud = IsModEnabled('Invoice Cloud');
      //*** End Add *** 03/25/21 ****************************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
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
     C*
     C*  Parm list for call to pgm for centering (SRCENTER)
     C     CNTRPL        PLIST
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
      *
      *  LINE FORMAT
      *
     C     LINEPL        PLIST
     C                   PARM                    ##LINE          256
     C                   PARM                    ##LEN             3 0
     C                   PARM                    ##REM           256
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQREQ#        XXREQ#
     C                   MOVE      'P'           XXTYPE
     C*
     C                   MOVE      *BLANKS       XXFAD1           40
     C                   MOVE      *BLANKS       XXFAD2           40
     C                   MOVE      *BLANKS       XXFCSZ           40
      *
     C                   TIME                    DSDTTM
     C                   MOVE      'C'           ##TYP                          CANCEL
     C                   MOVE      Q1JOBN        $$JOB#            6
     C                   Z-ADD     *HIVAL        ##SEQ#
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSTIME        SYSTM
     C                   CLEAR                   P#@01
      *
     C                   MOVEL     'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              60
     C                   MOVEL     'SY'          ##KEY
     C     SYKY          CHAIN     SSY01                              60
      *
      *   GET CANCELLATION DETAIL RECORD
      *
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
      *   GET FUND INFORMATION
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       XXFNAM
     C                   ELSE
     C                   MOVEL     FNNAME        XXFNAM           60
     C                   ENDIF
     C     F1KY          CHAIN     PF101                              60
      *
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
     C                   SELECT
     C     CLTID         WHENEQ    '129'
     C     CXAGT#        ANDEQ     1
     C                   MOVEL     *BLANKS       SVAOUT
     C     CLTID         WHENEQ    '129'
     C     MASSCX        ANDEQ     *ON
     C                   MOVEL     'QPRINT'      SVAOUT
     C                   OTHER
     C                   MOVEL     EXAOUT        SVAOUT
     C                   ENDSL
      *
      *  Determine Member Output Queue
      *
     C     EXMOUT        IFNE      *BLANKS
     C                   MOVE      EXMOUT        QQOUTQ
     C                   ENDIF                                                  EXMOUT
      *
      *  else User
     C     QQOUTQ        IFEQ      *BLANKS
     C     Q1USER        CHAIN     PUP01                              60
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
      *
      *
      *   GET AGENT NAME
      *
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C     AGKY          CHAIN     PAG01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       XXANAM           40
     C                   CLEAR                   XXAARE
     C                   CLEAR                   XXAPHN
     C                   ELSE
     C                   MOVEL     AGNAME        XXANAM
     C     *LIKE         DEFINE    AGARE1        XXAARE
     C     *LIKE         DEFINE    AGPHN1        XXAPHN
     C                   Z-ADD     AGARE1        XXAARE
     C                   Z-ADD     AGPHN1        XXAPHN
     C                   ENDIF
      *
     C     CLTID         IFEQ      '102'
     C     AHKY          CHAIN     PAH01                              50
     C     APKY          CHAIN     PAP01                              50
     C                   MOVEL     APPROD        XXPROD            5
     C                   ENDIF                                                  CLTID = 102
      *
      *  GET THE EMPLOYER NAME, ADDRESS, CITY, STATE AND ZIP
      *
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXEAD1           40
     C                   MOVE      *BLANKS       XXEAD2           40
     C                   MOVE      *BLANKS       XXECSZ           40
     C                   MOVE      *BLANK        XXLSID
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMLSID        XXLSID
     C                   MOVEL     EMDBA         XXENAM
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
      *
     C     XXCITY        CAT(P)    ',':0         XXECSZ
     C                   CAT(P)    XXST:1        XXECSZ
     C                   CAT(P)    XXZIP5:1      XXECSZ
     C     XXZIP4        IFNE      *ZERO
     C                   CAT(P)    '-':0         XXECSZ
     C                   CAT(P)    XXZIP4:0      XXECSZ
     C                   ENDIF
     C*
     C     XXEAD2        IFNE      *BLANK
     C                   MOVE      *ON           *IN25
     C                   ELSE
     C                   MOVE      *OFF          *IN25
     C                   ENDIF
     C*
     C                   MOVEL(P)  EMFDNM        XXFDNM
     C*  Get the employer detail record
     C     ELKY          CHAIN     PEL01                              60
     C                   eval      x#RRN = dbRRN
     C*
     C*
     C* Use old NCCI Carrier # for LCTA if processing policy prior to 1/1/2016.
     C*
     C                   If        cltid = '045' and ELPfdt < 1160101
     C                   Z-add     33057         F2Car#
     C                   Z-add     33057         xxCarc
     C                   Endif
     C                   MOVE      SYSDT         XXEFDT
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      RSJRST        XXST
      *
     C     XCKY3         SETGT     PXC01
     C     XCKY4         READPE    PXC01                                  60
      *
      *
      *  GET CANCEL REASON DESCRIPTION
      *
     C                   MOVEL(P)  CXRSN         XXKEY
     C*
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
          GetStRptDv(ELco#:ELfnd:ELemp#) = ELdiv And
          Not (BSfreq = 'W' And BSseFL = 'S' And
               QSweDt <> '00000000' And QSwedt <> ' ');
          //
          // There must be a late fee and a late fee code
          If CRlFee <> 0 And CRlfCd <> ' ';
             // Write A/R Record
             Clear pea014;

             EAco#  = ELco#;
             EAfnd  = ELfnd;
             EAfyr  = ELfyr;
             EAemp# = ELemp#;
             EAdiv  = ELdiv;

             Chain (ELco#:CRlfCd) ptt01;
             EAarCd = CRlfCd;
             EAarTy = TTtype;
             EAdesc = TTdesc;
             EAcTyp = TTcTyp;
             EAamt  = CRlFee;

             EApoDt = GetPostDate(ELco#:ELfnd:ELfyr:ELemp#:ELdiv:sysDt:
                      outInvoice#);
             EAninv = outInvoice#;

             EAstat = 'A';

             // Update these fields with reinstatement info
             EAtrDt = sysDt;
             EAuser = Q1user;
             EApgm  = Q1pgm;
             DStime += 1;
             EAtime = DStime;
             EAdt   = sysDt;

             Write pea014;
          EndIf;
       EndIf;
       //*** End   Add *** 07/28/25 ****************************************
     C*
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   MOVE      $CRNCC        XXCD              2
     C                   MOVE      'CXCD'        XXTBID
     C                   MOVEL(P)  XXCD          XXKEY
     C     GTKY          CHAIN     PGT01                              61
      *
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     GTDESC        XXRESN           40
     C                   ELSE
     C                   MOVE      *BLANK        XXRESN
     C                   ENDIF                                                  *IN61 IFEQ
      *
      *  DETERMINE THE TYPE OF CANCELATION
      *
     C     ELPFDT        IFEQ      CXCXDT
     C                   MOVE      *ON           *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   ENDIF
      *
     C     ELPFDT        IFNE      CXCXDT
     C     ELSRPR        ANDEQ     'P'
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *ON           *IN72
     C                   MOVE      *OFF          *IN73
     C                   ENDIF
      *
     C     ELPFDT        IFNE      CXCXDT
     C     ELSRPR        ANDEQ     'S'
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *ON           *IN73
     C                   ENDIF
      *
      *  LOAD THE ISSUING OFFICE (COMPANY FILE) FOR NCCI FORM
      *
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
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *  LOAD AGENT NAME TO NCCI TRANS FILE
      *
     C                   MOVEL     XXANAM        XXAGNM           30
     C                   MOVEL     CXAGT#        XXA#C             5
     C     XXAGNM        CAT(P)    '/':1         XXAOUT           60
     C                   CAT       XXA#C:1       XXAOUT
     C     CLTID         IFEQ      '102'
     C                   CAT       '- Prod':1    XXAOUT
     C                   CAT       ' #':0        XXAOUT
     C                   CAT       XXPROD:1      XXAOUT
     C                   ENDIF                                                  CLTID = 102
      *
      *  UPDATE EMPLOYER MASTER WITH CANCELLATION DATE, EXCEPT ON A
      *  REPRINT OPTION
      *    OR
      *  If processing Auto Flat CX for renewal year
      *
     C     QQREPR        IFEQ      'N'
     C     CXAFLT        ANDNE     'Y'
     C                   Z-ADD     1             X                 2 0
      *
     C     *ZERO         LOOKUP    ED(X)                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF
      *
     C     X             IFNE      *ZERO
     C                   Z-ADD     CXCXDT        CD(X)
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   Z-ADD     DSTIME        EMTIME
      *** Beg Add ***** 01/24/24 ************************************
      * Set the Cancellation date to previous delete/cancel date
      /Free
       Setgt (EMco#:EMfnd:EMemp#:EMdiv) pcx10;
       Readpe (EMco#:EMfnd:EMemp#:EMdiv) pcx10;
       Dow not %eof(wdcxl10);
          If p10_CXrein <> 'Y' and p10_CXcxdt < CXcxdt;
             CD(X) = p10_CXcxdt;
             Leave;
          Endif;
          Readpe (EMco#:EMfnd:EMemp#:EMdiv) pcx10;
       Enddo;
      /End-Free
      *** End Add ***** 01/24/24 ************************************
     C                   UPDATE    PEM01
     C                   ENDIF
      *
     C                   ENDIF                                                  QQREPR
     C                   ENDIF                                                  EMKY
      *
     C                   UNLOCK    WMEML
      *
      * GET LAST CANCELLATION HISTORY RECORD.
      *
     C     XHKY          SETGT     PXH01
     C     XHKY          READPE    PXH01                                  61
     C     *IN61         IFEQ      *ON
     C                   MOVE      'N'           XHRVFL
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
      * WRITE A DARTS REMARK IF 1)THE NOTICE IS A NOT A REPRINT
      * & 2) ORIGINAL/REVISED DARTS CODE IS NOT BLANK.
      *
     C                   MOVE      'N'           XXDRT3            1
     C     QQREPR        IFNE      'Y'
     C     XHRVFL        ANDNE     'R'
     C     EXCXCD        ANDNE     *BLANK
     C     QQREPR        ORNE      'Y'
     C     XHRVFL        ANDEQ     'R'
     C     EXRCCD        ANDNE     *BLANK
     C                   MOVE      'Y'           XXDRT3            1
      *
      *********
     C                   ENDIF
      *
      *  ISSUE NCCI REINSTATEMENT NOTICE FORM
      *
     C     WCFL03        IFEQ      'Y'
     C     SYOFIF        OREQ      'Y'
     C                   EXSR      TRANS
     C                   ENDIF
     C*
     C     QQCERT        IFEQ      'Y'
     C     $CRCRT        ANDEQ     'Y'
     C                   EXSR      SROUTP
     C                   ENDIF                                                  QQCERT='Y'
     C                   MOVE      XXLSID        CRLSID
      *
      *   UPDATE THE CX RECORD WITH THE NCCI REINSTATE TYPE
      *
     C                   If        $CRIDOV <> ' '
     C                   Move      $CRIDOV       CXREVL
     C                   Else
     C                   SELECT
     C     CRCRSN        WHENEQ    '00'
     C                   MOVE      '3'           CXREVL
     C     CRFLAT        WHENEQ    'Y'
     C                   MOVE      '1'           CXREVL
     C                   OTHER
     C                   MOVE      '2'           CXREVL
     C                   ENDSL
     C                   Endif
      *
     C*** Beg Add *** 07/11/25 *************************************
      * Reset Pending/Processing Flag
       Chain (XXCO#:XXFND:XXFYR:XXEMP#:XXDIV:XXREQ#) PCX01;
       If %Found(wdcxl);
          Clear CXpdPr;
       EndIf;
     C*** End Add *** 07/11/25 *************************************

     C                   UPDATE    PCX01
     C*
     C*  Delete billings which are posted on or after the effective
     C*  date of cancellation (CXCXDT) if admin/DARTS flag = 'Y'
     C*  and policy year has not been audited.
     C*
       //*** Beg Add *** 03/25/21 ****************************************
                isOnlyInactIC = 'N';
       //*** End Add *** 03/25/21 ****************************************
     C     EXDLAR        IFEQ      'Y'
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
      *
     C     ELFAF         IFNE      'A'                                          NOT AUDITED
     C                   EXSR      SRDBIL
     C                   ENDIF                                                  ELFAF <> Y
      *
     C                   ENDIF
       //*** Beg Add *** 03/25/21 ****************************************
       If isInvoiceCloud and EXdLar <> 'Y' and EXicCxInv = 'Y';
       // Handle Invoice Cloud when group is NOT set to Delete AR but
       // Invoice Cloud is set to inactivate AR.

          Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pel01;
          If %found(wdell) and ELfaf <> 'A';
             isOnlyInactIC = 'Y';
             Exsr SrdBil;
          Endif;
       Endif;
       //*** End Add *** 03/25/21 ****************************************
      *
      *
      * WRITE A/R BACK IF NOT A REPRINT AND IF THE CX DATE HAS BEEN REVISED
      *
     C     QQREPR        IFNE      'Y'
     C                   EXSR      SRWARB
     C                   ENDIF                                                  QQREPR<>'Y'
      /Free
      /End-Free
     C                   If        xmlFileName <> ' ' and massFlag <> 'Y'
     C                   Eval      QQFlow = xmlFileName + '$'
     C                   Endif
     C     TERM          TAG
      *
      /Free
       If Qqcert = 'Y' and $Crcrt = 'Y' and isUseSpoolOut;
          xxPrt = '0';
          Chain ORKY POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
      /End-free
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
     C                   IN        DLYA
     C                   MOVEL     DLYA          XXENV             3
     C*
     C     XXENV         IFEQ      'TWC'
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM      'W7165TST'    XXOBJ            10
     C                   PARM      '*DTAARA'     XOTYPE            7
     C                   PARM                    XXERR             1
     C*
     C     XXERR         IFEQ      *OFF
     C     *DTAARA       DEFINE    W7165TST      TSTFLR           60
     C     *LOCK         IN        TSTFLR
     C                   OUT       TSTFLR
     C                   ENDIF
     C*
     C                   CALL      'SRCHKDLO'
     C                   PARM      'CNLFORM'     XXDLO            12
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
     C*
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C     EATRDT        IFLT      SYSDT
     C*
     C                   ADD       1             SYSTM
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1USER        EAUSER
     C                   Z-ADD     SYSDT         EADT
     C                   Z-ADD     SYSTM         EATIME
     C*
     C                   WRITE     PEA0#
      //*** Beg Add *** 03/25/21 ****************************************
      //*** Beg Del *** 09/06/21 ****************************************
      //*** n = n + 1;
      //*** arrInvoice#(n) = EAnInv;
      //*** Exsr SrInvoiceCloud;
      //*** End Del *** 09/06/21 ****************************************
      //*** End Add *** 03/25/21 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      *
      *===============================================================
      * PRTNCC - Print NCCI Form (If state form not printed)
      *===============================================================
     C     PRTNCC        BEGSR

      /Free
       If Not(isUseSpoolOut);
          LeaveSr;
       EndIf;
      /End-Free

     C     XCCXDC        IFEQ      *BLANKS                                      NO STATE FORM
      *
      *  Print NCCI CX Form - Member Copy
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
     C                   MOVEL(P)  'CNLFORM'     ##FDOC
     C                   MOVE(P)   *BLANKS       ##TFLR
     C                   MOVE(P)   *BLANKS       ##TDOC
      *
     C     XXDRT3        IFEQ      'Y'
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##TFLR           80
     C                   PARM      *BLANKS       ##TDOC           12
     C                   PARM      'CC'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##FND             3
     C                   PARM      'P'           ##RTCD            1
     C                   ENDIF
      *
      *
     C                   CALL      'W7165C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM                    ##TFLR           80
     C                   PARM                    ##TDOC           12
     C                   PARM      'CC'          ##PRE             2
     C                   PARM      QQOUTQ        ##OUTQ           10
     C                   PARM      'Y'           ##PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'M'           XXRUN             1
     C                   EXSR      WRTAGP
      *
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
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
      *
     C                   Z-ADD     *ZERO         X1DATE
     C                   MOVE      *BLANKS       DACOMM
      *
     C     XHRVFL        IFNE      'R'
     C                   MOVEL     DA(1)         DACOMM
     C                   MOVEL(P)  EXCXCD        Q#RMCD            5
      *
     C                   ELSE                                                   RVFL <> 'R'
      *
     C                   MOVEL     DA(2)         DACOMM
     C                   MOVEL     EXRCCD        Q#RMCD
     C                   ENDIF                                                  RVFL <> 'R'
      *
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1DATE
      *
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CC          XXCC
     C                   MOVE      X1YY          XXYY
     C                   MOVE      '/'           XXSL1
     C                   MOVE      '/'           XXSL2
     C                   MOVEL     DACOMM        DCCOMM
     C                   MOVEL(P)  X1COMM        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL     'N'           Q#SBYL            1
     C                   MOVEL(P)  XHUSER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL     'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  ##TDOC        Q#DCID           12
     C                   MOVEL(P)  ##TFLR        Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL     *BLANKS       Q#APID            2
     C                   MOVEL     *BLANKS       Q#IDOC           12
     C                   MOVEL     *BLANKS       Q#IQUE           10
     C                   MOVEL     'O'           Q#IMGF            1
     C                   MOVEL     *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL     *BLANK        Q#QPTY            1
     C                   MOVEL     *BLANKS       Q#DCTP            5
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
     C     'Cancella'    CAT(P)    'tion'        Q#CM75
     C                   CAT(P)    'Issue':1     Q#CM75
     C                   CAT(P)    'Date:':1     Q#CM75
      *
      * Get CX issue date, CX Req#
      *
     C                   MOVE      CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXTEMP           80
      *
     C                   CALL      'SA020R  '
     C                   PARM                    XXTEMP           80
     C                   PARM      'DATE'        #1MASK           10
      *
     C                   CAT(P)    XXTEMP:1      Q#CM75
     C                   CAT(P)    ',':0         Q#CM75
     C                   CAT(P)    'Req#:':1     Q#CM75
      *
     C                   MOVEL(P)  CXREQ#        XXTMP1           80
     C     '0'           CHECK     XXTMP1        X
     C                   SUBST(P)  XXTMP1:X      XXTEMP           80
     C                   CAT(P)    XXTEMP:1      Q#CM75
      *
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75           75
     C                   PARM                    Q#PNAM           10
     C                   PARM                    Q#SECL            3
     C                   PARM                    Q#USER           10
     C                   PARM                    Q#PGM            10
     C                   PARM                    Q#RMCD            5
     C                   PARM      *BLANKS       Q#ASQ#            5
      *
     C                   ENDIF                                                  XXDRT3
      *
      *
      *  Print NCCI CX Form - Agent Copy
      *
     C     SVAOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7165C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM      *BLANKS       ##TFLR           80
     C                   PARM      *BLANKS       ##TDOC           12
     C                   PARM      'CC'          ##PRE             2
     C                   PARM      SVAOUT        ##OUTQ           10
     C                   PARM      'N'           ##PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'A'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
      *  Print NCCI CX Form - State Copy
      *
     C     EXSOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7165C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM      *BLANKS       ##TFLR           80
     C                   PARM      *BLANKS       ##TDOC           12
     C                   PARM      'CC'          ##PRE             2
     C                   PARM      EXSOUT        ##OUTQ           10
     C                   PARM      'N'           ##PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
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
      *===============================================================
      * EMPINF - Get Employer Information
      *===============================================================
     C     EMPINF        BEGSR
     C                   MOVE      *BLANKS       EM
      *
     C                   MOVEL(P)  EMNAME        EM(1)
     C                   Z-ADD     2             J3                3 0
      *
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL(P)  EMFDNM        EM(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C     EMMPFL        IFEQ      'M'
      *
     C     EMMAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMMAD1        EM(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C     EMMAD2        IFNE      *BLANKS
     C                   MOVEL(P)  EMMAD2        EM(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C     EMMCTY        IFNE      *BLANKS
     C                   MOVEL(P)  EMMCTY        EM(J3)
     C                   CAT(P)    ',':0         EM(J3)
     C                   CAT(P)    EMMST:1       EM(J3)
     C                   MOVEL     EMMZIP        XXZIP5            5
     C                   CAT(P)    XXZIP5:1      EM(J3)
     C                   MOVE      EMMZIP        XXZIP4            4
      *
     C     XXZIP4        IFNE      *ZERO
     C                   CAT(P)    '-':0         EM(J3)
     C                   CAT(P)    XXZIP4:0      EM(J3)
     C                   ENDIF                                                  XXZIP4
      *
     C                   ADD       1             J3
     C                   ENDIF                                                  EMMCTY
      *
     C                   ELSE
      *
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD1        EM(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C     EMPAD2        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD2        EM(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C     EMPCTY        IFNE      *BLANKS
     C                   MOVEL(P)  EMPCTY        EM(J3)
     C                   CAT(P)    ',':0         EM(J3)
     C                   CAT(P)    EMPST:1       EM(J3)
     C                   MOVEL     EMPZIP        XXZIP5            5
     C                   CAT(P)    XXZIP5:1      EM(J3)
     C                   MOVE      EMPZIP        XXZIP4            4
      *
     C     XXZIP4        IFNE      *ZERO
     C                   CAT(P)    '-':0         EM(J3)
     C                   CAT(P)    XXZIP4:0      EM(J3)
     C                   ENDIF                                                  XXZIP4
      *
     C                   ADD       1             J3
     C                   ENDIF                                                  EMPCTY
      *
     C                   ENDIF                                                  EMMPFL
      *
     C                   ENDSR                                                  EMPINF
      *
      *
      *
      *
      *===================================================================
      * SRORIG - Get Orig Pol Num, Orig Pol Eff Dt, Prior CX Date
      *===================================================================
     C     SRORIG        BEGSR
      *
     C     ELKY          CHAIN     PEL01                              60
     C  N60ELKY3         READPE    PEL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     ELKY3         CHAIN     PEL01                              60
     C     RPKY1         CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL(P)  RPCAP#        CROPOL
      *
     C                   ELSE
      *
      /Free
                         Clear CRoPol;
      /End-Free
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
     C     ELKY          CHAIN     PEL01                              60
      *
      *
     C     XHKY1         SETGT     PXH01
     C     XHKY1         READPE    PXH01                                  61
      *
     C  N61XHRVFL        IFEQ      'R'
     C     XHKY1         READPE    PXH01                                  61
      *
     C  N61XHCXDT        IFNE      CXCXDT
      *
     C                   MOVE      XHCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRPCXD
      *
     C                   ENDIF                                                  CXDT
      *
     C                   ENDIF                                                  XHRVFL = R
      *
      *
     C                   Z-ADD     XXFEIN        CRGFIN
      *
     C                   ENDSR                                                  SRORIG
      *
      *
      *
      ********************************************************************
      *  TRANS SUBROUTINE  -  CREATE TRANSACTION FOR NCCI REPORTING
      ********************************************************************
     C     TRANS         BEGSR
      *
      *
     C     CRKY1         SETGT     PC#01
     C     CRKY1         READPE    PC#01                                  60
     C  N60              Z-ADD     CRSEQ#        SVSEQ#
     C   60              Z-ADD     *ZERO         SVSEQ#
     C     SVSEQ#        ADD       1             CRSEQ#
      *
     C                   MOVE      XXCO#         CRCO#
     C                   MOVE      XXFND         CRFND
     C                   MOVE      XXEMP#        CREMP#
     C                   MOVE      XXFYR         CRFYR
     C                   TIME                    CRUNID
     C                   MOVEL     'C'           CRCORR
      *
     C                   MOVE      *BLANKS       CRFLAT
     C                   MOVE      *BLANKS       CRPROR
     C                   MOVE      *BLANKS       CRSHRT
     C                   MOVE      *BLANK        CRNRNW
     C     $CRNCC        IFEQ      '00'
     C     $CRIDOV       OREQ      '3'
     C                   MOVE      'Y'           CRNRNW
     C                   ELSE
     C   71              MOVE      'Y'           CRFLAT
     C   72              MOVE      'Y'           CRPROR
     C   73              MOVE      'Y'           CRSHRT
     C                   ENDIF
      *
      * Set Non-Renewal flag if rating state = 'MA' and
      * CX effective date = Policy expiration date
      *
     C     ELKY          CHAIN     PEL01                              60
     C  N60              MOVE      ELRTCD        XXRTCD
     C  N60RSKY          CHAIN     PRS01                              60
      *
     C  N60RSJRST        IFEQ      'MA'
     C     CXCXDT        ANDEQ     ELPTDT
     C                   MOVE      'Y'           CRNRNW
     C                   ENDIF
      *
      *
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
     C                   MOVEL(P)  ##LINE        CRNAM1
      *
     C                   MOVE(P)   CRNAM2        XXTMP4           50
     C                   MOVEL(P)  XXTMP4        CRNAM2
      *
     C                   MOVEL(P)  CRFDN1        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN1
      *
     C                   MOVE(P)   CRFDN2        XXTMP4           50
     C                   MOVE(P)   XXTMP4        CRFDN2
      *
     C     XXEAD1        CAT(P)    ', ':0        CREADR
     C                   CAT(P)    XXECSZ:1      CREADR
      *
     C                   MOVE      CXCXDT        CREFDT
     C                   MOVE      CXCDAT        CRMLDT
      *
     C                   MOVEL     'A'           CRSTAT
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C                   Z-ADD     DSTIME        CRTIME
      *
     C                   MOVE      XXCARC        CRCARR
     C                   MOVE      ELPFDT        CRPFDT
     C                   MOVE      XXCD          CRCRSN
     C                   MOVE      *ZERO         CRTORE
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
     C                   MOVEL     CRECSZ        CREAD2
     C                   MOVEL     *BLANKS       CRECSZ
     C                   ENDIF                                                  CREAD2
      *
      *
     C                   MOVE      XXFND         #@FND             3
     C                   MOVE      XXEMP#        #@EMP#            9
     C                   MOVE      XXFYR         #@FYR             3
      *
     C     #@FND         CAT(P)    #@EMP#:0      CRPOL#
     C                   CAT       #@FYR:0       CRPOL#
      *
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
     C                   MOVE      ELPTDT        CRPTDT
     C                   MOVEL     XXRESN        CRRESN
     C                   Eval      CRCXRN = CXRSN
     C                   Eval      CRDESC = $CRDSC
     C                   MOVEL     XXISUO        CRISUO
     C                   MOVEL     XXAOUT        CRAOUT
     C                   Z-ADD     XXAARE        CRAARE
     C                   Z-ADD     XXAPHN        CRAPHN
      *
     C                   MOVE      CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRISUD
      *
      *  Month DD, CCYY
      *
     C                   MOVEL     SRDTO         X3                2 0
     C                   MOVE      CRISUD        X3DCY             6
     C                   MOVEL     X3DCY         X3DD              2
     C                   MOVEL     X3DD          X3DD1             1
     C     X3DD1         COMP      '0'                                    74
     C   74              MOVE      X3DD          X3DD1             1
     C                   MOVE      X3DCY         X3CCYY            4
      *
     C                   MOVEL(P)  MN(X3)        CRCM03
     C  N74              CAT(P)    X3DD:1        CRCM03
     C   74              CAT(P)    X3DD1:1       CRCM03
     C                   CAT(P)    ',':0         CRCM03
     C                   CAT(P)    X3CCYY:1      CRCM03
      *
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRCISD
      *
     C                   MOVE      ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRPFRD
      *
     C                   MOVE      ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRPTOD
      *
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRCAND
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
     C                   EXSR      EMPINF
     C                   ENDIF
      *
      *
     C                   MOVE      CXAMTD        CRAMTD
     C*** Begin Add *** 07/28/25 *******************************************
     C                   Eval      CRamtD += CXlFee
     C*** End   Add *** 07/28/25 *******************************************
     C                   MOVE      CXRFEE        CRRFEE
      *
     C                   Z-ADD     CXRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         CRRPMY
     C                   MOVE      CXERR         CRERR
      *
      *  CARRY COMMENTS FORWARD TO WX#@P
      *
     C                   MOVEL     CXCOM1        CRCOM1
     C                   MOVEL     CXCOM2        CRCOM2
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
     C*
     C                   MOVEL(P)  CRGAD1        XXFAD1
     C                   MOVEL(P)  CRGCSZ        XXFCSZ
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
      *
      * Get original Policy number, Policy effective date, Prior CX date
      *
     C                   EXSR      SRORIG
      *
      *
      * Get user name
      *
     C     Q1USER        CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CRCM01
     C  N60              MOVEL(P)  UPEXT#        CRCM02
      *
      *    Remove Leading Zeros from extension
     C                   MOVEL     CRCM02        XXTMP2            5
     C     '0'           CHECK     XXTMP2        X4                3 0
     C                   SUBST(P)  XXTMP2:X4     CRCM02
      *
      *  Get Agent Name / Address
      *
     C     AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AGNAME        CRC01B
     C                   MOVEL(P)  AGMAD1        CRC02B
     C                   MOVEL(P)  AGMCTY        CRC03B
     C                   CAT(P)    ',':0         CRC03B
     C                   CAT(P)    AGMST:1       CRC03B
     C                   MOVEL(P)  AGMZIP        X1ZIP5            5
     C                   MOVE(P)   AGMZIP        X1ZIP4            4
     C                   CAT(P)    X1ZIP5:1      CRC03B
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
     C     X1ZIP4        IFNE      *ZERO
     C                   CAT(P)    '-':0         CRC03B
     C                   CAT(P)    X1ZIP4:0      CRC03B
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   Z-ADD     CXCNC#        CRCNC#
      *
     C                   MOVE      *OFF          *IN36
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN36
      *
     C                   MOVE      CRAMTD        XXAMT            11 2
     C                   CALL      'WA082R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXEMP#
     C                   PARM                    XXFYR
     C                   PARM                    XXDIV
     C                   PARM      CRCAND        XXDUDT
     C                   PARM                    XXAMT
     C                   PARM                    XXSCAN
     C                   MOVEL     XXSCAN        CROCR
     C                   ENDIF
      *
      *
      *  UPDATE FILE
     C     WCFL03        IFEQ      'Y'
     C     EMSTRP        ANDEQ     'Y'
     C     QQREPR        ANDNE     'Y'
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
      * DOCUMENT FOR THE STATE CANCELLATION FORM
      *
     C                   MOVE      *BLANKS       QQCXFL           60
     C                   MOVE      *BLANKS       QQCXDC           12
     C                   MOVE      ELRTCD        XXRTCD
     C                   CLEAR                   XXST
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      RSJRST        CRJRST
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT
      *
     C     XCKY          SETGT     PXC01
     C     XCKY2         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  XCCXFL        QQCXFL
     C                   MOVEL(P)  XCCXDC        QQCXDC
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   MOVE      CXCFFL        CRCFFL
      *
      *  CREATE A DB FILE TO USE IN A DOCUMENT QUERY, THEN EXECUTE THE
      *  CODE TO PRT THE DOC WITH THE DATA
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
     C*
     C*** Beg Add *** 02/26/24 *** WC2956 **********************************
      * Reset Pending/Processing Flag
     C*** Beg Del *** 07/11/25 *************************************
      * Moving these changes outside of TRANS subroutine
      * and just before WDCXP file update

       // Chain (XXCO#:XXFND:XXFYR:XXEMP#:XXDIV:XXREQ#) WDCXL;
       // If %Found;
       //    Clear CXpdPr;
       //    //Update Pcx02;
       // EndIf;

     C*** End Del *** 07/11/25 *************************************
     C*** End Add *** 02/26/24 *** WC2956 **********************************
     C*
     C                   If        isUseXml
     C                   Open      wxc7p
     C                   Exsr      ScriptXML
     C                   Close     wxc7p
     C                   Endif
      *
      *   Multiple CX letter
      *
      /Free
       Exsr srCltr;
       //  If cover letter, print and resolve to darts
       Exsr prtMcx;
       //  If state form, then print and resolve to darts in .pdf format
       Exsr dart3;
       //  If state form not printed, the print ncci cx form and resolve
       //  to darts in .pdf format
       Exsr prtNcc;
      /End-Free
     C                   Exsr      ScriptOrder

     C                   ENDIF                                                  SYOFIF IFEQ 'Y'
      *
      *
      *  Write record to NCCI Submission History File
      *
     C     WCFL03        IFEQ      'Y'
     C     EMSTRP        ANDEQ     'Y'
     C     QQREPR        ANDNE     'Y'
      *
     C     SHKY          CHAIN     PSH02                              60
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
      *
     C                   MOVE      *BLANKS       XXSST            30
     C                   MOVE      *BLANKS       XXSSS            15
     C     SHKY          CHAIN     PSH02                              60
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     1             X                 2 0
      *
     C     ELKY2         CHAIN     PEL02                              60
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
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     CLTID         IFEQ      '093'
      *
      * ADD NCCI STATE RECORD IF POLICY HAS EXPERIENCE RATING OF INTERSTATE ('1'
      * INTERSTATE AND INTRASTATE ('2'),  IF NEEDED
      *
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
      *
     C                   ENDIF                                                  CLTID = 093
     C*
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
     C                   MOVE      XXEMP#        SHEMP#
     C                   MOVE      XXFYR         SHFYR
     C                   MOVE      XXDIV         SHDIV
     C                   MOVE      CRUNID        SHUNID
     C                   MOVE      CRDT          SHTRDT
      *
     C                   MOVE      *BLANKS       SHSBDT
     C                   MOVE      *BLANKS       SHSBDT
     C                   MOVE      'C'           SHTYPE                               CANCEL
     C                   MOVE      'O'           SHCODE
     C                   MOVE      '3'           SHSTAT
      *
     C     ##SEQ#        IFNE      *ZEROS
     C                   MOVE      ##SEQ#        SHSEQ#
     C                   ELSE
     C                   MOVE      *BLANKS       SHSEQ#
     C                   ENDIF
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   Z-ADD     DSTIME        SHTIME
     C                   Z-ADD     SHDT          SHIDT
     C                   MOVEL     Q1USER        SHIUSR
      *
     C                   MOVE      ELPFDT        SHPFDT
     C                   WRITE     PSH01
     C                   ENDIF                                                  SHSTAT IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  WCFL03 IFEQ 'Y'
      *
     C     ETRANS        ENDSR
      *
      *
      *
      ********************************************************************
      *  PRTMCX - Print/Resolve CX cover letter
      ********************************************************************
     C     PRTMCX        BEGSR
      *
     C     XCKY3         SETGT     PXC01
     C     XCKY4         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   Exsr      ScriptMCX
      *
      *  If CX Cover letter specified
      *
     c                   If        xcCcLd <> *blanks and
     c                             isUseSpoolOut
      *
      *  NCCI Reason Code must match (if entered)
      *
     C     XCCCRS        IFEQ      $CRNCC
     C     XCCCRS        OREQ      *BLANKS
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'CC'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##FND             3
     C                   PARM      *BLANKS       ##RTCD            1
      *
     C                   EXSR      DART2
      *
     C                   MOVEL(P)  XCCCLF        #3CCLF
     C                   MOVEL(P)  XCCCLD        #3CCLD
      *
      *  Print Member Copy - CX Cover Letter
      *
     C                   CALL      'W7165C3'
     C                   PARM                    #3CCLF           60
     C                   PARM                    #3CCLD           12
     C                   PARM                    #3STFL           80
     C                   PARM                    #3STDC           12
     C                   PARM      'CC'          #3PRE             2
     C                   PARM      QQOUTQ        #3OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'M'           XXRUN
     C                   EXSR      WRTAGP
      *
      *  Print Agent Copy - CX Cover Letter
      *
     C     SVAOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7165C3'
     C                   PARM                    #3CCLF           60
     C                   PARM                    #3CCLD           12
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'CC'          #3PRE             2
     C                   PARM      SVAOUT        #3OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'A'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
     C                   ENDIF                                                  XCCCRS
      *
     C                   ENDIF                                                  XCCCLD
      *
     C                   ENDIF                                                  XCKY
      *
     C                   ENDSR                                                  PRTMCX
      *
      *
      *
      *
      *
      *
      ********************************************************************
      *  DART2 - Resolve CX Cover Letter to DARTS
      ********************************************************************
      *
     C     DART2         BEGSR
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C                   MOVEL(P)  QCKEY         Q#KEY            40
      *
     C     XHRVFL        IFNE      'R'
     C                   MOVEL(P)  EXCXCD        Q#RMCD            5
      *
     C                   ELSE                                                   RVFL <> 'R'
      *
     C                   MOVEL     EXRCCD        Q#RMCD
     C                   ENDIF                                                  RVFL <> 'R'
      *
      ****
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
      *
     C                   MOVEL(P)  #3STDC        Q#DCID
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVEL(P)  #3STFL        Q#FOLD
      *
     C                   MOVEL(P)  MASCX         Q#COMM
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
     C                   PARM      *BLANK        Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *ZEROS        Q#PIDX            5
     C                   PARM      *BLANK        Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
      *
     C                   ENDSR                                                  DART2
      *
      *
      *
      *===================================================================
      *  DART3 - Resolve state forms to DARTS as .PDF
      *===================================================================
      *
     C     DART3         BEGSR

      /Free
       If Not(isUseSpoolOut);
          LeaveSr;
       EndIf;
      /End-Free
      *
     C     QQCXFL        IFNE      *BLANKS
     C     QQCXDC        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       #3STFL
     C                   MOVE      *BLANKS       #3STDC
      *
     C     XXDRT3        IFEQ      'Y'
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'CC'          #3PRE             2
     C                   PARM      QQCO#         #3CO#             3
     C                   PARM      QQFND         #3FND             3
     C                   PARM      'P'           #3RTCD            1
     C                   ENDIF
      *
      *  Print State CX form - Member Copy
      *
     C                   CALL      'W7165C3'
     C                   PARM      QQCXFL        #3CXFL           60
     C                   PARM      QQCXDC        #3CXDC           12
     C                   PARM                    #3STFL           80
     C                   PARM                    #3STDC           12
     C                   PARM      'CC'          #3PRE             2
     C                   PARM      QQOUTQ        #3OUTQ           10
     C                   PARM      'Y'           #3PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
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
     C     XHRVFL        IFNE      'R'
     C                   MOVEL(P)  EXCXCD        Q#RMCD            5
      *
     C                   ELSE                                                   RVFL <> 'R'
      *
     C                   MOVEL     EXRCCD        Q#RMCD
     C                   ENDIF                                                  RVFL <> 'R'
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
     C                   Z-ADD     *ZERO         X1DATE
     C                   MOVE      *BLANKS       DACOMM
     C     XHRVFL        IFNE      'R'
     C                   MOVEL     DA(1)         DACOMM
     C                   MOVEL(P)  EXCXCD        Q#RMCD            5
     C                   ELSE                                                   RVFL <> 'R'
     C                   MOVEL     DA(2)         DACOMM
     C                   MOVEL     EXRCCD        Q#RMCD
     C                   ENDIF                                                  RVFL <> 'R'
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CC          XXCC
     C                   MOVE      X1YY          XXYY
     C                   MOVE      '/'           XXSL1
     C                   MOVE      '/'           XXSL2
     C                   MOVEL     DACOMM        DCCOMM
     C     XXST          CAT(P)    'State':1     XXTMP3           10
     C     XXTMP3        CAT(P)    X1COMM:1      Q#COMM          256
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
      *
      *  Print State CX form - Agent Copy
      *
     C     SVAOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7165C3'
     C                   PARM      QQCXFL        #3CXFL           60
     C                   PARM      QQCXDC        #3CXDC           12
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'CC'          #3PRE             2
     C                   PARM      SVAOUT        #3OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'A'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
      *  Print State CX form - State Copy
      *
     C     EXSOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7165C3'
     C                   PARM      QQCXFL        #3CXFL           60
     C                   PARM      QQCXDC        #3CXDC           12
     C                   PARM      *BLANKS       #3STFL           80
     C                   PARM      *BLANKS       #3STDC           12
     C                   PARM      'CC'          #3PRE             2
     C                   PARM      EXSOUT        #3OUTQ           10
     C                   PARM      'N'           #3PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'S'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXSOUT
      *
     C                   ENDIF                                                   CXFL CXDC
      *
     C                   ENDSR                                                  DART3
      *
      *
      *
      *
      *================================================================
      *  SRCLTR - Set up to print CX letter.
      *================================================================
     C     SRCLTR        BEGSR
     C                   MOVE      *BLANKS       ##CFFL           60
     C                   MOVE      *BLANKS       ##CFDC           12                        ENT
     C                   MOVE      *BLANKS       ##CTFL           60
     C                   MOVE      *BLANKS       ##CTDC           12
      *
      *  If CX for reason of Non-payment
      *
     C     $CRNCC        IFEQ      '05'                                         NON-PAYMENT
     C     CXAFLT        ANDNE     'Y'                                          NOT RENEWAL YR
      *
      *  Select CX Letter
      *
     C                   SELECT
      *
     C     CXCNC#        WHENEQ    1                                            # OF CX
     C     XC#1LD        ANDNE     *BLANKS
     C                   MOVEL(P)  XC#1LF        ##CFFL                         FOLDER
     C                   MOVEL(P)  XC#1LD        ##CFDC                         LETTER DOCUMENT
     C                   MOVEL(P)  XC#1DR        XXCLDR                         DARTS CODE
      *
     C     CXCNC#        WHENEQ    2
     C     XC#2LD        ANDNE     *BLANKS
     C                   MOVEL(P)  XC#2LF        ##CFFL
     C                   MOVEL(P)  XC#2LD        ##CFDC
     C                   MOVEL(P)  XC#2DR        XXCLDR
      *
     C     CXCNC#        WHENEQ    3
     C     XC#3LD        ANDNE     *BLANKS
     C                   MOVEL(P)  XC#3LF        ##CFFL
     C                   MOVEL(P)  XC#3LD        ##CFDC
     C                   MOVEL(P)  XC#3DR        XXCLDR
      *
     C     CXCNC#        WHENEQ    4
     C     XC#4LD        ANDNE     *BLANKS
     C                   MOVEL(P)  XC#4LF        ##CFFL
     C                   MOVEL(P)  XC#4LD        ##CFDC
     C                   MOVEL(P)  XC#4DR        XXCLDR
      *
     C                   OTHER
     C                   MOVE(P)   *BLANKS       ##CFFL
     C                   MOVE(P)   *BLANKS       ##CFDC
     C                   MOVE(P)   *BLANKS       XXCLDR
      *
     C                   ENDSL
     C                   Exsr      ScriptCLTR
      *
      *  Resolve CX Letter to DARTS as required
      *
     c                   If        xxClDr <> *blanks and
     c                             isUseSpoolOut
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #1FOLD           80
     C                   PARM      *BLANKS       #1DCID           12
     C                   PARM      'CC'          #1PRE             2
     C                   PARM      QQCO#         #1CO#             3
     C                   PARM      QQFND         #1FND             3
     C                   PARM      *BLANKS       #1RTCD            1
      *
     C                   MOVEL     #1FOLD        ##CTFL
     C                   MOVEL     #1DCID        ##CTDC
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      QQCO#         QCCO#
     C                   MOVE      QQFND         QCFND
     C                   MOVE      QQEMP#        QCEMP#
     C                   MOVE      QQDIV         QCDIV
     C                   MOVEL(P)  QCKEY         #1KEY
      *
     C     'MULTIPLE'    CAT(P)    'CX':1        #1COMM
     C                   CAT(P)    'LETTE':1     #1COMM
     C                   CAT(P)    'R':0         #1COMM
      *
      *************
     C                   MOVE      DSDATE        #1CDAT
     C                   MOVEL(P)  'W0050R'      #1PNAM
     C                   MOVEL(P)  XXCLDR        #1RMCD
      *
      *  API to create Single DARTS remark
      *
     C                   CALL      'DA700R'
     C                   PARM                    #1KEY            40
     C                   PARM                    #1COMM          256
     C                   PARM                    #1CDAT            8
     C                   PARM                    #1DCID           12
     C                   PARM                    #1PNAM           10
     C                   PARM      *BLANKS       #1SECL            3
     C                   PARM      Q1USER        #1USER           10
     C                   PARM      Q1PGM         #1PGM            10
     C                   PARM                    #1FOLD           80
     C                   PARM      'N'           #1DIFL            1
     C                   PARM      Q1PGM         #1IPGM           10
     C                   PARM      'N'           #1FL01            1
     C                   PARM      'N'           #1FL02            1
     C                   PARM                    #1RMCD            5
     C                   PARM      *BLANKS       #1SEQ#            5
      *
      *  Print Multiple CX Letter - Member Copy
      *
     C                   MOVEL(P)  ##CFFL        ##FFLR
     C                   MOVEL(P)  ##CFDC        ##FDOC
     C                   MOVEL(P)  ##CTFL        ##TFLR
     C                   MOVEL(P)  ##CTDC        ##TDOC
      *
     C                   CALL      'W7165C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM                    ##TFLR           80
     C                   PARM                    ##TDOC           12
     C                   PARM      'CC'          ##PRE             2
     C                   PARM      QQOUTQ        ##OUTQ           10
     C                   PARM      'N'           ##PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'M'           XXRUN
     C                   EXSR      WRTAGP
      *
      *  Print Multiple CX Letter - Agent Copy
      *
     C     SVAOUT        IFNE      *BLANKS
      *
     C                   CALL      'W7165C3'
     C                   PARM                    ##FFLR           60
     C                   PARM                    ##FDOC           12
     C                   PARM      *BLANKS       ##TFLR           80
     C                   PARM      *BLANKS       ##TDOC           12
     C                   PARM      'CC'          ##PRE             2
     C                   PARM      SVAOUT        ##OUTQ           10
     C                   PARM      'N'           ##PPDF            1
      *
      *
      *  writes the spool file record to file WXAGP
     C                   MOVEL     'A'           XXRUN
     C                   EXSR      WRTAGP
      *
     C                   ENDIF                                                  EXAOUT
      *
      *
     C                   ENDIF                                                  XXCLDR
      *
     C                   ENDIF                                                  $CRNCC
      *
     C                   ENDSR                                                  SRCLTR
      *
      *
     C*================================================================
     C*  SROUTP - OUTPUT FOR CERTIFIED NOTIFICATION
     C*================================================================
     C     SROUTP        BEGSR
     C*
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZCXDT            8 0
     C*
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZCDAT            8 0
     C*
     C*  PRINT ROUTING PAGE
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKY          CHAIN     POR01                              60
     C                   MOVE      *ON           *INOA
     C*
     C*  Center fund name and address
     C                   MOVEL(P)  XXFNAM        SRDFLD
     C                   CALL      'SRCENTER'    CNTRPL
     C                   MOVEL     SRDFLD        ZZFNAM           40
     C*
     C                   MOVEL(P)  XXFAD1        SRDFLD
     C                   CALL      'SRCENTER'    CNTRPL
     C                   MOVEL     SRDFLD        XXFAD1
     C*
     C                   MOVEL(P)  XXFCSZ        SRDFLD
     C                   CALL      'SRCENTER'    CNTRPL
     C                   MOVEL     SRDFLD        XXFCSZ
     C*
      /Free
       If isUseSpoolOut;
          If *inoa = *on;
             Except head;                                                        //check for overflo
          Endif;                                                                 //*inoa ifeq *on
          Except det;
          Except tot;
       Endif;
      /End-free
     C                   Exsr      ScriptCertMail
     C*
     C                   ENDSR                                                  SROUTP
      *
      *
      *
     C*================================================================
     C*  SRDBIL - For billings posted on or after cancellation date:
     C*           write record to WMXAP and then delete from WMEAP
     C*           Also handle Invoice Cloud if needed
     C*================================================================
     C     SRDBIL        BEGSR
     C*
     C     CXCXDT        IFNE      *ZERO
     C     QQREPR        ANDNE     'Y'                                          NOT REPRINT
     C     CXCXDT        IFLT      SYSDT
     C                   Z-ADD     SYSDT         XXKDAT
     C                   ELSE
     C                   Z-ADD     CXCXDT        XXKDAT
     C                   ENDIF                                                  CXCXDT IFLT SYSDT
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      *BLANKS       IFIFBL
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      RSJRST        XXST
     C     IFKY          SETGT     PIF01
     C     IFKY1         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY1         SETLL     PEA01
     C     EAKY2         READE     PEA01                                  60
       //*** Beg Add *** 03/25/21 ****************************************************
       n = 0;
       Clear arrInvoice#;
       //*** End Add *** 03/25/21 ****************************************************
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAPODT        IFGE      CXCXDT
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'H'
     C     EAARCD        OREQ      F1RCCD
     C*           Write record then delete unless only for Invoice Cloud
       //*** Beg Add *** 03/25/21 ****************************************************
       If isOnlyInactIC <> 'Y';
       //*** End Add *** 03/25/21 ****************************************************
     C                   EXSR      SRWRIT
     C                   EXSR      HOLDAR
     C***  Begin Chg  ***  07/21/22  *********************
     C**********         DELETE    PEA01
     C     eaRrn6        Delete    pea0p                              60
     C***  End   Chg  ***  07/21/22  *********************
       //*** Beg Add *** 03/25/21 ****************************************************
       Else;
          If EAtrDt <= sysDt;
             n = n + 1;
             arrInvoice#(n) = EAnInv;
             Exsr SrInvoiceCloud;
          Endif;
       Endif;
       //*** End Add *** 03/25/21 ****************************************************

     C                   ENDIF                                                  EAARTY
     C*
     C     EAARTY        IFEQ      'F'
     C                   MOVE      EACO#         KKCO#
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C*
     C  N60TTCTYP        IFEQ      'T'
     C     TTCTYP        OREQ      'M'
     C     TTCTYP        OREQ      'S'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C     EAARCD        OREQ      IFIFBL
     C*           Write record then delete unless only for Invoice Cloud
       //*** Beg Add *** 03/25/21 ****************************************************
       If isOnlyInactIC <> 'Y';
       //*** End Add *** 03/25/21 ****************************************************
     C                   EXSR      SRWRIT
     C                   EXSR      HOLDAR
     C***  Begin Chg  ***  07/21/22  *********************
     C**********         DELETE    PEA01
     C     eaRrn6        Delete    pea0p                              60
     C***  End   Chg  ***  07/21/22  *********************
       //*** Beg Add *** 03/25/21 ****************************************************
       Else;
          If EAtrDt <= sysDt;
             n = n + 1;
             arrInvoice#(n) = EAnInv;
             Exsr SrInvoiceCloud;
          Endif;
       Endif;
       //*** End Add *** 03/25/21 ****************************************************
     C                   ENDIF                                                  TTCTYP
     C*
     C                   ENDIF                                                  EAARTY = F
     C*
     C                   ENDIF                                                  EAPODT
     C*
     C     EAKY2         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C                   ENDIF                                                  CXCXDT
       //*** Beg Add *** 03/25/21 ***********************************
       If isInvoiceCloud and IcGroupOpt(EAco#:EAfnd);
          FinalizeThirdPartyInvoicing(EAco#:EAfnd);
          Clear arrInvoice#;
       Endif;
       //*** End Add *** 03/25/21 ***********************************
     C*
     C                   ENDSR                                                  SRDBIL
       //*** Beg Add *** 03/25/21 ***********************************
       //************************************************************
       // SrInvoiceCloud - Delete or Inactivate AR for Invoice Cloud
       //************************************************************
       Begsr SrInvoiceCloud;
          o = 1;
          isDupInv# = 'N';
          Dow o < n;
             If EAnInv = arrInvoice#(o);
                isDupInv# = 'Y';
                Leave;
             Endif;
             o = o + 1;
          Enddo;

          If isDupInv# = 'N';
             //*** Begin Chg *** 08/15/23 **********************************
             //*** inJson =
             //***   '{"invoice":"'  + %Editc(arrInvoice#(n):'X') + '",' +
             //***   '"company":"' + %Editc(EAco#:'X') + '",' +
             //***   '"group":"' + %Editc(EAfnd:'X') + '",' +
             //***   '"employer":"' + %Editc(EAemp#:'X') + '",' +
             //***   '"division":"' + %Editc(EAdiv:'X') + '",';
             inJson = '{"type":"billing",' +
               '"invoice":"'   + %Editc(arrInvoice#(n):'X') + '",' +
               '"company":"'   + %Editc(EAco#:'X') + '",' +
               '"group":"'     + %Editc(EAfnd:'X') + '",' +
               '"employer":"'  + %Editc(EAemp#:'X') + '",' +
               '"division":"'  + %Editc(EAdiv:'X') + '",';
             //*** End   Chg *** 08/15/23 **********************************

             // If IC flag Inactivate Invoices after Cx = Y then inactive
             // Otherwise delete
             If EXicCxInv = 'Y';
                inJson = inJson + '"active":' + '"false"' + ',' +
                '"delete":' + '"false"';
             Else;
                inJson = inJson + '"active":' + '"true"' + ',' +
                '"delete":' + '"true"';
             Endif;
             inJson = inJson + '}';
             ThirdPartyInvoicing(inJson);
          Endif;
       Endsr;
       //*** End Add *** 03/25/21 ***********************************
     C*================================================================
     C*  SRWRIT - Write WMEAP record to WMAXP
     C*================================================================
     C     SRWRIT        BEGSR
     C*
     C                   MOVE      EACO#         XACO#
     C                   MOVE      EACO#         XACO#
     C                   MOVE      EAFND         XAFND
     C                   MOVE      EAFYR         XAFYR
     C                   MOVE      EAEMP#        XAEMP#
     C                   MOVE      EADIV         XADIV
     C                   MOVE      EAARCD        XAARCD
     C                   MOVE      EAARTY        XAARTY
     C                   MOVE      EADESC        XADESC
     C                   MOVE      EAAMT         XAAMT
     C                   MOVE      EATRDT        XATRDT
     C                   MOVE      EAPODT        XAPODT
     C                   MOVE      EAEDIT        XAEDIT
     C                   MOVE      EASTAT        XASTAT
     C                   MOVE      EAUSER        XAUSER
     C                   MOVE      EAPGM         XAPGM
     C                   MOVE      EATIME        XATIME
     C                   MOVE      EADT          XADT
     C                   MOVE      EABUL1        XABUL1
     C                   MOVE      EACHK#        XACHK#
     C                   MOVE      EANINV        XANINV
     C                   MOVE      EALATE        XALATE
     C                   MOVE      EAFL01        XAFL01
     C                   MOVE      EAFL02        XAFL02
     C                   MOVE      EADT01        XADT01
     C                   MOVE      EADT02        XADT02
     C                   MOVE      EACTYP        XACTYP
     C                   MOVE      EARDIV        XARDIV
     C                   MOVE      EACCTP        XACCTP
     C                   MOVE      EACNUM        XACNUM
     C                   MOVE      EACEXP        XACEXP
     C                   MOVE      EACNAM        XACNAM
     C*
     C*              Maintenance fields
     C                   MOVE      Q1USER        XAIUSR
     C                   MOVE      Q1PGM         XAIPGM
     C                   MOVE      DSTIME        XAITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XAIDT
     C*
          //*** Beg Add ***** 06/29/22 ***************************
     C                   MOVE      EAref#        XAref#
          //*** End Add ***** 06/29/22 ***************************
     C                   WRITE     PXA01
      //*** Beg Add *** 09/06/21 ****************************************
       n = n + 1;
       arrInvoice#(n) = EAnInv;
       Exsr SrInvoiceCloud;
      //*** End Add *** 09/06/21 ****************************************
     C*
     C                   ENDSR                                                  SRWRIT
      *
      *
      *
      *================================================================
      *  SRWARB - If the cx date is revised, write A/R back into WMEAP
      *  from WMXAP if removed A/R's posting date is prior to the new
      *  revised cx date
      *================================================================
     C     SRWARB        BEGSR
      *
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQREQ#        XXREQ#
      *
     C     XHKY1         SETGT     PXH01
     C     XHKY1         READPE    PXH01                                  60
     C     *IN60         IFEQ      *OFF
     C     XHRVFL        ANDEQ     'R'
      *
     C                   Z-ADD     XHCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
      *
     C                   Z-ADD     0             XXACDT            8 0
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     XXEMP#        @@EMP#
     C                   Z-ADD     XXDIV         @@DIV
     C                   Z-ADD     XXFYR         @@FYR
      *
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
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C                   Z-ADD     @@AREA        CRAARE
     C                   Z-ADD     @@PHON        CRAPHN
      *
      *    Determine posting date for a/r (revised interim days + system dt)
      *
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
      *
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT            7 0
     C                   ENDIF
      *
     C     XXACDT        IFNE      *ZERO
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT
     C                   ENDIF                                                  XXACDT IFNE
      *
      *      Read cancellation A/R detail file
      *
     C     XAKY          CHAIN     PXA0L                              60
     C     *IN60         DOWEQ     *OFF
      *
      *      If posting date is greater than the revised cx date write to A/R
      *      and delete from WMXAP. Write A/R with posting date of ZZPODT if
      *      podt is less. Gather amounts if necessary.
      *
     C     XAPODT        IFLE      XHCXDT
      *
     C     XAPODT        IFGT      ZZPODT
     C                   EXSR      WRTREC
     C                   DELETE    PXA0L
     C*
     C                   ELSE
     C                   MOVE      XAARCD        XXARCD
     C                   Z-ADD     ZZPODT        XXPODT
     C     EAKY          CHAIN     PEA014                             61
     C     *IN61         IFEQ      *OFF
     C                   ADD       XAAMT         EAAMT
     C                   MOVE(P)   Q1PGM         EAPGM
     C                   Z-ADD     SYSDT         EATRDT
     C                   UPDATE    PEA014
     C                   DELETE    PXA0L
     C*
     C                   ELSE
     C                   Z-ADD     ZZPODT        XAPODT
     C                   EXSR      WRTREC
     C                   DELETE    PXA0L
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   ENDIF                                                  XAPODT>ZZPODT
     C*
     C                   ENDIF                                                  XAPODT<XXCXDT
     C*
     C     XAKY          READE     PXA0L                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDSR                                                  SRWARB
     C*================================================================
     C*  WRTREC - writes record to a/r file exactly as they were when
     C*           they were deleted except for the maintenance fields,
     C*           transaction date, and possibly posting date.
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
     C*
     C*    Update these fields with reinstatement info
     C                   Z-ADD     SYSDT         EATRDT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   MOVE      DSTIME        EATIME
     C                   Z-ADD     SYSDT         EADT
     C*
     C                   WRITE     PEA014
     C*
     C                   ENDSR                                                  WRTREC
     C*================================================================
     C*  WRTAGP - Writes the record for the created spool files
     C*================================================================
     C     WRTAGP        BEGSR
      *
     C     *LIKE         DEFINE    AGHDR         XXHDR
      *
     C     AGKY1X        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHDR
     C                   KFLD                    CXAGT#
      *
     C     *DTAARA       DEFINE    WXAGP         WXAGPX
     C     *LOCK         IN        *DTAARA
      *
      *  Check to see if there is a header record (AGHDR=Y) for the
      *  current agent (user)
      *  NOTE: File already overridden to only member for current user
      *
     C                   OPEN      WXAGP
     C                   OPEN      WXAGL1
     C*
     C                   MOVEL     'Y'           XXHDR
     C*
     C                   MOVEL     'N'           EXISTS            1
     C     AGKY1X        CHAIN     XAG01                              60
     C                   SELECT
     C     *IN60         WHENEQ    *OFF
     C                   MOVEL     'Y'           EXISTS
     C                   MOVEL     *BLANKS       AGOUTQ
     C                   MOVEL     *BLANKS       AGSTAT
     C                   UPDATE    XAG01
     C                   ENDSL
      *
      *  No header record found, create one based on datara WXAGP
     C     EXISTS        IFEQ      'N'
     C                   CLEAR                   PAG01X
     C                   MOVE      XXCO#         AGCO#
     C                   MOVE      CXAGT#        AGAGT#
     C                   MOVEL     'Y'           AGHDR
     C                   MOVEL     *BLANKS       AGSTAT
     C                   WRITE     PAG01X
     C                   ENDIF
      *
      * Write the record to the file
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
     C*                    MOVELQ1PGM     AGPGM
     C                   MOVEL     WXPGM         AGPGM
     C                   WRITE     PAG01X
      *
     C                   CLOSE     WXAGP
     C                   CLOSE     WXAGL1
     C*
     C                   ENDSR                                                  WRTREC
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
     C                   SUBST     ZXFND:X       X1TMP1            3
     C     X1TMP1        CAT       '-':0         X1PLCY           13
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
              'MassCancel':'000':'000':massFlag:xmlLink);
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
       In *Lock *dtaara;
       ldOutputFile = outputFile;
       Out *dtaara;

       Setgt (xxCO#:xxFND:RSJrSt:ELPfdt) pcr00;
       Readpe (xxCO#:xxFND:RSJrSt) pcr00;
       If not %eof;
          If STCRCXDocC <> ' ';
             XPDcid = %trim(STCRCXDocC);
             XPPath = %trim(STCRCXPathC);
          Else;
             If STCRCXDoc <> ' ';
                XPDcid = %trim(STCRCXDoc);
                XPPath = %trim(STCRCXPath);
             Endif;
          Endif;
       Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pcr00;
          Readpe (xxCO#:xxFnd:'  ') pcr00;
          If not %eof;
             If STCRCXDocC <> ' ';
                XPDcid = %trim(STCRCXDocC);
                XPPath = %trim(STCRCXPathC);
             Else;
                If STCRCXDoc <> ' ';
                   XPDcid = %trim(STCRCXDoc);
                   XPPath = %trim(STCRCXPath);
                Endif;
             Endif;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pcr00;
             Readpe (xxCO#:000:RSJrst) pcr00;
             If not %eof;
                If STCRCXDocC <> ' ';
                   XPDcid = %trim(STCRCXDocC);
                   XPPath = %trim(STCRCXPathC);
                Else;
                   If STCRCXDoc <> ' ';
                      XPDcid = %trim(STCRCXDoc);
                      XPPath = %trim(STCRCXPath);
                   Endif;
                Endif;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pcr00;
                Readpe (xxCO#:000:'  ') pcr00;
                If not %eof;
                   If STCRCXDocC <> ' ';
                      XPDcid = %trim(STCRCXDocC);
                      XPPath = %trim(STCRCXPathC);
                   Else;
                      If STCRCXDoc <> ' ';
                         XPDcid = %trim(STCRCXDoc);
                         XPPath = %trim(STCRCXPath);
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

       C7CNC# = CRCNC#;
       C7RPMY = CRRPMY;
       If QSRPMY <> *Blanks;
          C7RPMY = %DEC(QSRPMY:6:0);
       EndiF;
       C7WEDT = 0;
       If QSWEDT <> *Blanks;
          C7WEDT = %DEC(QSWEDT:8:0);
       EndiF;
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
       // *  ScriptCLTR - Write record to work file for Scriptura output
       // *=========================================================================
       Begsr ScriptCLTR;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear xxTemplate;
       Clear xxPath;

       Setgt (xxCO#:xxFND:RSJrSt:ELPfdt) pcr00;
       Readpe (xxCO#:xxFND:RSJrSt) pcr00;
       If not %eof;
          Select;
          When CXCNC# = 1 and STCR1stC <> ' ';
             xxTemplate = %trim(STCR1stC);
             xxPath = %trim(STCR1stCP);
          When CXCNC# = 2 and STCR2ndC <> ' ';
             xxTemplate = %trim(STCR2ndC);
             xxPath = %trim(STCR2ndCP);
          When CXCNC# = 3 and STCR3rdC <> ' ';
             xxTemplate = %trim(STCR3rdC);
             xxPath = %trim(STCR3rdCP);
          When CXCNC# = 4 and STCR4thC <> ' ';
             xxTemplate = %trim(STCR4thC);
             xxPath = %trim(STCR4thCP);
          Endsl;
       Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pcr00;
          Readpe (xxCO#:xxFnd:'  ') pcr00;
          If not %eof;
             Select;
             When CXCNC# = 1 and STCR1stC <> ' ';
                xxTemplate = %trim(STCR1stC);
                xxPath = %trim(STCR1stCP);
             When CXCNC# = 2 and STCR2ndC <> ' ';
                xxTemplate = %trim(STCR2ndC);
                xxPath = %trim(STCR2ndCP);
             When CXCNC# = 3 and STCR3rdC <> ' ';
                xxTemplate = %trim(STCR3rdC);
                xxPath = %trim(STCR3rdCP);
             When CXCNC# = 4 and STCR4thC <> ' ';
                xxTemplate = %trim(STCR4thC);
                xxPath = %trim(STCR4thCP);
             Endsl;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pcr00;
             Readpe (xxCO#:000:RSJrst) pcr00;
             If not %eof;
                Select;
                When CXCNC# = 1 and STCR1stC <> ' ';
                   xxTemplate = %trim(STCR1stC);
                   xxPath = %trim(STCR1stCP);
                When CXCNC# = 2 and STCR2ndC <> ' ';
                   xxTemplate = %trim(STCR2ndC);
                   xxPath = %trim(STCR2ndCP);
                When CXCNC# = 3 and STCR3rdC <> ' ';
                   xxTemplate = %trim(STCR3rdC);
                   xxPath = %trim(STCR3rdCP);
                When CXCNC# = 4 and STCR4thC <> ' ';
                   xxTemplate = %trim(STCR4thC);
                   xxPath = %trim(STCR4thCP);
                Endsl;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pcr00;
                Readpe (xxCO#:000:'  ') pcr00;
                If not %eof;
                   Select;
                   When CXCNC# = 1 and STCR1stC <> ' ';
                      xxTemplate = %trim(STCR1stC);
                      xxPath = %trim(STCR1stCP);
                   When CXCNC# = 2 and STCR2ndC <> ' ';
                      xxTemplate = %trim(STCR2ndC);
                      xxPath = %trim(STCR2ndCP);
                   When CXCNC# = 3 and STCR3rdC <> ' ';
                      xxTemplate = %trim(STCR3rdC);
                      xxPath = %trim(STCR3rdCP);
                   When CXCNC# = 4 and STCR4thC <> ' ';
                      xxTemplate = %trim(STCR4thC);
                      xxPath = %trim(STCR4thCP);
                   Endsl;
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
       XTType = 'Multiple CX Letter';
       XTDcid = xxTemplate;
       XTPath = xxPath;

       Update xt100;

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
          If STCRCCov <> ' ' and (STCRCCRC = $CRNCC or STCRCCRC = ' ');
             xxTemplate = %trim(STCRCCov);
             xxPath = %trim(STCRCCovP);
          Endif;
       Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pcr00;
          Readpe (xxCO#:xxFnd:'  ') pcr00;
          If not %eof;
             If STCRCCov <> ' ' and (STCRCCRC = $CRNCC or STCRCCRC = ' ');
                xxTemplate = %trim(STCRCCov);
                xxPath = %trim(STCRCCovP);
             Endif;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pcr00;
             Readpe (xxCO#:000:RSJrst) pcr00;
             If not %eof;
                If STCRCCov <> ' ' and (STCRCCRC = $CRNCC or STCRCCRC = ' ');
                   xxTemplate = %trim(STCRCCov);
                   xxPath = %trim(STCRCCovP);
                Endif;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pcr00;
                Readpe (xxCO#:000:'  ') pcr00;
                If not %eof;
                   If STCRCCov <> ' ' and (STCRCCRC = $CRNCC or STCRCCRC = ' ');
                      xxTemplate = %trim(STCRCCov);
                      xxPath = %trim(STCRCCovP);
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
       XTType = 'CX Cover Letter';
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
       // *=========================================================================
       // *  ScriptCertMail - Write record to work file for Scriptura output
       // *=========================================================================
       Begsr ScriptCertMail;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear xxTemplate;
       Clear xxPath;

       Setgt (xxCO#:xxFND:RSJrSt:ELPfdt) pcr00;
       Readpe (xxCO#:xxFND:RSJrSt) pcr00;
       If not %eof;
          If STCRMail <> ' ';
             xxTemplate = %trim(STCRMail);
             xxPath = %trim(STCRMailP);
          Endif;
       Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pcr00;
          Readpe (xxCO#:xxFnd:'  ') pcr00;
          If not %eof;
             If STCRMail <> ' ';
                xxTemplate = %trim(STCRMail);
                xxPath = %trim(STCRMailP);
             Endif;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pcr00;
             Readpe (xxCO#:000:RSJrst) pcr00;
             If not %eof;
                If STCRMail <> ' ';
                   xxTemplate = %trim(STCRMail);
                   xxPath = %trim(STCRMailP);
                Endif;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pcr00;
                Readpe (xxCO#:000:'  ') pcr00;
                If not %eof;
                   If STCRMail <> ' ';
                      xxTemplate = %trim(STCRMail);
                      xxPath = %trim(STCRMailP);
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

       XTType = 'Certified Mailing Letter';
       XTDcid = xxTemplate;
       XTPath = xxPath;

       Update xt100;

       Endsr;

      /end-free
     C*================================================================
     O*
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       OROUTQ              30
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2 33
     O                                           17 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       XXCO#         4     31
     O          E            ROUTE          2
     O                                           26 'GROUP          :'
     O                       XXFND         4     31
     O          E            ROUTE          2
     O                                           26 'CERTIFIED MAIL#:'
     O                       CXCEMN              37
     O          E            ROUTE          2
     O                                           26 'REPORT MONTH/YR:'
     O                       CXRPMY              35 '  /    '
     O          E            ROUTE          2
     O                                           26 'CANCEL REASON  :'
     O                       CXRSN               30
     O                       $CRDSC              91
     O          E            ROUTE          2
     O                                           26 'COMMENT 1      :'
     O                       CXCOM1              88
     O          E            ROUTE          2
     O                                           26 'COMMENT 2      :'
     O                       CXCOM2              88
     O          E            ROUTE          2
     O                                           26 'CX NOTICE DATE :'
     O                       ZZCXDT              38 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'CX EFFECTIVE DT:'
     O                       ZZCDAT              38 '  /  /    '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              12 '  :  :  '
     O                                           65 'CERTIFIED'
     O                                           73 'MAILING'
     O                       DSDATE        Y    123
     O          E            HEAD           1
     O                       Q1PGM               14
     O                       ZZFNAM              85
     O          E            HEAD           1
     O                       XXFAD1              85
     O          E            HEAD           1
     O                       XXFCSZ              85
     O          E            HEAD           0
     O                                           16 '____________'
     O                                           25 '_________'
     O          E            HEAD           1
     O                                           16 'NOTICE DATE:'
     O                       ZZCXDT              27 '  /  /    '
     O          E            HEAD           1
     O                                           12 'POLICY #'
     O                                           27 'NAME'
     O                                           74 'ADDRESS'
     O          E            HEAD           0
     O                                           31 'DBA NAME'
     O                                           91 'CITY, STATE AND ZIP CODE'
     O          E            HEAD           1
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           76 '________________________'
     O                                          100 '________________________'
     O                                          114 '________________________'
     O                                          124 '__________'
     OQSYSPRT   E            DET         1
     O                       CXEMP#        4     13
     O                       CXDIV         4     19
     O                                           21 '|'
     O                       EMNAME              63
     O                       XXEAD1             107
     O          E            DET         1
     O                                           21 '|'
     O                       XXFDNM              63
     O               25      XXEAD2             107
     O              N25      XXECSZ             107
     O          E    25      DET         1
     O                       XXECSZ             107
     OQSYSPRT   E            TOT              57
     O                                           24 'State Office: Please'
     O                                           49 'return one copy to above'
     O                                           71 'address after putting'
     O                                           77 'check'
     O          E            TOT         1
     O                                           41 'mark in box to indicate'
     O                                           65 'receipt of cancellation'
     O                                           73 'notice.'
     O          E            TOT         1
     O                                           83 '________________________'
     O                                          101 '__________________'
     O          E            TOT         1
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT              61
     O                                           28 '________________________'
     O                                           52 '________________________'
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT         1
     O                                           41 'Please sign            '
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT         1
     O                                           60 '|'
     O                                          101 '|'
     O          E            TOT         0
     O                                           83 '________________________'
     O                                          101 '__________________'
     P*==========================================================
     P* Get the post date and the invoice# from previous billing
     P*==========================================================
     PGetPostDate      b
     DGetPostDate      pi             7  0
     D inCompany#                     3  0 const
     D inGroup#                       3  0 const
     D inYear                         3  0 const
     D inEmployer#                    9  0 const
     D inDivision                     5  0 const
     D inAsOf                         7  0 const
     D outInvoice#                   10  0

     D decPostDate     s              7  0 Inz
     D decInvoice#     s             10  0 Inz
     d inKey           s             20a   Inz

       // Search for latest billing A/R to get posting date
       // and invoice#.
       SetGt (inCompany#:inGroup#:inEmployer#:inDivision:
              inYear:inAsOf:*hival) pea07;
       ReadPE (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pea07;
       DoW Not %eof(wmeal7);
          If E2arTy = 'A' Or E2arTy = 'B' Or E2arTy = 'D' Or E2arTy = 'F' Or
             E2arTy = 'H';
             outInvoice# = E2ninv;
             Return E2poDt;
          EndIf;
          ReadPE (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pea07;
       EndDo;

       // Can't find so let's look for future A/R Billing
       SetLL (inCompany#:inGroup#:inEmployer#:inDivision:
              inYear:inAsOf:*hival) pea07;
       ReadE (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pea07;
       DoW Not %eof(wmeal7);
          If E2arTy = 'A' Or E2arTy = 'B' Or E2arTy = 'D' Or E2arTy = 'F' Or
             E2arTy = 'H';
             outInvoice# = E2ninv;
             Return E2poDt;
          EndIf;
          ReadE (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pea07;
       EndDo;

       // Wasn't able to find A/R so now must look in A/R cancellation
       // hold file.
       If outInvoice# = 0;
          SetGt (inCompany#:inGroup#:inYear:inEmployer#:
                 inDivision:inAsOf) pxa02;
          ReadPE (inCompany#:inGroup#:inYear:inEmployer#:
                  inDivision) pxa02;
          DoW Not %eof(wmxal2);
             If XAarTy = 'A' Or XAarTy = 'B' Or XAarTy = 'D' Or XAarTy = 'F' Or
                XAarTy = 'H';
                outInvoice# = XAninv;
                Return XApoDt;
             EndIf;
             ReadPE (inCompany#:inGroup#:inYear:inEmployer#:
                     inDivision) pxa02;
          EndDo;

          // Can't find so let's look for future A/R Billing
          SetLL (inCompany#:inGroup#:inYear:inEmployer#:
                 inDivision:inAsOf) pxa02;
          ReadE (inCompany#:inGroup#:inYear:inEmployer#:
                  inDivision) pxa02;
          DoW Not %eof(wmxal2);
             If XAarTy = 'A' Or XAarTy = 'B' Or XAarTy = 'D' Or XAarTy = 'F' Or
                XAarTy = 'H';
                outInvoice# = XAninv;
                Return XApoDt;
             EndIf;
             ReadE (inCompany#:inGroup#:inYear:inEmployer#:
                     inDivision) pxa02;
          EndDo;
       EndIf;

       // No Billing found so need to assign an invoice# and
       // posting date using the system date
       inKey = %EditC(inCompany#:'X') + %EditC(inGroup#:'X') +
               %EditC(inEmployer#:'X') + %EditC(inDivision:'X');
       outInvoice# = GetNext3rdPartySeq#(inKey);
       decPostDate = inAsOf;

       Return decPostDate;

     PGetPostDate      e
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
**
CANCELLATION NOTICE ISSUED TO BE EFFECTIVE  (          ), REASON
CANCELLATION NOTICE REVISED TO BE EFFECTIVE (          ), REASON
**  MONTHS
January
February
March
April
May
June
July
August
September
October
November
December
