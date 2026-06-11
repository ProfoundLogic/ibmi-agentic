     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7291R - EMPLOYER LATE NOTICE
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*         08  -  BATCH MODE                                     *
     H*         09  -  DEMAND MODE                                    *
     H*         20-49  USED TO LIST SELECTED A/R CODES FROM PROMPT    *
     H*                SCREEN ON ROUTING PAGE.                        *
     H*         50  -  EOF INDICATOR FOR MAIN READ LOOP               *
     H*         51  -  *ON : A/R CODES 'INCLUDED', *OFF : 'EXCLUDED'  *
     H*         55  -  PRINT LATE NOTICES SWITCH                      *
     H*         60  -  GENERAL CHAINING                               *
     H*         61  -  GENERAL CHAINING                               *
     H*         71  -  ACCT AUDIT STATUS (ON=AUDITED)                 *
     H*         72  -  ACCT ACTIVE & NOT CX PEND AS OF RUN DATE       *
     H*         73  -  ACCT IS CX PEND AS OF RUN DATE                 *
     H*         74  -  ACCT IX CANCELLED AS OF RUN DATE               *
     H*         75  -  *ON - SELF REPORTING, *OFF - ESTIMATING        *
     H*         77  -  EMFDNM FEDERAL NAME EXISTS                     *
     H*         80  -  CHAIN EMPLYR MSTR OK                           *
     H*         81  -  CHAIN EMPLYR DTL OK                            *
     H*         82  -  CHAIN FND DTL OK                               *
     H*         83  -  VALIDATE SLECTED ACCT STATUS                   *
     H*         85  -  XXDEPD <> 0                                    *
     H*         86  -  XXPRMD <> 0                                    *
     H*         87  -  XXEXCD <> 0                                    *
     H*         88  -  XXDEDD <> 0                                    *
     H*         89  -  XXFEED <> 0                                    *
     H*         90  -  XXTAX  <> 0                                    *
     H*         91  -  Used to specify whether to print all accounts  *
     H*                that have a credit only, a debit only, or      *
     H*                both.                                          *
     H*         93  -  "Direct inquiries to" name is blank            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/06/23 C:342 Support: 140/113208                            *
     H*                Disable emailing of Estimated Late Notice      *
     H*                Report.                                        *
     H*                                                               *
     H* 02/15/23 C:79  Support: 133/102795                            *
     H*                Populate JRST with blank for *all              *
     H*                                                               *
     H* 05/26/21 C:65  Project: WC2760                                *
     H*                Populate new agent fields in Scriptura workfile*
     H*                                                               *
     H* 05/18/21 C:65  Project: WC2760                                *
     H*                Access template file for report.               *
     H*                                                               *
     H* 05/17/21 C:318 Support: 95935                                 *
     H*                Resolved infinity loop                         *
     H*                                                               *
     H* 03/24/21 C:65  Project: Phw#94661 (Luba)                      *
     H*                Ensure Scriptura work files get generated for  *
     H*                report only mode so report will produce.       *
     H*                                                               *
     H* 03/05/21 C:180 Support: 94891 Luba                            *
     H*                Increase employer size from 5 to 9 in WXL8P    *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 01/03/20 C:300 Support: 86217 (CareWest)                      *
     H*                Add Contact Person Field to Late Notice        *
     H*                                                               *
     H* 01/03/19 C:180 Support 71487/72092                            *
     H*                Fix address 2 issue causing City/State/Zip     *
     H*                to print twice.                                *
     H*                                                               *
     H* 05/23/18 C:62  Ticket 71008(133)                              *
     H*                Correct first name extraction                  *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 04/07/17 C:306 Support: 46260                                 *
     H*                Fixed the program to generate the late notice  *
     H*                report with group select functionality and with*
     H*                for single employer (standalone) using new     *
     H*                prompt screen program.                         *
     H*                                                               *
     H* 04/04/17 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Write a final record linked to report xml file *
     H*                for the total due and total # of employers.    *
     H*                                                               *
     H* 01/10/17 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Populate work file for phase 2 scriptura.      *
     H*                Write workfile records for both each late      *
     H*                notice to be generated and for the report.     *
      *                                                               *
      * 10/25/16 C:300 Project: WC2456                                *
      *                - Modified code to print report as per the     *
      *                  groups selected on group select screen.      *
     H*                                                               *
     H* 12/11/15 C:062 Support: 35065(133) 133WNI                     *
     H*                Change date Format'                            *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 02/13/13 C:283 SUPPORT:WA133 81583                            *
     H*                LATE NOTICE IN CASE OF CANCELLATION DATE       *
     H*                GREATER THAN SYSTEM DATE, IS MISSING THE LAST  *
     H*                DIGIT OF THE DUE DATE. CORRECTED THE ISSUE BY  *
     H*                MOVING THE NEXT LINE ONE POSITION AHEAD.       *
     H*                                                               *
     H* 04/18/11 C:136 SUPPORT: WA109 76573                           *
     H*               -ADJUST VERBIAGE FOR NON-AUDITED POLICIES       *
     H*                PENDING CANCELLATION TO MATCH DOCUMENTATION    *
     H*               -ALSO ADJUSTED RESOLVE TO DARTS COPY SO THAT    *
     H*                IT DOESN'T RUN OFF THE PAGE                    *
     H*                                                               *
     H* 09/23/10 C:191 SUPPORT:73400 WA109                            *
     H*                WRONG KEYLIST BEING USED TO ACCESS WDRRP       *
     H*                                                               *
     H* 08/09/10 C:191 PROJECT: 71WKU                                 *
     H*                Ignore Retro payments with transaction dates   *
     H*                future of the balance date                     *
     H*                                                               *
     H* 05/06/10 C:189 SUPPORT:71742                                  *
     H*                Printer printing the last line of a new page   *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/14/09 C:220 SEQ#: 68000                                    *
     H*                Program was coded incorrectly for calling parms*
     H*                                                               *
     H* 03/06/09 C:65  PROJECT: WC1568/SUPP#67377                     *
     H*                ENSURE ASSESSMENTS LISTED AS SEPERATE LINE     *
     H*                ITEMS ARE ADDED INTO THE TOTAL AMOUNT DUE.     *
     H*                                                               *
     H* 02/26/09 C:191 SEQ#: WA114/67333                              *
     H*                FIX: SOME TRANSACTION WERE ERRORNEOUSLY SKIPPED*
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 08/21/08 C:191 PROJECT 102WEP                                 *
     H*                BACK TO BASE: BASE OPTION                      *
     H*                 - SELECT CRITERIA BY STATE/LATE NOTICES       *
     H*                                                               *
     H* 02/11/08 C:33  Seq#: WA133/62173                              *
     H*                Remove 'FUND' from DARTS comment.              *
     H*                                                               *
     H* 09/17/07 C:183 PROJECT 132WIM                                 *
     H*                WORDING CHANGE ON INSURED LATE NOTICE.         *
     H*                                                               *
     H* 07/18/07 C:183 PROJECT WC1491                                 *
     H*                ADD PROCESSING FOR DEPOSIT REFUDN AND VOID     *
     H*                TYPE TRANSACTIONS.                             *
     H*                                                               *
     H*05/15/07 C:184 PROJECT WC1568                                  *
     H*               CHARGE ASSESSMENTS ON THE SAME BASIS AS PREMIUM*
     H*               INSTALLMENTS                                   *
     H*
     H* 05/17/07 C:136 SUPPORT: WA129 50816                           *
     H*                MAKE THE EXCLUSION FOR GRP 1 CLIENT SPECIFIC   *
     H*                FOR FCC                                        *
     H*                                                               *
     H* 11/03/06 C:53  SUPPORT: 54829                                 *
     H*                CORRECT ACTIVITY STATUS CODE ISSUE             *
     H*                                                                *
     H*  01/10/07  C:65  Project: 71WHE                                *
     H*                  ADD PROMPT FOR ADDITIONAL DAYS FOR INSTALLMNT *
     H*                  DATE DUE, TO BASE.                            *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY POLICY YEAR MODE                *
     H*                                                               *
     H* 09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE PHASE 1)          *
     H*                CLIENT SPECIFIC - CHANGE POLICY# TO MEMBER#    *
     H*                AND REFORMAT THE POLICY NUMBER.                *
     H*                                                               *
     H* 01/10/06 C:136 SUPPORT: 50816                                 *
     H*                PULL VERBIAGE FOR 'STATE TAX DUE:' FROM WDRRP  *
     H*                IF NOT GRP 1                                   *
     H*                                                               *
     H* 08/26/05 C:132 PROJECT: WA099 - 48594
     H*              - Change heading "Brokerage" to print whole name *
     H*                instead of just "Brokerag"                     *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
     H*                                                               *
     H* 09/24/04 C:53  PROJECT 132WBD                                 *
     H*                ADD AGENT HEADER VARIABLE FIELD                *
     H*                                                               *
     H*  08/11/04 C:136 Support: WA121 43522                          *
     H*                 Reference 40866. Include the audit transaction*
     H*                 in the balance calculation if it is a credit  *
     H*                 regardless of it's posting date.              *
     H*                                                               *
     H*  07/08/04 C:09   Project: SY100                               *
     H*                  Add additional logic to handle client folders*
     H*                                                               *
     H*  02/19/04 C:136  PROJECT: WC1033                              *
     H*                  Add code for new A/R type of 'T' (True Void) *
     H*                                                               *
     H*  01/16/04  C:33  Project: WC1132  WA121/40866                 *
     H*                  Only process audit billings thru end date.   *
     H*                                                               *
     H*  01/07/04  C:33  Project: WC1132  WA072/40698                 *
     H*                  Only process billing credits thru run date.  *
     H*                                                               *
     H*  10/27/03  C:33  Project: WC1132                              *
     H*                  Revamp Dunning Process                       *
     H*                                                               *
     H* 07/01/03 C:61  SUPPORT WA099 36927                            *
     H*                Load correct cancel date to work file field.   *
     H*                                                               *
     H*  06/02/03 C:132 Project: WC1117                               *
     H*                 Changed to use GROUP NAME instead of Office   *
     H*                 Accounting Name if FNCGFL <> "Y" for          *
     H*                 Self-Insured Groups                           *
     H*                                                               *
     H*  05/08/03 C:132 Project: WC1117                               *
     H*                 Remove LOCK BOX address changes per FCC       *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX address check in group         *
     H*                 auxiliary file. If present in WMF2P, use the  *
     H*                 Lock box address instead of accounting office *
     H*                 address.                                      *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                                                               *
     H* 07/29/02 C:61  SUPPORT WA081 33589                            *
     H*                If selected A/R codes entered, determine due   *
     H*                date using A/R type of selected codes.         *
     H*                                                               *
     H* 02/28/02 C:61  PROJECT: WC577 / SUPPORT WA100 31227           *
     H*                Print Carrier Policy # WMRPP/RPCAP# if entered.*
     H*                Otherwise, print grp/emp/fyr.                  *
     H*                                                               *
     H* 01/11/02 C:61  SUPPORT WA095 29335                            *
     H*                Change so that billings that are credits are   *
     H*                picked up thru payment thru date.              *
     H*                                                               *
     H* 12/20/01 C:61  SUPPORT WA107 30434                            *
     H*                Change to print out of country address line    *
     H*                if entered.                                    *
     H*                                                               *
     H* 03/28/01 C:61  PROJECT WC577                                  *
     H*                Correct print of Routing Information.          *
     H*                                                               *
     H* 01/25/01  C:35   PROJECT: SUPP WA109 #26063                   *
     H*                  DO NOT WRITE DARTS REMARK IF RUNNING REPORT- *
     H*                  ONLY.                                        *
     H*                                                               *
     H* 11/17/00  C:35   PROJECT: WC577                               *
     H*                  ADD CODE FOR INCLUDE/EXCLUDE ACTIVITY STATUS *
     H*                  CODES.                                       *
     H*                                                               *
     H* 10/09/00  C:33   PROJECT: WC918   WA107/24556                 *
     H*                  EXCLUDE ACCOUNT CURRENT POLICIES FROM REPORT *
     H*                  ALSO                                         *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097                                *
     H*                 Chg ALL PDS pgms                              *
     H*                 1. Replace write to SDCMP with API DA700R     *
     H*                                                               *
     H*  03/31/00 C:31  Support: 22408                                *
     H*                 1. Add initialization of CMIPGM               *
     H*                                                               *
     H* 03/20/00 C:118 PROJECT: WC911                                 *
     H*                IF THE 'EXCLUDE CONTESTED AUDIT' FLAG IS PASSED*
     H*                AS A Y, THEN BYPASS ANY EMPLOYER DETAIL RECORD *
     H*                WITH ELCTST=Y.                                 *
     H*                                                               *
     H* 03/03/00 C:118 PROJECT: WC974                                 *
     H*                ADD ENTRY PARM QQRPTF, REPORT ONLY FLAG.       *
     H*                ADD 'REPORT ONLY' SELECTION TO ROUTING PAGE.   *
     H*                CONDITION NOTICE PRINTING ON REPORT ONLY FLAG. *
     H*                                                               *
     H* 12/12/99 C:61  PROJECT: WC577                                 *
     H*                Correct output of copyright info.              *
     H*                                                               *
     H* 08/30/99 C:90  PROJECT: WC918                                 *
     H*                ADDED QSYSPRT1 TO PRINT A LISTING OF EMPLOYERS *
     H*                FOR WHOM THE LATE NOTICE WAS GENERATED. PAGE   *
     H*                BREAK FOR AGENT IF AGENT # OR NAME SORT IS     *
     H*                SELECTED.                                      *
     H*                                                               *
     H* 08/23/99 C:87  PROJECT: SUPP WA093 #19191                     *
     H*                Change pgm to base audit status on ELFAF.      *
     H*                                                               *
     H* 07/22/99 C:87  PROJECT: SUPP WA090 #18552                     *
     H*                Billing through date was not being processed   *
     H*                properly.  Modified to correct.                *
     H*                                                               *
     H* 07/02/99 C:87  PROJECT: SUPP WA072 #18705                     *
     H*                Modify so that 'Tax billed' is calculated the  *
     H*                same as other billing transactions (a/r types  *
     H*                'B' and 'A')                                   *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 04/12/99  C:65  PROJECT: WC875                                *
     H*                 ADD TAX ON PREMIUM AND TAX ON DEPOSIT TOGETHER*
     H*                                                               *
     H* 04/05/99  C:90   PROJECT: 96WAD (IN BASE: 07/19/99 C:33 WC899)*
     H*                  EXCLUDE ACCOUNT CURRENT POLICIES.            *
     H*                                                               *
     H* 02/24/99  C:87   PROJECT: SUPPORT TASB #NONE                  *
     H*                  Change to use the current date as opposed to *
     H*                  the policy eff date when reading the canx    *
     H*                  reason code table file(WTCRP).               *
     H*                                                               *
     H* 12/10/98 C:61 SUPPORT 14595                                   *
     H*               Only process active (WMA3P/A3STAT = 'A')        *
     H*               Employer 3rd Address records.                   *
     H*                                                               *
     H*  11/10/98 C:87 PROJECT: WC852                                 *
     H*                Change to use WTCRL instead of WTABL to get    *
     H*                the cancellation reason code record.           *
     H*                                                               *
     H*  08/31/98 C:87 PROJECT: 72WCD                                 *
     H*                CALCULATE 'STATE TAX DUE' AND MODIFY OUTPUT TO *
     H*                INCLUDE A DETAILED LINE ITEM. REALIGNED OUTPUT.*
     H*                                                               *
     H*  08/20/98 C:87 PROJECT: CM033                                 *
     H*                CHANGE TO DISPLAY LINE ITEMS ON LATE NOTICE IF *
     H*                THEY ARE NOT EQUAL TO ZERO INSTEAD OF JUST > 0 *
     H*                                                               *
     H*  07/28/98 C:65 PROJECT: CM039                                 *
     H*                USE THE ACCTING OFFICE FIELD FOR DIRECT INQRY. *
     H*                                                               *
     H*  06/01/98 C:65 PROJECT: CM039                                 *
     H*                USE THE ACCOUNTING OFFICE INFO FOR ADDRESSES   *
     H*                AND NAMES. ALSO CHANGE DETACH MESSAGE.         *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                ADDED CTSMCD MOVED TO CMSMCD.                  *
     H*                                                               *
     H*  12/11/97 C:87 PROJECT#: CM033                                *
     H*                Completely rewrote program while incorporating *
     H*                the following changes:                         *
     H*                  Late Notices will select accts which have    *
     H*                installments within a given date range. Prompt *
     H*                for unique entries for 'Interim period'(# days)*
     H*                Verbiage changes regarding acct status. Allow  *
     H*                selection of specific A/R codes based upon an  *
     H*                include or exclude flag. Aging report removed, *
     H*                to be established as a separate report.        *
     H*                From date  = installment from date             *
     H*                To date    = installment to date               *
     H*                Run date   = installment to date               *
     H*                Acct stat  = 'R' which will select accts which *
     H*                             have A/R installment billings     *
     H*                             posted within from/to dates       *
     H*                If acct has canx dt > Installment to date then *
     H*                it is considered active pending cancellation.  *
     H*                Include billing, adjustment and audit trans    *
     H*                through 'billing through' date. Include payment*
     H*                trans through 'payment through' date.          *
     H*                For non-audited accts : due date = last posting*
     H*                  date with A/R type of B, A or H found within *
     H*                  date range; past due date = due date + estim-*
     H*                  ated interim days( if eapodt = billing start *
     H*                  date use interim days for first installment) *
     H*                For audited accts : due date = posting date of *
     H*                  last A/R type of D within date range; past   *
     H*                  due date = add # of days depending on revised*
     H*                  audit flag in WDELP.                         *
     H*                * if due date not found within date range then *
     H*                  use eapodt of last A/R type F                *
     H*                                                               *
     H*  06/25/97 C:79 PROJECT WC711                                  *
     H*               -USE 3RD ADDRESS IN PLACE OF THE WMEMP ADDRESS  *
     H*                IF ITS NOT BLANK. VERSION 3 STANDARDS          *
     H*                                                               *
     H*  06/13/97 C:62 PROJECT WC681                                  *
     H*                CHANGE HARD-CODED 800 AREA CODE TO FNPH1A      *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F*** BEGIN DEL *** 04/26/00 **************************************
     F********SDCML   O   E           K        DISK
     F********SDCMLA  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM02
     F*** END DEL *** 04/26/00 ****************************************
     FSTCTL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*** BEGIN ADD ***  06/01/98  ************************************
     FWMOFL     IF   E           K DISK
     F*** END ADD *****  06/01/98  ************************************
     FWMEML     IF   E           K DISK
     FWMA3L     IF   E           K DISK
     FWMEAL8    IF   E           K DISK
     FWMEAL3    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     FWMEAL6    IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     F***  Begin Add  ***  10/27/03  **************************************
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     F                                     RENAME(PAG01:PAG00)
     F***  End   Add  ***  10/27/03  **************************************
     FWDELL1    IF   E           K DISK
     F*** begin add ***  01/10/17  ********************************************
     F                                     infds(dbfds)
     F*** end add *****  01/10/17  ********************************************
     FWDF2L     IF   E           K DISK
     FWXEBL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F***  BEGIN ADD  ***  11/10/98  **********************************
     F                                     IGNORE(PRS01)
     F*
     FWTCRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F***  END   ADD  ***  11/10/98  **********************************
     FWDCXL1    IF   E           K DISK
      **** BEGIN ADD **** 11/17/00 ************************************
     FWTAVL     IF   E           K DISK
      **** END   ADD **** 11/17/00 ************************************
      ***** BEG ADD ******* 02/28/02 **********************************
      *
      * Carrier/State Reporting Master
     FWMRPL     IF   E           K DISK
     F*** BEG ADD ***  09/24/04 ************************************
     FWMEXL     IF   E           K DISK
     F*** END ADD ***  09/24/04 ************************************
      *** BEG ADD ***** 01/10/06 **************************************
     FWDRRL     IF   E           K DISK
      *** END ADD ***** 01/10/06 **************************************
      ***** BEG-ADD ***** 10/25/16 ************************************
     FWXGSL     IF   E           K DISK
      ***** END-ADD ***** 10/25/16 ************************************
     F*** begin add ***  01/10/17  *****************************************************
     Fwxl8p     o    e             disk    usropn
     F                                     rename(pl801:pl80p)
     FXMLPREL1  UF   E           K DISK
     F*** end add *****  01/10/17  *****************************************************
     F*** begin add ***  05/18/21  *******************************************
     FSTSP1L    IF   E           K DISK    rename(psp011:psp010)
     F*** end add *****  05/18/21  *******************************************
      *
      ***** END ADD ******* 02/28/02 **********************************
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)
      **** BEGIN ADD **** 08/30/99 ************************************
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)
      **** END   ADD **** 08/30/99 ************************************
     F***  Begin Add  ***  10/27/03  **************************************
     FQSYSPRT2  O    F   80        PRINTER OFLIND(*INOB)
     F                                     USROPN
     F***  End   Add  ***  10/27/03  **************************************
      *
     d*** begin add ***  01/10/17  *****************************************************
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x@RRN           s              9  0 inz(0)
     d xmlFile         s                   like(L8XML)
     d xmlRptFile      s                   like(L8XML)
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('LN')
     d xxProcessType   s            100
     d processType     c                   const('Estimated Late Notice')
     d count           s              5  0
     d xxGroups        s           3000
     D xxPolCity       s                   like(EMMCTY)
     D xxPolSt         s                   like(EMMST)
     D xxPolZip        s                   like(EMMZIP)
      *** Begin Add *** 01/03/19 *******************************************
     D isADR2          S              1n   inz
      *** End   Add *** 01/03/19 *******************************************
      *** Begin Add *** 01/03/20 *******************************************
     d xxCont          s                   like(L8Cont)
      *** End   Add *** 01/03/20 *******************************************

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
     d
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const
     d
     DObjExists        pr             1n
     D object                        10    const
     D objType                        7    const
     D
     D/copy sPrModule
     D/copy sPrXML
     D                 DS
     D  arrARCD                1     60
     D                                     DIM(30)
     D  L8ARCD01               1      2
     D  L8ARCD02               3      4
     D  L8ARCD03               5      6
     D  L8ARCD04               7      8
     D  L8ARCD05               9     10
     D  L8ARCD06              11     12
     D  L8ARCD07              13     14
     D  L8ARCD08              15     16
     D  L8ARCD09              17     18
     D  L8ARCD10              19     20
     D  L8ARCD11              21     22
     D  L8ARCD12              23     24
     D  L8ARCD13              25     26
     D  L8ARCD14              27     28
     D  L8ARCD15              29     30
     D  L8ARCD16              31     32
     D  L8ARCD17              33     34
     D  L8ARCD18              35     36
     D  L8ARCD19              37     38
     D  L8ARCD20              39     40
     D  L8ARCD21              41     42
     D  L8ARCD22              43     44
     D  L8ARCD23              45     46
     D  L8ARCD24              47     48
     D  L8ARCD25              49     50
     D  L8ARCD26              51     52
     D  L8ARCD27              53     54
     D  L8ARCD28              55     56
     D  L8ARCD29              57     58
     D  L8ARCD30              59     60
     D                 DS
     D  arrARDS                1    900
     D                                     DIM(30)
     D  L8ARDS01               1     30
     D  L8ARDS02              31     60
     D  L8ARDS03              61     90
     D  L8ARDS04              91    120
     D  L8ARDS05             121    150
     D  L8ARDS06             151    180
     D  L8ARDS07             181    210
     D  L8ARDS08             211    240
     D  L8ARDS09             241    270
     D  L8ARDS10             271    300
     D  L8ARDS11             301    330
     D  L8ARDS12             331    360
     D  L8ARDS13             361    390
     D  L8ARDS14             391    420
     D  L8ARDS15             421    450
     D  L8ARDS16             451    480
     D  L8ARDS17             481    510
     D  L8ARDS18             511    540
     D  L8ARDS19             541    570
     D  L8ARDS20             571    600
     D  L8ARDS21             601    630
     D  L8ARDS22             631    660
     D  L8ARDS23             661    690
     D  L8ARDS24             691    720
     D  L8ARDS25             721    750
     D  L8ARDS26             751    780
     D  L8ARDS27             781    810
     D  L8ARDS28             811    840
     D  L8ARDS29             841    870
     D  L8ARDS30             871    900
     D                 DS
     D  arrASCD                1     10
     D                                     DIM(5)
     D  L8ASCD01               1      2
     D  L8ASCD02               3      4
     D  L8ASCD03               5      6
     D  L8ASCD04               7      8
     D  L8ASCD05               9     10
     D                 DS
     D  arrASDS                1    150
     D                                     DIM(5)
     D  L8ASDS01               1     30
     D  L8ASDS02              31     60
     D  L8ASDS03              61     90
     D  L8ASDS04              91    120
     D  L8ASDS05             121    150
     d*** end add *****  01/10/17  *****************************************************
      *

     D*
     D COD             S              2    DIM(30) ASCEND                       A/R CODES
     D DUM             S              2    DIM(30) ASCEND                       DUMMY ARRAY
     D DES             S             30    DIM(30)                              A/R CODE DESC
     D CDE             S              2    DIM(30)                              AR CDE FOR OUTPUT
      **** BEGIN ADD **** 11/17/00 *********************************************
     D AS              S              2    DIM(5) ASCEND                        ACTIVITY STATUS
     D ASDS            S             30    DIM(30)                              ACT STS DESC
      ***** BEG-ADD ***** 10/25/16 ************************************
     D strGrpSlct      S              1A   inz('N')
     D strGroup        S            110A   inz(*Blanks)
     D strGroup1       S             59A   inz(*Blanks)
     D strGroup2       S             51A   inz(*Blanks)
      ***** END-ADD ***** 10/25/16 ************************************
      **** END   ADD **** 11/17/00 *********************************************
      ***** BEG ADD ******* 07/01/03 **********************************
      *
      ***** END ADD ******* 07/01/03 **********************************
      *
      *
     D*
     D*** BEGIN DEL *** 04/26/00 **************************************
     D********PCM02
     D********      CMKEY                           C2KEY
     D********      CMCSID                          C2CSID
     D********      CMCOMM                          C2COMM
     D********      CMCDAT                          C2CDAT
     D********      CMCODE                          C2CODE
     D********      CMDCID                          C2DCID
     D********      CMPNAM                          C2PNAM
     D********      CMTRDT                          C2TRDT
     D********      CMTRTM                          C2TRTM
     D********      CMSEQ#                          C2SEQ#
     D********      CMSECL                          C2SECL
     D********      CMSBYL                          C2SBYL
     D********      CMSTAT                          C2STAT
     D********      CMUSER                          C2USER
     D********      CMPGM                           C2PGM
     D********      CMTIME                          C2TIME
     D********      CMDT                            C2DT
     D********      CMIUSR                          C2IUSR
     D********      CMFOLD                          C2FOLD
     D********      CMDCDT                          C2DCDT
     D********      CMDIFL                          C2DIFL
     D********
     D********      CMAPID                          C2APID
     D********      CMIDOC                          C2IDOC
     D********      CMIQUE                          C2IQUE
     D********      CMIMGF                          C2IMGF
     D********      CMIPTH                          C2IPTH
     D********      CMPIDX                          C2PIDX
     D********      CMQPTY                          C2QPTY
     D********      CMDCTP                          C2DCTP
     D********      CMIPGM                          C2IPGM
     D********      CMSMCD                          C2SMCD
     D********      CMFL01                          C2FL01
     D********      CMFL02                          C2FL02
     D********      CMFL03                          C2FL03
     D********      CMFL04                          C2FL04
     D********      CMFL05                          C2FL05
     D********      CMRMCD                          C2RMCD
     D********      CMIDT                           C2IDT
     D*** END DEL *** 04/26/00 ****************************************
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*
     D***  Begin Add  ***  10/27/03  **************************************
     D                 DS
     D  QQPOL#                 1     15
     D  QPFND                  1      3
     D  QPEMP#                 4     12
     D  QPFYR                 13     15
     D***  End   Add  ***  10/27/03  **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D***  Begin Add  ***  10/27/03  **************************************
     D QQDT            DS
     D  QQFRDT                 1      8
     D  QQTODT                 9     16
     D QQEA            DS
     D  QQFREA                 1      2
     D  QQTOEA                 3      4
     D QQEN            DS
     D  QQFREN                 1      9
     D  QQTOEN                10     18
     D QQAA            DS
     D  QQFRAA                 1      2
     D  QQTOAA                 3      4
     D QQAN            DS
     D  QQFRAN                 1      5
     D  QQTOAN                 6     10
     D QQCMT           DS
     D  QQCMT1                 1      2
     D  QQCMT2                 3      4
     D  QQCMT3                 5      6
     D  QQCMT4                 7      8
     D  QQCMT5                 9     10
     D QQEF            DS
     D  QQFREF                 1      8  0
     D  QQTOEF                 9     16  0
     D*
     D AGTAPI          C                   CONST('WA002R')
     D OUTPUT          C                   CONST('W7291COUT')
     D AGTNAM          C                   CONST('* *NO AGENT INFO* *')
     D***  End   Add  ***  10/27/03  **************************************
     D*** BEG ADD *** 09/24/04 ******************************          ******
     D CHGNUM          C                   CONST('SA020R')
     D*** END ADD *** 09/24/04 ******************************          ******
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
     *******************************************************************
     **    LIKE DEFINES
     *******************************************************************

     C     *LIKE         DEFINE    EAPODT        XXDAT2
     C     *LIKE         DEFINE    EAPODT        XXFUTR
     C     *LIKE         DEFINE    EAPODT        XXABDT
     C     *LIKE         DEFINE    EAPODT        XXAUDT
     C     *LIKE         DEFINE    EBEMP#        XXEMP#
     C     *LIKE         DEFINE    EBEMP#        RSTOEN
     C     *LIKE         DEFINE    EBEMP#        RSFREN
     C     *LIKE         DEFINE    EBDIV         XXDIV
     C     *LIKE         DEFINE    EBFND         XXFND
     C***  Begin Add  ***  10/27/03  **************************************
     C     *LIKE         DEFINE    EBFND         XXFFND
     C     *LIKE         DEFINE    EBFND         XXTFND
     C***  End   Add  ***  10/27/03  **************************************
     C     *LIKE         DEFINE    EBFYR         XXFYR
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    FNMAD1        XFMAD1
     C     *LIKE         DEFINE    FNMAD1        XFMAD2
     C     *LIKE         DEFINE    FNMAD1        XFMAD3
     C*** BEGIN ADD ***  06/01/98  ************************************
     C     *LIKE         DEFINE    OFAAD1        XXAAD1
     C     *LIKE         DEFINE    OFAAD1        XXAAD2
     C     *LIKE         DEFINE    OFAAD1        XXAAD3
     C*** END ADD *****  06/01/98  ************************************
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C***  END   ADD  ***  11/10/98  **********************************
      **** BEGIN ADD **** 08/30/99 ************************************
     C     *LIKE         DEFINE    EBAGT#        HLDAGT
      **** END   ADD **** 08/30/99 ************************************
      *
     *******************************************************************
     **   KEY LISTING
     *******************************************************************
     C*** BEG ADD *** 09/24/04 ******************************          ******
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*** END ADD *** 09/24/04 ***********************************     ******
     C*
      ***** BEG ADD ******* 02/28/02 **********************************
      *
      *   Carrier/State reporting Master
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      ***** END ADD ******* 02/28/02 **********************************
     **
     **   KEY LIST FOR THE USER PARAMETER FILE
     **
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     **
     **   KEY LIST FOR THE COMPANY FILE
     **
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     **
     **   KEY LIST FOR THE FUND
     **
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  Begin Add  ***  10/27/03  **************************************
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C***  End   Add  ***  10/27/03  **************************************
      ***** BEG-ADD ***** 10/25/16 ************************************
     C     XGKY          KLIST
     C                   KFLD                    GsCO#
     C                   KFLD                    GsFnd
      ***** END-ADD ***** 10/25/16 ************************************
     C*** BEGIN ADD ***  06/01/98  ************************************
     **
     **   KEY LIST FOR THE OFFICE
     **
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*** END ADD *****  06/01/98  ************************************
     **
     **   KEY LIST FOR THE EMPLOYER MASTER FILE
     **
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     **   KEY LIST FOR THE EMPLOYER DETAIL FILE
     **
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     **   KEY LIST FOR THE 3RD ADDRESS FILE
     **
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     **   KEY LISTS FOR THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     **
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXARTY
      *
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C                   KFLD                    XXDAT             7 0
      *
     C     EAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY
     **
     C     EAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDAT
      *
     C     EAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** BEG ADD ***** 01/10/06 **************************************
     **
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    KKST
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    KKST
     C                   KFLD                    KKPFDT
     C*** END ADD ***** 01/10/06 **************************************
     **
     **   KEY LIST FOR THE FUND DETAIL FILE
     **
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     **
     **   KEY LIST FOR THE SELECTED EMPLOYER WORK FILE
     **
     C     EBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     **
     **  KEY LIST FOR THE OVERRIDE FILE
     **
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     **
     **  KEY LIST FOR THE TABLE FILE
     **
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKRTCD
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C***  END   ADD  ***  11/10/98  **********************************
     **
     **  KEY LIST FOR FUND DETAIL
     **
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     **
      *** BEG ADD ***** 08/09/06 *************************************
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
      *** END ADD ***** 08/09/06 *************************************
     **
     **  BILLING COMMENT FILE
     **
     C     BCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBCOM           10
     C*** BEGIN DEL *** 04/26/00 **************************************
     *********
     ****DARTS REMARKS FILE
     *********
     C********   C2KY      KLIST
     C********             KFLD           QCKEY
     C********             KFLD           ##SEQ#
     *********
     C********   C2KY1     KLIST
     C********             KFLD           QCKEY
     C*** END DEL *** 04/26/00 ****************************************
     **
     **  REMARK TYPE TABLE FILE
     **
     C     CTKY          KLIST
     C                   KFLD                    QQDRMK
     **
     **  KEY LIST FOR CANX DETAIL FILE
     **
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    EBCNDT
      **** BEGIN ADD **** 11/17/00 *********************************************
      *
      *  KEY LIST
      *
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      **** END   ADD **** 11/17/00 *********************************************
     C*** BEG ADD *** 09/24/04 ************************************
     C     EXKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C*** END ADD *** 09/24/04 ************************************
      *
     ******************************************************************
     **    ENTRY PARAMETERS
     ******************************************************************
     **
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQFRDT  8
     C**********           PARM           QQTODT  8
     C                   PARM                    QQDT             16
     C***  End   Chg  ***  10/27/03  **************************************
     C                   PARM                    QQBLDT            8
     C                   PARM                    QQMAMT            9
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C                   PARM                    QQRPTF            1
     C***** END   ADD **** 03/03/00 ***********************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   PARM                    QQCTST            1
     C***** END   ADD **** 03/20/00 ***********************************
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQFIDY            2
     C                   PARM                    QQEBDY            2
     C                   PARM                    QQABDY            2
     C                   PARM                    QQRADY            2
     C                   PARM                    QQSRT1            2
     C                   PARM                    QQSRT2            2
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQFREA  2
     C**********           PARM           QQTOEA  2
     C                   PARM                    QQEA              4
     C**********           PARM           QQFREN  9
     C**********           PARM           QQTOEN  9
     C                   PARM                    QQEN             18
     C**********           PARM           QQFRAA  2
     C**********           PARM           QQTOAA  2
     C                   PARM                    QQAA              4
     C**********           PARM           QQFRAN  5
     C**********           PARM           QQTOAN  5
     C                   PARM                    QQAN             10
     C***  End   Chg  ***  10/27/03  **************************************
     C                   PARM                    QQACST            1
     C                   PARM                    QQESSR            1
     C                   PARM                    QQPAYD            8
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQCMT1  2
     C**********           PARM           QQCMT2  2
     C**********           PARM           QQCMT3  2
     C**********           PARM           QQCMT4  2
     C**********           PARM           QQCMT5  2
     C                   PARM                    QQCMT            10
     C***  End   Chg  ***  10/27/03  **************************************
     C                   PARM                    QQDRMK            5
     C                   PARM                    QQAUST            1
     C                   PARM                    QQCODE           60
     C                   PARM                    QQARIE            1
      **** BEGIN ADD **** 11/17/00 ************************************
     C                   PARM                    QQIEFL            1
     C                   PARM                    QQASTC           10
      **** END   ADD **** 11/17/00 ************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C                   PARM                    QQTFND            3
     C                   PARM                    QQDART            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQEF
     C                   PARM                    QQINDP            1
     C***  End   Add  ***  10/27/03  **************************************
     C*** BEGIN ADD ***  01/10/07  *****************************************
     C                   PARM                    QQADDU            3
     C*** END ADD *****  01/10/07  *****************************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C                   PARM                    QQJRST            2
     C*** END ADD ****** 08/21/08 ************************************
      ***** BEG-ADD ***** 10/25/16 ************************************
     C                   Parm                    qqGrpSlct         1
     C*** begin add ***  01/10/17  *****************************************
      /Free
       xxProcessType = processType;

       isUseSpoolOut = UseSpoolOut(xxProcessType);
       isUseXML = UseScriptura(xxProcessType)
        or Use3rdXml(xxProcessType);

       xxGroups = ' ';
      /end-free
     C                   MOVE      QQCO#         XXCO#             3 0
     C*** end add *****  01/10/17  *****************************************
     C                   If        %Parms > 24
     C                   MOVE      qqGrpSlct     strGrpSlct
     C                   ENDIF
     C                   IF        strGrpSlct = 'Y'
     C                   EVAL      *In13 = *OFF
     C                   EVAL      *In14 = *ON
     **
     C     FNKY1         SETLL     PFN01
     C                   READ      PFN01
     C                   DOW       NOT %EOF
     C                   Eval      GsCo# = FnCo#
     C                   Eval      GsFnd = Fnfnd
     C     XGKY          chain     PGS01
     C                   If        %Found and GsIEFl ='I'
     C                   MOVE      GsFnd         XXFND
     C*** begin add ***  01/10/17  *****************************************
     C                   If        xxGroups = ' '
     C                   Eval      xxGroups = %editc(xxFnd:'X')
     C                   Else
     C                   Eval      xxGroups =%trim(xxGroups) +
     C                              %editc(xxFnd:'X')
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
     C                   If        strGroup = ' '
     C                   eval      strGroup = %editc(XXFND:'X')
     C                   Else
     C                   eval      strGroup =%trim(strGroup) + ',' +
     C                                        %editc(XXFND:'X')
     C                   Endif
     C                   eval      strGroup1 =%subst(strGroup:1:59)
     C                   eval      strGroup2 =%subst(strGroup:60:51)
     C                   If        strGroup2 <> *Blanks
     C                   EVAL      *In15 = *ON
     C                   Endif
     C                   Endif
     C                   READ      PFN01
     C                   Enddo
     **
     C                   ELSE
     C                   EVAL      *In13 = *ON
     C                   EVAL      *In14 = *OFF
     C                   ENDIF
      ***** END ADD ***** 10/25/16 ************************************
     C*** begin add ***  01/10/17  *****************************************
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQTFND        XXTFND
      /free

       Clear xmlLink;
       Clear xmlFlowName;
       Clear xmlRptFile;

       If QQEmp# <> *zeros and QQDiv <> *blanks;
          isMassReq = *off;
          massFlag = ' ';
       Else;
          isMassReq = *on;
          massFlag = 'Y';
       Endif;
       //*** begin add ***  03/24/21  *************************************
       If QQRPTF <> *blank;
          isMassReq = *off;
          massFlag = ' ';
       Endif;
       //*** end add *****  03/24/21  *************************************

       //* Set the groups processed into string if not running group select.
       If xxGroups = ' ' and isUseXml;
          Setll (xxCo#:xxFnd) pfn01;
          Reade(n) (xxCo#) pfn01;
          Dow not %eof;
             If FNFnd > xxTFnd and xxTFnd <> 0;
                Leave;
             Endif;
             If xxGroups = ' ';
                xxGroups = %editc(FNFnd:'X');
             Else;
                xxGroups =%trim(xxGroups) + %editc(FNFnd:'X');
             Endif;
             Reade(n) (xxCo#) pfn01;
          Enddo;
        Endif;

       If isUseXML;
          Open WXL8P;
       Endif;

      /end-free
     C*** end add *****  01/10/17  *****************************************
     **
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     **
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     ******************************************************************
     **    SET UP AND INITALIZE
     ******************************************************************
      *
     C*** BEG ADD *******  09/20/06  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
      *** END ADD ***** 09/20/06 **************************************
      *** BEG ADD ***** 09/17/07 ******************************************
     C     CLTID         IFEQ      '132'
     C                   MOVE      *ON           *IN67
     C                   ELSE
     C                   MOVE      *OFF          *IN67
     C                   ENDIF
      *** END ADD ***** 09/17/07 **************************************
     C                   TIME                    DSDTTM
      **** BEGIN ADD **** 08/30/99 ************************************
     C                   Z-ADD     *ZERO         XXCNT             5 0
     C                   Z-ADD     *ZERO         XXTOTL           11 2
     C                   Z-ADD     *ZERO         XXAGTL           11 2
      **** END   ADD **** 08/30/99 ************************************
     C*
     C***  BEGIN ADD  ***  02/24/99  **********************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
     C***  END   ADD  ***  02/24/99  **********************************
      *
     C                   MOVE      QQPAYD        XXPAYD            8 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      QQFND         XXFFND
     C                   MOVE      QQTFND        XXTFND
     C***  End   Add  ***  10/27/03  **************************************
     C                   MOVE      QQMAMT        XXMAMT            9 2
     C                   MOVE      QQFIDY        XXFIDY            2 0
     C                   MOVE      QQEBDY        XXEBDY            2 0
     C                   MOVE      QQABDY        XXABDY            2 0
     C                   MOVE      QQRADY        XXRADY            2 0
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           MOVE QQTODT    XXRNDT  80
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXRNDT            8 0
     C***  End   Chg  ***  10/27/03  **************************************
     C                   MOVE      QQBLDT        XXBDAT            8 0
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      QQTODT        XXTODT            8 0
     C                   MOVE      QQFREF        X8FREF            8 0
     C                   MOVE      QQTOEF        X8TOEF            8 0
     C***  End   Add  ***  10/27/03  **************************************
     C                   MOVEA     QQCODE        COD
      **** BEGIN ADD **** 11/17/00 ************************************
     C                   MOVEA     QQASTC        AS
      **** END   ADD **** 11/17/00 ************************************
     C*** BEGIN ADD ***  01/10/07  *****************************************
     C                   MOVE      QQADDU        XXADDU            3 0
     C*** END ADD *****  01/10/07  *****************************************
      *
     C                   Z-ADD     XXBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBLDT            7 0
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           Z-ADDXXPAYD    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXPTHR  70                PAID THROUGH
     C***  End   Del  ***  10/27/03  **************************************
      *
     C                   MOVE(P)   QQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0                   PAID THROUG
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE(P)   QQFREF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FREF            7 0
     C*
     C                   MOVE(P)   QQTOEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TOEF            7 0
     C***  End   Add  ***  10/27/03  **************************************
      *
      *  IF COD,30 IN ARRAY CONTAINS '@@' THEN CHG TO BLANKS. (THIS
      *    IS TO PREVENT PSBL ERR IN PASSING PARM, SEE W7290R)
      *
     C     COD(30)       IFEQ      '@@'
     C                   MOVE      *BLANKS       COD(30)
     C                   ENDIF
      *
      *  BATCH OR DEMAND  MODE
      *
     C     QQMODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09                          DEMAND MODE
     C                   ELSE
     C                   MOVE      *ON           *IN08                          BATCH MODE
     C                   ENDIF
      *
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           Z-ADDXXRNDT    SRDTI
     C                   Z-ADD     XXTODT        SRDTI
     C***  End   Chg  ***  10/27/03  **************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT
     C                   MOVEL     SRDTO         XXDAT2
     **
     C                   MOVE      *ALL'_'       SEPA80           75
     **
     **   CHAIN TO THE USER PARAMETER FILE
     **
     C     UPKY          CHAIN     PUP01                              60
     **
     **  CHAIN TO THE COMPANY FILE
     **
     C     COKY          CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     **
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        CONAME
     C                   ENDIF
     C***  Begin Add  ***  10/27/03  **************************************
     C*
     C     *LIKE         DEFINE    XXCONM        SVCONM
     C*
     C*   CENTER COMPANY NAME IN FIELD
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL(P)  SRDFLD        XXCONM           40
     C                   MOVEL(P)  SRDFLD        SVCONM
     C***  End   Add  ***  10/27/03  **************************************
     C***  Begin Del  ***  10/27/03  **************************************
     ***********
     *****CHAIN*TO THE FUND MASTER FILE
     ***********
     C********** FNKY      CHAINPFN01                60
     ***********
     C********** *IN60     IFEQ *OFF
      ******BEG*ADD ******* 06/02/03 **********************************
     C********** FNCGFL    COMP 'Y'                      78
      ******END*ADD ******* 06/02/03 **********************************
     ***********
     ***USE*ALTERNATE FUND NAME IF FOUND IN FUND MASTER
     ***********
     C**********           MOVELFNNAME    XXFNAM
     C********** FNANAM    IFNE *BLANKS
     C**********           MOVELFNANAM    CONAME
     C**********           ENDIF                           *IN60 IFEQ
     ***********
     C**********           ENDIF                           *IN60 IFEQ
     ***********
      ****CENTER COMPANY NAME IN FIELD
     ***********
     C**********           MOVELCONAME    SRDFLD    P
     C**********           CALL 'SRCENTER'
     C**********           PARM           SRDFLD 80
     C**********           PARM 40        SRDLEN  30
     C**********           MOVELSRDFLD    XXCONM 40
     ***********
     *****GET*THE FUND AUXILIARY FILE
     ***********
     C********** F2KY      CHAINPF201                60
     ***********
      ******BEG*ADD **** 02/13/03 *************************************
      ******BEG*DEL **** 05/08/03 *************************************
      ***Check*for Lock box address in Group Auxiliary file
      ***If*exists use lock box address instead of Accounting Ofc addr
     C**********           MOVE ' '       XXLKBX  1
     C**N60***** F2LAD1    IFGT *BLANKS
     C**********           MOVE 'Y'       XXLKBX
     C**********           ENDIF
      ******END*DEL **** 05/08/03 *************************************
      ******END*ADD **** 02/13/03 *************************************
     ***********
     *****LOAD*FUND ADDRESS, CITY, STATE, & ZIP
     ***********
     C**********           MOVELFNMAD1    XFMAD1    P
     C**********           MOVELFNMAD2    XFMAD2    P
     C**********           MOVELFNMCTY    XXCITY 25 P
     C**********           MOVELFNMST     XXSTAT  2
     C**********           MOVELFNMZIP    XXZIP1  5
     C**********           MOVE FNMZIP    XXZIP2  4
     ***********
     C**********           EXSR SRCSTZ
     C**********           MOVELXXCSTZ    XFMAD3
     ***********
     C********** FNMAD2    IFEQ *BLANK
     C**********           MOVELXFMAD3    XFMAD2
     C**********           MOVEL*BLANK    XFMAD3
     C**********           ENDIF
     C***  End   Del  ***  10/27/03  **************************************
     **
     **  MOVE CORRECT FUND PHONE NUMBER TO WORK FIELDS
     **
     C*** BEGIN DEL ***  07/28/98  ************************************
     C*****      FNPH1     IFEQ *ZERO
     C*****                MOVE FNAREA    XXAREA  3
     C*****                MOVELFNPH2     XXPHN1  3
     C*****                MOVE FNPH2     XXPHN2  4
     C*****                ELSE
     C*****      FNPH1A    IFNE *ZERO
     C*****                MOVE FNPH1A    XXAREA
     C*****                ELSE
     C*****                MOVE 800       XXAREA
     C*****                ENDIF
     C*****                MOVELFNPH1     XXPHN1
     C*****                MOVE FNPH1     XXPHN2
     C*****                ENDIF
     **
     **  BUILD UP CONTACT USER NAME & PHONE NUMBER
     **
     C*****                MOVELF2UNM1    XXNAME 49 P
     C*****                CAT  ', (':0   XXNAME
     C*****                CAT  XXAREA:0  XXNAME
     C*****                CAT  ')':0     XXNAME
     C*****                CAT  XXPHN1:1  XXNAME
     C*****                CAT  '-':0     XXNAME
     C*****                CAT  XXPHN2:0  XXNAME
     C*****                CAT  '.':0     XXNAME
     **
     C*****                MOVELF2UNM2    XXNAM2 49 P
     C*****                CAT  ', (':0   XXNAM2
     C*****                CAT  XXAREA:0  XXNAM2
     C*****                CAT  ')':0     XXNAM2
     C*****                CAT  XXPHN1:1  XXNAM2
     C*****                CAT  '-':0     XXNAM2
     C*****                CAT  XXPHN2:0  XXNAM2
     C*****                CAT  '.':0     XXNAM2
     C*** END DEL *****  07/28/98  ************************************
     **
     **  GET COMMENT CODE DESCRIPTIONS (1-5)
     **
     C     QQCMT1        IFNE      *BLANK
     C                   MOVEL     QQCMT1        XXBCOM
     C     BCKY1         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM01           40
     C                   ENDIF
     C                   ENDIF
     **
     C     QQCMT2        IFNE      *BLANK
     C                   MOVEL     QQCMT2        XXBCOM
     C     BCKY1         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM02           40
     C                   ENDIF
     C                   ENDIF
     **
     C     QQCMT3        IFNE      *BLANK
     C                   MOVEL     QQCMT3        XXBCOM
     C     BCKY1         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM03           40
     C                   ENDIF
     C                   ENDIF
     **
     C     QQCMT4        IFNE      *BLANK
     C                   MOVEL     QQCMT4        XXBCOM
     C     BCKY1         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM04           40
     C                   ENDIF
     C                   ENDIF
     **
     C     QQCMT5        IFNE      *BLANK
     C                   MOVEL     QQCMT5        XXBCOM
     C     BCKY1         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM05           40
     C                   ENDIF
     C                   ENDIF
     **
     *****************************************************************
     ** MOVE ENTRY PARMS TO ROUTING PAGE
     *****************************************************************
      *
     C                   MOVE      QQCO#         RSCO#             3 0
     C                   MOVE      QQFND         RSFND             3 0
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      QQTFND        RSTFND            3 0
     C***  End   Add  ***  10/27/03  **************************************
     C                   MOVE      QQFRDT        RSFRDT            8 0
     C                   MOVE      QQTODT        RSTODT            8 0
     C                   MOVE      QQTODT        RSRNDT            8 0
     C***  Begin Del  ***  10/27/03  **************************************
     C***********          MOVE QQBLDT    RSBLDT  80
     C***  End   Del  ***  10/27/03  **************************************
     C                   MOVE      QQMAMT        RSMAMT            9 2
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C                   MOVE      QQRPTF        RSRPTF            1
     C***** END   ADD **** 03/03/00 ***********************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   MOVE      QQCTST        RSCTST            1
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     *LIKE         DEFINE    EBEMP#        RSEMP#
     C     *LIKE         DEFINE    EBDIV         RSDIV
     C*
     C                   MOVE      QQEMP#        RSEMP#
     C                   MOVE      QQDIV         RSDIV
     C*
     C                   SELECT
     C     QQEMP#        WHENEQ    *ZEROS
     C                   MOVEL(P)  '*ALL'        RSEDN            16
     C     QQDIV         WHENEQ    *BLANK
     C                   MOVEL(P)  '*ALL'        RSEDN
     C                   ENDSL
     C*** BEG ADD ****** 08/21/08 ************************************
     C*** BEG ADD ****** 04/14/09 ************************************
     C     Q1PRMS        IFGT      36
     C*** END ADD ****** 04/14/09 ************************************
     C                   SELECT
     C     QQJRST        WHENEQ    *BLANK
     C                   MOVEL(P)  '*ALL'        RXJRST           16
     C     QQJRST        WHENNE    *BLANK
     C                   MOVEL(P)  QQJRST        RXJRST
     C                   ENDSL
     C*** BEG ADD ****** 04/14/09 ************************************
     C                   ENDIF                                                  Q1PRMS > 36
     C*** END ADD ****** 04/14/09 ************************************
     C*** END ADD ****** 08/21/08 ************************************
     C*
     C                   CALL      'WA005R'
     C                   PARM                    RSEMP#
     C                   PARM                    RSDIV
     C                   PARM      RSEDN         RSNAME           40
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL(P)  XXEDN         RSEDN
     C                   MOVEL     QQINDP        RSINDP            1
     C***  End   Add  ***  10/27/03  **************************************
     **
     ** INITIALIZE SORT PARAMETERS
     **
     C                   MOVE      *BLANK        RSFREA            2
     C                   MOVE      *BLANK        RSTOEA            2
     C                   MOVE      *ZEROS        RSFREN
     C                   MOVE      *ZEROS        RSTOEN
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      QQFREF        RSFREF            8 0
     C                   MOVE      QQTOEF        RSTOEF            8 0
     C***  End   Add  ***  10/27/03  **************************************
     C                   MOVE      *BLANK        RSFRAA            2
     C                   MOVE      *BLANK        RSTOAA            2
     C                   MOVE      *ZEROS        RSFRAN            5 0
     C                   MOVE      *ZEROS        RSTOAN            5 0
     **
     ** DISPLAY CORRECT SORT CODES
     **
      * * * * * * *
      *   SORT 1
      * * * * * * *
     C                   SELECT
      *
     C     QQSRT1        WHENEQ    'AN'
     C*** BEG CHG *** 09/24/04 *****************************
     C***********          MOVEL'AGENT #' RSSRT1 13
     C     EXAHDR        IFNE      *BLANKS
     C                   MOVEL(P)  EXAHDR        RSSRT1           13
     C                   ELSE
     C                   MOVEL(P)  'AGENT #'     RSSRT1
     C                   ENDIF
     C*** END CHG *** 09/24/04 *****************************
      **** BEGIN ADD **** 08/30/99 ************************************
     C                   MOVE      *ON           AGTSRT            1
      **** END   ADD **** 08/30/99 ************************************
      *
     C     QQSRT1        WHENEQ    'AA'
     C*** BEG ADD *** 09/24/04 *****************************
     C     EXAHDR        IFNE      *BLANKS
     C     EXAHDR        CAT(P)    'NAME':1      RSSRT1
     C                   ELSE
     C*** END ADD *** 09/24/04 *****************************
     C     'AGENT'       CAT       'NAME':1      RSSRT1
     C*** BEG ADD *** 09/24/04 *****************************
     C                   ENDIF
     C*** END ADD *** 09/24/04 *****************************
      **** BEGIN ADD **** 08/30/99 ************************************
     C                   MOVE      *ON           AGTSRT            1
      **** END   ADD **** 08/30/99 ************************************
      *
     C     QQSRT1        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSSRT1
      *
     C     QQSRT1        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSSRT1
      *
     C                   ENDSL
      * * * * * * *
      *   SORT 2
      * * * * * * *
     C                   SELECT
      *
     C     QQSRT2        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSSRT2           13
      *
     C     QQSRT2        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSSRT2
      *
     C     QQSRT2        WHENEQ    *BLANK
     C                   MOVEL     'NONE'        RSSRT2
      *
     C                   ENDSL
     **
     C                   MOVE      QQFRAN        RSFRAN
     C                   MOVE      QQTOAN        RSTOAN
     C                   MOVE      QQFRAA        RSFRAA
     C                   MOVE      QQTOAA        RSTOAA
     C                   MOVE      QQFREN        RSFREN
     C                   MOVE      QQTOEN        RSTOEN
     C                   MOVE      QQFREA        RSFREA
     C                   MOVE      QQTOEA        RSTOEA
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           MOVE QQPAYD    RSPAYD  80
     C***  End   Del  ***  10/27/03  **************************************
     **
     C                   SELECT
     C     QQACST        WHENEQ    'A'
     C                   MOVEL     'ACTIVE'      RSACT            25            ACTIVE
     C     QQACST        WHENEQ    'C'
     C     'CANCELLE'    CAT       'D':0         RSACT                          CANCELLED
     C     QQACST        WHENEQ    'B'
     C     'BOTH ACT'    CAT       'IVE AN':0    RSACT                          BOTH
     C                   CAT       'D CANC':0    RSACT
     C                   CAT       'ELLED':0     RSACT
     C                   ENDSL
     **
     C                   SELECT
     C     QQAUST        WHENEQ    'A'
     C                   MOVEL     'AUDITED'     RSAUST           26            AUDITED
     C     QQAUST        WHENEQ    'N'
     C     'NON-AUDI'    CAT       'TED':0       RSAUST                         NOT AUTITED
     C     QQAUST        WHENEQ    'B'
     C     'BOTH AUD'    CAT       'ITED &':0    RSAUST                         BOTH
     C                   CAT       ' NON-A':0    RSAUST
     C                   CAT       'UDITED':0    RSAUST
     C                   ENDSL
      *
     C                   SELECT
     C     QQESSR        WHENEQ    'E'
     C     'ESTIMATI'    CAT       'NG':0        RSESSR           33
     C     QQESSR        WHENEQ    'S'
     C     'SELF-REP'    CAT       'ORTING':0    RSESSR
     C                   OTHER
     C     'BOTH EST'    CAT       'IMATED':0    RSESSR
     C                   CAT       'AND SE':1    RSESSR
     C                   CAT       'LF-REP':0    RSESSR
     C                   CAT       'ORTING':0    RSESSR
     C                   ENDSL
     **
     ** Sort the Code Array
     **
     C                   SORTA     COD
     C                   MOVE      *BLANK        CDE
     C                   MOVE      *BLANK        DES
      *
     C     QQARIE        IFNE      *BLANK
     **
     ** Fill the Code Decription Array which Corresponds to the Code
     **
     C                   Z-ADD     1             Z                 2 0
     C     1             DO        30            Y                 2 0
     **
     ** Indicators 20 to 49 are used for Output Purposes
     **
     C     COD(Y)        IFNE      *BLANK
     C     Z             ADD       19            Z1                2 0
     C                   MOVE      *ON           *IN(Z1)
     C                   MOVEL(P)  COD(Y)        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     COD(Y)        CDE(Z)
     C                   MOVEL     TTDESC        DES(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO                                                  1 DO 30  Y
      *
      *   SET INDICATOR FOR EXCLUDE VS INCLUDE VERBIAGE
      *
     C     QQARIE        IFEQ      'E'
     C                   MOVE      *OFF          *IN51
     C                   ELSE
     C                   MOVE      *ON           *IN51
     C                   ENDIF                                                  QQARIE = 'I'
      *
     C                   ELSE
      *
      *   PRINT ALL CODES ON ROUTING PAGE
      *
     C                   MOVE      *ON           *IN51
     C                   MOVE      '*ALL'        RSCODE            4
     **
     C                   ENDIF                                                  QQARIE IFNE *BLANK
      **** BEGIN ADD **** 11/17/00 *********************************************
      *
      * SORT THE ACTIVITY STATUS ARRAY
      *
     C                   SORTA     AS
     C                   MOVE      *BLANK        ASDS
      *
     C     QQIEFL        IFNE      *BLANK
      *
      * Fill the Code Decription Array which Corresponds to the Code
      *
     C     1             DO        5             Y
      *
      * Indicators 15 to 19 are used for Output Purposes
      *
     C     AS(Y)         IFNE      *BLANK
      *
     C     Y             ADD       14            Z1                2 0
     C                   MOVE      *ON           *IN(Z1)
     C                   MOVEL(P)  AS(Y)         XXKEY
      *
     C     TBKEY         CHAIN     PAV01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AVDESC        ASDS(Y)
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  AS,Y IFNE *BLANK
      *
     C                   ENDDO                                                  1 DO 30  Y
      *
      *   SET INDICATOR FOR EXCLUDE VS INCLUDE VERBIAGE
      *
     C     QQIEFL        IFEQ      'E'
     C                   MOVE      *OFF          *IN14
     C                   ELSE
     C                   MOVE      *ON           *IN14
     C                   ENDIF                                                  QQIEFL = 'E'
      *
     C                   ELSE
      *
      *   PRINT ALL CODES ON ROUTING PAGE
      *
     C                   MOVE      *ON           *IN14
     C                   MOVE      '*ALL'        RSASTC            4
      *
     C                   ENDIF                                                  QQIEFL IFNE *BLANK
      **** END   ADD **** 11/17/00 *********************************************
     **
     **   GET THE DISTRIBUTION INFORMATION & PRINT
     **
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C     QQRPTF        ANDEQ     *BLANK
     C*
     C*  LEFT JUSTIFY REPORT TITLE
     C*
     C     ' '           CHECK     ORTITL        X                 2 0
     C*
     C     X             IFNE      0
     C                   SUBST(P)  ORTITL:X      XXTITL           60
     C                   ENDIF
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  LEFT JUSTIFY REPORT TITLE
      *
     C     ' '           CHECK     ORTITL        X                 2 0
      *
     C     X             IFNE      0
     C                   SUBST(P)  ORTITL:X      XXTITL           60
     C                   ENDIF                                                  P IFNE 0
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C******BEGIN ADD **** 03/03/00 ***********************************
     C********** QQRPTF    IFEQ *BLANK
     C******END*  ADD **** 03/03/00 ***********************************
     C**********           EXCPTROUTE
     C******BEGIN ADD **** 03/03/00 ***********************************
     C**********           ENDIF                           QQRPTF=BLK
     C***  End   Del  ***  10/27/03  **************************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
     C***** END   ADD **** 03/03/00 ***********************************
     C                   ENDIF
     **
     C*****Begin Add  ***  04/07/17  **********************************
    c** If user invokes the screen using group select, then core logic will
    c** be executed in loop for selected group, Otherwise (single employer)
    C** it will work as it was.
     c
     c                   If        strGrpSlct = 'Y'
     C
     C     FNKY1         SETLL     PGS01
     C     FNKY1         Reade     PGS01
     C                   Dow       Not %eof(WXGSL)
     C                   If        GsIEFl ='I'
     C                   MOVE      GsFnd         XXFND
     C                   MOVE      GsFnd         XXFFND
     C                   MOVE      GsFnd         XXTFND
     C                   Exsr      MainPrc
     C                   EndIf
     C
     C     FNKY1         Reade     PGS01
     C                   EndDo
     c
     c                   Else
     c                   Exsr      MainPrc
     C                   EndIf
      /Free
       If isUseSpoolOut;
          xxprt = '0';
          Chain orky POR01;
          If %found;
             Except Route;
          Endif;
          xxprt = '1';
          Chain orky POR01;
          If %found;
            Except Route1;
          Endif;
       EndIf;

       If xmlRptFile <> ' ';

          L8XML = xmlRptFile;

          Clear L8Fnd;
          Clear L8Emp#;
          Clear L8Div;
          Clear L8Fyr;
          Clear L8SrvName;
          Clear L8GrpName;
          Clear L8PolName;
          Clear L8PolFdnm;
       //*** Begin Add *** 01/03/20 ******************************************
          Clear L8Cont;
       //*** End   Add *** 01/03/20 ******************************************
          Clear L8PolAdr1;
          Clear L8PolAdr2;
          Clear L8PolCity;
          Clear L8PolSt;
          Clear L8PolZip;
          Clear L8AgtHdg;
          Clear L8Agt#;
          Clear L8AgtName;
          Clear L8Prd#;
          Clear L8PrdLsn;
          Clear L8PrdFsn;
          Clear L8DueDate;
          Clear L8PastDueD;
          Clear L8Audit;
          Clear L8TotalD;
          Clear L8Hdg01;
          Clear L8Hdg02;
          Clear L8Hdg03;
          Clear L8Hdg04;
          Clear L8Hdg05;
          Clear L8Hdg06;
          Clear L8Hdg07;
          Clear L8Hdg08;
          Clear L8Hdg09;
          Clear L8Hdg10;
          Clear L8Hdg11;
          Clear L8Hdg12;
          Clear L8Hdg13;
          Clear L8Hdg14;
          Clear L8Hdg15;
          Clear L8Hdg16;
          Clear L8Hdg17;
          Clear L8Hdg18;
          Clear L8Hdg19;
          Clear L8Hdg20;
          Clear L8Hdg21;
          Clear L8Hdg22;
          Clear L8Hdg23;
          Clear L8Hdg24;
          Clear L8Hdg25;
          Clear L8Hdg26;
          Clear L8Hdg27;
          Clear L8Hdg28;
          Clear L8Hdg29;
          Clear L8Hdg30;
          Clear L8Amt01;
          Clear L8Amt02;
          Clear L8Amt03;
          Clear L8Amt04;
          Clear L8Amt05;
          Clear L8Amt06;
          Clear L8Amt07;
          Clear L8Amt08;
          Clear L8Amt09;
          Clear L8Amt10;
          Clear L8Amt11;
          Clear L8Amt12;
          Clear L8Amt13;
          Clear L8Amt14;
          Clear L8Amt15;
          Clear L8Amt16;
          Clear L8Amt17;
          Clear L8Amt18;
          Clear L8Amt19;
          Clear L8Amt20;
          Clear L8Amt21;
          Clear L8Amt22;
          Clear L8Amt23;
          Clear L8Amt24;
          Clear L8Amt25;
          Clear L8Amt26;
          Clear L8Amt27;
          Clear L8Amt28;
          Clear L8Amt29;
          Clear L8Amt30;

          L8TotalD = xxTotl;
          L8EmCnt = xxCnt;

          L8Pgm  = Q1PGM;
          L8User = Q1USER;
          L8Time = DSTime;
          L8Dt   = sysDt;

          Write pl80p;

         Endif;

         If isUseXML;
            Close WXL8P;
         Endif;

         Select;
         When xmlFlowName   = *Blanks;
         When objExists('PDSDEV':'*LIB');
            Monitor;
            S4401C(xmlFlowName + '$');
            On-error;
            Endmon;
         Other;
            S4401C(xmlFlowName + '$');
         Endsl;

      /End-Free
     C
     C                   MOVE      *ON           *INLR
     C
    C** Put the core logic in a subroutine (MainPrc)
     C     MainPrc       BegSr
     C*****End Add    ***  04/07/17  *********************************
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      XXFFND        XXFND
     C     FNKY          SETLL     PFN01
     C                   EXSR      GETGRP
     C*
     C     NOGRP         DOWEQ     *OFF
     C***  End   Add  ***  10/27/03  **************************************
     C*
      **** BEGIN ADD **** 08/30/99 ************************************
     C*  INITIALIZE AGENT COUNT
     C                   Z-ADD     0             AGTCNT            5 0
     C                   MOVE      *ON           *INOF
      **** END   ADD **** 08/30/99 ************************************
     C                   MOVE      *ON           *INOA
     C                   Z-ADD     *ZERO         Z
     **
     **  MAIN LOOP BEGINS HERE
     **
     **   START READING THE LATE NOTICE WORK FILE
     **
     C     EBKY          SETLL     PEB01
     C     EBKY          READE     PEB01                                  50
     **
     C     *IN50         DOWEQ     *OFF
     C*** BEG ADD ****** 08/21/08 ************************************
     C*** BEG ADD ****** 04/14/09 ************************************
     C     Q1PRMS        IFGT      36
     C*** END ADD ****** 04/14/09 ************************************
     C     QQJRST        IFNE      *BLANK
     C     EBJRST        ANDNE     QQJRST
     C                   GOTO      NEXTEB
     C                   ENDIF
     C*** BEG ADD ****** 04/14/09 ************************************
     C                   ENDIF                                                  Q1PRMS > 36
     C*** END ADD ****** 04/14/09 ************************************
     C*** END ADD ****** 08/21/08 ************************************
      **** BEGIN ADD **** 08/30/99 ************************************
     C*   IF SORT IS BY AGENT# AND CURRENT AGENT IS DIFFERENT FROM LAST
     C*   AGENT PROCESSED: PRINT AGENT TOTALS FOR PREVIOUS AGENT AND
     C*   RESET AGENT TOTAL BUCKETS
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        ANDNE     HLDAGT
     C*
     C*     IF AGNTCNT = 0, DO NOT PRINT TOTALS
     C*
     C     AGTCNT        IFNE      0
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     QQRPTF        IFEQ      *BLANK
     C                   EXSR      AGTINF
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** end add *****  01/10/17  *****************************************
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   EXCEPT    AGTOTL
     C                   ELSE
     C                   EXCEPT    AGTOTL
     C                   ENDIF
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
     C*
     C                   Z-ADD     *ZERO         AGTCNT
     C                   Z-ADD     *ZERO         XXAGTL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EBAGT#        HLDAGT
     C                   MOVE      *ON           *INOF
     C*
     C                   ENDIF                                                  AGTCNT<>0
     C*
     C                   ENDIF                                                  AGSTRT=*ON
      **** END   ADD **** 08/30/99 ************************************
     **
     C                   MOVE      EBEMP#        XXEMP#
     C                   MOVE      EBDIV         XXDIV
     C                   MOVE      EBFND         XXFND
     C                   MOVE      EBFYR         XXFYR
      *** BEG ADD ****** 01/10/06 *************************************
     C                   Z-ADD     EBPFDT        KKPFDT            7 0
      *** END ADD ****** 01/10/06 *************************************
      *
     C                   MOVE      *ZERO         XXDUDT            8 0
     C                   MOVE      *ZERO         XXPDDT            8 0
      *
     **   LOAD POLICY NUMBER
     **
      *** BEG ADD ***** 09/20/06 **************************************
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   EXSR      SRP71#
     C                   OTHER
      *** END ADD ***** 09/20/06 **************************************
     C                   CALL      'WA022R'
     C                   PARM      *BLANKS       XXPLCY           15
     C                   PARM                    XXEMP#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
      *** BEG ADD ***** 09/20/06 **************************************
     C                   ENDSL
      *** END ADD ***** 09/20/06 **************************************
      ***** BEG ADD ******* 02/28/02 **********************************
      *
     C                   MOVEL(P)  *BLANKS       XXFIL#           15
     C                   MOVEL(P)  *BLANKS       X1PLCY           18
     C     RPKY          CHAIN     PRP01                              52
      *
     C     *IN52         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVE      XXPLCY        XXFIL#
     C                   MOVEL(P)  RPCAP#        X1PLCY
      *
     C                   ELSE
      *
     C                   MOVE(P)   XXPLCY        X1PLCY
     C                   SETON                                        52
     C                   ENDIF
      *
      ***** END ADD ******* 02/28/02 **********************************
     **
     **  VALIDATE RECORD TO EMPLOYER NAME & AGENT RANGE SELECTIONS
     **
     C                   MOVEL(P)  EBENAM        XXNM              2
     C                   MOVEL(P)  EBENAM        XXENM2           38
     **
      *   CHECK FOR PREVIOUS FUND YEAR
      *
     C                   SUB       1             XXFYR
     C     ELKY          CHAIN     PEL01                              92
      *
     C                   ADD       1             XXFYR
      *** BEG ADD *** 11/03/06 **********************************
     C*
     C*** RESET THE FILE TO THE CURRENT YEAR
     C*
     C     ELKY          CHAIN     PEL01                              81
      *** END ADD *** 11/03/06 **********************************
     C*** begin add ***  01/10/17  *********************************************
     C                   eval      x@RRN = dbRRN
     C*** end add *****  01/10/17  *********************************************
      *
     **   CHAIN TO EMPLOYER MASTER FILE
     **
     C     EMKY          CHAIN     PEM01                              80
      **** BEGIN ADD **** 11/17/00 *********************************************
     C     *IN80         IFEQ      *OFF
      *
      ***** BEG DEL ******* 07/01/03 **********************************
     C*********  QQIEFL    ANDNE*BLANK
      ***** END DEL ******* 07/01/03 **********************************
      ***** BEG ADD ******* 07/01/03 **********************************
      *
      *  Get Employer Cancellation date
      *
     C                   Z-ADD     5             I1                3 0
      *
     C     ED(I1)        DOWEQ     *ZERO
     C     I1            ANDGT     1
     C                   SUB       1             I1
     C                   ENDDO
      *
     C                   Z-ADD     CD(I1)        EBCNDT
      *
     C     QQIEFL        IFNE      *BLANK
      ***** END ADD ******* 07/01/03 **********************************
     C***** BEG ADD ******* 07/07/05 **********************************
     C                   MOVEL     ELASCD        EMASTS            2
     C***** END ADD ******* 07/07/05 **********************************
      *
     C                   Z-ADD     1             Y
     C     EMASTS        LOOKUP    AS(Y)                                  60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQIEFL        IFEQ      'E'
      *
     C     EMASTS        IFEQ      *BLANK
     C                   MOVE      *ON           *IN60
     C                   ELSE
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  EMASTS IFEQ *BLANK
      *
     C                   ELSE
      *
     C     EMASTS        IFEQ      *BLANK
     C                   MOVE      *OFF          *IN60
     C                   ENDIF                                                  EMASTS IFEQ *BLANK
      *
     C                   ENDIF                                                  QQIEFL = 'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN80
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      ***** BEG ADD ******* 07/01/03 **********************************
     C                   ENDIF                                                  QQIEFL NE BLANK
      *
      ***** END ADD ******* 07/01/03 **********************************
     C                   ENDIF                                                  *IN80 IFEQ *OFF
      **** END   ADD **** 11/17/00 *********************************************
     **
     C*** BEGIN ADD ***  06/01/98  ************************************
     C                   MOVE      *BLANKS       OFCKNM
     C*** BEGIN ADD ***  07/28/98  ************************************
     C                   MOVE      *BLANKS       XXNAME
     C*** END ADD *****  07/28/98  ************************************
     **
     **   CHAIN TO THE FUND MASTER FILE
     **
     C     OFKY          CHAIN     POF01                              60
     **
     C     *IN60         IFEQ      *OFF
     **
      ***** BEG CHG **** 06/02/03 *********************************************
      * If Self-INsured Group, use Group Name intead of Office Acct Name
     C***********          MOVELOFACNM    SRDFLD    P
     C   78              MOVEL(P)  OFACNM        SRDFLD
     C  N78              MOVEL(P)  FNNAME        SRDFLD
      ***** END CHG **** 06/02/03 *********************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM           40
     **
      ***** BEG CHG **** 06/02/03 *********************************************
     C**************       MOVELOFACNM    XXACNM 40 P
     C   78              MOVEL(P)  OFACNM        XXACNM           40
     C  N78              MOVEL(P)  FNNAME        XXACNM           40
      ***** END CHG **** 06/02/03 *********************************************
     C                   MOVEL(P)  OFAAD1        XXAAD1
     C                   MOVEL(P)  OFAAD2        XXAAD2
     C                   MOVEL(P)  OFACTY        XXCITY           25
     C                   MOVEL     OFAST         XXSTAT            2
     C                   MOVEL     OFAZIP        XXZIP1            5
     C                   MOVE      OFAZIP        XXZIP2            4
     **
     C                   EXSR      SRCSTZ
     C                   MOVEL     XXCSTZ        XXAAD3
     **
     C     OFAAD2        IFEQ      *BLANK
     C                   MOVEL     XXAAD3        XXAAD2
     C                   MOVEL     *BLANK        XXAAD3
     C                   ENDIF
      ***** BEG ADD **** 02/13/03 *************************************
      ***** BEG DEL **** 05/08/03 *************************************
      ***If EXISTS use LOCK BOX address instead of Accounting Ofc addr
     C*******    XXLKBX    IFEQ 'Y'
     C*******              MOVELF2LAD1    XXAAD1    P
     C*******              MOVELF2LAD2    XXAAD2    P
     C*******              MOVELF2LCTY    XXCITY 25 P
     C*******              MOVELF2LST     XXSTAT  2
     C*******              MOVELF2LZIP    XXZIP1  5
     C*******              MOVE F2LZIP    XXZIP2  4
     C*******              EXSR SRCSTZ
     C*******              MOVELXXCSTZ    XXAAD3
     C*******    F2LAD2    IFEQ *BLANK
     C*******              MOVELXXAAD3    XXAAD2
     C*******              MOVEL*BLANK    XXAAD3
     C*******              ENDIF
     C*******              ENDIF
      ***** END DEL **** 05/08/03 *************************************
      ***** END ADD **** 02/13/03 *************************************
     **
     C*** BEGIN ADD ***  07/28/98  ************************************
     C*
     C*   MOVE CONTACT USER NAME AND PHONE TO OUTPUT FIELD
     C*
     C                   MOVEL(P)  OFLNNM        XXNAME           49
     C*** BEGIN ADD ***  08/31/98  ************************************
     C     OFLNNM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN93
     C                   ELSE
     C                   MOVE      *OFF          *IN93
     C                   ENDIF
     C*** END   ADD ***  08/31/98  ************************************
     C*
     C     OFAPHN        IFNE      *ZERO
     C                   MOVE      OFAARA        XXAREA            3
     C                   MOVEL     OFAPHN        XXPHN1            3
     C                   MOVE      OFAPHN        XXPHN2            4
     C   93              CAT       ',':0         XXNAME
     C  N93              CAT       '(':0         XXNAME
     C   93              CAT       '(':1         XXNAME
     C                   CAT       XXAREA:0      XXNAME
     C                   CAT       ')':0         XXNAME
     C                   CAT       XXPHN1:1      XXNAME
     C*** Begin Chg *** 12/11/15 *********************************
     C**********         CAT       '-':0         XXNAME
     C**********         CAT       XXPHN2:0      XXNAME
     C                   CAT       '-':1         XXNAME
     C                   CAT       XXPHN2:1      XXNAME
     C*** End   Chg *** 12/11/15 *********************************
     C                   ENDIF                                                  OFAPHN IFNE
     C*** Begin Del *** 12/11/15 *********************************
     C**********         CAT       '.':0         XXNAME
     C*** End   Del *** 12/11/15 *********************************
     C*** END ADD *****  07/28/98  ************************************
     **
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  06/01/98  ************************************
      *** BEG DEL *** 11/03/06 **********************************
     ** ALREADY CHAINED TO THE CORRECT RECORD WITH *IN81
     ***********
     *****CHAIN*TO EMPLOYER DETAIL FILE
     ***********
     C********** ELKY      CHAINPEL01                81
      *** END DEL *** 11/03/06 **********************************
     **
     C***  Begin Add  ***  10/27/03  **************************************
     C     ELPFDT        IFLT      X7FREF
     C     ELPFDT        ORGT      X7TOEF
     C     X7TOEF        ANDNE     *ZERO
     C                   MOVE      *ON           *IN81
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*
     C*  IF 'EXCLUDE CONTESTED AUDITS' FLAG (QQCTST) IS Y
     C*      AND ELCTST=Y, THEN BYPASS EMP DETAIL RECORD
     C*
     C     QQCTST        IFNE      'Y'
     C     ELCTST        ORNE      'Y'
     C*
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     **   CHAIN TO THE FUND DETAIL FILE
     **
     C     F1KY          CHAIN     PF101                              82
      *
      *   MAKE SURE FILES GOT A HIT
      *
     C     *IN80         IFEQ      *OFF
     C     *IN81         ANDEQ     *OFF
     C     *IN82         ANDEQ     *OFF
      *
     **  IF FEDERAL NAME IS NOT BLANK THEN PRINT IT
     **
     C                   MOVEL(P)  EMFDNM        XXFDNM           38
     **
     C     XXFDNM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     **
     **  SETUP THE ACCOUNT STATUS INDICATORS: 72 = ACTIVE
     **                                       73 = CANCEL PEND
     **                                       74 = CANCELLED
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
     **
     C     EBCNDT        IFEQ      *ZERO                                        ACTIVE
     C                   MOVE      *ON           *IN72
     C                   ENDIF
     **
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** EBCNDT    IFGT XXDAT                      CX PENDING
     C     EBCNDT        IFGT      SYSDT                                        CX PENDING
     C***  End   Chg  ***  10/27/03  **************************************
     **
     C                   MOVE      *ON           *IN73
     **
     C                   Z-ADD     EBCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
     **
     C                   ENDIF                                                  EBCNDT > XXDAT
     **
     C     EBCNDT        IFNE      *ZERO                                        CANCELLED
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** EBCNDT    ANDLEXXDAT
     C     EBCNDT        ANDLE     SYSDT
     C***  End   Chg  ***  10/27/03  **************************************
     **
     C                   Z-ADD     EBCNDT        SRDTI
     **
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT
     C                   MOVE      *ON           *IN74
     **
     C                   ENDIF                                                  EBCNDT NE *ZERO
      *
      *   IF CANX PENDING (IN73 = *ON) THEN GET CANX REASON FROM FILE
      *
     C     CXKY          CHAIN     PCX01                              60
      *
     C                   MOVEL(P)  CXRSN         XXKEY            10
      *
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C********** TBKY      CHAINPCR01                60
     C*
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C                   MOVE      *BLANKS       XXRESN
     C*
     C***  BEGIN ADD  ***  02/24/99  **********************************
     C                   MOVE      SYSDT         KKEFDT
     C***  END   ADD  ***  02/24/99  **********************************
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 12/11/02 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 12/11/02 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   CHG  ***  11/10/98  **********************************
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CRDESC        XXRESN           60
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C     EBSEFL        IFEQ      'S'
     C                   MOVE      *ON           *IN75
     C                   ELSE
     C                   MOVE      *OFF          *IN75
     C                   ENDIF                                                  EBSEFL = 'S'
     **
     C*** BEGIN ADD *** 05/15/07 **************************************
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C                   ELSE
     C                   MOVE      *BLANKS       KKST
     C                   ENDIF
     C*
     C                   MOVE      ELPFDT        KKEFDT
     C*** BEGIN CHG ****** 09/23/10 ************************************
     C********   RRKY      SETGTPRR01
     C********   RRKY1     REDPEPRR01                    60
     C     RRKY1         SETGT     PRR01
     C     RRKY          READPE    PRR01                                  60
     C*** END   CHG ****** 09/23/10 ************************************
     C*
     C***  END  ADD *** 05/15/07 **************************************
     C*
     C                   Z-ADD     *ZERO         XXAMTD            9 2
     C                   Z-ADD     *ZERO         XXDEPD            9 2
     C                   Z-ADD     *ZERO         XXPRMD            9 2
     C                   Z-ADD     *ZERO         XXEXCD            9 2
     C                   Z-ADD     *ZERO         XXDEDD            9 2
     C                   Z-ADD     *ZERO         XXFEED            9 2
     C                   Z-ADD     *ZERO         XXTOTD            9 2
     C                   Z-ADD     *ZERO         XXAUDT
     C                   Z-ADD     *ZERO         XXPAY             9 2
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   Z-ADD     *ZERO         XXTAX             9 2
     C*** END   ADD *** 08/31/98 **************************************
     C***BEGIN  ADD *** 05/15/07 **************************************
     C                   Z-ADD     *ZERO         XX1STB            9 2
     C                   Z-ADD     *ZERO         XX2NDB            9 2
     C                   Z-ADD     *ZERO         XX3RDB            9 2
     C                   Z-ADD     *ZERO         XX4THB            9 2
     C                   Z-ADD     *ZERO         XX5THB            9 2
     C***  Begin Add  ***  12/11/08  **************************************
     C                   Z-ADD     *ZERO         XX6THB            9 2
     C                   Z-ADD     *ZERO         XX7THB            9 2
     C***  End   Add  ***  12/11/08  **************************************
     C*
     C*** END   ADD *** 05/15/07 **************************************
     **
     **  FIND THE LAST AUDIT DATE FOR THE FUND YEAR
     **
     C                   MOVEL     'D'           XXARTY
     **
     C     EAKY2         SETGT     PEA03
     C     EAKY3         READPE    PEA03                                  60
     **
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EAPODT        XXAUDT
     C                   ENDIF
     **
     **  IF AUDIT DATE IS GREATER THAN BALANCE DATE, DON'T TREAT
     **     THIS AS AN AUDITED ACCOUNT
     **
     C     XXAUDT        IFGT      XXDAT2
     C***  BEGIN ADD  ***  08/23/99  **********************************
     C     ELFAF         ORNE      'A'
     C***  END   ADD  ***  08/23/99  **********************************
     C                   Z-ADD     *ZERO         XXAUDT
     C                   ENDIF
     **
     **  SET AUDIT FLAG
     **
     C     XXAUDT        IFGT      *ZERO
     C                   MOVE      'A'           ADFLG             1
     C                   MOVE      *ON           *IN71
     C                   ELSE
     C                   MOVE      'N'           ADFLG
     C                   MOVE      *OFF          *IN71
     C                   ENDIF
     **
     **   VALIDATE TO SELECTED AUDIT STATUS
     **
     C     QQAUST        IFEQ      'B'
     C     QQAUST        OREQ      ADFLG
     **
     **  USE CANCEL DATE IF IT LESS THAN THE BALANCE DATE AND THE
     **  EMPLOYER HAS NOT BEEN AUDITTED
     **
     C***  BEGIN CHG  ***  07/22/99  **********************************
     C*******************  Z-ADDXXRNDT    SRDTI
     C*******************  CALL 'SRCVTDAT'DTCYMD
     C*******************  MOVELSRDTO     XXABDT
     C                   Z-ADD     XXBLDT        XXABDT
     C***  END   CHG  ***  07/22/99  **********************************
     C*
     C                   MOVE      *OFF          *IN95
     **
     C     ADFLG         IFNE      'A'
     C     EBCNDT        ANDNE     *ZERO
     C     EBCNDT        ANDLT     XXABDT
     **
     C                   Z-ADD     EBCNDT        XXABDT
     C                   MOVE      *ON           *IN95
     **
     C                   ENDIF
     **
     **   START READING THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     **
     C     EAKY          SETLL     PEA01
     C     EAKY          READE     PEA01                                  60
     C     *IN60         DOWEQ     *OFF

      *** beg add ***** 074/02/13 **********************************************
      /free
       // Exclude dividend transactions from the report balance calc
       If EAarcd = F1dvbl or EAarcd = F1dvpd or EAarcd = F1dvxr;
          Reade eaKy pea01;
      *** begin add ***  05/17/21  ********************************************
          If %Eof(wmeal8);
             *In60 = *on;
          Endif;
      *** End add ***  05/17/21  ********************************************
          Iter;
       Endif;
      /end-free
      *** beg add ***** 074/02/13 **********************************************

     C*** BEG ADD ****** 02/26/09 **********************************************
     C* RETRIEVE A/R TYPE CODE IN BEGINNING AS IT IS USED IN VARIOUS CONDTIONS
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C*** END ADD ****** 02/26/09 **********************************************
     **
     **   A/R CODE MUST BE ONE OF THE CODES ENTERED UNLESS
     **    CODES WERE LEFT BLANK THEN ALL CODES WILL BE USED
     **
     C     QQARIE        IFNE      *BLANK
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQARIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ENDIF                                                  QQARIE NE *BLANK
     **
     C     *IN60         IFEQ      *ON
     C     EAARCD        ANDNE     *BLANK
     C     QQARIE        OREQ      *BLANK
     **
     **   INCLUDE 'A' & 'B' AR TYPES UP TO CANX DATE OR BILLING THROUGH
     **   DATE, WHICHEVER COMES FIRST. INCLUDE ALL OTHERS UP TO BILLING
     **   THRU DATE EXCEPT INCLUDE PAYMENT TRANS THRU PAYMENT THRU DATE
      *
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
      *
     C     EAPODT        IFLE      XXABDT
     C     *IN95         ANDEQ     *OFF                                         NOT CANCELLE
     C     EAPODT        ORLT      XXABDT
     C     *IN95         ANDEQ     *ON                                          CANCELLED
      ***** BEG ADD ******* 01/11/02 **********************************
     C     EAAMT         ORLT      0
     C***  Begin Add  ***  01/07/04  **************************************
     C     EAPODT        ANDLE     SYSDT
     C***  End   Add  ***  01/07/04  **************************************
     C***  Begin Del  ***  10/27/03  **************************************
     C********** EAPODT    ANDLEXXPTHR
     C***  End   Del  ***  10/27/03  **************************************
      ***** END ADD ******* 01/11/02 **********************************
     C                   EXSR      SRDUE1
     C                   ADD       EAAMT         XXAMTD
     C                   ENDIF                                                  EAPODT LE XXABDT
      *
     C                   ELSE                                                   EAARTY = 'B'
     C*
     C***  BEGIN ADD  ***  07/02/99  **********************************
     C*   Calculate tax billed the same as 'A' and 'B' a/r types and calc
     C*   the other fees like the others
     C     EAARTY        IFEQ      'F'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     TTCTYP        IFEQ      'T'                                          tax billed type
     C***** BEG ADD ******* 05/15/07 **********************************
     C*
     C     TTCTYP        OREQ      '1'
     C     RR1STB        ANDEQ     'Y'
     C     TTCTYP        OREQ      '2'
     C     RR2NDB        ANDEQ     'Y'
     C     TTCTYP        OREQ      '3'
     C     RR3RDB        ANDEQ     'Y'
     C     TTCTYP        OREQ      '4'
     C     RR4THB        ANDEQ     'Y'
     C     TTCTYP        OREQ      '5'
     C     RR5THB        ANDEQ     'Y'
     C***  Begin Add  ***  12/11/08  **************************************
     C     TTCTYP        OREQ      '6'
     C     RR6THB        ANDEQ     'Y'
     C     TTCTYP        OREQ      '7'
     C     RR7THB        ANDEQ     'Y'
     C***  End   Add  ***  12/11/08  **************************************
     C*
     C***** END ADD ******* 05/15/07 **********************************
     C     EAPODT        IFLE      XXABDT
     C     *IN95         ANDEQ     *OFF
     C     EAPODT        ORLT      XXABDT
     C     *IN95         ANDEQ     *ON
      ***** BEG ADD ******* 01/11/02 **********************************
     C     EAAMT         ORLT      0
     C***  Begin Add  ***  01/07/04  **************************************
     C     EAPODT        ANDLE     SYSDT
     C***  End   Add  ***  01/07/04  **************************************
     C***  Begin Del  ***  10/27/03  **************************************
     C********** EAPODT    ANDLEXXPTHR
     C***  End   Del  ***  10/27/03  **************************************
      ***** END ADD ******* 01/11/02 **********************************
     C                   EXSR      SRDUE1
     C                   ADD       EAAMT         XXAMTD
     C                   ENDIF                                                  EAPODT<=XXABDT
     C*
     C                   ELSE
     C*
     C     EAPODT        IFLE      XXBLDT
     C***  Begin Add  ***  10/27/03  **************************************
     C     EAAMT         ORLT      0
     C***  End   Add  ***  10/27/03  **************************************
     C                   EXSR      SRDUE1
     C                   ADD       EAAMT         XXAMTD
     C                   ENDIF                                                  EAPODT<=XXBLDT
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C***  END   ADD  ***  07/02/99  **********************************
      *
     C     EAARTY        IFEQ      'D'
     C***  Begin Add  ***  01/16/04  **************************************
     C     EAPODT        ANDLE     XXBLDT
     C***  End   Add  ***  01/16/04  **************************************
     C*** BEG ADD *******  08/11/04  **************************************
     C     EAARTY        OREQ      'D'
     C     EAAMT         ANDLT     0
     C*** END ADD *******  08/11/04  **************************************
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'R'
     C***  BEGIN DEL  ***  07/02/99  **********************************
     C********** EAARTY    OREQ 'F'
     C***  END   DEL  ***  07/02/99  **********************************
     C***  BEG ADD ******  07/18/07  **********************************
     C     TTCTYP        ANDEQ     'P'
     C***  END ADD ******  07/18/07  **********************************
     **
     C***  Begin Del  ***  10/27/03  **************************************
     C********** EAPODT    IFLE XXBLDT
     C***  End   Del  ***  10/27/03  **************************************
     C                   EXSR      SRDUE1
     C                   ADD       EAAMT         XXAMTD
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           ENDIF                           EAPODT IFLE XXBLDT
     C***  End   Del  ***  10/27/03  **************************************
     **
     C                   ELSE                                                   EAARTY = 'D'
     **
     C     EAARTY        IFEQ      'P'
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C     EAARCD        ANDNE     F1RCCD
     C     EAARCD        OREQ      F1RCCD
     C     EAARTY        ANDEQ     'P'
     C     EATRDT        ANDLE     XXBLDT
     C*** END   ADD ****** 08/09/10 ************************************
     C     EAARTY        OREQ      'Q'
      *** BEG ADD ***** 02/19/04 **********************************
     C     EAARTY        OREQ      'T'
      *** END ADD ***** 02/19/04 **********************************
     C***  BEG ADD ******  07/18/07  **********************************
     C     TTCTYP        ANDEQ     'P'
     C***  END ADD ******  07/18/07  **********************************
     **
     C***  Begin Del  ***  10/27/03  **************************************
     C********** EAPODT    IFLE XXPTHR
     C***  End   Del  ***  10/27/03  **************************************
     C                   EXSR      SRDUE1
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XXPAY
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           ENDIF                           EAPODT IFLE XXPTHR
     C***  End   Del  ***  10/27/03  **************************************
     **
     C                   ENDIF                                                  EAARTY = 'P'
     **
     C                   ENDIF                                                  EAARTY = 'D'
     C*
     C***  BEGIN ADD  ***  07/02/99  **********************************
     C                   ENDIF                                                  EAARTY = 'F'
     C***  END   ADD  ***  07/02/99  **********************************
     **
     C                   ENDIF                                                  EAARTY = 'B'
     **
     **  ADD DEDUCTIBLE BILLINGS AFTER CANCEL DATE AND BEFORE RUN DATE
     **
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     EAARTY        IFEQ      'B'                                          BILLING TYPE
     C     TTCTYP        ANDEQ     'R'                                          DED BILLING
     C     EAPODT        ANDGT     XXABDT                                       POST DT < RUNDT
     C     EAPODT        ANDLE     XXBLDT                                       POST DT < RUNDT
     **
     C                   EXSR      SRDUE1
     C                   ADD       EAAMT         XXAMTD
     **
     C                   ENDIF                                                  EAARTY = 'B'
     **
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     **
     C                   ENDIF                                                  *IN60 EQ *ON
     **
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 EQ *OFF
     C*** BEG ADD *****  08/09/06  ************************************
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDPYM) IF THE POLICY YEAR
      *  IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXDPYM            1            CUR YR DEP MODE
     C                   ELSE
     C                   MOVE      'N'           XXDPYM            1
     C                   ENDIF
     C*** END ADD *****  08/09/06  ************************************
     **
     **  ACCUMULATE H & I TRANSACTIONS
      *    CHECK ALL FUND YEARS
     **
     C                   MOVE      'H'           XXARTY
      *
     C     EAKY1         SETLL     PEA02
     C     EAKY5         READE     PEA02                                  60
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     QQINDP        IFEQ      'N'
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C     *IN60         DOWEQ     *OFF
      *** BEG CHG ***** 07/18/07 ******************************************
     C********** EAARTY    ANDLE'I'
     C     EAARTY        ANDLE     'T'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'D'
      *** END CHG ***** 07/18/07 ******************************************
     **
     **   A/R CODE MUST BE ONE OF THE CODES ENTERED UNLESS
     **    CODES WERE LEFT BLANK THEN ALL CODES WILL BE USED
     **
     C     QQARIE        IFNE      *BLANK
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQARIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ENDIF                                                  QQARIE IFNE *BLANK
     **
     C     *IN60         IFEQ      *ON
     C     EAARCD        ANDNE     *BLANK
     C     QQARIE        OREQ      *BLANK
     **
      *** BEG DEL ***** 08/09/06 **********************************
     C********** EAFYR     IFLE EBFYR
      *** END DEL ***** 08/09/06 **********************************
      *** BEG ADD ***** 08/09/06 **********************************
     C     EAFYR         IFLT      EBFYR
     C     XXDPYM        ANDEQ     'N'
     C     EAFYR         OREQ      EBFYR
      *
     C                   MOVE      'N'           XXPYDM            1
     C     EAFYR         IFLT      EBFYR
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXPYDM            1            PRIOR YEAR DEP MODE
     C                   ENDIF
     C                   ENDIF                                                                 R
      *
     C     XXPYDM        IFEQ      'N'
      *** END ADD ***** 08/09/06 **********************************
     **
     **   INCLUDE 'H' TRANS THRU CANX DATE OR BILLING THROUGH DATE,
     **   WHICHEVER COMES FIRST. INCLUDE 'I' TRANS THRU PAYMENT THRU DT
     **
     C     EAARTY        IFEQ      'H'
      *** BEG ADD ***** 07/18/07 **********************************
     C     EAARTY        OREQ      'R'
      *** END ADD ***** 07/18/07 **********************************
     **
     C     EAPODT        IFLE      XXABDT
     C     *IN95         ANDEQ     *OFF                                         NOT CANCELLE
     C     EAPODT        ORLT      XXABDT
     C     *IN95         ANDEQ     *ON                                          CANCELLED
     C***  Begin Add  ***  10/27/03  **************************************
     C     EAAMT         ORLT      0
     C***  End   Add  ***  10/27/03  **************************************
     C                   EXSR      SRDUE1
     C                   ADD       EAAMT         XXAMTD
     C                   ENDIF                                                  EAPODT IFLE XXABDT
      *
     C                   ENDIF                                                  EAARTY = 'H'
     **
     C     EAARTY        IFEQ      'I'
      *** BEG ADD ***** 07/18/07 **********************************
     C     EAARTY        OREQ      'T'
      *** END ADD ***** 07/18/07 **********************************
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C********** EAPODT    IFLE XXPTHR
     C***  End   Del  ***  10/27/03  **************************************
     C                   EXSR      SRDUE1
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XXPAY
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           ENDIF                           EAPODT IFLE XXPTHR
     C***  End   Del  ***  10/27/03  **************************************
      *
     C                   ENDIF                                                  EAARTY = 'I'
     **
     C*** BEG ADD ***** 08/09/06 ******************************************
     C                   ENDIF                                                  XXPYDM = 'N'
     C*** END ADD ***** 08/09/06 ******************************************
     **
     C                   ENDIF                                                  EAFYR IFLE EBFYR
     **
     C                   ENDIF                                                  *IN60 = *ON
      *
     C*** BEG ADD ***** 07/18/07 ******************************************
     C                   ENDIF                                                  TTCTYP = 'D'
     C*** END ADD ***** 07/18/07 ******************************************
      *
     C     EAKY5         READE     PEA02                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     **
     **   CALCULATE DUE DATES AND PAST DUE DATES
     **
     C     EAKY4         SETGT     PEA04
     C                   READP     PEA04                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     EAPODT        ANDGE     XXFRDT
      ***** BEG ADD ******* 07/29/02 **********************************
     C                   SETON                                        69
      *
     C     QQARIE        IFNE      *BLANK
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 69
      *
     C     QQARIE        IFEQ      'E'
      *
     C     *IN69         IFEQ      *OFF
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF
      *
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ENDIF                                                  QQARIE IFNE *BLANK
      *
     C     *IN69         IFEQ      *ON
      ***** END ADD ******* 07/29/02 **********************************
      *
      *     FOR NON-AUDITED ACCOUNTS
      *
     C     XXAUDT        IFEQ      *ZERO
      *
      *       DUE DATE = EAPODT OF LAST EAARTY TRANS = 'B', 'A' OR 'H'
      *
     C     EAARTY        IFEQ      'B'
      *** beg add ***** 07/02/13 **************************************
     c     EAARCD        ANDNE     F1DVBL
      *** end add ***** 07/02/13 **************************************
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'H'
      *
     C                   MOVE      *ON           *IN60
     C                   MOVE      EAPODT        XXDUDT
      *
     C     ELBSDT        IFEQ      EAPODT
     C     *IN92         ANDEQ     *ON
     C                   Z-ADD     XXFIDY        XXDAYS            2 0
     C                   ELSE
     C                   Z-ADD     XXEBDY        XXDAYS
     C                   ENDIF                                                  ELBSDT = EAPODT
      *
     C                   ELSE                                                   EAARTY = 'B'
      *
     C                   READP     PEA04                                  60
      *
     C                   ENDIF                                                  EAARTY = 'B'
      *
     C                   ELSE                                                   XXAUDT=*ZERO
      *
      *     FOR AUDITED ACCOUNTS
      *
      *       DUE DATE = EAPODT OF LAST EAARTY TRANS = 'D'
      *
     C     EAARTY        IFEQ      'D'
     C     EAARTY        OREQ      'A'
      *
     C                   MOVE      *ON           *IN60
     C                   MOVE      EAPODT        XXDUDT
      *
     C     ELRAFL        IFEQ      'R'
     C                   Z-ADD     XXRADY        XXDAYS
     C                   ELSE
     C                   Z-ADD     XXABDY        XXDAYS
     C                   ENDIF                                                  ELRAFL = 'Y'
      *
     C                   ELSE                                                   EAARTY = 'D'
      *
     C                   READP     PEA04                                  60
      *
     C                   ENDIF                                                  EAARTY = 'D'
      *
     C                   ENDIF                                                  XXAUDT = *ZERO
      ***** BEG ADD ******* 07/29/02 **********************************
      *
     C                   ELSE
      *
     C                   READP     PEA04                                  60
     C                   ENDIF                                                  *IN69
      ***** END ADD ******* 07/29/02 **********************************
      *
     C                   ENDDO                                                  *IN60 = *OFF
      *
      *    IF DUE DATE WITHIN DATE RANGE NOT FOUND ABOVE THEN FIND
      *    EAPODT OF LAST EAARTY TRANS = 'F'
      *
     C     *IN60         IFEQ      *OFF
      ***** BEG ADD ******* 07/29/02 **********************************
     C     XXDUDT        OREQ      *ZERO
      ***** END ADD ******* 07/29/02 **********************************
     C                   MOVE      'F'           XXARTY
      *
     C     EAKY2         SETGT     PEA03
     C     EAKY3         READPE    PEA03                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EAPODT        XXDUDT
      *
     C     XXAUDT        IFEQ      *ZERO
      *
     C     ELBSDT        IFEQ      EAPODT
     C     *IN92         ANDEQ     *ON
     C                   Z-ADD     XXFIDY        XXDAYS            2 0
     C                   ELSE
     C                   Z-ADD     XXEBDY        XXDAYS
     C                   ENDIF                                                  ELBSDT = EAPODT
      *
     C                   ELSE
      *
     C     ELRAFL        IFEQ      'R'
     C                   Z-ADD     XXRADY        XXDAYS
     C                   ELSE
     C                   Z-ADD     XXABDY        XXDAYS
     C                   ENDIF                                                  ELRAFL = 'Y'
      *
     C                   ENDIF                                                  XXAUDT = *ZERO
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   EXSR      SRPDDT
     C                   Z-ADD     XXFUTR        XXPDDT
     C*** BEGIN ADD ***  01/10/07  *****************************************
     C*
     C* ADD ADDITIONAL DAYS TO INSTALLMENT DATE DUE.
     C*
     C     XXADDU        IFNE      *ZERO
     C*
     C                   Z-ADD     XXADDU        SRDAYS
     C                   Z-ADD     XXDUDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDAT1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDAT1
     C                   PARM                    SRDAYS
     C                   PARM      *BLANK        SRERR
     C*
     C                   Z-ADD     SRDAT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT7             7 0
     C*
     C                   Z-ADD     XXDT7         XXDUDT
     C*
     C                   ENDIF                                                  XXADDU IFNE
     C*** END ADD *****  01/10/07  *****************************************
      *
      *  CONVERT DATES FOR OUTPUT
      *
     C                   Z-ADD     XXDUDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDUDT
      *
     C                   Z-ADD     XXPDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPDDT
     **
     **   IF THE AMT DUE IS GREATER THAN OR EQUAL TO THE MINIMUM
     **   AMOUNT THEN PRINT THE NOTICE
      *
     C     XXAMTD        IFGE      XXMAMT
     **
     **  SETUP AMT DUE FIELDS FOR LATE NOTICE
     **
     C***  BEGIN CHG  ***  08/20/98  **********************************
     C********** XXDEPD    IFLE *ZERO
     C*******************  Z-ADD*ZERO     XXDEPD
     C*******************  MOVE *OFF      *IN85
     C*******************  ELSE
     C*******************  MOVE *ON       *IN85
     C*******************  ENDIF
     **
     C********** XXPRMD    IFLE *ZERO
     C*******************  Z-ADD*ZERO     XXPRMD
     C*******************  MOVE *OFF      *IN86
     C*******************  ELSE
     C*******************  MOVE *ON       *IN86
     C*******************  ENDIF
     **
     C********** XXEXCD    IFLE *ZERO
     C*******************  Z-ADD*ZERO     XXEXCD
     C*******************  MOVE *OFF      *IN87
     C*******************  ELSE
     C*******************  MOVE *ON       *IN87
     C*******************  ENDIF
     **
     C********** XXDEDD    IFLE *ZERO
     C*******************  Z-ADD*ZERO     XXDEDD
     C*******************  MOVE *OFF      *IN88
     C*******************  ELSE
     C*******************  MOVE *ON       *IN88
     C*******************  ENDIF
     **
     C********** XXFEED    IFLE *ZERO
     C*******************  Z-ADD*ZERO     XXFEED
     C*******************  MOVE *OFF      *IN89
     C*******************  ELSE
     C*******************  MOVE *ON       *IN89
     C*******************  ENDIF
     **
     C     XXDEPD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN85
     C                   ELSE
     C                   MOVE      *ON           *IN85
     C                   ENDIF
     **
     C     XXPRMD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN86
     C                   ELSE
     C                   MOVE      *ON           *IN86
     C                   ENDIF
     **
     C     XXEXCD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN87
     C                   ELSE
     C                   MOVE      *ON           *IN87
     C                   ENDIF
     **
     C     XXDEDD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN88
     C                   ELSE
     C                   MOVE      *ON           *IN88
     C                   ENDIF
     **
     C     XXFEED        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C***  END   CHG  ***  08/20/98  **********************************
     C*
     C***  BEGIN CHG  ***  08/31/98  **********************************
     C     XXTAX         IFEQ      *ZERO
     C                   MOVE      *OFF          *IN90
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C***  BEG ADD ***** 01/10/06 *************************************
     C                   CLEAR                   XXTXHD
     C     RRKY1         SETGT     PRR01
     C     RRKY          READPE    PRR01                                  60
     C     *IN60         IFEQ      *ON
     C*** BEG ADD ***** 05/17/07 **************************************
     C     XXFND         OREQ      1
     C     CLTID         ANDEQ     '129'
     C*** END ADD ***** 05/17/07 **************************************
     C                   MOVEL     'State'       XXTXHD           25
     C                   CAT       'Tax':1       XXTXHD
     C                   CAT       'Due:':1      XXTXHD
     C                   ELSE
     C*** BEG CHG ***** 05/17/07 **************************************
     C*********            SELEC
     C*********  XXFND     WHEQ 1
     C*********  RRTXHD    OREQ *BLANK
     C*********            MOVEL'State'   XXTXHD 25
     C*********            CAT  'Tax':1   XXTXHD
     C*********            CAT  'Due:':1  XXTXHD
     C*********            OTHER
     C*********            MOVELRRTXHD    XXTXHD
     C*********            CAT  'Due:':1  XXTXHD
     C*********            ENDSL
     C                   MOVEL     RRTXHD        XXTXHD
     C                   CAT       'Due:':1      XXTXHD
     C*** END CHG ***** 05/17/07 **************************************
     C                   ENDIF
     C***  END ADD ***** 01/10/06 *************************************
     C                   ENDIF
     C***BEGIN ADD ***** 05/15/07 *************************************
     C     XX1STB        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN53
     C                   ELSE
     C                   MOVE      *ON           *IN53
     C                   MOVE      *BLANKS       XX1AHD           25
     C*
     C     RR1STH        IFNE      *BLANKS
     C                   MOVEL     RR1STH        XX1AHD
     C                   CAT       'DUE':1       XX1AHD
     C                   ELSE
     C                   MOVEL     '1ST ASSE'    XX1AHD
     C                   CAT       'SSMENT':0    XX1AHD
     C                   CAT       'DUE':1       XX1AHD
     C                   ENDIF
     C                   ENDIF
     C*
     C     XX2NDB        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN54
     C                   ELSE
     C                   MOVE      *ON           *IN54
     C                   MOVE      *BLANKS       XX2AHD           25
     C*
     C     RR2NDH        IFNE      *BLANKS
     C                   MOVEL     RR2NDH        XX2AHD
     C                   CAT       'DUE':1       XX2AHD
     C                   ELSE
     C                   MOVEL     '2ND ASSE'    XX2AHD
     C                   CAT       'SSMENT':0    XX2AHD
     C                   CAT       'DUE':1       XX2AHD
     C                   ENDIF
     C                   ENDIF
     C*
     C     XX3RDB        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN55
     C                   ELSE
     C                   MOVE      *ON           *IN55
     C                   MOVE      *BLANKS       XX3AHD           25
     C*
     C     RR3RDH        IFNE      *BLANKS
     C                   MOVEL     RR3RDH        XX3AHD
     C                   CAT       'DUE':1       XX3AHD
     C                   ELSE
     C                   MOVEL     '3RD ASSE'    XX3AHD
     C                   CAT       'SSMENT':0    XX3AHD
     C                   CAT       'DUE':1       XX3AHD
     C                   ENDIF
     C                   ENDIF
     C*
     C     XX4THB        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN56
     C                   ELSE
     C                   MOVE      *ON           *IN56
     C                   MOVE      *BLANKS       XX4AHD           25
     C*
     C     RR4THH        IFNE      *BLANKS
     C                   MOVEL     RR4THH        XX4AHD
     C                   CAT       'DUE':1       XX4AHD
     C                   ELSE
     C                   MOVEL     '4TH ASSE'    XX4AHD
     C                   CAT       'SSMENT':0    XX4AHD
     C                   CAT       'DUE':1       XX4AHD
     C                   ENDIF
     C                   ENDIF
     C*
     C     XX5THB        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN57
     C                   ELSE
     C                   MOVE      *ON           *IN57
     C                   MOVE      *BLANKS       XX5AHD           25
     C*
     C     RR5THH        IFNE      *BLANKS
     C                   MOVEL     RR5THH        XX5AHD
     C                   CAT       'DUE':1       XX5AHD
     C                   ELSE
     C                   MOVEL     '5TH ASSE'    XX5AHD
     C                   CAT       'SSMENT':0    XX5AHD
     C                   CAT       'DUE':1       XX5AHD
     C                   ENDIF
     C                   ENDIF
     C***END   ADD ***** 05/15/07 *************************************
     C*
     C***  Begin Add  ***  12/11/08  **************************************
     C     XX6THB        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN58
     C                   ELSE
     C                   MOVE      *ON           *IN58
     C                   MOVE      *BLANKS       XX6AHD           25
     C*
     C     RR6THH        IFNE      *BLANKS
     C                   MOVEL     RR6THH        XX6AHD
     C                   CAT       'DUE':1       XX6AHD
     C                   ELSE
     C                   MOVEL     '6TH ASSE'    XX6AHD
     C                   CAT       'SSMENT':0    XX6AHD
     C                   CAT       'DUE':1       XX6AHD
     C                   ENDIF
     C                   ENDIF
     C*
     C     XX7THB        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN59
     C                   ELSE
     C                   MOVE      *ON           *IN59
     C                   MOVE      *BLANKS       XX7AHD           25
     C*
     C     RR7THH        IFNE      *BLANKS
     C                   MOVEL     RR7THH        XX7AHD
     C                   CAT       'DUE':1       XX7AHD
     C                   ELSE
     C                   MOVEL     '7TH ASSE'    XX7AHD
     C                   CAT       'SSMENT':0    XX7AHD
     C                   CAT       'DUE':1       XX7AHD
     C                   ENDIF
     C                   ENDIF
     C***  End   Add  ***  12/11/08  **************************************
     C***  END   CHG  ***  08/31/98  **********************************
     **
     C     XXDEPD        ADD       XXPRMD        XXTOTD
     C                   ADD       XXEXCD        XXTOTD
     C                   ADD       XXDEDD        XXTOTD
     C                   ADD       XXFEED        XXTOTD
     C***  BEGIN CHG  ***  08/31/98  **********************************
     C                   ADD       XXTAX         XXTOTD
     C***  END   CHG  ***  08/31/98  **********************************
     C*** BEGIN ADD ***  03/06/09  ************************************
     C                   ADD       XX1STB        XXTOTD
     C                   ADD       XX2NDB        XXTOTD
     C                   ADD       XX3RDB        XXTOTD
     C                   ADD       XX4THB        XXTOTD
     C                   ADD       XX5THB        XXTOTD
     C                   ADD       XX6THB        XXTOTD
     C                   ADD       XX7THB        XXTOTD
     C*** END ADD *****  03/06/09  ************************************
     **
      **** BEGIN ADD **** 08/30/99 ************************************
      *   IF SORT IS BY AGENT# AND CURRENT AGENT IS DIFFERENT FROM LAST
      *   AGENT PROCESSED START A NEW PAGE
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        ANDNE     HLDAGT
     C                   MOVE      *ON           *INOF
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  10/09/00  *********************************
     C     EBACUR        IFNE      'Y'
     C***  END   ADD  ***  10/09/00  *********************************
      *
      * PRINT THE LATE NOTICE REPORT
      *
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** end add *****  01/10/17  *****************************************
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
      *
     C                   ADD       1             XXCNT
     C                   ADD       XXTOTD        XXTOTL
      * ADD 1 FOR AGENT COUNT
     C                   ADD       1             AGTCNT
     C                   ADD       XXTOTD        XXAGTL
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** end add *****  01/10/17  *****************************************
     C                   EXCEPT    DET
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
     C*
     C***  BEGIN ADD  ***  10/09/00  *********************************
     C                   ENDIF                                                  EBACUR=Y
     C***  END   ADD  ***  10/09/00  *********************************
     C*
      **** END   ADD **** 08/30/99 ************************************
     C                   EXSR      SRLOAD
     **
     **   PRINT LATE NOTICES
     **
     **
      **** BEGIN ADD **** 04/05/99 ************************************
      *
      *****************************************************************
      *  DO NOT PRINT ACCOUNT CURRENT POLICIES                        *
      *****************************************************************
      *                                                               *
     C     EBACUR        IFNE      'Y'
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C*** begin del ***  03/24/21  ************************************
     C*****QQRPTF        ANDEQ     *BLANK
     C*** end del *****  03/24/21  ************************************
     C***** END   ADD **** 03/03/00 ***********************************
      *
      **** END   ADD **** 04/05/99 ************************************
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** begin add ***  03/24/21  ************************************
     C                             and QQRPTF = *BLANK
     C*** end add *****  03/24/21  ************************************
     C*** end add *****  01/10/17  *****************************************
     C                   EXCEPT    NOTICE
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     *LIKE         DEFINE    EBCO#         ##CO#
     C     *LIKE         DEFINE    EBFND         ##FND
     C     *LIKE         DEFINE    EBEMP#        ##EMP#
     C     *LIKE         DEFINE    EBDIV         ##DIV
     C     *LIKE         DEFINE    EBFYR         ##FYR
     C     *LIKE         DEFINE    EBAGT#        ##AGT#
     C*
     C     AGTPL         PLIST
     C                   PARM      EBCO#         ##CO#
     C                   PARM      EBFND         ##FND
     C                   PARM      EBEMP#        ##EMP#
     C                   PARM      EBDIV         ##DIV
     C                   PARM      EBFYR         ##FYR
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    ##AGT#
     C***********        PARM                    ##ANAM           60
     C                   PARM      *zeros        ##AGT#
     C                   PARM      *blanks       ##ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C     *LIKE         DEFINE    *IN60         PRTPRD
     C*
     C                   MOVE      *OFF          PRTPRD
     C*
     C     EBAGT#        IFEQ      ##AGT#
     C                   MOVEL     ##PNAM        XXPNAM           55
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** begin add ***  03/24/21  ************************************
     C                             and QQRPTF = *BLANK
     C*** end add *****  03/24/21  ************************************
     C*** end add *****  01/10/17  *****************************************
     C                   EXCEPT    PROD
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
     C                   MOVE      *ON           PRTPRD
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C*** begin add ***  01/10/17  *****************************************
     C                   TIME                    DSDTTM
     C                   Exsr      ScriptXML
     C*** end add *****  01/10/17  *****************************************
     C*
      **** BEGIN DEL **** 01/25/01 *********************************************
      **** BEGIN ADD **** 04/05/99 ************************************
     C******************** ENDIF                           EBACUR NE 'Y'
      **** END   ADD **** 04/05/99 ************************************
      **** END   DEL **** 01/25/01 *********************************************
     **
     C     QQDRMK        IFNE      *BLANK
     C*** begin add ***  01/10/17  *****************************************
     C     isUseSpoolOut Andeq     *on
     C*** begin add ***  03/24/21  ************************************
     C     QQRPTF        Andeq     *BLANK
     C*** end add *****  03/24/21  ************************************
     C*** end add *****  01/10/17  *****************************************
     C                   EXSR      DARTS
     C                   ENDIF
      **** BEGIN ADD **** 01/25/01 *********************************************
     C                   ENDIF                                                  EBACUR NE 'Y'
      **** END   ADD **** 01/25/01 *********************************************
      **** BEGIN ADD **** 08/30/99 ************************************
      * SET VALUE OF HOLD AGT# TO CURRENT AGT#
     C                   Z-ADD     EBAGT#        HLDAGT
      **** END   ADD **** 08/30/99 ************************************
     **
     C                   ENDIF                                                  XXAMTD >= XXMAMT
      *
     C                   ENDIF                                                  QQAUST IFNE 'B'
      *
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   ENDIF                                                  *IN56=OFF
     C***** END   ADD **** 03/20/00 ***********************************
     C                   ENDIF                                                  *IN80 = *OFF
      **** BEGIN ADD **** 08/30/99 ************************************
      *
      *   IF SORT IS BY AGENT# AND CURRENT AGENT IS DIFFERENT FROM LAST
      *   AGENT PROCESSED START A NEW PAGE
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        ANDNE     HLDAGT
     C                   MOVE      *ON           *INOF
     C                   ENDIF
      **** END   ADD **** 08/30/99 ************************************
      *
     C*** BEG ADD ****** 08/21/08 ************************************
     C     NEXTEB        TAG
     C*** END ADD ****** 08/21/08 ************************************
     C     EBKY          READE     PEB01                                  50
      *
     C                   ENDDO                                                  *IN50=*OFF
     **
      **** BEGIN ADD **** 08/30/99 ************************************
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        ANDEQ     HLDAGT
     C***  Begin Add  ***  10/27/03  **************************************
     C     EBAGT#        ANDNE     *ZERO
     C     AGTCNT        ANDNE     0
     C     QQRPTF        IFEQ      *BLANK
     C                   EXSR      AGTINF
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** end add *****  01/10/17  *****************************************
     C                   EXCEPT    AGTOTL
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
     C                   ENDIF
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      *ON           *INOF
     C                   CLEAR                   HLDAGT
     C*
     C                   EXSR      GETGRP
     C*
     C                   ENDDO
     C***  End   Add  ***  10/27/03  **************************************
      *
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** end add *****  01/10/17  *****************************************
     C     XXCNT         IFGT      *ZERO
     C                   EXCEPT    TOT
     C                   ENDIF
      **** END   ADD **** 08/30/99 ************************************
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       //*****Begin Chg  ***  04/07/17  ***********************************
       // Commented out the code to change the place.
       // xxprt = '0';
       // Chain orky POR01;
       // If %found;
       //    Except Route;
       // Endif;
       // xxprt = '1';
       // Chain orky POR01;
       // If %found;
       //   Except Route1;
       // Endif;
       //*****End Chg    ***  04/07/17  ***********************************
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
      *****Begin Chg  ***  04/07/17  ***********************************
     C                   EndSr
    c*** Commented out the below code. Need to change the place of code.
     c***
      /Free
       //*** begin add ***  04/04/17  **************************************
       //If xmlRptFile <> ' ';

       // L8XML = xmlRptFile;

       // Clear L8Fnd;
       // Clear L8Emp#;
       // Clear L8Div;
       // Clear L8Fyr;
       // Clear L8SrvName;
       // Clear L8GrpName;
       // Clear L8PolName;
       // Clear L8PolFdnm;
       // Clear L8PolAdr1;
       // Clear L8PolAdr2;
       // Clear L8PolCity;
       // Clear L8PolSt;
       // Clear L8PolZip;
       // Clear L8AgtHdg;
       // Clear L8Agt#;
       // Clear L8AgtName;
       // Clear L8Prd#;
       // Clear L8PrdLsn;
       // Clear L8PrdFsn;
       // Clear L8DueDate;
       // Clear L8PastDueD;
       // Clear L8Audit;
       // Clear L8TotalD;
       // Clear L8Hdg01;
       // Clear L8Hdg02;
       // Clear L8Hdg03;
       // Clear L8Hdg04;
       // Clear L8Hdg05;
       // Clear L8Hdg06;
       // Clear L8Hdg07;
       // Clear L8Hdg08;
       // Clear L8Hdg09;
       // Clear L8Hdg10;
       // Clear L8Hdg11;
       // Clear L8Hdg12;
       // Clear L8Hdg13;
       // Clear L8Hdg14;
       // Clear L8Hdg15;
       // Clear L8Hdg16;
       // Clear L8Hdg17;
       // Clear L8Hdg18;
       // Clear L8Hdg19;
       // Clear L8Hdg20;
       // Clear L8Hdg21;
       // Clear L8Hdg22;
       // Clear L8Hdg23;
       // Clear L8Hdg24;
       // Clear L8Hdg25;
       // Clear L8Hdg26;
       // Clear L8Hdg27;
       // Clear L8Hdg28;
       // Clear L8Hdg29;
       // Clear L8Hdg30;
       // Clear L8Amt01;
       // Clear L8Amt02;
       // Clear L8Amt03;
       // Clear L8Amt04;
       // Clear L8Amt05;
       // Clear L8Amt06;
       // Clear L8Amt07;
       // Clear L8Amt08;
       // Clear L8Amt09;
       // Clear L8Amt10;
       // Clear L8Amt11;
       // Clear L8Amt12;
       // Clear L8Amt13;
       // Clear L8Amt14;
       // Clear L8Amt15;
       // Clear L8Amt16;
       // Clear L8Amt17;
       // Clear L8Amt18;
       // Clear L8Amt19;
       // Clear L8Amt20;
       // Clear L8Amt21;
       // Clear L8Amt22;
       // Clear L8Amt23;
       // Clear L8Amt24;
       // Clear L8Amt25;
       // Clear L8Amt26;
       // Clear L8Amt27;
       // Clear L8Amt28;
       // Clear L8Amt29;
       // Clear L8Amt30;

       // L8TotalD = xxTotl;
       // L8EmCnt = xxCnt;

       // L8Pgm  = Q1PGM;
       // L8User = Q1USER;
       // L8Time = DSTime;
       // L8Dt   = sysDt;

       // Write pl80p;

       //Endif;
       //*** end add *****  04/04/17  **************************************

       //If isUseXML;
       //   Close WXL8P;
       //Endif;

       //Select;
       //When xmlFlowName   = *Blanks;
       //When objExists('PDSDEV':'*LIB');
       //   Monitor;
       //   S4401C(xmlFlowName + '$');
       //   On-error;
       //   Endmon;
       //Other;
       //   S4401C(xmlFlowName + '$');
       //Endsl;

      /End-free
     C*** end add *****  01/10/17  *****************************************
     C*****              MOVE      *ON           *INLR
      *****End   Chg  ***  04/07/17  *********************************
     **
     *******************************************************************
     **   SRPDDT - SUBROUTINE TO FIND THE AUDIT DATE THEN ADD THE
     **            NUMBER OF INTERIM DAYS TO CALCULATE FUTURE DATE.
     *******************************************************************
     C     SRPDDT        BEGSR
     **
     C                   Z-ADD     0             XXFUTR
     C                   Z-ADD     0             SRDAT1
      *
     C     XXDAYS        IFNE      *ZERO
     **
     C                   Z-ADD     XXDAYS        SRDAYS            5 0
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           Z-ADDXXDUDT    SRDTI
     C                   Z-ADD     SYSDT         SRDTI
     C***  End   Chg  ***  10/27/03  **************************************
     **
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDAT1            8 0
     **
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDAT1
     C                   PARM                    SRDAYS
     C                   PARM      *BLANK        SRERR
     **
     C                   Z-ADD     SRDAT1        SRDTI
     **
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR
     **
     C                   ELSE
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           Z-ADDXXDUDT    XXFUTR
     C                   Z-ADD     SYSDT         XXFUTR
     C***  End   Chg  ***  10/27/03  **************************************
     **
     C                   ENDIF
     **
     C                   ENDSR
      *                                                                *
     *******************************************************************
     **   SRLOAD - SUBROUTINE TO LOAD WORK FIELDS WITH NAMES AND ADDRESS
     *******************************************************************
     C     SRLOAD        BEGSR
     **
     **   LOAD EMPLOYER ADDRESS, CITY, STATE, & ZIP
     **
     C     EMMPFL        IFEQ      'M'
     **
     C                   MOVEL(P)  EMMAD1        XXADR1           34
     C                   MOVEL(P)  EMMAD2        XXADR2           34
     C                   MOVEL(P)  EMMCTY        XXCITY
     C                   MOVEL     EMMST         XXSTAT
     C                   MOVEL     EMMZIP        XXZIP1
     C                   MOVE      EMMZIP        XXZIP2
     C*** begin add ***  01/10/17  ********************************************
     C                   Eval      xxPolCity = EMMCty
     C                   Eval      xxPolSt   = EMMSt
     C                   Eval      xxPolZip  = EMMZip
     C*** end add *****  01/10/17  ********************************************
     **
     C                   ELSE
     **
     C                   MOVEL(P)  EMPAD1        XXADR1
     C                   MOVEL(P)  EMPAD2        XXADR2
     C                   MOVEL(P)  EMPCTY        XXCITY
     C                   MOVEL     EMPST         XXSTAT
     C                   MOVEL     EMPZIP        XXZIP1
     C                   MOVE      EMPZIP        XXZIP2
     C*** begin add ***  01/10/17  ********************************************
     C                   Eval      xxPolCity = EMPCty
     C                   Eval      xxPolSt   = EMPSt
     C                   Eval      xxPolZip  = EMPZip
     C*** end add *****  01/10/17  ********************************************
     **
     C                   ENDIF
     **
     ** LOAD THE BILLING ADDRESS IF ITS NOT BLANK
     **
      ***** BEG ADD ******* 12/20/01 **********************************
     C                   MOVEL     *BLANKS       A3OOC
      ***** END ADD ******* 12/20/01 **********************************
       //*** Begin Add *** 01/03/20 ******************************************
       A3Cont = *Blanks;
       xxCont = *Blanks;
       //*** End   Add *** 01/03/20 ******************************************
     C     A3KY          CHAIN     PA301                              60
     **
     C     *IN60         IFEQ      *ZERO
      ***** BEG ADD ******* 12/10/98 **********************************
     C     A3STAT        ANDEQ     'A'
      ***** END ADD ******* 12/10/98 **********************************
     C     A3ADR1        IFNE      *BLANK
     **
     C                   MOVEL(P)  A3ADR1        XXADR1
     C                   MOVEL(P)  A3ADR2        XXADR2
     C                   MOVEL(P)  A3CTY         XXCITY
     C                   MOVEL     A3ST          XXSTAT
     C                   MOVEL     A3ZIP1        XXZIP1
     C                   MOVEL     A3ZIP2        XXZIP2
     C*** begin add ***  01/10/17  ********************************************
     C                   Eval      xxPolCity = A3Cty
     C                   Eval      xxPolSt   = A3St
      /free
          Monitor;
             xxPolZip  = %int(A3Zip1 + A3Zip2);
          On-error;
             xxPolZip = 0;
          Endmon;
      /end-free
     C*** end add *****  01/10/17  ********************************************
     **
     C                   ENDIF                                                  *IN60  IFEQ
     **
       //*** Begin Add *** 01/03/20 ******************************************
       xxCont = A3Cont;
       //*** End   Add *** 01/03/20 ******************************************
     C                   ENDIF                                                  A3ADR1 IFNE
     **
     C                   EXSR      SRCSTZ
     C                   MOVEL(P)  XXCSTZ        XXADR3           34
      *
      ***** BEG ADD ******* 12/20/01 **********************************
      *
      *  Print 'OUT OF COUNTRY ADDRESS' if entered
      *
     C     A3OOC         IFNE      *BLANKS
     C                   MOVEL(P)  A3OOC         XXADR3
     C                   ENDIF                                                  A3OOC
      *
      ***** END ADD ******* 12/20/01 **********************************
     **
     **  REMOVE BLANK LINE FROM EMPLOYER ADDRESS IF NECESSARY
     **
      /Free
       //*** Begin Add *** 01/03/19 ******************************************
       If xxADR2 = *Blanks;
          isADR2 = *Off;
       Else;
          isADR2 = *On;
       Endif;
       //*** End   Add *** 01/03/19 ******************************************
      /End-Free

     C     XXADR2        IFEQ      *BLANK
     C                   MOVEL(P)  XXADR3        XXADR2
     C                   MOVEL     *BLANK        XXADR3
     C                   ENDIF
     **
     C                   ENDSR
      *
      *
      *
     ************************************************************
     **  SRCSTZ - SUBROUTINE TO BUILD THE CITY/ ST/ ZIP LINE    *
     ************************************************************
     C     SRCSTZ        BEGSR
     **
     C     XXCITY        CAT(P)    ',':0         XXCSTZ           40
     C                   CAT       XXSTAT:1      XXCSTZ
     C                   CAT       XXZIP1:1      XXCSTZ
     **
     C     XXZIP2        IFNE      *ZERO
     C     XXZIP2        ANDNE     *BLANK
     C                   CAT       '-':0         XXCSTZ
     C                   CAT       XXZIP2:0      XXCSTZ
     C                   ENDIF
     **
     C                   ENDSR
      *
     *********************************************************************
     **  DARTS SUBROUTINE  -  CREATE DARTS RECORD
     *********************************************************************
     C     DARTS         BEGSR
     **
     **  ESTABLISH KEY
     **
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      EBEMP#        QCEMP#
     C                   MOVE      EBDIV         QCDIV
     **  ESTABLISH RECORD
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             CLEARPCM01
     C********             MOVELQCKEY     CMKEY
     C********             MOVELQQDRMK    CMRMCD
     *********
     C********             MOVE EBFYR     XAFYR   2
     C********             MOVE EBAGT#    XAAGT#  5
     *********
     C********   2         SUBSTQQPAYD:1  XAPAYM  2
     C********   2         SUBSTQQPAYD:3  XAPAYD  2
     C********   4         SUBSTQQPAYD:5  XAPAYY  4
     *********
     C********             MOVELXXAMTD    XAAMT   7
     C********             MOVE XXAMTD    XAAMT2  2
     C********   '0'       CHECKXAAMT     Z                FIND 1ST SIGNIF
     C********   Z         IFGT *ZERO
     *********
     C********             SUBSTXAAMT:Z   XAAMT1  7 P
     *********
     C********             ELSE
     C********             MOVEL'0'       XAAMT1    P
     *********
     C********             ENDIF
     *********
     C********   CTKY      CHAINPCT01                60
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********             MOVE CTSMCD    CMSMCD
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVELCTDESC    CMCOMM
     C********             CAT  'FUND':1  CMCOMM
     C********             CAT  'YEAR:':1 CMCOMM
     C********             CAT  XAFYR:1   CMCOMM
     C********             CAT  'PAST D':1CMCOMM
     C********             CAT  'UE DAT':0CMCOMM
     C********             CAT  'E:':0    CMCOMM
      ********
     C********             CAT  XAPAYM:1  CMCOMM
     C********             CAT  '/':0     CMCOMM
     C********             CAT  XAPAYD:0  CMCOMM
     C********             CAT  '/':0     CMCOMM
     C********             CAT  XAPAYY:0  CMCOMM
      ********
     C********             CAT  'PAST D':1CMCOMM
     C********             CAT  'UE AMT':0CMCOMM
     C********             CAT  ':':0     CMCOMM
     C********             CAT  XAAMT1:1  CMCOMM
     C********             CAT  '.':0     CMCOMM
     C********             CAT  XAAMT2:0  CMCOMM
     C********             CAT  'AGENT#':1CMCOMM
     C********             CAT  ':':0     CMCOMM
     C********             CAT  XAAGT#:1  CMCOMM
      ********
     C********             MOVEL*BLANKS   CMDCID
     C********             MOVEL'W0050R'  CMPNAM
      ********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********             MOVELSRDTO     CMTRDT
     C********             MOVELSRDTO     CMDT
     C********             MOVELSRDTO     CMIDT
     C****BEGIN ADD *** 03/31/00 **************************************
     C********             MOVELQ1PGM     CMIPGM
     C****END*ADD *** 03/31/00 ****************************************
      ********
     C********             Z-ADDDSTIME    CMTRTM
     C********             MOVE *ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE *BLANKS   CMSTAT
     C********             MOVE Q1USER    CMUSER
     C********             MOVELQ1PGM     CMPGM
     C********             MOVELQ1USER    CMIUSR
     *********
     *****FIND NEXT AVAILABLE SEQUENCE #
     *********
     C********             MOVE *HIVAL    ##SEQ#  50
     C********   C2KY      SETGTPCM02
     C********   C2KY1     REDPEPCM02                    60
     *********
     C********   *IN60     IFEQ *OFF
     C********   C2SEQ#    ADD  1         CMSEQ#
     *********
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     *********
     C********             ENDIF                           *IN60=*OFF
     *********
     ****WRITE THE DARTS COMMENT
     *********
     C********             WRITEPCM01
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C*
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C*
     C     QQDART        IFEQ      'Y'
     C*
     C                   OPEN      QSYSPRT2
     C                   EXCEPT    NOT2
     C     PRTPRD        IFEQ      *ON
     C                   EXCEPT    PROD2
     C                   ENDIF
     C                   CLOSE     QSYSPRT2
     C*
     C                   ADD       1             XXSPOL            4 0
     C                   MOVE      XXSPOL        QQSPL#            4
     C*
     C                   MOVE      XXFND         QPFND
     C                   MOVE      XXEMP#        QPEMP#
     C                   MOVE      XXFYR         QPFYR
     C                   MOVE      XXDIV         QPDIV
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       Q#FOLD           80
     C                   PARM      *BLANKS       Q#DCID
     C                   PARM      'LN'          QQPRE             2
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD *** 07/08/04 **************************************
     C                   PARM      '00000'       QQSUB#            5
     C                   PARM      *BLANKS       QQPFLR           80
     C*
     C     QQPFLR        IFNE      *BLANKS
     C*
     C     QQPFLR        CAT       '/':0         QQPFLR
     C*
     C     F2FOLD        IFEQ      *BLANKS
     C     QQPFLR        CAT(P)    'WCPOLS':0    QQSFLD
     C                   ELSE
     C     QQPFLR        CAT(P)    F2FOLD:0      QQSFLD
     C                   ENDIF                                                  F2FOLD IFEQ
     C*
     C                   ENDIF                                                  QQPFLR IFNE
     C*** END   ADD *** 07/08/04 **************************************
     C*
     C                   CALL      OUTPUT
     C                   PARM                    Q#DCID           12
     C                   PARM                    QQPOL#
     C                   PARM                    QPDIV             5
     C*** BEGIN CHG *** 07/08/04 **************************************
     C*********************PARM F2FOLD    QQSFLD 40
     C                   PARM                    QQSFLD           40
     C*** END CHG ***** 07/08/04 **************************************
     C                   PARM                    Q#FOLD
     C*
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVE      EBFYR         XAFYR             2
     C                   MOVE      EBAGT#        XAAGT#            5
     C     2             SUBST     QQPAYD:1      XAPAYM            2
     C     2             SUBST     QQPAYD:3      XAPAYD            2
     C     4             SUBST     QQPAYD:5      XAPAYY            4
     C                   MOVEL     XXAMTD        XAAMT             7
     C                   MOVE      XXAMTD        XAAMT2            2
     C     '0'           CHECK     XAAMT         Z                              FIND 1ST SIGNIF
     C     Z             IFGT      0
     C                   SUBST(P)  XAAMT:Z       XAAMT1            7
     C                   ELSE                                                   Z = 0
     C                   MOVEL(P)  '0'           XAAMT1
     C                   ENDIF                                                  Z = 0
     C     CTKY          CHAIN     PCT01                              60
     C                   MOVEL(P)  CTDESC        Q#COMM          256
     C***  Begin Del  ***  02/11/08  **************************************
     C**********           CAT  'FUND':1  Q#COMM
     C***  End   Del  ***  02/11/08  **************************************
     C                   CAT       'YEAR:':1     Q#COMM
     C                   CAT       XAFYR:1       Q#COMM
     C                   CAT       'PAST D':1    Q#COMM
     C                   CAT       'UE DAT':0    Q#COMM
     C                   CAT       'E:':0        Q#COMM
      *
     C                   CAT       XAPAYM:1      Q#COMM
     C                   CAT       '/':0         Q#COMM
     C                   CAT       XAPAYD:0      Q#COMM
     C                   CAT       '/':0         Q#COMM
     C                   CAT       XAPAYY:0      Q#COMM
      *
     C                   CAT       'PAST D':1    Q#COMM
     C                   CAT       'UE AMT':0    Q#COMM
     C                   CAT       ':':0         Q#COMM
     C                   CAT       XAAMT1:1      Q#COMM
     C                   CAT       '.':0         Q#COMM
     C                   CAT       XAAMT2:0      Q#COMM
     C                   CAT       'AGENT#':1    Q#COMM
     C                   CAT       ':':0         Q#COMM
     C                   CAT       XAAGT#:1      Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           MOVEL*BLANKS   Q#DCID 12 P
     C***  End   Del  ***  10/27/03  **************************************
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           MOVEL*BLANKS   Q#FOLD 80 P
     C***  End   Del  ***  10/27/03  **************************************
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  QQDRMK        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
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
     C*** END ADD *** 04/26/00 ****************************************
     **
     C                   ENDSR
     *******************************************************************
     **  SRDUE1 - CALCULATE AMTS DUE FOR LATE NOTICE
     *******************************************************************
     C     SRDUE1        BEGSR
     **
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   SELECT
     C     EAARTY        WHENEQ    'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'R'
      *** BEG ADD ***** 07/18/07 **********************************************
     C     TTCTYP        ANDNE     'D'
      *** END ADD ***** 07/18/07 **********************************************
     **
     C     TTCTYP        IFEQ      'E'
     C                   ADD       EAAMT         XXEXCD
     C                   ELSE
     C                   ADD       EAAMT         XXPRMD
     C                   ENDIF                                                  TTCTYP IFEQ B
     **
     C     EAARTY        WHENEQ    'P'
      *** BEG ADD ***** 02/19/04 **********************************
     C     EAARTY        OREQ      'T'
      *** END ADD ***** 02/19/04 **********************************
      *** BEG ADD ***** 07/18/07 **********************************************
     C     TTCTYP        ANDNE     'D'
      *** END ADD ***** 07/18/07 **********************************************
     **
     C     TTCTYP        IFEQ      'E'
     C                   SUB       EAAMT         XXEXCD
     C                   ELSE
     C                   SUB       EAAMT         XXPRMD
     C                   ENDIF                                                  TTCTYP IFEQ P
     **
     C     EAARTY        WHENEQ    'F'
     **
     C     TTCTYP        IFEQ      'R'
     C                   ADD       EAAMT         XXDEDD
     C*
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   ELSE
     C     TTCTYP        IFEQ      'T'
     C*** BEGIN ADD ***  04/12/99  ************************************
     C     TTCTYP        OREQ      'D'
     C*** END ADD *****  04/12/99  ************************************
     C                   ADD       EAAMT         XXTAX
     C*** END   ADD *** 08/31/98 **************************************
     **
     C                   ELSE
     C*****BEG DEL********05/15/07*************************************
     C*******              ADD  EAAMT     XXFEED
     C*****END DEL********05/15/07*************************************
     C*****BEGIN ADD******05/15/07*************************************
     C                   SELECT
     C     TTCTYP        WHENEQ    '1'
     C     RR1STB        ANDEQ     'Y'
     C                   ADD       EAAMT         XX1STB
     C     TTCTYP        WHENEQ    '2'
     C     RR2NDB        ANDEQ     'Y'
     C                   ADD       EAAMT         XX2NDB
     C     TTCTYP        WHENEQ    '3'
     C     RR3RDB        ANDEQ     'Y'
     C                   ADD       EAAMT         XX3RDB
     C     TTCTYP        WHENEQ    '4'
     C     RR4THB        ANDEQ     'Y'
     C                   ADD       EAAMT         XX4THB
     C     TTCTYP        WHENEQ    '5'
     C     RR5THB        ANDEQ     'Y'
     C                   ADD       EAAMT         XX5THB
     C***  Begin Add  ***  12/11/08  **************************************
     C     TTCTYP        WHENEQ    '6'
     C     RR6THB        ANDEQ     'Y'
     C                   ADD       EAAMT         XX6THB
     C     TTCTYP        WHENEQ    '7'
     C     RR7THB        ANDEQ     'Y'
     C                   ADD       EAAMT         XX7THB
     C***  End   Add  ***  12/11/08  **************************************
     C*
     C                   OTHER
     C                   ADD       EAAMT         XXFEED
     C                   ENDSL
     C******END  ADD******05/15/07*************************************
     **
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   ENDIF                                                  TTCTYP='T'
     C*** END   ADD *** 08/31/98 **************************************
     **
     C                   ENDIF                                                  TTCTYP IFEQ L
     **
     C     EAARTY        WHENEQ    'Q'
     **
     C     TTCTYP        IFEQ      'R'
     C                   SUB       EAAMT         XXDEDD
     C*
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   ELSE
     C     TTCTYP        IFEQ      'T'
     C*** BEGIN ADD ***  04/12/99  ************************************
     C     TTCTYP        OREQ      'D'
     C*** END ADD *****  04/12/99  ************************************
     C                   SUB       EAAMT         XXTAX
     C*** END   ADD *** 08/31/98 **************************************
     **
     C                   ELSE
     C*** BEGIN DEL *** 05/15/07 **************************************
     C*******              SUB  EAAMT     XXFEED
     C***  END  DEL *** 05/15/07 **************************************
     **
     C*****BEGIN ADD******05/15/07*************************************
     C                   SELECT
     C     TTCTYP        WHENEQ    '1'
     C     RR1STB        ANDEQ     'Y'
     C                   SUB       EAAMT         XX1STB
     C     TTCTYP        WHENEQ    '2'
     C     RR2NDB        ANDEQ     'Y'
     C                   SUB       EAAMT         XX2NDB
     C     TTCTYP        WHENEQ    '3'
     C     RR3RDB        ANDEQ     'Y'
     C                   SUB       EAAMT         XX3RDB
     C     TTCTYP        WHENEQ    '4'
     C     RR4THB        ANDEQ     'Y'
     C                   SUB       EAAMT         XX4THB
     C     TTCTYP        WHENEQ    '5'
     C     RR5THB        ANDEQ     'Y'
     C                   SUB       EAAMT         XX5THB
     C***  Begin Add  ***  12/11/08  **************************************
     C     TTCTYP        WHENEQ    '6'
     C     RR6THB        ANDEQ     'Y'
     C                   SUB       EAAMT         XX6THB
     C     TTCTYP        WHENEQ    '7'
     C     RR7THB        ANDEQ     'Y'
     C                   SUB       EAAMT         XX7THB
     C***  End   Add  ***  12/11/08  **************************************
     C*
     C                   OTHER
     C                   SUB       EAAMT         XXFEED
     C                   ENDSL
     C******END  ADD******05/15/07*************************************
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   ENDIF                                                  TTCTYP='T'
     C*** END   ADD *** 08/31/98 **************************************
     **
     C                   ENDIF                                                  TTCTYP IFEQ Y
     **
     C     EAARTY        WHENEQ    'H'
      *** BEG ADD ***** 07/18/07 ******************************************
     C     EAARTY        OREQ      'R'
     C     TTCTYP        ANDEQ     'D'
      *** END ADD ***** 07/18/07 ******************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C     QQINDP        ANDEQ     'Y'
     C***  End   Add  ***  10/27/03  **************************************
     C                   ADD       EAAMT         XXDEPD
     **
     C     EAARTY        WHENEQ    'I'
      *** BEG ADD ***** 07/18/07 ******************************************
     C     EAARTY        OREQ      'T'
     C     TTCTYP        ANDEQ     'D'
      *** END ADD ***** 07/18/07 ******************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C     QQINDP        ANDEQ     'Y'
     C***  End   Add  ***  10/27/03  **************************************
     C                   SUB       EAAMT         XXDEPD
     **
     C                   ENDSL
     **
     C                   ENDIF                                                  *IN60 IFEQ  B
     **
     C                   ENDSR
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C*====================================================================
     C*  Get next group and group information
     C*====================================================================
     C     GETGRP        BEGSR
     C*
     C     *LIKE         DEFINE    *IN60         NOGRP
     C*
     C*  CHAIN TO THE FUND MASTER FILE
     C*
     C     FNTEST        DOUNE     'Y'
     C     FNFND         ORGT      XXTFND
     C     *IN60         OREQ      *ON
     C     FNKY1         READE     PFN01                                  60
     C     XXFFND        IFEQ      XXTFND
     C                   CLEAR                   FNTEST
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVE      FNFND         XXFND
     C*
     C     FNFND         IFGT      XXTFND
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   MOVE      *IN60         NOGRP
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     FNCGFL        COMP      'Y'                                    78
     C*
     C*  USE ALTERNATE FUND NAME IF FOUND IN FUND MASTER
     C*
     C                   MOVEL     FNNAME        XXFNAM
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL(P)  FNANAM        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL(P)  SRDFLD        XXCONM           40
     C                   ELSE
     C                   MOVEL(P)  SVCONM        XXCONM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   GET THE FUND AUXILIARY FILE
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C*   LOAD FUND ADDRESS, CITY, STATE, & ZIP
     C*
     C                   MOVEL(P)  FNMAD1        XFMAD1
     C                   MOVEL(P)  FNMAD2        XFMAD2
     C                   MOVEL(P)  FNMCTY        XXCITY           25
     C                   MOVEL     FNMST         XXSTAT            2
     C                   MOVEL     FNMZIP        XXZIP1            5
     C                   MOVE      FNMZIP        XXZIP2            4
     C*
     C                   EXSR      SRCSTZ
     C                   MOVEL     XXCSTZ        XFMAD3
     C*
     C     FNMAD2        IFEQ      *BLANK
     C                   MOVEL     XFMAD3        XFMAD2
     C                   MOVEL     *BLANK        XFMAD3
     C                   ENDIF
     C*
     C*** BEG ADD *** 09/24/04 *****************************
     C*
     C     EXKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  '   AGENT'    EXAHDR
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  '   AGENT'    EXAHDR
     C                   ENDIF
      *
      ***** BEG CHG ***** 08/26/05 ****************************************
      ***** END CHG ***** 08/26/05 ****************************************
      *
      ***** BEG CHG ***** 08/26/05 ****************************************
     C***************      MOVELEXAHDR    XXAHDR  8 P
     C                   MOVEL(P)  EXAHDR        XXAHDR            9
      ***** END CHG ***** 08/26/05 ****************************************
     C*** END ADD *** 09/24/04 *****************************
     C                   ENDSR
     C*====================================================================
     C*  Get Agent information
     C*====================================================================
     C     AGTINF        BEGSR
     C*
     C     AAKY          KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    HLDAGT
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C                   MOVEL     AGTNAM        AGNAME
     C                   CLEAR                   AGMAD1
     C                   CLEAR                   AGMCTY
     C                   CLEAR                   AGMST
     C                   CLEAR                   AGMZIP
     C                   CLEAR                   AGPAD1
     C                   CLEAR                   AGPCTY
     C                   CLEAR                   AGPST
     C                   CLEAR                   AGPZIP
     C*
     C     AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG00                              60
     C*
     C     AGMAD1        IFEQ      *BLANK
     C                   MOVEL     AGPAD1        AGMAD1
     C                   MOVEL     AGPCTY        AGMCTY
     C                   MOVEL     AGPST         AGMST
     C                   MOVEL     AGPZIP        AGMZIP
     C                   ENDIF
     C*
     C                   MOVEL(P)  AGMCTY        XXCITY
     C                   MOVEL(P)  AGMST         XXSTAT
     C                   MOVEL(P)  AGMZIP        XXZIP1
     C                   MOVE(P)   AGMZIP        XXZIP2
     C                   EXSR      SRCSTZ
     C                   MOVEL(P)  XXCSTZ        XPMCSZ           60
     C*
     C*** begin add ***  01/10/17  *****************************************
     C                   If        isUseSpoolOut
     C*** end add *****  01/10/17  *****************************************
     C                   EXCEPT    AGINFO
     C*** begin add ***  01/10/17  *****************************************
     C                   Endif
     C*** end add *****  01/10/17  *****************************************
     C*
     C                   ENDSR
     C***  End   Add  ***  10/27/03  **************************************
      *** BEG ADD ***** 09/20/06 **************************************
      *================================================================
      *   SRP71# - CONCAT GRP-EMP-DIV WITHOUT LEADING ZEROS
      *================================================================
     C     SRP71#        BEGSR
      *
     C                   CLEAR                   XXPLCY
     C                   CLEAR                   X1PLCY
     C                   MOVE(P)   XXFND         ZXFND             3
     C                   MOVE(P)   XXEMP#        ZXEMP             5
     C                   MOVE(P)   XXFYR         ZXFYR             3
     C     '0'           CHECK     ZXFND         X
     C                   SUBST     ZXFND:X       XXTMP1            3
     C     XXTMP1        CAT       '-':0         X1PLCY
     C     X1PLCY        CAT       ZXEMP:0       X1PLCY
     C     X1PLCY        CAT       '-':0         X1PLCY
     C     X1PLCY        CAT       ZXFYR:0       X1PLCY
     C                   MOVEL     X1PLCY        XXPLCY
      *
     C                   ENDSR
     C*** END ADD *****  09/20/06  ************************************
     C*** begin add ***  01/10/17  *****************************************
      /free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       If not isUseXML;
          Leavesr;
       Endif;

       //* xmlFile - contains the name of the template XML file, used for work file linking.
       //* xmlFlowName - contains the name of the flow XML file, used for S4401C call.
       //* xmlLink - used to link individual records together in a mass run - XML flow file name.

       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';

       //* Request Master record for mass run.
       If isMassReq;

          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
           'EstLateNot':'000':'000':massFlag:xmlLink);

          isMassReq = *off;
          //* Flow XML file name is used for the S4401C call.
          xmlFlowName = xmlFile;
          xmlLink = xmlFile;

          Chain (xmlFile) xp100;
          XPCat = 'PS';
          Update xp100;

          //* Request record for Report.
          xmlFile = ' ';
          message = %trim(processType);

          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
           'EstLateRpt':'000':'000':massFlag:xmlLink);

          xmlRptFile = xmlFile;

          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          XPType = %trim(processType) + ' Report';
          XPDcid = 'Estimated Late Notice Report.scd';

          //*** begin add ***  05/18/21  **************************************
             //* Determine report template information.
             Setgt (XPType:sysDt) psp010;
             Readpe(n) (XPType) psp010;
             If not %eof;
                If S1SCltT <> ' ';
                   XPDcid = %trim(S1SCltT);
                   XPPath = %trim(S1PathC);
                Else;
                   If S1SDefT <> ' ';
                      XPDcid = %trim(S1SDefT);
                      XPPath = %trim(S1PathD);
                   Endif;
                Endif;
             Endif;
          //*** end add *****  05/18/21  **************************************

          XPDart = ' ';
          XPRmcd = ' ';
          XPData = ' ';
          //*** beg add ***  10/06/23  **************************************
          XPMail = *blanks;
          XPMailAdd = *blanks;
          //*** end add ***  10/06/23  **************************************
          Update xp100;

       Endif;

       //*** begin add ***  03/24/21  *************************************
       If QQRPTF = *blank;
       //*** end add *****  03/24/21  *************************************

       //* Request record for individual Estimated Late Notice.
       StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x@RRN:'X'):
        xxPre:string:%editc(EBCo#:'X'):%editc(EBFnd:'X'):massFlag:xmlLink);

       //* Flow XML file name is used for the S4401C call.
       If xmlFlowName = ' ';
          xmlFlowName = xmlFile;
       Endif;

       //* The template XML file name must be pulled from record for work files for a
       //* single request.  Otherwise it has been returned from the XML Request call.
       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       XPData = 'Y';

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DSTime;
       XPDt   = sysDt;

       Update xp100;

       //*** begin add ***  03/24/21  *************************************
       Else;
       //* Build Report request and workfile name if report only mode.
          If xmlFlowName = *blanks;

             //* Request record for Report.
             xmlFile = ' ';
             message = %trim(processType);

             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'EstLateRpt':'000':'000':massFlag:xmlLink);

             xmlFlowName = xmlFile;

             Chain (xmlFile) xp100;
             xmlRptFile = XPXmlT;
             XPType = %trim(processType) + ' Report';
             XPDcid = 'Estimated Late Notice Report.scd';

          //*** begin add ***  05/18/21  **************************************
             //* Determine report template information.
             Setgt (XPType:sysDt) psp010;
             Readpe(n) (XPType) psp010;
             If not %eof;
                If S1SCltT <> ' ';
                   XPDcid = %trim(S1SCltT);
                   XPPath = %trim(S1PathC);
                Else;
                   If S1SDefT <> ' ';
                      XPDcid = %trim(S1SDefT);
                      XPPath = %trim(S1PathD);
                   Endif;
                Endif;
             Endif;
          //*** end add *****  05/18/21  **************************************

             XPDart = ' ';
             XPRmcd = ' ';
             XPData = ' ';
             Update xp100;

             xmlFile = ' ';

          Endif;

       Endif;
       //*** end add *****  03/24/21  *************************************

       //* Write data for late notice work file.
       Clear pl80p;

       L8XML  = xmlFile;

       L8Co#  = EBCo#;
       L8Fnd  = EBFnd;
       L8Emp# = EBEmp#;
       L8Div  = EBDiv;
       L8Fyr  = EBFyr;

       L8CoName  = %trim(COName);
       L8UsrName = %trim(UPusnm);
       If *in08 = *on;
          L8Dis1 = %trim(ORDis1);
          L8Dis2 = %trim(ORDis2);
          L8Dis3 = %trim(ORDis3);
          L8Dis4 = %trim(ORDis4);
       Endif;

       L8Titl = %trim(xxTitl);
       L8Frdt = RSFrdt;
       L8Todt = RSTodt;
       L8MinAmt = RSMAmt;
       L8ExcAud = RSCtst;
       L8RptOnly = RSRptf;
       L8Sort1 = RSSrt1;
       L8Sort2 = RSSrt2;
       If QQEmp# <> ' ';
          L8RptEmp = %int(QQEmp#);
       Endif;
       L8RptDiv = QQDiv;
       L8Agt#Frm = RSFRAN;
       L8Agt#To  = RSTOAN;
       L8AgtNFrm = RSFRAA;
       L8AgtNTo  = RSTOAA;
       L8Emp#Frm = RSFREN;
       L8Emp#To  = RSTOEN;
       L8EmpNFrm = RSFREA;
       L8EmpNTo  = RSTOEA;
       L8PolFrdt = RSFREF;
       L8PolTodt = RSTOEF;
       //*** Beg Chg *** 02/15/23 ******************************************
       //***L8RptState = RXJRST;
       L8RptState = qqJrSt;
       //*** End Chg *** 02/15/23 ******************************************
       L8Int1stIn = xxFIDY;
       L8IntEstBl = xxEBDY;
       L8IntAudBl = xxABDY;
       L8IntRvAud = xxRADY;
       L8RptActSt = RSACT;
       L8RptAudSt = RSAUST;
       L8RptActTy = RSESSR;
       L8IncDep   = RSINDP;
       L8AddDays  = xxADDU;

       L8ARFlag = QQARIE;
       Clear arrARCD;
       Clear arrARDS;
       count = 1;
       Dow count <= 30;
          arrARCD(count) = CDE(count);
          arrARDS(count) = DES(count);
          count = count + 1;
       Enddo;

       L8ASFlag = QQIEFL;
       Clear arrASCD;
       Clear arrASDS;
       count = 1;
       Dow count <= 5;
          arrASCD(count) = AS(count);
          arrASDS(count) = ASDS(count);
          count = count + 1;
       Enddo;

       L8ComC01 = QQCMT1;
       L8ComD01 = xxCM01;
       L8ComC02 = QQCMT2;
       L8ComD02 = xxCM02;
       L8ComC03 = QQCMT3;
       L8ComD03 = xxCM03;
       L8ComC04 = QQCMT4;
       L8ComD04 = xxCM04;
       L8ComC05 = QQCMT5;
       L8ComD05 = xxCM05;

       L8SrvName = xxACNM;
       L8GrpName = FNName;
       L8PolName = %trim(EMName) + ' ' + %trim(EMNam2);
       L8PolFdnm = %trim(EMFdnm) + ' ' + %trim(EMFdn2);
       //*** Begin Add *** 01/03/20 ******************************************
       L8Cont    = xxCont;
       //*** End   Add *** 01/03/20 ******************************************
       L8PolAdr1 = xxAdr1;
       //*** Begin Change *** 01/03/19 **************************************
       //***L8PolAdr2 = xxAdr2;
       If isADR2 = *On;
          L8PolAdr2 = xxAdr2;
       Else;
          L8PolAdr2 = *Blanks;
       Endif;
       //*** End   Change *** 01/03/19 **************************************
       L8PolCity = xxPolCity;
       L8PolSt   = xxPolSt;
       L8PolZip  = xxPolZip;

       L8AgtHdg = xxAhdr;
       L8Agt# = EBAgt#;
       L8AgtName = EBANam;
       L8Prd# = ##Prod;
       //*** begin add ***  05/26/21  ***********************************
       L8AgtFein = AGFein;
       L8AgtMod  = AGMod;
       L8AgtAdr1 = AGMad1;
       L8AgtAdr2 = ' ';
       L8AgtCity = AGMcty;
       L8AgtSt   = AGMst;
       L8AgtZip  = AGMzip;
       //*** end add *****  05/26/21  ***********************************

       If %scan(',':xxPNAM) <> 0;
          L8PrdLsn = %subst(xxPNAM:1:%scan(',':xxPNAM)-1);
          //*** Begin Chg *** 05/23/18 **********************************
          //*** L8PrdFsn = %subst(xxPNAM:1:%scan(',':xxPNAM)+1);
          L8PrdFsn = %subst(xxPNAM:%scan(',':xxPNAM)+2:15);
          //*** End   Chg *** 05/23/18 **********************************
       Else;
          L8PrdLsn = xxPNAM;
       Endif;

       L8DueDate = xxDUDT;
       L8PastDueD = xxPDDT;
       If *in71 = *on;
          L8Audit = 'Y';
       Endif;

       If *in75 = *on and *in85 = *on;
          L8Hdg01 = 'Deposit Due';
          L8Amt01 = xxDEPD;
       Endif;

       If *in86 = *on;
          L8Hdg02 = 'Premium Due';
          L8Amt02 = xxPRMD;
       Endif;

       If *in87 = *on;
          L8Hdg03 = 'Expense Constant Due';
          L8Amt03 = xxEXCD;
       Endif;

       If *in88 = *on;
          L8Hdg04 = 'Deductible Fees Due';
          L8Amt04 = xxDEDD;
       Endif;

       If *in90 = *on;
          L8Hdg05 = 'State Tax Due';
          L8Amt05 = xxTAX;
       Endif;

       If *in53 = *on;
          L8Hdg06 = xx1AHD;
          L8Amt06 = xx1STB;
       Endif;

       If *in54 = *on;
          L8Hdg07 = xx2AHD;
          L8Amt07 = xx2NDB;
       Endif;

       If *in55 = *on;
          L8Hdg08 = xx3AHD;
          L8Amt08 = xx3RDB;
       Endif;

       If *in56 = *on;
          L8Hdg09 = xx4AHD;
          L8Amt09 = xx4THB;
       Endif;

       If *in57 = *on;
          L8Hdg10 = xx5AHD;
          L8Amt10 = xx5THB;
       Endif;

       If *in58 = *on;
          L8Hdg11 = xx6AHD;
          L8Amt11 = xx6THB;
       Endif;

       If *in59 = *on;
          L8Hdg12 = xx7AHD;
          L8Amt12 = xx7THB;
       Endif;

       If *in89 = *on;
          L8Hdg13 = 'Fees Due';
          L8Amt13 = xxFEED;
       Endif;

       L8TotalD = xxTOTD;


       L8Groups = %trim(xxGroups);

       L8Pgm  = Q1PGM;
       L8User = Q1USER;
       L8Time = DSTime;
       L8Dt   = sysDt;

       //*** begin chg ***  03/24/21  *************************************
       //*****Write pl80p;
       If xmlFile <> ' ';
          Write pl80p;
       Endif;
       //*** end chg *****  03/24/21  *************************************

       //* Write data for report if needed.
       If xmlRptFile <> ' ';
          L8XML = xmlRptFile;
          Write pl80p;
       Endif;

       Endsr;

      /end-free
     C*** end add *****  01/10/17  *****************************************
     C*____________________________________________________________________
      *
     O******************************************************************
     O*   LATE NOTICES
     O*****************************************************************
     OQSYSPRT   E            ROUTE          1 03
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
      **** BEGIN CHG **** 08/30/99 ************************************
     O******* E  2             ROUTE
     O**********E            ROUTE          1
      **** END   CHG **** 08/30/99 ************************************
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
      **** BEGIN CHG **** 08/30/99 ************************************
     O******* E  2             ROUTE
     O          E            ROUTE          1
      **** END   CHG **** 08/30/99 ************************************
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          1
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**   SELECTION CRITERIA
      **** BEGIN CHG **** 08/30/99 ************************************
     O******* E  1             ROUTE
     O*******                            20 'COMPANY      :'
     O*******                  RSCO# Z   24
     O*******                            60 'GROUP        :'
     O*******                  RSFND Z   64
     O******* E  1             ROUTE
     O*******                            20 'DATE RANGE   :'
     O*******                  RSFRDT    31 '  /  /    '
     O*******                          +001 '-'
     O*******                  RSTODT  +001 '  /  /    '
     O******* E  1             ROUTE
     O*******                            20 'BILLINGS      '
     O*******                            60 'PAYMENTS      '
     O******* E  1             ROUTE
     O*******                            20 'THROUGH DATE :'
     O*******                  RSBLDT    31 '  /  /    '
     O*******                            60 'THROUGH DATE :'
     O*******                  RSPAYD    71 '  /  /    '
     O******* E  1             ROUTE
     O*******                            20 'MINIMIUM AMT :'
     O*******                  RSMAMTJ   33
     O******* E  1             ROUTE
     O*******                            20 'SRT1 ORDER   :'
     O*******                  RSSRT1    34
     O*******                            60 'SRT2 ORDER   :'
     O*******                  RSSRT2    74
     O******* E  1             ROUTE
     O*******                            20 'AGT # RANGE  :'
     O*******                  RSFRANZ   30
     O*******                            32 '-'
     O*******                  RSTOANZ   38
     O*******                            60 'AGTNAME RANGE:'
     O*******                  RSFRAA    63
     O*******                            65 '-'
     O*******                  RSTOAA    68
     O******* E  1             ROUTE
     O*******                            20 'EMP # RANGE  :'
     O*******                  RSFRENZ   30
     O*******                            32 '-'
     O*******                  RSTOENZ   42
     O*******                            60 'EMPNAME RANGE:'
     O*******                  RSFREA    63
     O*******                            65 '-'
     O*******                  RSTOEA    68
     O******* E  1             ROUTE
     O*******                            24 'INTERIM # DAYS FOR'
     O******* E  1             ROUTE
     O*******                            20 '1ST INSTLMNT:'
     O*******                  XXFIDY    23
     O*******                            38 'EST. BILLING:'
     O*******                  XXEBDY    41
     O******* E  1             ROUTE
     O*******                            20 'AUDIT BILLNG:'
     O*******                  XXABDY    23
     O*******                            38 'RVSD AUD BLG:'
     O*******                  XXRADY    41
     O******* E  1             ROUTE
     O*******                            20 'ACCT STATUS  :'
     O*******                  RSACT     46
     O******* E  1             ROUTE
     O*******                            20 'AUDIT STATUS :'
     O*******                  RSAUST    47
     O******* E  2             ROUTE
     O*******                            20 'ACCOUNT TYPE :'
     O*******                  RSESSR    54
     O******* E  1             ROUTE
     O*******          51                30 'A/R CODES INCLUDED IN'
     O*******          51                42 'CALCULATION'
     O*******         N51                28 'A/R CODES EXCLUDED '
     O*******         N51                44 'FROM CALCULATION'
      ***** BEG DEL ******* 03/28/01 **********************************
      *******
     O******* E  1             ROUTE
     O*******                            20 'Company      :'
     O*******                  RSCO# Z   24
     O******* E  1             ROUTE
     O*******                            20 'Group        :'
     O*******                  RSFND Z   24
     O******* E  1             ROUTE
     O*******                            20 'Date Range   :'
     O*******                  RSFRDT    31 '  /  /    '
     O*******                          +001 '-'
     O*******                  RSTODT  +001 '  /  /    '
     O******* E  1             ROUTE
     O*******                            20 'Billings      '
     O******* E  1             ROUTE
     O*******                            20 'Through Date :'
     O*******                  RSBLDT    31 '  /  /    '
     O******* E  1             ROUTE
     O*******                            20 'Payments      '
     O******* E  1             ROUTE
     O*******                            20 'Through Date :'
     O*******                  RSPAYD    31 '  /  /    '
     O******* E  1             ROUTE
     O*******                            20 'Minimum Amt  :'
     O*******                  RSMAMTJ +001
     O******* E  1             ROUTE
     O*******                            20 'SRT1 Order   :'
     O*******                  RSSRT1    34
     O******* E  1             ROUTE
     O*******                            20 'SRT2 Order   :'
     O*******                  RSSRT2    34
     O******* E  1             ROUTE
     O*******                            20 'AGT # Range  :'
     O*******                  RSFRANZ   30
     O*******                            32 '-'
     O*******                  RSTOANZ   38
     O******* E  1             ROUTE
     O*******                            20 'EMP # Range  :'
     O*******                  RSFRENZ   30
     O*******                            32 '-'
     O*******                  RSTOENZ   42
     O******* E  1             ROUTE
     O*******                            20 'AgtName Range:'
     O*******                  RSFRAA    23
     O*******                            25 '-'
     O*******                  RSTOAA    28
     O******* E  1             ROUTE
     O*******                            20 'EmpName Range:'
     O*******                  RSFREA    23
     O*******                            25 '-'
     O*******                  RSTOEA    28
     O******* E  1             ROUTE
     O*******                            24 'Interim # Days For'
     O******* E  1             ROUTE
     O*******                            20 '1ST Instlmnt:'
     O*******                  XXFIDY    23
     O*******                            38 'Est. Billing:'
     O*******                  XXEBDY    41
     O******* E  1             ROUTE
     O*******                            20 'Audit Billng:'
     O*******                  XXABDY    23
     O*******                            38 'Rvsd Aud Blg:'
     O*******                  XXRADY    41
     O******* E  1             ROUTE
     O*******                            20 'Acct Status  :'
     O*******                  RSACT     46
     O******* E  1             ROUTE
     O*******                            20 'Audit Status :'
     O*******                  RSAUST    47
     O******BEGIN ADD **** 03/20/00 ***********************************
     O******* E  1             ROUTE
     O*******                            20 'Excl Cont Aud:'
     O*******                  RSCTST    22
     O******END   ADD **** 03/20/00 ***********************************
     O******* E  2             ROUTE
     O*******                            20 'Account Type :'
     O*******                  RSESSR    54
      ***** END DEL ******* 03/28/01 **********************************
      ***** BEG ADD ******* 03/28/01 **********************************
     O          E            ROUTE          1
     O                                           20 'Company      :'
     O                       RSCO#         Z     24
     C***** BEG-CHG ***** 10/25/16 *****************************************
     O*****                                      60 'Group        :'
     O*****                  RSFND         Z     64
     O***  Begin Add  ***  10/27/03  **************************************
     O*****                                    +001 '-'
     O*****                  RSTFND        Z   +001
     O***  End   Add  ***  10/27/03  **************************************
     O               13                          60 'Group        :'
     O               13      RSFND         Z     56
     O               13                        +001 '-'
     O               13      RSTFND        Z   +001
     O          E    14      ROUTE          1
     O               14                          20 'Groups       :'
     O               14      strGroup1         +001
     O          E    15      ROUTE          1
     O               15      strGroup2           72
     C***** END-CHG ***** 10/25/16 *****************************************
      *
     O          E            ROUTE          1
     O                                           20 'Date Range   :'
     O                       RSFRDT              31 '  /  /    '
     O                                         +001 '-'
     O                       RSTODT            +001 '  /  /    '
      *
     O***  Begin Del  ***  10/27/03  **************************************
     O********E* 1             ROUTE
     O**********                         14 'Billings'
     O**********                       +001 'Through Date:'
     O**********               RSBLDT  +001 '  /  /    '
     O**********                       +007 'Payments'
     O**********                       +001 'Through Date :'
     O**********               RSPAYD  +001 '  /  /    '
     O***  End   Del  ***  10/27/03  **************************************
      *
     O          E            ROUTE          1
     O                                           20 'Minimum Amt  :'
     O                       RSMAMT        J   +001
      *
     O          E            ROUTE          1
     O                                           20 'SRT1 Order   :'
     O                       RSSRT1            +001
     O                                           60 'SRT2 Order   :'
     O                       RSSRT2            +001
      *
     O***  Begin Add  ***  10/27/03  **************************************
     O          E            ROUTE          1
     O                                           20 'Employer/Div :'
     O                       RSEDN             +001
     O***  End   Add  ***  10/27/03  **************************************
      *
     O          E            ROUTE          1
     O                                           20 'AGT # Range  :'
     O                       RSFRAN        Z   +001
     O                                         +001 '-'
     O                       RSTOAN        Z   +001
     O                                           58 'EMP # Range:'
     O                       RSFREN        Z   +001
     O                                         +001 '-'
     O                       RSTOEN        Z   +001
      *
     O          E            ROUTE          1
     O                                           20 'AgtName Range:'
     O                       RSFRAA            +001
     O                                         +001 '-'
     O                       RSTOAA            +001
     O                                           60 'EmpName Range:'
     O                       RSFREA            +001
     O                                         +001 '-'
     O                       RSTOEA            +001
     O***  Begin Add  ***  10/27/03  **************************************
     O          E            ROUTE          1
     O                                           20 'Policy Eff Dt:'
     O                       RSFREF            +001 '  /  /    '
     O                                         +001 '-'
     O                       RSTOEF            +001 '  /  /    '
     O***  End   Add  ***  10/27/03  **************************************
      *
     O          E            ROUTE          1
     O                                           24 'Interim # Days For'
      *
     O          E            ROUTE          1
     O                                           23 '1ST Installment:'
     O                       XXFIDY            +001
     O                                         +003 'Est. Billing:'
     O                       XXEBDY            +001
      *
     O          E            ROUTE          1
     O                                           20 'Audit Billng:'
     O                       XXABDY            +001
     O                                         +002 'Rvsd Aud Blg:'
     O                       XXRADY            +001
      *
     O          E            ROUTE          1
     O                                           20 'Acct Status  :'
     O                       RSACT             +001
      *
     O          E            ROUTE          1
     O                                           20 'Audit Status :'
     O                       RSAUST              47
      *
     O          E            ROUTE          1
     O                                           20 'Excl Cont Aud:'
     O                       RSCTST            +001
      *
     O          E            ROUTE          1
     O                                           20 'Account Type :'
     O                       RSESSR            +001
     O*** BEG ADD ****** 08/21/08 ************************************
     O          E            ROUTE          1
     O                                           20 'Rating State :'
     O                       RXJRST            +001
     O*** END ADD ****** 08/21/08 ************************************
      *
      ***** END ADD ******* 03/28/01 **********************************
     O***  Begin Add  ***  10/27/03  **************************************
     O          E            ROUTE          1
     O                                           20 'Deposit Trans:'
     O                       RSINDP            +001
     O***  End   Add  ***  10/27/03  **************************************
     O***  Begin Add  ***  01/10/07  **************************************
     O          E            ROUTE          1
     O                                           20 'Addl Days for '
     O          E            ROUTE          1
     O                                           20 'Inst Date Due:'
     O                       XXADDU            +001
     O***  End   Add  ***  01/10/07  **************************************
     O          E            ROUTE       1  1
     O               51                          30 'A/R CODES INCLUDED IN'
     O               51                          42 'CALCULATION'
     O              N51                          28 'A/R CODES EXCLUDED '
     O              N51                          44 'FROM CALCULATION'
      **** END   CHG **** 08/30/99 ************************************
     O          E      N20   ROUTE          1
     O                       RSCODE              26
     O          E       20   ROUTE          1
     O                  20                        6 '1.'
     O                       CDE(1)               9
     O                       DES(1)              40
     O                  35                       45 '16.'
     O                       CDE(16)             48
     O                       DES(16)             79
     O          E       21   ROUTE          1
     O                  21                        6 '2.'
     O                       CDE(2)               9
     O                       DES(2)              40
     O                  36                       45 '17.'
     O                       CDE(17)             48
     O                       DES(17)             79
     O          E       22   ROUTE          1
     O                  22                        6 '3.'
     O                       CDE(3)               9
     O                       DES(3)              40
     O                  37                       45 '18.'
     O                       CDE(18)             48
     O                       DES(18)             79
     O          E       23   ROUTE          1
     O                  23                        6 '4.'
     O                       CDE(4)               9
     O                       DES(4)              40
     O                  38                       45 '19.'
     O                       CDE(19)             48
     O                       DES(19)             79
     O          E       24   ROUTE          1
     O                  24                        6 '5.'
     O                       CDE(5)               9
     O                       DES(5)              40
     O                  39                       45 '20.'
     O                       CDE(20)             48
     O                       DES(20)             79
     O          E       25   ROUTE          1
     O                  25                        6 '6.'
     O                       CDE(6)               9
     O                       DES(6)              40
     O                  40                       45 '21.'
     O                       CDE(21)             48
     O                       DES(21)             79
     O          E       26   ROUTE          1
     O                  26                        6 '7.'
     O                       CDE(7)               9
     O                       DES(7)              40
     O                  41                       45 '22.'
     O                       CDE(22)             48
     O                       DES(22)             79
     O          E       27   ROUTE          1
     O                  27                        6 '8.'
     O                       CDE(8)               9
     O                       DES(8)              40
     O                  42                       45 '23.'
     O                       CDE(23)             48
     O                       DES(23)             79
     O          E       28   ROUTE          1
     O                  28                        6 '9.'
     O                       CDE(9)               9
     O                       DES(9)              40
     O                  43                       45 '24.'
     O                       CDE(24)             48
     O                       DES(24)             79
     O          E       29   ROUTE          1
     O                  29                        6 '10.'
     O                       CDE(10)              9
     O                       DES(10)             40
     O                  44                       45 '25.'
     O                       CDE(25)             48
     O                       DES(25)             79
     O          E       30   ROUTE          1
     O                  30                        6 '11.'
     O                       CDE(11)              9
     O                       DES(11)             40
     O                  45                       45 '26.'
     O                       CDE(26)             48
     O                       DES(26)             79
     O          E       31   ROUTE          1
     O                  31                        6 '12.'
     O                       CDE(12)              9
     O                       DES(12)             40
     O                  46                       45 '27.'
     O                       CDE(27)             48
     O                       DES(27)             79
     O          E       32   ROUTE          1
     O                  32                        6 '13.'
     O                       CDE(13)              9
     O                       DES(13)             40
     O                  47                       45 '28.'
     O                       CDE(28)             48
     O                       DES(28)             79
     O          E       33   ROUTE          1
     O                  33                        6 '14.'
     O                       CDE(14)              9
     O                       DES(14)             40
     O                  48                       45 '29.'
     O                       CDE(29)             48
     O                       DES(29)             79
     O          E       34   ROUTE          1
     O                  34                        6 '15.'
     O                       CDE(15)              9
     O                       DES(15)             40
     O                  49                       45 '30.'
     O                       CDE(30)             48
     O                       DES(30)             79
      **** BEGIN ADD **** 11/17/00 *********************************************
     O          E            ROUTE       1  1
     O               14                          32 'ACTIVITY CODES INCLUDED'
     O               14                          47 'IN CALCULATION'
     O              N14                          33 'ACTIVITY CODES EXCLUDED'
     O              N14                          50 'FROM CALCULATION'
     O          E       14   ROUTE          1
     O                       RSASTC              26
     O          E       15   ROUTE          1
     O                  15                        6 '1.'
     O                       AS(1)                9
     O                       ASDS(1)             40
     O          E       16   ROUTE          1
     O                  16                        6 '2.'
     O                       AS(2)                9
     O                       ASDS(2)             40
     O          E       17   ROUTE          1
     O                  17                        6 '3.'
     O                       AS(3)                9
     O                       ASDS(3)             40
     O          E       18   ROUTE          1
     O                  18                        6 '4.'
     O                       AS(4)                9
     O                       ASDS(4)             40
     O          E       19   ROUTE          1
     O                  19                        6 '5.'
     O                       AS(5)                9
     O                       ASDS(5)             40
      **** END   ADD **** 11/17/00 *********************************************
      **** BEGIN CHG **** 08/30/99 ************************************
     O******* E  160           ROUTE
     O*******                            23 '(C) COPYRIGHT FISERV IN'
     O*******                          +000 'SURANCE SOLUTIONS, INC.,'
     O*******                  *YEAR   +001
      ***** BEG CHG ******* 12/12/99 **********************************
     O******  E  162           ROUTE
      ***** END CHG ******* 12/12/99 **********************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT FISERV IN'
     O***                              +000 'SURANCE SOLUTIONS, INC.,'
     O***                      *YEAR   +001
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      **** END   CHG **** 08/30/99 ************************************
      *
     O******************************************************************
     OQSYSPRT   E            NOTICE         1 07
     O                                         +005 '    L A T E     N O '
     O                                         +000 'T I C E   -   P A S '
     O                                         +000 'T     D U E     B A '
     O                                         +000 'L A N C E '
     O          E            NOTICE         2
     O                       SEPA80            +005
     O          E            NOTICE         1
     O*** BEGIN CHG ***  06/01/98  ************************************
     O*****                    XXFNAM  +006
     O                       XXACNM            +006
     O*** END CHG *****  06/01/98  ************************************
     O                                           61 'Run Date :'
      ***** BEG CHG ******* 02/28/02 **********************************
     O*********                XXRNDT    77 '  /  /    '
     O                       XXRNDT              80 '  /  /    '
      ***** END CHG ******* 02/28/02 **********************************
     O          E            NOTICE         1
     O*** BEGIN CHG ***  06/01/98  ************************************
     O*****                    XFMAD1  +006
     O                       XXAAD1            +006
     O*** END CHG *****  06/01/98  ************************************
     O*** BEG CHG *****  09/20/06  ************************************
     O***********                        61 'Policy # :'
     O              N66                          61 'Policy # :'
     O               66                          61 'Member # :'
     O*** END CHG *****  09/20/06  ************************************
      ***** BEG CHG ******* 02/28/02 **********************************
     O*********                XXPLCY    77
     O                       X1PLCY              80
      ***** END CHG ******* 02/28/02 **********************************
     O          E            NOTICE         1
     O*** BEGIN CHG ***  06/01/98  ************************************
     O*****                    XFMAD2  +006
     O                       XXAAD2            +006
     O*** END CHG *****  06/01/98  ************************************
     O                                           61 'Division :'
      ***** BEG CHG ******* 02/28/02 **********************************
     O*********                EBDIV     77
     O                       EBDIV               80
      ***** END CHG ******* 02/28/02 **********************************
      ***** BEG CHG ******* 02/28/02 **********************************
     O******* E  3             NOTICE
     O          E            NOTICE         1
      ***** END CHG ******* 02/28/02 **********************************
     O*** BEGIN CHG ***  06/01/98  ************************************
     O*****                    XFMAD3  +006
     O                       XXAAD3            +006
     O*** END CHG *****  06/01/98  ************************************
     O                                           61 'Policy Yr:'
      ***** BEG CHG ******* 02/28/02 **********************************
     O*********                EBFYR Z   77
     O                       EBFYR         Z     80
      ***** END CHG ******* 02/28/02 **********************************
      ***** BEG ADD ******* 02/28/02 **********************************
      *
     O          E            NOTICE         2
     O              N52                          61 'File #   :'
     O              N52      XXFIL#              80
      ***** END ADD ******* 02/28/02 **********************************
      *
     O          E            NOTICE         0
     O                       XXENM2            +008
     O          E    77      NOTICE      1
     O               77      XXFDNM            +008
     O          E            NOTICE         1
     O***  Begin Del  ***  10/27/03  **************************************
     O**********                         61 'Balance Date :'
     O**********               XXPAYDY +001
     O***  End   Del  ***  10/27/03  **************************************
     O          E            NOTICE         1
     O                       XXADR1            +008
     O                                           61 'Due Date     :'
     O                       XXDUDT        Y   +001
     O          E            NOTICE         1
     O                       XXADR2            +008
     O                                           61 'Past Due Date:'
     O                       XXPDDT        Y   +001
     O          E            NOTICE         2
     O                       XXADR3            +008
     O          E            NOTICE         3
     O                       SEPA80            +005
      *  Non-Audited Accts - Active
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  1    N71N74   NOTICE
     O          E   N71N74N73NOTICE         1
      *** END CHG ***** 04/18/11 **************************************
     O                                         +006 'We have not received'
     O                                         +000 ' your payment which '
     O                                         +000 'was due on'
     O                       XXDUDT        Y   +001
     O                                         +000 '.  To avoid'
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  1    N71N74   NOTICE
     O          E   N71N74N73NOTICE         1
      *** END CHG ***** 04/18/11 **************************************
      ***** BEG CHG ******* 09/17/07 **********************************
     O**********                       +006 'cancellation, please'
     O**********                       +000 ' send this payment b'
     O**********                       +000 'y'
     O**********      N73      XXPDDTY   58
     O**********       73      XXCXDTY   58
     O**********                       +000 '.  If payment has'
     O              N67                          26 'cancellation, please'
     O              N67                          46 ' send this payment b'
     O              N67                          47 'y'
     O              N73N67   XXPDDT        Y     58
     O               73N67   XXCXDT        Y     58
     O                 N67                       75 '.  If payment has'
     O               67                          26 'cancellation, paymen'
     O               67                          46 't must be received i'
     O               67                          62 'n our lockbox by'
     O              N73 67   XXPDDT        Y     73
     O               73 67   XXCXDT        Y     73
     O                  67                       74 '.'
      ***** END CHG ******* 09/17/07 **********************************
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  1    N71N74   NOTICE
     O          E   N71N74N73NOTICE         1
      *** END CHG ***** 04/18/11 **************************************
      ***** BEG CHG ******* 09/17/07 **********************************
     O**********                       +006 'already been mailed,'
     O**********                       +000 ' thank you for your '
     O**********                       +000 'payment; please disr'
     O**********                       +000 'egard this'
     O                 N67                       26 'already been mailed,'
     O                 N67                       46 ' thank you for your '
     O                 N67                       66 'payment; please disr'
     O                 N67                       76 'egard this'
     O                  67                       26 'If payment has alrea'
     O                  67                       46 'dy been mailed, than'
     O                  67                       66 'k you for your payme'
     O                  67                       77 'nt; please '
      ***** END CHG ******* 09/17/07 **********************************
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  2    N71N74   NOTICE
     O          E   N71N74N73NOTICE         2
      *** END CHG ***** 04/18/11 **************************************
      ***** BEG CHG ******* 09/17/07 **********************************
     O**********                       +006 'notice.'
     O              N67                          13 'notice.'
     O               67                          26 'disregard this notic'
     O               67                          28 'e.'
      ***** END CHG ******* 09/17/07 **********************************
      *** BEG ADD ***** 04/18/11 **************************************
      *
      *  Non-Audited Accts - Pending cancellation
      *
     O          E   N71N74 73NOTICE         1
     O                                         +006 'We have not received'
     O                                         +000 ' your payment which '
     O                                         +000 'was due on'
     O                       XXDUDT        Y   +001
     O              N67                          75 '.  Please'
     O               67                          77 '.  To avoid'
     O          E   N71N74 73NOTICE         1
     O              N67                        +006 'send this payment by'
     O              N67      XXPDDT        Y   +001
     O              N67                        +000 '.  If payment has'
     O              N67                        +000 ' already been mailed,'
     O               67                          26 'cancellation, paymen'
     O               67                          46 't must be received i'
     O               67                          62 'n our lockbox by'
     O               67      XXPDDT        Y     73
     O               67                          74 '.'
     O          E   N71N74 73NOTICE         1
     O                 N67                     +006 'thank you for your '
     O                 N67                     +000 'payment; please disr'
     O                 N67                     +000 'egard this notice.'
     O                  67                       26 'If payment has alrea'
     O                  67                       46 'dy been mailed, than'
     O                  67                       66 'k you for your payme'
     O                  67                       77 'nt; please '
     O          E   N71N74 73NOTICE         2
     O              N67                        +001 ' '
     O               67                          26 'disregard this notic'
     O               67                          28 'e.'
      *** END ADD ***** 04/18/11 **************************************
      *  Non-Audited Accts - Cancelled as of the Balance Date
     O          E   N71 74   NOTICE         1
     O                                         +006 'Your policy is under'
     O                                         +000 ' cancellation effect'
     O                                         +000 'ive'
     O                       XXCXDT        Y   +001
     O                                         +000 '.  We have not'
     O          E   N71 74   NOTICE         1
     O                                         +006 'received your paymen'
     O                                         +000 't which was due on'
     O                       XXDUDT        Y   +001
     O                                         +000 '.  If payment has al'
     O                                         +000 'ready'
     O          E   N71 74   NOTICE         3
     O                                         +006 'been mailed, thank y'
     O                                         +000 'ou for your payment;'
     O                                         +000 ' please disregard th'
     O                                         +000 'is notice.'
      *  Audited Accts that are Active or Cancel Pending
     O          E    71N74   NOTICE         1
     O                                         +006 'Please note your aud'
     O                                         +000 'it balance is now pa'
     O                                         +000 'st due.  In order to'
     O                                         +000 ' avoid'
     O          E    71N74   NOTICE         1
     O                                         +006 'cancellation, please'
     O                                         +000 ' remit the following'
     O                                         +000 ' balance immediately'
     O                                         +000 '.  If payment'
     O          E    71N74   NOTICE         1
     O                                         +006 'has already been mai'
     O                                         +000 'led, thank you for y'
     O                                         +000 'our payment; please '
     O                                         +000 'disregard this'
     O          E    71N74   NOTICE         2
     O                                         +006 'notice.'
      *  Audited Accts that are Cancelled
     O          E    71 74   NOTICE         1
     O                                         +006 'Please note your can'
     O                                         +000 'cellation audit bala'
     O                                         +000 'nce is now past due.'
     O                                         +000 '  In order to'
     O          E    71 74   NOTICE         1
     O                                         +006 'avoid any legal acti'
     O                                         +000 'on, please remit the'
     O                                         +000 ' following balance i'
     O                                         +000 'mmediately.'
     O          E    71 74   NOTICE         1
     O                                         +006 'If payment has alrea'
     O                                         +000 'dy been mailed, than'
     O                                         +000 'k you for your payme'
     O                                         +000 'nt; please'
     O          E    71 74   NOTICE         2
     O                                         +006 'disregard this noti'
     O                                         +000 'ce.'
      *  Common O-Specs restart here
     O          E            NOTICE         2
     O                                         +006 'Outstanding Balance:'
     O          E            NOTICE         1
     O               75 85                     +016 'Deposit Due'
     O               75 85   XXDEPD        K     65
     O          E            NOTICE         1
     O                  86                     +016 'Premium Due:'
     O                  86   XXPRMD        K     65
     O          E            NOTICE         1
     O                  87                     +016 'Expense Constant Due:'
     O                  87   XXEXCD        K     65
     O          E            NOTICE         1
     O                  88                     +016 'Deductible Fees Due:'
     O                  88   XXDEDD        K     65
     O*** BEGIN ADD *** 08/31/98 **************************************
     O          E            NOTICE         1
      *** BEG CHG ***** 01/10/06 **************************************
     O***********         90           +016 'State Tax Due:'
     O                  90   XXTXHD            +016
      *** END CHG ***** 01/10/06 **************************************
     O                  90   XXTAX         K     65
     O*** END   ADD *** 08/31/98 **************************************
     O******** BEGIN ADD 05/15/07  ************************************
     O          E            NOTICE         1
     O               53      XX1AHD            +016
     O               53      XX1STB        K     65
     O*
     O          E            NOTICE         1
     O               54      XX2AHD            +016
     O               54      XX2NDB        K     65
     O*
     O          E            NOTICE         1
     O               55      XX3AHD            +016
     O               55      XX3RDB        K     65
     O*
     O          E            NOTICE         1
     O               56      XX4AHD            +016
     O               56      XX4THB        K     65
     O*
     O          E            NOTICE         1
     O               57      XX5AHD            +016
     O               57      XX5THB        K     65
     O******** END ADD***05/15/07  ************************************
     O*
     O***  Begin Add  ***  12/11/08  **************************************
     O          E            NOTICE         1
     O               58      XX6AHD            +016
     O               58      XX6THB        K     65
     O*
     O          E            NOTICE         1
     O               59      XX7AHD            +016
     O               59      XX7THB        K     65
     O***  End   Add  ***  12/11/08  **************************************
     O*
     O          E            NOTICE         0
     O                                           65 '_____________'
     O          E            NOTICE         1
     O                  89                     +016 'Fees Due:'
     O                  89   XXFEED        K     65
     O          E            NOTICE         2
     O                                         +016 'Total Amount Due:'
     O                       XXTOTD        K     65 '$'
     O          E            NOTICE         3
     O                       SEPA80            +005
     O          E            NOTICE         2
     O                                         +006 'Make Checks Payable to:'
     O*** BEGIN CHG ***  06/01/98  ************************************
     O*****                    FNSHNM  +002
     O                       OFCKNM            +002
     O*** END CHG *****  06/01/98  ************************************
     O          E            NOTICE         2
     O                                         +006 'Serviced By:           '
     O*** BEGIN CHG ***  08/31/98  ************************************
     O***********************  XXCONM  +002
     O                       XXACNM            +002
     O*** END   CHG ***  08/31/98  ************************************
     O          E            NOTICE         2
     O                                         +006 'Direct Inquiries to:   '
     O                       XXNAME              80
     O          E            NOTICE         1
     O                       XXCM01            +006
     O          E            NOTICE         1
     O                       XXCM02            +006
     O          E            NOTICE         1
     O                       XXCM03            +006
     O          E            NOTICE         1
     O                       XXCM04            +006
     O*** BEG CHG *** 05/06/10 ************************************
     O********E  2             NOTICE
     O          E            NOTICE         1
     O*** END CHG *** 05/06/10 ************************************
     O                       XXCM05            +006
     O          E            NOTICE         1
     O               73                        +008 'YOUR POLICY IS CURRE'
     O               73                        +000 'NTLY PENDING CANCELL'
     O               73                        +000 'ATION EFFECTIVE'
     O               73      XXCXDT            +001 '  /  /    '
     O*** BEG CHG *** 05/06/10 ************************************
     O******  E  2             NOTICE
     O          E            NOTICE         1
     O*** END CHG *** 05/06/10 ************************************
     O               73                        +008 'REASON: '
     O               73      XXRESN            +001
     O          E            NOTICE         1
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                          +006 'Agent:'
     O                       EXAHDR            +006
     O*** END CHG *** 09/24/04 ************************************
     O                       EBAGT#        Z   +001
     O                       EBANAM            +002
     O***  Begin Add  ***  10/27/03  **************************************
     O          E            PROD           1
     O                                         +006 'Producer:'
     O                       ##PROD        Z   +001
     O                       XXPNAM            +002
     OQSYSPRT   E            AGINFO         1 07
     O                       AGNAME            +005
     O          E            AGINFO         1
     O                       AGMAD1            +005
     O          E            AGINFO         1
     O                       XPMCSZ            +005
     O***  End   Add  ***  10/27/03  **************************************
     O***** BEGIN ADD **** 03/03/00 ***********************************
     O*****************************************************************
     O* ESTIMATED LATE NOTICE LISING
     O*****************************************************************
     OQSYSPRT1  E            ROUTE1         1 03
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         1
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE1         1
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**   SELECTION CRITERIA
      ***** BEG DEL ******* 03/28/01 **********************************
      *******
     O******* E  1             ROUTE1
     O*******                            20 'Company      :'
     O*******                  RSCO# Z   24
     O******* E  1             ROUTE1
     O*******                            20 'Group        :'
     O*******                  RSFND Z   24
     O******* E  1             ROUTE1
     O*******                            20 'Date Range   :'
     O*******                  RSFRDT    31 '  /  /    '
     O*******                          +001 '-'
     O*******                  RSTODT  +001 '  /  /    '
     O******* E  1             ROUTE1
     O*******                            20 'Billings      '
     O******* E  1             ROUTE1
     O*******                            20 'Through Date :'
     O*******                  RSBLDT    31 '  /  /    '
     O******* E  1             ROUTE1
     O*******                            20 'Payments      '
     O******* E  1             ROUTE1
     O*******                            20 'Through Date :'
     O*******                  RSPAYD    31 '  /  /    '
     O******* E  1             ROUTE1
     O*******                            20 'Minimum Amt  :'
     O*******                  RSMAMTJ +001
     O******BEGIN ADD **** 03/20/00 ***********************************
     O******* E  1             ROUTE1
     O*******                            20 'Excl Cont Aud:'
     O*******                  RSCTST  +001
     O******END   ADD **** 03/20/00 ***********************************
     O******* E  1             ROUTE1
     O*******                            20 'Report only  :'
     O*******                  RSRPTF  +001
     O******* E  1             ROUTE1
     O*******                            20 'SRT1 Order   :'
     O*******                  RSSRT1    34
     O******* E  1             ROUTE1
     O*******                            20 'SRT2 Order   :'
     O*******                  RSSRT2    34
     O******* E  1             ROUTE1
     O*******                            20 'AGT # Range  :'
     O*******                  RSFRANZ   30
     O*******                            32 '-'
     O*******                  RSTOANZ   38
     O******* E  1             ROUTE1
     O*******                            20 'EMP # Range  :'
     O*******                  RSFRENZ   30
     O*******                            32 '-'
     O*******                  RSTOENZ   42
     O******* E  1             ROUTE1
     O*******                            20 'AgtName Range:'
     O*******                  RSFRAA    23
     O*******                            25 '-'
     O*******                  RSTOAA    28
     O******* E  1             ROUTE1
     O*******                            20 'EmpName Range:'
     O*******                  RSFREA    23
     O*******                            25 '-'
     O*******                  RSTOEA    28
     O******* E  1             ROUTE1
     O*******                            24 'Interim # Days For'
     O******* E  1             ROUTE1
     O*******                            20 '1ST Instlmnt:'
     O*******                  XXFIDY    23
     O*******                            38 'Est. Billing:'
     O*******                  XXEBDY    41
     O******* E  1             ROUTE1
     O*******                            20 'Audit Billng:'
     O*******                  XXABDY    23
     O*******                            38 'Rvsd Aud Blg:'
     O*******                  XXRADY    41
     O******* E  1             ROUTE1
     O*******                            20 'Acct Status  :'
     O*******                  RSACT     46
     O******* E  1             ROUTE1
     O*******                            20 'Audit Status :'
     O*******                  RSAUST    47
     O******* E  2             ROUTE1
     O*******                            20 'Account Type :'
     O*******                  RSESSR    54
      ***** END DEL ******* 03/28/01 **********************************
      ***** BEG ADD ******* 03/28/01 **********************************
      *
     O          E            ROUTE1         1
     O                                           20 'Company      :'
     O                       RSCO#         Z     24
     C***** BEG-CHG ***** 10/25/16 *****************************************
     O*****                                      60 'Group        :'
     O*****                  RSFND         Z   +001
     O***  Begin Add  ***  10/27/03  **************************************
     O*****                                    +001 '-'
     O*****                  RSTFND        Z   +001
     O***  End   Add  ***  10/27/03  **************************************
     O               13                          60 'Group        :'
     O               13      RSFND         Z   +001
     O               13                        +001 '-'
     O               13      RSTFND        Z   +001
     O          E    14      ROUTE1         1
     O               14                          20 'Groups       :'
     O               14      strGroup          +001
     C***** END-CHG ***** 10/25/16 *****************************************
      *
     O          E            ROUTE1         1
     O                                           20 'Date Range   :'
     O                       RSFRDT              31 '  /  /    '
     O                                         +001 '-'
     O                       RSTODT            +001 '  /  /    '
      *
     O***  Begin Del  ***  10/27/03  **************************************
     O********E* 1             ROUTE1
     O**********                         14 'Billings'
     O**********                       +001 'Through Date :'
     O**********               RSBLDT  +001 '  /  /    '
     O**********                       +006 'Payments'
     O**********                       +001 'Through Date:'
     O**********               RSPAYD  +001 '  /  /    '
     O***  End   Del  ***  10/27/03  **************************************
      *
     O          E            ROUTE1         1
     O                                           20 'Minimum Amt  :'
     O                       RSMAMT        J   +001
     O                                           60 'Excl Cont Aud:'
     O                       RSCTST            +001
      *
     O          E            ROUTE1         1
     O                                           20 'Report only  :'
     O                       RSRPTF            +001
     O                                           60 'SRT1 Order   :'
     O                       RSSRT1            +001
     O                                           90 'SRT2 Order   :'
     O                       RSSRT2            +001
      *
     O***  Begin Add  ***  10/27/03  **************************************
     O          E            ROUTE1         1
     O                                           20 'Employer/Div :'
     O                       RSEDN             +001
     O***  End   Add  ***  10/27/03  **************************************
      *
     O          E            ROUTE1         1
     O                                           20 'AGT # Range  :'
     O                       RSFRAN        Z   +001
     O                                         +001 '-'
     O                       RSTOAN        Z   +001
     O                                           60 'EMP # Range  :'
     O                       RSFREN        Z   +001
     O                                         +001 '-'
     O                       RSTOEN        Z   +001
      *
     O          E            ROUTE1         1
     O                                           20 'AgtName Range:'
     O                       RSFRAA            +001
     O                                         +001 '-'
     O                       RSTOAA            +001
     O                                           60 'EmpName Range:'
     O                       RSFREA            +001
     O                                         +001 '-'
     O                       RSTOEA            +001
     O*** BEG ADD ****** 08/21/08 ************************************
     O          E            ROUTE1         1
     O                                           20 'Rating State :'
     O                       RXJRST            +001
     O*** END ADD ****** 08/21/08 ************************************
      *
     O***  Begin Add  ***  10/27/03  **************************************
     O          E            ROUTE1         1
     O                                           20 'Policy Eff Dt:'
     O                       RSFREF            +001 '  /  /    '
     O                                         +001 '-'
     O                       RSTOEF            +001 '  /  /    '
     O***  End   Add  ***  10/27/03  **************************************
      *
     O          E            ROUTE1         1
     O                                           24 'Interim # Days For'
      *
     O          E            ROUTE1         1
     O                                           20 '1ST Instlmnt:'
     O                       XXFIDY              23
     O                                           38 'Est. Billing:'
     O                       XXEBDY              41
      *
     O          E            ROUTE1         1
     O                                           20 'Audit Billng:'
     O                       XXABDY              23
     O                                           38 'Rvsd Aud Blg:'
     O                       XXRADY              41
      *
     O          E            ROUTE1         1
     O                                           20 'Acct Status  :'
     O                       RSACT               46
      *
     O          E            ROUTE1         1
     O                                           20 'Audit Status :'
     O                       RSAUST              47
      *
     O          E            ROUTE1         1
     O                                           20 'Account Type :'
     O                       RSESSR              54
      *
      ***** END ADD ******* 03/28/01 **********************************
     O***  Begin Add  ***  10/27/03  **************************************
     O          E            ROUTE1         1
     O                                           20 'Deposit Trans:'
     O                       RSINDP            +001
     O***  End   Add  ***  10/27/03  **************************************
     O***  Begin Add  ***  01/10/07  **************************************
     O          E            ROUTE1         1
     O                                           20 'Addl Days for '
     O          E            ROUTE1         1
     O                                           20 'Inst Date Due:'
     O                       XXADDU            +001
     O***  End   Add  ***  01/10/07  **************************************
     O          E            ROUTE1      1  1
     O               51                          30 'A/R CODES INCLUDED IN'
     O               51                          42 'CALCULATION'
     O              N51                          28 'A/R CODES EXCLUDED '
     O              N51                          44 'FROM CALCULATION'
     O          E      N20   ROUTE1         1
     O                       RSCODE              26
     O          E       20   ROUTE1         1
     O                  20                        6 '1.'
     O                       CDE(1)               9
     O                       DES(1)              40
     O                  35                       45 '16.'
     O                       CDE(16)             48
     O                       DES(16)             79
     O          E       21   ROUTE1         1
     O                  21                        6 '2.'
     O                       CDE(2)               9
     O                       DES(2)              40
     O                  36                       45 '17.'
     O                       CDE(17)             48
     O                       DES(17)             79
     O          E       22   ROUTE1         1
     O                  22                        6 '3.'
     O                       CDE(3)               9
     O                       DES(3)              40
     O                  37                       45 '18.'
     O                       CDE(18)             48
     O                       DES(18)             79
     O          E       23   ROUTE1         1
     O                  23                        6 '4.'
     O                       CDE(4)               9
     O                       DES(4)              40
     O                  38                       45 '19.'
     O                       CDE(19)             48
     O                       DES(19)             79
     O          E       24   ROUTE1         1
     O                  24                        6 '5.'
     O                       CDE(5)               9
     O                       DES(5)              40
     O                  39                       45 '20.'
     O                       CDE(20)             48
     O                       DES(20)             79
     O          E       25   ROUTE1         1
     O                  25                        6 '6.'
     O                       CDE(6)               9
     O                       DES(6)              40
     O                  40                       45 '21.'
     O                       CDE(21)             48
     O                       DES(21)             79
     O          E       26   ROUTE1         1
     O                  26                        6 '7.'
     O                       CDE(7)               9
     O                       DES(7)              40
     O                  41                       45 '22.'
     O                       CDE(22)             48
     O                       DES(22)             79
     O          E       27   ROUTE1         1
     O                  27                        6 '8.'
     O                       CDE(8)               9
     O                       DES(8)              40
     O                  42                       45 '23.'
     O                       CDE(23)             48
     O                       DES(23)             79
     O          E       28   ROUTE1         1
     O                  28                        6 '9.'
     O                       CDE(9)               9
     O                       DES(9)              40
     O                  43                       45 '24.'
     O                       CDE(24)             48
     O                       DES(24)             79
     O          E       29   ROUTE1         1
     O                  29                        6 '10.'
     O                       CDE(10)              9
     O                       DES(10)             40
     O                  44                       45 '25.'
     O                       CDE(25)             48
     O                       DES(25)             79
     O          E       30   ROUTE1         1
     O                  30                        6 '11.'
     O                       CDE(11)              9
     O                       DES(11)             40
     O                  45                       45 '26.'
     O                       CDE(26)             48
     O                       DES(26)             79
     O          E       31   ROUTE1         1
     O                  31                        6 '12.'
     O                       CDE(12)              9
     O                       DES(12)             40
     O                  46                       45 '27.'
     O                       CDE(27)             48
     O                       DES(27)             79
     O          E       32   ROUTE1         1
     O                  32                        6 '13.'
     O                       CDE(13)              9
     O                       DES(13)             40
     O                  47                       45 '28.'
     O                       CDE(28)             48
     O                       DES(28)             79
     O          E       33   ROUTE1         1
     O                  33                        6 '14.'
     O                       CDE(14)              9
     O                       DES(14)             40
     O                  48                       45 '29.'
     O                       CDE(29)             48
     O                       DES(29)             79
     O          E       34   ROUTE1         1
     O                  34                        6 '15.'
     O                       CDE(15)              9
     O                       DES(15)             40
     O                  49                       45 '30.'
     O                       CDE(30)             48
     O                       DES(30)             79
      **** BEGIN ADD **** 11/17/00 *********************************************
     O          E            ROUTE1      1  1
     O               14                          32 'ACTIVITY CODES INCLUDED'
     O               14                          47 'IN CALCULATION'
     O              N14                          33 'ACTIVITY CODES EXCLUDED'
     O              N14                          50 'FROM CALCULATION'
     O          E       14   ROUTE1         1
     O                       RSASTC              26
     O          E       15   ROUTE1         1
     O                  15                        6 '1.'
     O                       AS(1)                9
     O                       ASDS(1)             40
     O          E       16   ROUTE1         1
     O                  16                        6 '2.'
     O                       AS(2)                9
     O                       ASDS(2)             40
     O          E       17   ROUTE1         1
     O                  17                        6 '3.'
     O                       AS(3)                9
     O                       ASDS(3)             40
     O          E       18   ROUTE1         1
     O                  18                        6 '4.'
     O                       AS(4)                9
     O                       ASDS(4)             40
     O          E       19   ROUTE1         1
     O                  19                        6 '5.'
     O                       AS(5)                9
     O                       ASDS(5)             40
      **** END   ADD **** 11/17/00 *********************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E  160           ROUTE1
     O***                                23 '(C) COPYRIGHT FISERV IN'
     O***                              +000 'SURANCE SOLUTIONS, INC.,'
     O***                      *YEAR   +001
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      *
     O***** END ADD **** 03/03/00 *************************************
      **** BEGIN ADD **** 08/30/99 ************************************
      *
      *   ESTIMATED LATE NOTICE LISTING
      *
     O***** BEGIN CHG **** 03/03/00 ***********************************
     O********** QSYSPRT1E  106           HEAD
     O          E            HEAD           1 06
     O***** END   CHG **** 03/03/00 ***********************************
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              85
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           06 'Group:'
     O                       FNFND         Z     09
     O                       FNNAME              71
     O                                           91 'SRT1 Order:'
     O                       RSSRT1             105
     O                                          118 'SRT2 Order:'
     O                       RSSRT2             132
     O          E            HEAD           1
     O                                           62 'Interim # Days For'
     O          E            HEAD           1
     O                                           18 'Date Range       :'
     O                       RSFRDT              29 '  /  /    '
     O                                         +001 '-'
     O                       RSTODT            +001 '  /  /    '
     O                                           63 '1st Instllmnt:'
     O                       XXFIDY              66
     O                                           87 'AgtName Range:'
     O                       RSFRAA              99
     O                                          101 '-'
     O                       RSTOAA             104
     O          E            HEAD           1
     O                                           18 'Billing thru date:'
     O***  Begin Chg  ***  10/27/03  **************************************
     O**********               XXRNDTY   29
     O                       XXTODT        Y     29
     O***  End   Chg  ***  10/27/03  **************************************
     O                                           63 'Est. Billing :'
     O                       XXEBDY              66
     O                                           87 'EmpName Range:'
     O                       RSFREA              99
     O                                          101 '-'
     O                       RSTOEA             104
     O          E            HEAD           1
     O***  Begin Del  ***  10/27/03  **************************************
     O**********                         18 'Payment thru date:'
     O**********               RSPAYDY   29
     O***  End   Del  ***  10/27/03  **************************************
     O                                           63 'Audit Billing:'
     O                       XXABDY              66
     O                                           87 'Agt# Range   :'
     O                       RSFRAN        Z     99
     O                                          101 '-'
     O                       RSTOAN        Z    107
     O          E            HEAD           1
     O                                           18 'Minimum Amt.     :'
     O                       RSMAMT        J     43
     O                                           63 'Rvsd Aud Blg :'
     O                       XXRADY              66
     O                                           87 'Emp# Range   :'
     O                       RSFREN        Z     99
     O                                          101 '-'
     O                       RSTOEN        Z    111
     O          E            HEAD           2
     O                                           12 'Acct Status:'
     O                       RSACT               38
     O                                           54 'Audit Status:'
     O                       RSAUST              81
     O                                           97 'Account Type:'
     O                       RSESSR             131
     O          E            HEAD           2
     O               51                          21 'A/R Codes Included in'
     O               51                          33 'Calculation'
     O              N51                          19 'A/R CODES EXCLUDED '
     O              N51                          35 'FROM CALCULATION'
     O              N20      RSCODE              42
     O               20                          55 '- SEE ROUTING PAGE'
     O          E            HEAD           2
     O                                           12 'EMPLR'
     O                                           19 'DIV'
     O                                           23 'YR'
     O                                           32 'NAME'
     O                                           79 'TOTAL DUE'
     O*** BEG CHG *** 09/24/04 ************************************
     O***********                        88 'AGENT'
      *
      ***** BEG CHG ***** 08/26/05 ****************************************
     O**************           XXAHDR    88
     O                       XXAHDR              89
      ***** END CHG ***** 08/26/05 ****************************************
      *
     O*** END CHG *** 09/24/04 ************************************
     O                                           94 'NAME'
     O* Detail Line
     O          E            DET            1
     O                       EBEMP#        Z     12
     O                       EBDIV         Z     19
     O                       EBFYR         Z     23
     O                       EBENAM              68
     O                       XXTOTD        J     80 '$'
     O                       EBAGT#        Z     88
     O                       EBANAM             130
     O* Total Line
     O          E            AGTOTL      1  1
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                            22 'AGENT:'
     O                       EXAHDR              22
     O*** END CHG *** 09/24/04 ************************************
     O                       HLDAGT        Z     29
     O                                           39 'NUMBER OF'
     O                                           50 'EMPLOYERS:'
     O                       AGTCNT        Z     56
     O                       XXAGTL        J     80 '$'
     O* Total Line
     O          E            TOT         2  2
     O                                           39 'TOTAL NUMBER OF'
     O                                           50 'EMPLOYERS:'
     O                       XXCNT         Z     56
     O                       XXTOTL        J     80 '$'
      **** END   ADD **** 08/30/99 ************************************
     O***  Begin Add  ***  10/27/03  **************************************
     O*** BEG CHG ***** 04/18/11 ******************************************
     O*********** QSYSPRT2E  107           NOT2
     OQSYSPRT2  E            NOT2           1 06
     O*** END CHG ***** 04/18/11 ******************************************
     O                                         +005 '    L A T E     N O '
     O                                         +000 'T I C E   -   P A S '
     O                                         +000 'T     D U E     B A '
     O                                         +000 'L A N C E '
     O          E            NOT2           2
     O                       SEPA80            +005
     O          E            NOT2           1
     O                       XXACNM            +006
     O                                           61 'Run Date :'
     O                       XXRNDT              80 '  /  /    '
     O          E            NOT2           1
     O                       XXAAD1            +006
     O                                           61 'Policy # :'
     O*** BEG CHG *****  09/20/06  ************************************
     O***********                        61 'Policy # :'
     O              N66                          61 'Policy # :'
     O               66                          61 'Member # :'
     O*** END CHG *****  09/20/06  ************************************
     O                       X1PLCY              80
     O          E            NOT2           1
     O                       XXAAD2            +006
     O                                           61 'Division :'
     O                       EBDIV               80
     O          E            NOT2           1
     O                       XXAAD3            +006
     O                                           61 'Policy Yr:'
     O                       EBFYR         Z     80
      *
     O          E            NOT2           2
     O              N52                          61 'File #   :'
     O              N52      XXFIL#              80
     O          E            NOT2           0
     O                       XXENM2            +008
     O          E    77      NOT2        1
     O               77      XXFDNM            +008
     O          E            NOT2           1
     O          E            NOT2           1
     O                       XXADR1            +008
     O                                           61 'Due Date     :'
     O                       XXDUDT        Y   +001
     O          E            NOT2           1
     O                       XXADR2            +008
     O                                           61 'Past Due Date:'
     O                       XXPDDT        Y   +001
     O          E            NOT2           2
     O                       XXADR3            +008
     O          E            NOT2           3
     O                       SEPA80            +005
      *  Non-Audited Accts - Active
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  1    N71N74   NOT2
     O          E   N71N74N73NOT2           1
      *** END CHG ***** 04/18/11 **************************************
     O                                         +006 'We have not received'
     O                                         +000 ' your payment which '
     O                                         +000 'was due on'
     O                       XXDUDT        Y   +001
     O                                         +000 '.  To avoid'
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  1    N71N74   NOT2
     O          E   N71N74N73NOT2           1
      *** END CHG ***** 04/18/11 **************************************
      ***** BEG CHG ******* 09/17/07 **********************************
     O**********                       +006 'cancellation, please'
     O**********                       +000 ' send this payment b'
     O**********                       +000 'y'
     O**********      N73      XXPDDTY   58
     O**********       73      XXCXDTY   58
     O**********                       +000 '.  If payment has'
     O              N67                          26 'cancellation, please'
     O              N67                          46 ' send this payment b'
     O              N67                          47 'y'
     O              N73N67   XXPDDT        Y     58
     O               73N67   XXCXDT        Y     58
     O                 N67                       75 '.  If payment has'
     O               67                          26 'cancellation, paymen'
     O               67                          46 't must be received i'
     O               67                          62 'n our lockbox by'
     O              N73 67   XXPDDT        Y     73
     O               73 67   XXCXDT        Y     73
     O                  67                       74 '.'
      ***** END CHG ******* 09/17/07 **********************************
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  1    N71N74   NOT2
     O          E   N71N74N73NOT2           1
      *** END CHG ***** 04/18/11 **************************************
      ***** BEG CHG ******* 09/17/07 **********************************
     O**********                       +006 'already been mailed,'
     O**********                       +000 ' thank you for your '
     O**********                       +000 'payment; please disr'
     O**********                       +000 'egard this'
     O                 N67                       26 'already been mailed,'
     O                 N67                       46 ' thank you for your '
     O                 N67                       66 'payment; please disr'
     O                 N67                       76 'egard this'
     O                  67                       26 'If payment has alrea'
     O                  67                       46 'dy been mailed, than'
     O                  67                       66 'k you for your payme'
     O                  67                       77 'nt; please '
      ***** END CHG ******* 09/17/07 **********************************
      *** BEG CHG ***** 04/18/11 **************************************
     O******* E  2    N71N74   NOT2
     O          E   N71N74N73NOT2           2
      *** END CHG ***** 04/18/11 **************************************
      ***** BEG CHG ******* 09/17/07 **********************************
     O**********                       +006 'notice.'
     O              N67                          13 'notice.'
     O               67                          26 'disregard this notic'
     O               67                          28 'e.'
      ***** END CHG ******* 09/17/07 **********************************
      *** BEG ADD ***** 04/18/11 **************************************
      *
      *  Non-Audited Accts - Pending cancellation
      *
     O          E   N71N74 73NOT2           1
     O                                         +006 'We have not received'
     O                                         +000 ' your payment which '
     O                                         +000 'was due on'
     O                       XXDUDT        Y   +001
      ***** BEG CHG ******* 02/13/13 **********************************
     O************  N67                          75 '.  Please'
     O************   67                          77 '.  To avoid'
     O              N67                          76 '.  Please'
     O               67                          78 '.  To avoid'
      ***** END CHG ******* 02/13/13 **********************************
     O          E   N71N74 73NOT2           1
     O              N67                        +006 'send this payment by'
     O              N67      XXPDDT        Y   +001
     O              N67                        +000 '.  If payment has'
     O              N67                        +000 ' already been mailed,'
     O               67                          26 'cancellation, paymen'
     O               67                          46 't must be received i'
     O               67                          62 'n our lockbox by'
     O               67      XXPDDT        Y     73
     O               67                          74 '.'
     O          E   N71N74 73NOT2           1
     O                 N67                     +006 'thank you for your '
     O                 N67                     +000 'payment; please disr'
     O                 N67                     +000 'egard this notice.'
     O                  67                       26 'If payment has alrea'
     O                  67                       46 'dy been mailed, than'
     O                  67                       66 'k you for your payme'
     O                  67                       77 'nt; please '
     O          E   N71N74 73NOT2           2
     O              N67                        +001 ' '
     O               67                          26 'disregard this notic'
     O               67                          28 'e.'
      *** END ADD ***** 04/18/11 **************************************
      *  Non-Audited Accts - Cancelled as of the Balance Date
     O          E   N71 74   NOT2           1
     O                                         +006 'Your policy is under'
     O                                         +000 ' cancellation effect'
     O                                         +000 'ive'
     O                       XXCXDT        Y   +001
     O                                         +000 '.  We have not'
     O          E   N71 74   NOT2           1
     O                                         +006 'received your paymen'
     O                                         +000 't which was due on'
     O                       XXDUDT        Y   +001
     O                                         +000 '.  If payment has al'
     O                                         +000 'ready'
     O          E   N71 74   NOT2           3
     O                                         +006 'been mailed, thank y'
     O                                         +000 'ou for your payment;'
     O                                         +000 ' please disregard th'
     O                                         +000 'is notice.'
      *  Audited Accts that are Active or Cancel Pending
     O          E    71N74   NOT2           1
     O                                         +006 'Please note your aud'
     O                                         +000 'it balance is now pa'
     O                                         +000 'st due.  In order to'
     O                                         +000 ' avoid'
     O          E    71N74   NOT2           1
     O                                         +006 'cancellation, please'
     O                                         +000 ' remit the following'
     O                                         +000 ' balance immediately'
     O                                         +000 '.  If payment'
     O          E    71N74   NOT2           1
     O                                         +006 'has already been mai'
     O                                         +000 'led, thank you for y'
     O                                         +000 'our payment; please '
     O                                         +000 'disregard this'
     O          E    71N74   NOT2           2
     O                                         +006 'notice.'
      *  Audited Accts that are Cancelled
     O          E    71 74   NOT2           1
     O                                         +006 'Please note your can'
     O                                         +000 'cellation audit bala'
     O                                         +000 'nce is now past due.'
     O                                         +000 '  In order to'
     O          E    71 74   NOT2           1
     O                                         +006 'avoid any legal acti'
     O                                         +000 'on, please remit the'
     O                                         +000 ' following balance i'
     O                                         +000 'mmediately.'
     O          E    71 74   NOT2           1
     O                                         +006 'If payment has alrea'
     O                                         +000 'dy been mailed, than'
     O                                         +000 'k you for your payme'
     O                                         +000 'nt; please'
     O          E    71 74   NOT2           2
     O                                         +006 'disregard this noti'
     O                                         +000 'ce.'
      *  Common O-Specs restart here
     O          E            NOT2           2
     O                                         +006 'Outstanding Balance:'
     O          E            NOT2           1
     O               75 85                     +016 'Deposit Due'
     O               75 85   XXDEPD        K     65
     O          E            NOT2           1
     O                  86                     +016 'Premium Due:'
     O                  86   XXPRMD        K     65
     O          E            NOT2           1
     O                  87                     +016 'Expense Constant Due:'
     O                  87   XXEXCD        K     65
     O          E            NOT2           1
     O                  88                     +016 'Deductible Fees Due:'
     O                  88   XXDEDD        K     65
     O          E            NOT2           1
     O                  90                     +016 'State Tax Due:'
     O                  90   XXTAX         K     65
     O******** BEGIN ADD 05/15/07  ************************************
     O          E            NOT2           1
     O               53      XX1AHD            +016
     O               53      XX1STB        K     65
     O*
     O          E            NOT2           1
     O               54      XX2AHD            +016
     O               54      XX2NDB        K     65
     O*
     O          E            NOT2           1
     O               55      XX3AHD            +016
     O               55      XX3RDB        K     65
     O*
     O          E            NOT2           1
     O               56      XX4AHD            +016
     O               56      XX4THB        K     65
     O*
     O          E            NOT2           1
     O               57      XX5AHD            +016
     O               57      XX5THB        K     65
     O******** END ADD***05/15/07  ************************************
     O*
     O***  Begin Add  ***  12/11/08  **************************************
     O          E            NOT2           1
     O               58      XX6AHD            +016
     O               58      XX6THB        K     65
     O*
     O          E            NOT2           1
     O               59      XX7AHD            +016
     O               59      XX7THB        K     65
     O***  End   Add  ***  12/11/08  **************************************
     O*
     O          E            NOT2           0
     O                                           65 '_____________'
     O          E            NOT2           1
     O                  89                     +016 'Fees Due:'
     O                  89   XXFEED        K     65
     O          E            NOT2           2
     O                                         +016 'Total Amount Due:'
     O                       XXTOTD        K     65 '$'
     O          E            NOT2           3
     O                       SEPA80            +005
     O          E            NOT2           2
     O                                         +006 'Make Checks Payable to:'
     O                       OFCKNM            +002
     O          E            NOT2           2
     O                                         +006 'Serviced By:           '
     O                       XXACNM            +002
     O          E            NOT2           2
     O                                         +006 'Direct Inquiries to:   '
     O                       XXNAME              80
     O          E            NOT2           1
     O                       XXCM01            +006
     O          E            NOT2           1
     O                       XXCM02            +006
     O          E            NOT2           1
     O                       XXCM03            +006
     O          E            NOT2           1
     O                       XXCM04            +006
      *** BEG CHG ***** 04/18/11 ************************************
     O******* E  2             NOT2
     O          E            NOT2           1
      *** END CHG ***** 04/18/11 ************************************
     O                       XXCM05            +006
     O          E            NOT2           1
     O               73                        +008 'YOUR POLICY IS CURRE'
     O               73                        +000 'NTLY PENDING CANCELL'
     O               73                        +000 'ATION EFFECTIVE'
     O               73      XXCXDT            +001 '  /  /    '
      *** BEG CHG ***** 04/18/11 *********************************
     O******* E  2             NOT2
     O          E            NOT2           1
      *** END CHG ***** 04/18/11 *********************************
     O               73                        +008 'REASON: '
     O               73      XXRESN            +001
     O          E            NOT2           1
     O*** BEG CHG *** 09/24/04 ************************************
     O*******                          +006 'Agent:'
     O                       EXAHDR            +006
     O*** END CHG *** 09/24/04 ************************************
     O                       EBAGT#        Z   +001
     O                       EBANAM            +002
     O          E            PROD2          1
     O                                         +006 'Producer:'
     O                       ##PROD        Z   +001
     O                       XXPNAM            +002
     O***  End   Add  ***  10/27/03  **************************************
      *** begin add ***  01/10/17  ****************************************
     P*====================================================================
     P* Determine if the object exists
     P*====================================================================
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
      *** end add *****  01/10/17  ****************************************
