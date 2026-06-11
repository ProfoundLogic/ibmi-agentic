     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     h dftactgrp(*no)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7231R - AUDIT REQUESTS
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM IS USED TO GENERATE AUDIT REQUEST FORMS TO BE   *
     H*  SENT OUT TO ONE OR MORE EMPLOYERS ON CANCELLATION AUDITS     *
     H*  OR END OF THE YEAR AUDITS.                                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    ACCOUNTING REPORT MENU                                     *
     H*    UPDATE CANCELLATION PROGRAM (W0164R)                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SRNAME                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:09                                             *
     H*  CREATED:    12/13/93              PROJECT: WC197             *
     H*****************************************************************
     H*                                                               *
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  SMORL - OUTPUT ROUTING BY PGM     (PNAM/FIL#)                *
     H*  SMCOL - SYSTEM COMPANY FILE       (CO#)                      *
     H*  WMFNL - FUND MASTER FILE          (CO#/FND)                  *
     H*  WDF1L - FUND DETAIL FILE          (CO#/FND/FYR)              *
     H*  WMECL1- EMPLOYER CONTACTS FILE    (CO#/FND/EMP#/DIV/PCNT)    *
     H*  WMEML - EMPLOYER MASTER FILE      (CO#/FND/EMP#/DIV)         *
     H*  WMEPL - EMPLOYER PAYROLL FILE     (CO#/FND/EMP#/DIV/FYR/     *
     H*                                     EFDT/CLAS/SPEC)           *
     H*  WMOOL - OWNER/OFFICERS FILE       (CO#/FND/EMP#/DIV)
     H*  WDELL - EMPLOYER DETAIL FILE      (CO#/FND/EMP#/DIV/FYR)     *
     H*  WDELL4- EMPLOYER DETAIL FILE      (CO#/FND/PTDT/EMP#/DIV)    *
     H*  WMPRL1- PAYROLL CLASS CODE FILE   (CO#/EFDT -                *
     H*                                     OMIT STATUS = 'I')        *
     H*  WTABL - TABLE FILES               (CO#/KEY)                  *
     H*  WMAHL - AGENT HISTORY             (CO#/FND/EMP#/DIV/FYR/EFDT)*
     H*  WMAAL1- AGENT ASSIGNMENT          (CO#/FND/AGT#)             *
     H*  WMAGL - AGENT MASTER              (CO#/FEIN/MOD)             *
     H*  WMALL - ADDL ENTITY/LOCATION FILE (CO#/FND/EMP#/DIV/ELID)    *
     H*  WMA3L - EMPLR 3RD ADDRESS FILE    (CO#/FND/EMP#/DIV)         *
     H*  WMAUL - AUDIT TRACKING LOG FILE   (CO#/FND/FYR/EMP#/DIV/RQDT)*
     H*  SDCML - DARTS REMARK DTL FILE     (KEY/TRDT/TRTM/SEQ# -      *
     H*                                     SELECT STAT <> 'I')       *
     H*  SDCMLA- DARTS REMARK DTL FILE     (KEY/SEQ# -                *
     H*                                     SELECT STAT <> 'I')       *
     H*  STCTL1- DARTS REMARK TYPE FILE    (RMCD)                     *
     H*  WMUPL - USER PROFILE FILE         (USR)                      *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  08                                                           *
     H*  09                                                           *
     H*  18                                                           *
     H*  22                                                           *
     H*  23                                                           *
     H*  24                                                           *
     H*  25                                                           *
     H*  26   CONTROL PRINT OF HEADER INFORMATION                     *
     H*  27   CONTROL PRINT OF HEADER INFORMATION                     *
     H*  28   CONTROL PRINT OF ENTITY/LOCATION NUMBER                 *
     H*  29   USED BASED ON COMPILE LISTING                           *
     H*  33   CANCELLED DIVISION - DO NOT PRINT UNDER ADD'L INSUREDS  *
     H*  34   CONTROLS PRINTING OF UNDERLINE IN PAYROLL CX DATE       *
     H*  35   CONTROLS HOW MANY OWNER/OFF AND EXP MOD LINES TO PRINT  *
     H*  36   CONTROLS HOW MANY OWNER/OFF AND EXP MOD LINES TO PRINT  *
     H*  37   CONTROLS HOW MANY OWNER/OFF AND EXP MOD LINES TO PRINT  *
     H*  38   CONTROLS PRINTING OF EXP MOD #1                         *
     H*  39   CONTROLS PRINTING OF EXP MOD #2                         *
     H*  40   CONTROLS PRINTING OF EXP MOD #3                         *
     H*  41   IF SET ON, PRINTS NO MORE EXP MOD LINES                 *
     H*  42   CONTROLS PRINTING OF AGENT AND EXP MOD HEADINGS         *
     H*  45   If Client 109, Prints OFFICE Address on Form            *
     H*  50   MAIN READ INDICATOR FOR THE EMPLOYER FILE               *
     H*  51                                                           *
     H*  52   CONTROLS PRINTING OF DIV# OR ENT/LOC#                   *
     H*  55   MAIN READ INDICATOR FOR THE EMPLOYER PAYROLL FILE       *
     H*  60   GLOBAL INDICATOR USED FOR CHAIN/READS                   *
     H*  61                                                           *
     H*  62                                                           *
     H*  63                                                           *
     H*  65   SET ON IF FUND MAILING ADDRESS LINE 2 IS BLANK          *
     H*  66   ADDRESS LINE 2 FOR BOTH MAILING/PHYSICAL ADDRESS IS     *
     H*       BLANK (NOT USED AS OF 06/18/01)                         *
     H*  70                                                           *
     H*  71 EMPLOYER IS SELF-REPORTING
     H*  72                                                           *
     H*  73 ON WHEN USER REQUESTS S/R PAYROLL DATA                    *
     H*  75                                                           *
     H*  76                                                           *
     H*  80                                                           *
     H*  81                                                           *
     H*  82                                                           *
     H*  83                                                           *
     H*  84                                                           *
     H*  88 - PRINT ANNIVERSARY RATING DATES BELOW EACH CLASS CODE    *
     H*  90                                                           *
     H*  91                                                           *
     H*  95                                                           *
     H*  96                                                           *
     H*  99                                                           *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/09/25 C:291 Support: PHW119025                             *
     H*                Fix File lock on WASRL8 when Scriptura is      *
     H*                requested.                                     *
     H*                                                               *
     H* 06/17/24 C:344 Project: WC2952                                *
     H*                Increase rate field to zoned 8.3.              *
     H*                                                               *
     H* 07/30/23 C:062 Project: WC2918                                *
     H*                Add exp od 2 and 3 not rated logic.            *
     H*                                                               *
     H* 03/18/23 C:300 Support: 107185 (UBIC)                         *
     H*                Audit Request Email Body not correct when call *
     H*                from Cancellation Confirmation                 *
     H*                                                               *
     H* 06/16/21 C:79  Project: WC2671                                *
     H*                BA's want to display exp mod 1 heading         *
     H*                regardless of split so I uncommented some of   *
     H*                the code that was commented out on 06/10/21    *
     H*                                                               *
     H* 06/15/21 C:65  Project: WC2671                                *
     H*                If called from CX confirmations then pull mass *
     H*                xml file name from LDA to link audit requests  *
     H*                togther with those documents.  Do not call     *
     H*                Scriptura from this program as the call        *
     H*                will happen from CX confirmation.  If called   *
     H*                from CX confirmations and all divisions not    *
     H*                requested, then produce all divisions in a     *
     H*                single document for state rpt div.             *
     H*                                                               *
     H* 06/10/21 C:65  Project: WC2671                                *
     H*                Do not display exp mod 1 heading for non-split *
     H*                policy and always print eff/exp dates for      *
     H*                payroll records, even if not split.            *
     H*                                                               *
     H* 10/28/20 C:079 Project: WC2671                                *
     H*                Add Group Select and Scriptura Processing      *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420                                 *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 05/08/19 C:300 Support: 78170 (LUBA)                          *
     H*                Audit Requests - Job Failure while printing    *
     H*                WC000311A endorsement form                     *
     H*                                                               *
     H* 04/30/19 C:062 Project: 152WAB                                *
     H*                Apply Optacomp Mods.                           *
     H*                                                               *
     H* 01/14/19 C:291 Support: 102WHW                                *
     H*                Add 'Region' to the entry parameters, process  *
     H*                and print Region selection on the routing page.*
     H*                                                               *
     H* 01/22/18  C:33  Support: 50992                                *
     H*                 Clear payroll values for single audit request *
     H*                  without payrolls (but inactive exists)       *
     H*                                                               *
     H* 11/24/16 C:309 Support: 44341                                 *
     H*                Deleted logic for Checking Owner/Officer name  *
     H*                and Coverage flag in array NM & IE for printing*
     H*                on Audit Request Form as this was causing issue*
     H*                by skipping Owners/Officers record having same *
     H*                name and Coverage flag as as that of the first *
     H*                occurence of that particular Owner/Officer name*
     H*                                                               *
     H* 10/31/16 C:136 Support: 39381                                 *
     H*                Print office information at top of page only   *
     H*                if client 109 (Don't use *IN45 for this)       *
     H*                                                               *
     H* 12/18/15 C:180 Support 38849 - Change audit tracking run      *
     H*                options so that re-run flag applies to both    *
     H*                single and mass runs.                          *
     H*                                                               *
     H* 11/24/15 C:180 Support 38461 - Update tracking on Mass option.*
     H*                                                               *
     H* 08/17/15 C:65  Phase# 36741 (Luba) Ref: 147WBR                *
     H*                Condition write to PDF printer file on it      *
     H*                being open.                                    *
     H*                                                               *
     H* 06/02/15 C:306 Support PHW35123 (Recompliation only)          *
     H*                -Owner/Officer text was getting overlapped     *
     H*                 for Multiple Owner/Officer.                   *
     H*                                                               *
     H* 05/04/15 C:298 Support: 34627                                 *
     H*                Don't write to qsysprt or qsysprt1 if not open *
     H*                                                               *
     H* 02/23/15 C:282 Project: 114WMZ                                *
     H*            Add Parm 'Requests to Process' and 'Previously Run'*
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/22/14 C:180 Support: WA102 86474                           *
     H*                Fix class detail spacing. PRTF changes only.   *
     H*                No changes to this pgm source.                 *
     H*                                                               *
     H* 07/16/14 C:136 Project: WC2295                                *
     H*                Add OK Rate Split for 02/01/2014               *
     H*                                                               *
     H* 06/16/14 C:136 Project: WC2283                                *
     H*                Apply TN Rate Split                            *
     H*                                                               *
     H* 05/27/14 C:180 Support: WA099 85353                           *
     H*                Fix printing of reported payrolls.             *
     H*                                                               *
     H* 10/07/13 C:190 Project: 25WSS                                 *
     H*                Change the program to determine if             *
     H*                Premium or Contribution should be printed      *
     H*                based on Group setup.                          *
     H*                                                               *
     H* 10/24/13 C:283 Support: WA114 83955                           *
     H*                Recompile Only. Printer file changed.          *
     H*                                                               *
     H* 09/26/13 C:283 Support: WA114 83591                           *
     H*                Recompile Only. Printer file changed.          *
     H*                                                               *
     H* 07/03/13 C:190 Project: 147WBR                                *
     H*                Change the RPG program to use the printer file *
     H*                instead of the output specs.                   *
     H*                Include a page segment statement to print the  *
     H*                logo. Pull the logo from Group Auxiliary.      *
     H*                                                               *
     H* 06/12/13 C:283 Support: WA114 82549                           *
     H*                Correctly pass thefDivision for additional     *
     H*                location/entity information printing.          *
     H*                                                               *
     H* 01/17/13 C:277 Support: WA102 81052                           *
     H*                Print Monthly report in PDF also               *
     H*                                                               *
     H* 05/07/12 C:283 Support: WA071 80137                           *
     H*                Recompile only.                                *
     H*                                                               *
     H* 04/10/12 C:282 PROJECT: 114WEG Priss - 17                     *
     H*                Adding the process for Multiple Employer       *
     H*                when Audit request run for Mass process.       *
     H*                                                               *
     H* 03/13/12 C:282 PROJECT: 114WEG                                *
     H*                Procedure PrtPolEnd added to print the         *
     H*                spool file. Procedure ResSplDDoc added to      *
     H*                resolve spool into document.                   *
     H*                                                               *
     H* 12/20/11 C:190 PROJECT: WC2077                                *
     H*                Print the Entity/Location Federal name         *
     H*                (both pieces), if not blank, in addition       *
     H*                to the Entity/Location DBA.                    *
     H*                                                               *
     H* 10/26/11 C:136 Support: WA114 78313                           *
     H*                Move logic that prevents billing contact       *
     H*                info from being pulled for CSI to SRADDR       *
     H*                                                               *
     H* 08/16/11 C:09  SUPPORT: 77551 WA114                           *
     H*                Current logic already exists to exclude        *
     H*                Owners/Officers whose Expiration Date falls    *
     H*                on or after the Policy Expiration Date.        *
     H*                New logic has been added to exclude Owners/    *
     H*                Officers whose Effective Date falls on or      *
     H*                after the Policy Expiration Date.              *
     H*                                                               *
     H*                                                               *
     H* 03/21/11 C:189 SUPPORT: 75467 WA139                           *
     H*                Federal id is not printing in the resolved     *
     H*                output                                         *
     H*                                                               *
     H* 03/04/11 C:136 Support: WA045 75953                           *
     H*               -Fix audit tracking issues                      *
     H*               --Don't clear audit co and type when writing    *
     H*                 cx audit tracking record                      *
     H*               --Correct auto assignment routines in WADTTRKCSR*
     H*               -Update logic for retrieving renewal status     *
     H*                                                               *
     H* 12/28/10 C:62  PROJECT: WA139 75309                           *
     H*                Expand Arrary to 999.                          *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                Print the HCN credit.  The credit will be      *
     H*                printed for each division and will list all    *
     H*                the opt in/opt out periods.                    *
     H*                                                               *
     A* 11/01/10  C:255 Project: 71WLA :Recompile only                *
     H*                                                               *
     H* 10/18/10 C:251 Project: WC1939                                *
     H*                09/01/2010 OK Rate split                       *
     H*                If a policy with OK coverage is unexpired as   *
     H*                of 09/01/10 then the policy will need to be    *
     H*                split for the rate decrease                    *
     H*                Fix bug with rate split header printing        *
     H*                                                               *
     H* 09/07/10 C:255 PROJECT: WC1929                                *
     H*                Add changes in program to check flag 'Disable  *
     H*                DE Rate flag' based on this, changes for proje-*
     H*                ct WC1768 are disabled.                        *
     H*                                                               *
     H* 05/31/10 C:189 SUPPORT: WA114 72932                           *
     H*                ONLY FIRST LOCATION IS GETTING PRINTED FOR     *
     H*                NON MAIN DIVISION                              *
     H*                                                               *
     H* 05/03/10 C:220 PROJECT: WC1881                                *
     H*                Add Ctrl flag to Final Audit Request Form      *
     H*                                                               *
      *
      * 04/21/10  C:61 SUPPORT WA093 72435
      *                Enable 46WGB changes for clt093.
     H*                                                               *
     H* 04/07/10 C:235 SUPPORT: WA114 72232                           *
     H*                FIX THE ISSUE OF AUDIT REQUEST PAGES HAVING    *
     H*                ALIGNMENT ISSUES                               *
     H*                                                               *
     H* 02/16/10 C:136 PRISS: 139WNR #16                              *
     H*                CORRECT PAYROLL AMOUNTS IN DETAIL AND IN       *
     H*                MONTHLY BREAKOUT                               *
     H*                                                               *
     H* 02/15/10 C:191 Project: 25WRC                                 *
     H*                Convert to RPGLE                               *
     H*                                                               *
     H* 02/10/10 C:235 SUPPORT: WA114 71286                           *
     H*                FIX THE ISSUE OF AUDIT REQUEST SOME PAGES      *
     H*                ARE PRINTING WITH INCOMPLETE DETAILS WHERE     *
     H*                POLICY HAVING MULTIPLE DIVISION WITH STATUS    *
     H*                WRITTEN AND CANCELLED.                         *
     H*                                                               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/21/10 C:235 SUPPORT: WA114 71286                           *
     H*                FIX THE ISSUE OF AUDIT REQUEST SOME PAGES      *
     H*                ARE PRINTING WITH INCOMPLETE DETAILS .         *
     H*                                                               *
     H* 01/18/10 C:232 SUPPORT: WA071 71168                           *
     H*                CORRECT THE LOGIC THAT PULLS IN THE DESCRIPTION*
     H*                FROM THE PAYROLL CLASS TABLE. IT CURRENTLY     *
     H*                GETS THE OLDEST RECORD FOR THE CLASS INSTEAD   *
     H*                OF THE ONE THAT CORRESPONDS TO THE RATING      *
     H*                DATE OF THE CLASS.                             *
     H*                                                               *
     H* 01/11/10  C:189  PROJECT: 93WIF                               *
     H*                  SSN REPRINTING IN MASS AUDIT REQUEST         *
     H*                                                               *
     H* 12/29/09 C:136 PROJECT: WC1861                                *
     H*                ADD AZ RATE SPLIT - 01/01/2010                 *
     H*                                                               *
     H* 11/23/09 C:136 PROJECT: 139WNR                                *
     H*                FL RATE SPLIT OF 07/01/09 - CLIENT SPECIFIC    *
     H*                                                               *
     H* 10/16/09 C:232 Seq#: WA139 69467                              *
     H*                 Fix the issue of Rerun of Audit request for   *
     H*                 prior months.                                 *
     H*                                                               *
     H* 09/17/09 C:220 Seq#: 69067                                    *
     H*                 Duplicate audit tracking record.              *
     H*                                                               *
     H* 07/28/09  C:189  PROJECT: 93WIF                               *
     H*                  CHANGE SSN DISPLAY ON AUDIT REQUEST          *
     H*                                                               *
     H* 05/18/09 C:189 SUPPORT: WA102/68277                           *
     H*                When an account cancels during polcy years     *
     H*                the Audit Request puts annual estimated        *
     H*                payrolls on the front sheet even its S/R       *
     H*                                                               *
     H* 05/01/09 C:189 SUPPORT: WA102/68278                           *
     H*                Owner officers with Same name but different    *
     H*                Include/Exclude status are being skipped       *
     H*                                                               *
     H* 04/13/09 C:191 SUPPORT: WA102/67875                           *
     H*                Print additional Entity/Location name field    *
     H*                (ALNAM2) when printing Other Workplace         *
     H*                information on the Audit Request.              *
     H*                                                               *
     H* 04/02/09 C:191 SUPPORT: WA102/67875                           *
     H*                Indicator 46 usage conflicting - change it for *
     H*                client id 102 - *IN52 used now                 *
     H*                                                               *
     H* 12/18/08 C:33  Project: 102WEP  WA102/66468                   *
     H*                Include the loss ctl rep on the resolved doc.  *
     H*                                                               *
     H* 12/16/08 C:136 PROJECT: WC1768                                *
     H*                DE RATE SPLIT FOR 10/01/08                     *
     H*                                                               *
     H* 09/10/08 C:61  PROJECT 46WGB                                  *
     H*                Print "Designated Excluded Workplaces" info    *
     H*                with Audit Request.                            *
     H*                                                               *
     H* 08/21/08 C:191 Project: 102WEP                                *
     H*                Back to Base                                   *
     H*                 - Various changes (#1, #4)   : Base Option    *
     H*                 - Various changes (#2, #3)   : Client Flagged *
     H*                 - Print S/R payroll by month : Base Option    *
     H*                 - Add FEIN#                  : Base Option    *
     H*                                                               *
     H* 06/20/08 C:136 SUPPORT: WA139 64055                           *
     H*                CORRECT THE LOGIC THAT PULLS IN THE DESCRIPTION*
     H*                FROM THE PAYROLL CLASS TABLE. IT CURRENTLY     *
     H*                GETS THE OLDEST RECORD FOR THE CLASS INSTEAD   *
     H*                OF THE ONE THAT CORRESPONDS TO THE RATING      *
     H*                DATE OF THE CLASS.                             *
     H*                                                               *
     H* 03/12/08 C:183 PRISS#5 71WJB                                  *
     H*                Change the Audit Period Date Correction.       *
     H*                                                               *
     H* 02/20/08 C:33  Seq#: WA071/62388                              *
     H*                Get the correct Cx date.                       *
     H*                                                               *
     H* 02/12/08 C:61  SUPPORT WA045 59912                            *
     H*              - Revised requirement for printing "Policy #"    *
     H*                and "File #"                                   *
     H*                                                               *
     H* 01/23/08 C:180  SUPPORT 61880
     H*                 Move down Employer address one line.
     H*
     H* 01/11/08 C:136 SUPPORT: WA090 61870                           *
     H*                PHONE NUMBER AND DIVISION NOT PRINTING         *
     H*                IN HEADER                                      *
     H*                                                               *
     H* 01/02/08 C:33  Project: WC1196                                *
     H*                If the Business Category has not yet been      *
     H*                 assigned, attempt to assign it (Rev 1.7).     *
     H*                                                               *
     H* 12/11/07 C:136 SUPPORT: WA071 61419 (71WIQ)                   *
     H*                CLEAR DSCLAS MODS BEFORE POPULATING TO         *
     H*                PREVENT CARRYOVER                              *
     H*                                                               *
     H* 12/10/07  C:33  Project: 71WIQ   WA071/61432                  *
     H*                 Use positioning for the address fields.       *
     H*                                                               *
     H* 12/05/07 C:136 SUPPORT: WA071 61362                           *
     H*                DIVISION FIELD NOT PRINTING CORRECTLY          *
     H*                                                               *
     H* 11/27/07 C:33  Seq#: WA114/61178                              *
     H*                For CLT114, do not use Billing Address.        *
     H*                                                               *
     H* 11/27/07 C:136 SUPPORT: WA109 61102                           *
     H*                FIX OUT OF BOUNDS ERROR FOR OWNER'S/OFFICERS   *
     H*                                                               *
     H*  10/24/07  C:189 PROJECT-132WIT                               *
     H*                  Automate Audit Req/Loss Exp/Darts Aggregate  *
     H*                                                               *
     H*  10/03/07  C:193 PROJECT: 71WIQ                               *
     H*                  INCL ECM PREPRTED FORM/BASE AUDIT FORM       *
     H*                                                               *
     H* 09/10/07 C:61  SUPPORT WA045 59912                            *
     H*                Print "File #" if "Carrier Policy #" not       *
     H*                entered.                                       *
     H*                                                               *
     H* 08/26/07 C:61  SUPPORT 57880                                  *
     H*                Change image flag on DARTS output to indicate  *
     H*                PDF format.                                    *
     H*                                                               *
     H*  07/16/07  C:190 Project: 93WGX
     H*               -Modify format of "POLICY NUMBER" to print as:  *
     H*                Group(3)-Employer(5)-Division(2)-Year(4).      *
     H*               -Add a new section for "SPECIAL AUDIT INFO" to  *
     H*                print just prior to the classifications.       *
     H*               -Remove comment fields that currently print.    *
     H*                In place of the comments, print the following: *
     H*                KEY RISK CONTACT, PHONE #, E-MAIL.             *
     H*               -Add a new section called "CLAIMS INFORMATION". *
     H*               -Change Contact fields to use the Underwriter   *
     H*                KEY RISK CONTACT, PHONE #, E-MAIL.             *
     H*               -Modify Owner-Officer logic to only print if    *
     H*                Expiration date = zero, bypass if > zero.      *
     H*               -Add accident date on claims information lines  *
     H*                and remove the claim numbers.                  *
     H*                                                               *
     H* 07/23/07 C:189 SUPPORT 58985                                  *
     H*               For The Division All Unique Owner/Officers Should*
     H*               Be Shown But For Entity Only Their Owner/Officer*
     H*               should be shown.Changing the code for WC1519.   *
     H*                                                               *
     H* 07/12/07 C:33  Project: WC1196  (PRISS# 24)                   *
     H*               -Process single Audit Requests regardless of the*
     H*                 'Mass Audit Request' flag                     *
     H*                                                               *
     H* 06/14/07 C:33  Project: WC1196  (PRISS# 7 WC1519)             *
     H*                Allow for audit request to produce without an  *
     H*                 audit company being assigned.                 *
     H*                                                               *
     H* 05/15/07 C:183 Project: WC1519                                *
     H*                Changes to Owner/Officer information Logic.    *
     H*                                                               *
     H* 05/08/07 C:183 Project: WC1519                                *
     H*                Introduce Expiration date in the Audit Request *
     H*                form.                                          *
     H*                                                               *
     H* 04/11/07 C:33  Project: WC1196  WA099/57880                   *
     H*                Produce a PDF to be resolved with the DARTS    *
     H*                 remark.                                       *
     H*                                                               *
     H* 04/09/07 C:180 SUPPORT: WA109 54387                           *
     H*                Re-align delivery and return addresses.        *
     H*                                                               *
     H* 11/16/06 C:180 114WMF
     H*                Add Employer Contact phone # to print on the   *
     H*                Audit Request and add spacing as requested     *
     H*                by Jennifer Stanley. SEARCH FOR DATE 03/26/04  *
     H*
     H*                Check Employer Contacts file and if one of the *
     H*                contacts is designated as primary contact #1,  *
     H*                then print this contact, else use A3CONT as    *
     H*                being done currently. This is only program in  *
     H*                series being modified for this change. Always  *
     H*                print primary contact #1 even if no billing    *
     H*                record. Also print Estimated annual premium    *
     H*                SEARCH FOR DATE 03/02/04.
     H*
     H* 10/02/06 C:33  Project: WC1196                                *
     H*               -Restrict Audit Requests for only New Business, *
     H*                 Renewal Business or both.                     *
     H*               -When running mass audit requests, do not       *
     H*                 regenerate an audit request if the flag in    *
     H*                 Employer Detail indicates that the audit      *
     H*                 request has already been run.                 *
     H*               -If the user specified 'Y' for "Update Audit    *
     H*                 Tracking" then assign Audit Co. if one is to  *
     H*                 be assigned.                                  *
     H*
     H* 06/20/06 C:132 SUPPORT: WA045 51918                           *
     H*                Do not list Owner/Officer on report if the     *
     H*                Expiration date is less that policy start date.*
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 01/25/06 C:33  Seq#: WA129/51061                              *
     H*                Correct page breaks for 'Additional Insured'.  *
     H*                                                               *
     H* 01/09/06 C:33  Seq#: WA109/50422                              *
     H*                Only print QSYSPRT1 routing page if needed.    *
     H*                                                               *
     H* 12/27/05 C:132 Project: WA129 - 50522                         *
     H*                Chg to only print office info on audit request *
     H*                if is client 109, print mailing of others.     *
     H*                                                               *
     H* 12/16/05 C:167 Project: 129WMR                                *
     H*                Audit Request - Print Primary Contact.         *
     H*                Added call to RTVCLTID for client specific     *
     H*                conditional processing.                        *
     H*                                                               *
     H* 12/09/05 C:53  SUPPORT: 50251                                 *
     H*                PREVENT AUTO ASSIGNMENT PROCESSING.
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109BY}                                        *
     H*               -Print the office name, address and phone       *
     H*                 number on the audit request.                  *
     H*               -Ignore employer detail for which an initial    *
     H*                 billing has never been run.                   *
     H*                {109CX}                                        *
     H*               -Add signature lines to bottom of last page.    *
     H*                {109CZ}                                        *
     H*               -Move phone number to bottom of address array   *
     H*                 so it doesn't show in envelope window.        *
     H*                                                               *
     H* 03/23/05 C:53  PROJECT: WC1196                                *
     H*                ADDED AUTO ASSIGNMENT PROCESSING.              *
     H*                                                               *
     H* 06/03/05 C:136 SUPPORT: WA114 47412                           *
     H*                STRAIGHTEN OUT EMPLOYER CONTACT INFO SO THAT   *
     H*                EVERYTHING PRINTS.                             *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                PRINT EXTENDED EMPLOYER NAME FIELDS.           *
     H*                                                               *
     H* 10/10/03 C:65  PROJECT: SUPP/WC1123/WC1149                    *
     H*                ENSURE RATING DATE ARRAYS ARE NOT FILLED WITH  *
     H*                RATE SPLIT DATES WHEN THE SPLIT IS NOT VALID.  *
     H*                                                               *
     H* 10/07/03 C:65  PROJECT: WC1149                                *
     H*                SPLIT PAYROLLS FOR FL 10/1/3 RATE CHANGE.      *
     H*                                                               *
     H* 04/30/03 C:65  PROJECT: WC1123                                *
     H*                SPLIT PAYROLLS FOR FL 4/1/3 RATE CHANGE.       *
     H*                                                               *
     H* 05/01/03 C:61  SUPPORT: WA122 36954                           *
     H*                Produce audit request for policy year if       *
     H*                cancel date is later than policy exp date.     *
     H*                                                               *
     H* 05/30/02 C:136 SUPPORT: WA033 SEQ: 32755                      *
     H*                Only prints first payroll record if single     *
     H*                employer is being run.                         *
     H*                                                               *
     H*                                                               *
     H* 04/02/02 C:90  SUPPORT WA114 31853                            *
     H*                When running for a single employer, do not     *
     H*                skip to next if no payroll records are         *
     H*                found.                                         *
     H*                                                               *
     H* 08/23/01 C:61  SUPPORT WA113 28673                            *
     H*                For Additional Entity/Locations: Print both    *
     H*                The Physical and Mailing address if not blank. *
     H*                                                               *
     H* 08/15/01 C:90  PROJECT: WC1035                                *
     H*                Modify report output to fit rate field change  *
     H*                from 5.2 to 7.2 .                              *
     H*                                                               *
     H* 08/08/01 C:90  PROJECT: WC1028                                *
     H*                Print "File Number" instead of "Policy Number" *
     H*                if carrier policy number has been asigned.     *
     H*                                                               *
     H* 06/18/01 C:135 PROJECT WC883                                  *
     H*                Display a prompt to ask if the user wants to   *
     H*                use self-reporting payrolls for self-          *
     H*                reporting employers                            *
     H*                                                               *
     H* 05/04/01 C:136 PROJECT WC903                                  *
     H*                Print division# or entity/location# next       *
     H*                next to additional insureds.                   *
     H*                                                               *
     H* 03/27/01 C:61  SUPPORT WA071 26839                            *
     H*                Fix print of ADDITIONAL INSURED.               *
     H*                                                               *
     H* 02/21/01 C:65 PROJECT: WC1026                                 *
     H*               PRINT MERIT RATING FOR OUTPUT IF APPLICABLE.    *
     H*                                                               *
     H* 10/16/00 C:61  SUPPORT WC999 24676                            *
     H*                Correct new page start for multi page reports. *
     H*                                                               *
     H* 07/21/00  C:90  PROJECT: SUPP WA033/23559                     *
     H*                 Load anniversary date array PDAT with 7       *
     H*                 digit date format.                            *
     H*                                                               *
     H* 07/03/00  C:87  PROJECT: SUPP WA102 #23337                    *
     H*                 Added code to not process for split payroll   *
     H*                 if exp mods were not effective before policy  *
     H*                 to date (in addition to original criteria)    *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 04/19/00  C:87  PROJECT: SUPP WA093 #22620                    *
     H*                 Chgd to assign xxptdt to AUPTDT so that canx  *
     H*                 dates are used when appropriate.              *
     H*                                                               *
     H* 04/03/00  C:118 SUPPORT: WA091/22385                          *
     H*                 Revise chg made 1/17/00 to include case where *
     H*                 policy has been cancelled in future policy    *
     H*                 year.                                         *
     H*                                                               *
     H* 02/29/00  C:118 PROJECT: WC577                                *
     H*                 LOAD Q1PGM INTO CMIPGM                        *
     H*                                                               *
     H* 01/17/00  C:118 SUPPORT: WA095/21200                          *
     H*                 Check cancellation date, if LT policy from    *
     H*                 date than load policy from date in policy to  *
     H*                 date field for printing policy period.  If    *
     H*                 cancellation is GT from date, dates will load *
     H*                 as they do presently.                         *
     H*                                                               *
     H* 01/14/00  C:87  PROJECT: SUPP #21299                          *
     H*                 Wrong division printing on 2 page of multiple *
     H*                 division employers.                           *
     H*                                                               *
     H* 10/21/99  C:87  PROJECT: SUPP                                 *
     H*                 Clear contact name for each employer          *
     H*                                                               *
     H* 10/12/99 C:87  PROJECT: WC705                                 *
     H*                Add carrier policy # to header info.           *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 08/25/99  C:87  PROJECT: SUPP                                 *
     H*                 On a cancellation audit request, if an audit  *
     H*                 tracking record already exists and is being   *
     H*                 inactivated so that a new one can be written, *
     H*                 carry over the audit company to the new rec.  *
     H*                                                               *
     H* 08/17/99 C:61  PROJECT CMO25                                  *
     H*                Load policy year in DART remark.               *
     H*                                                               *
     H* 07/09/99 C:118  PROJECT: WC577                                *
     H*                 Move secondary contact to audit contact, leave*
     H*                 blank if no secondary entered.                *
     H*                                                               *
     H* 07/07/99  C:87  PROJECT: SUPP WA093 #18450                    *
     H*                 Modified pgm to only print other divisions    *
     H*                 under the 'Additional insureds' section of    *
     H*                 report if the division has a policy year and  *
     H*                 it is not flat cancelled.                     *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*
     H* 03/31/99  C:87  PROJECT: SUPP WC999 #17423                    *
     H*                 Some of the dates for split payrolls were 6   *
     H*                 digit in an 8 digit field.  Changed to cvt    *
     H*                 to 8 digit dates.                             *
     H*                                                               *
     H* 03/15/99  C:87  PROJECT: SUPP #16936                          *
     H*                 Audit canx flag (AUCXFL) was not being set on *
     H*                 correctly because arrays EFF and CNX were not *
     H*                 being populated with anything.                *
     H*                                                               *
     H* 02/02/99  C:87  PROJECT: SUPP # 16362                         *
     H*                 Field for city,state,zip was not being        *
     H*                 cleared for new entry.                        *
     H*                                                               *
     H* 01/08/99  C:87  PROJECT: SUPP #15442                          *
     H*                 Experience mods and their effective dates     *
     H*                 were not printing sometimes.                  *
     H*                                                               *
     H* 01/07/98  C:113  PROJECT: SUPPORT #15807                      *
     H*                  Report not printing correctly over multiple  *
     H*                  pages.  Corrected XXLINE count.              *
     H*                                                               *
     H* 12/21/98  C:87   PROJECT: SUPP #15442                         *
     H*                  Assigned empl name when printing additional  *
     H*                  divs under additional insureds section.      *
     H*                                                               *
     H* 12/15/98  C:87  PROJECT: SUPP #14537                          *
     H*                 Date range for experience mod should start    *
     H*                 on exp mod eff date if it is greater than     *
     H*                 policy effective date.                        *
     H*                                                               *
     H* 11/05/98  C:87  PROJECT: SUPP #14820                          *
     H*                 Changed to produce reports for accounts which *
     H*                 are cancelled if ELPTDT=CX DT and if the      *
     H*                 account is not audited.                       *
     H*                                                               *
     H* 09/17/98  C:87  PROJECT: SUPP WA090/#13574                    *
     H*                 CHANGED OUTPUT SO THAT OUTPUT PRINTING IN     *
     H*                 COLUMN 81 IS ADJUSTED TO PRINT IN COL 80      *
     H*                                                               *
     H* 08/11/98  C:65  PROJECT: WC843                                *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*                                                               *
     H* 06/04/98  C:90  PROJECT: WC577                                *
     H*                 REPOSITIONED EXPERIENCE MOD ON REPORT         *
     H*                                                               *
     H* 06/03/98  C:87  PROJECT: SUPP#12011                           *
     H*                 PROGRAM IS INITIALIZING AUDIT RECEIVED DATE   *
     H*                 TO TODAYS DATE.  IT SHOULD BE LEFT BLANK.     *
     H*                                                               *
     H* 04/17/98  C:87  PROJECT: SUPP#11174                           *
     H*                 WHEN AUDIT RUNS TWO PAGES - AGENT NAME AND    *
     H*                 NUMBER DO NOT SHOW ON SUBSEQUENT AUDITS.      *
     H*                 RESET *IN42 AT START OF LOOP TO CORRECT.      *
     H*                                                               *
     H* 02/23/98  C:85  Project: WC577                                *
     H*                 ADDED FIELD CMSMCD, LOADED FROM CTSMCD.       *
     H*                                                               *
     H* 01/22/98  C:35  PROJECT: WC734                                *
     H*                 CORRECTED PROBLEM WHERE FIRST OWNER/OFFICER   *
     H*                 WAS BEING SKIPPED.  ALSO MADE A CHANGE SO     *
     H*                 THAT OWNER/OFFICERS ASSOCIATED WITH AN ADDT'L *
     H*                 LOCATION WOULD PRINT UNDER THAT LOCATION ONLY *
     H*                 INSTEAD OF ALSO PRINTING WITH THE PRIMARY     *
     H*                 LOCATION OWNER/OFFICERS.                      *
     H*                                                               *
     H*  12/08/97 C:79 PROJECT WC734                                  *
     H*                ADD CANCELLATION DATE, AGENT ADDRESS AND PHONE,*
     H*                AND COMMENTS                                   *
     H*                                                               *
     H* 11/12/97 C:80 PROJECT WC734                                   *
     H*               CHANGED CODE SO BOTH COMMERCIAL AND NON         *
     H*               COMMERCIAL NUMBERS ARE DISPLAYED IN THE FORMAT  *
     H*               FNDEMPLRFYR                                     *
     H*                                                               *
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H*  04/17/97 C:57 PROJECT:CM025                                  *
     H*                IF SINGLE EMPLOYER RUN; ALLOW EMPLOYERS THAT   *
     H*                HAVE BEEN CANCELLED IN A PRIOR YEAR.           *
     H*                                                               *
     H*  03/03/97 C:57 PROJECT:CM025                                  *
     H*                OK TO PRINT EMPLOYERS WITH CANCEL DATES THAT   *
     H*                ARE GREATER THAN ELPTDT (POL TO DATE), AS      *
     H*                THESE ARE CANCELLATIONS OF A DIFFERENT POLICY. *
     H*                                                               *
     H*  02/21/97 C:57 PROJECT:CM025                                  *
     H*                DO NOT PRINT CANCELLED DIVISIONS UNDER         *
     H*                ADDITIONAL INSUREDS.                           *
     H*                                                               *
     H*  12/27/96 C:57 PROJECT:CM025                                  *
     H*              - GENERAL SOURCE CLEANUP.                        *
     H*              - ALLOW FOR DATE RANGE SELECTION OR INDIVIDUAL   *
     H*                EMPLOYER (FYR/EMP#/DIV).                       *
     H*              - GENERATE DARTS COMMENTS.                       *
     H*              - USE ADDRESS FROM EMPLOYER BILLING ADDRESS FILE *
     H*                (WMA3P), IF ONE EXISTS FOR THIS EMPLOYER.      *
     H*              - ADD ROUTING PAGE                               *
     H*              - ADD WC AUDIT TRACKING LOG FUNCTIONALITY        *
     H*              - ADDED A BLANK COLUMN FOR ENTRY OF # OF EMPLOYEE*
     H*                                                               *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWMOOL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDELL4    IF   E           K DISK    INFDS(ELFDS4)
     F                                     RENAME(PEL01:PEL02)
     FWDELP     UF   E             DISK
     F                                     RENAME(PEL01:PEL0P)
     F* Needed for WADTTRK* copybook
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWMPRL1    IF   E           K DISK
     FWDF2L     IF   E           K DISK    RENAME(PF201:PF201)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PRT01)
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMA3L     IF   E           K DISK
     FWMAUL     UF A E           K DISK
     FSTCTL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMPXL1    IF   E           K DISK
     F* Files (WMRSL) found in copybook (WADTTRKFSR)
     FWMRPL     IF   E           K DISK
     FWASRL4    IF   E           K DISK
      * Office Master
     FWMOFL     IF   E           K DISK
     FWMCML     IF   E           K DISK
     FWMAIL     IF   E           K DISK
     FWMECL1    IF   E           K DISK
      *
      *  Excluded Workplace
     FWMEWL1    IF   E           K DISK
     F/COPY WADTTRKFSR
     fW7231O1   O    E             PRINTER OFLIND(*IN94)
     F                                     USROPN
     fW7231O    O    E             PRINTER OFLIND(*IN97)
     F                                     USROPN
     fW7231O2   O    E             PRINTER OFLIND(*IN98)
     F                                     USROPN
     F*** Beg Chg *** 10/28/20 ****************************************
     F***WDELL5    IF   E           K DISK
     FWDELL5    IF   E           K DISK    Infds(EL5fDs)
     F*** End Chg *** 10/28/20 ****************************************
     F                                     RENAME(PEL01:PEL04)
     FAUDTFRM   O    E             PRINTER OFLIND(*IN16)
     F                                     USROPN
     F                                     RENAME(PAGE1:PAGE_1)
     F                                     RENAME(PAGE2:PAGE_2)
     F                                     RENAME(PAGE4:PAGE_4)
     FWMEXL     IF   E           K DISK
     FWMRTL     IF   E           K DISK    rename(prt01:prt0x)
     fsdcmla    if   e           k disk    rename(pcm01:pcm02)
     f                                     infds(cmfds)
     FWDHCL3    IF   E           K DISK
     fwmrel     if   e           k disk    rename(pre01:pre00)
     f*** Beg Add *** 10/28/20 **********************************************************
     fwdell1q   if   e           k disk    rename(pel01:pel01q)
     fwxgsl     if   e           k disk
     fxmlprel1  uf   e           k disk
     fwxrwp     o    e             disk    UsrOpn
     fwxrw1p    o    e             disk    UsrOpn
     fwxrw2p    o    e             disk    UsrOpn
     fwxrw3p    o    e             disk    UsrOpn
     fwxrw4p    o    e             disk    UsrOpn
     fwxrw5p    o    e             disk    UsrOpn
     fwxrw6p    o    e             disk    UsrOpn
     fwmcdl     if   e           k disk
     fwasrl15   uf   e           k disk    Rename(psr01:psr15) Prefix(S15:2)
     fwasrl8    uf   e           k disk    Rename(psr01:psr08) Prefix(S8:2)
     fwmmul5    if   e           k disk
     fwtgtl     if   e           k disk
     fwdehl6    if   e           k disk
     fwmepl7e   if   e           k disk    Rename(pep01:pep07e)
     f*** End Add *** 10/28/20 ****************** ***************************************
     f*** Beg Add *** 03/18/23 **********************************************************
     FSTSPL     IF   E           K DISK    rename(psp01:psp00)
     f*** End Add *** 03/18/23 **********************************************************
     D/COPY WADTTRKDSR
     D*
     DisContri         s              1
     D/copy wPrGroup
     d*** Beg Add *** 10/28/20 **************************************************
     d/copy sPrXml
     d/copy sPrJobInfo
     d/copy wPrEndorse
     d/copy wPrEmplr
     d*
     d svLDA           DS          1024    DTAARA(*LDA)
     d xxDefDart                      2    overlay(svlda:128)
     d*** begin add ***  06/15/21  ****************************************
     D ldMassFile                    60    Overlay(svlda:129)
     d*** end add *****  06/15/21  ****************************************
     d xxXml                         60    overlay(svlda:191)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d*
     d psr15DS       e ds                  extName(wasrl15) prefix(S15:2)
     d psr08DS       e ds                  extName(wasrl8) prefix(S8:2)
     d*
     d intSeq#Rw1      s              5  0
     d intSeq#Rw2      s              5  0
     d intSeq#Rw3      s              5  0
     d intSeq#Rw4      s              5  0
     d intSeq#Rw6      s              5  0
     d*** begin add ***  06/15/21  ****************************************
     d isCXConfirm     s              1n
     d confirmStRpt    s              1n
     d stRptDone       s              1n
     d xsDiv           s              5  0
     d templateFile    s                   like(xpfile)
     d outputFile      s                   like(xpfile)
     d*** end add *****  06/15/21  ****************************************
     d isHeadRw1       s              1n
     d strHdTypRw1     s                   Like(RW1HdgT)
     d strHdDesc       s                   Like(RW1Desc)
     d decExpMod       s                   Like(ELexM1)
     d expModDate      s              8  0
     d strGtKey        s                   Like(GTkey)
     d strFmRpMy       s              5
     d strToRpMy       s              5
     d intFmRpMy       s              5  0
     d intToRpMy       s              5  0
     d intPtDt         s              7  0
     d isFirst         s              1n
     d isFoundSr       s              1n   Inz(*Off)
     d intPrvClas      s                   Like(SRclas)
     d decAmtP         s                   Like(SRAmtP)
     d decTotAmtP      s             13s 2
     d flgRptType      s              1
     d flgSrTotEmp     s              1
     d intBsCd         s                   Like(EHbScd)
     d xxFreq          s              1
     d intNumMths      s              3s 0
     d flgSumPeriod    s              1
     d intDateWkly     s              7  0
     d intDate7        s              7  0
     d intNumDay       s              1  0
     d strDayOfWeek    s              1  0
     d intN2           s              3  0
     d t               s              3  0
     d strPfDt         s              7
     d strPst          s                   Like(CMjrSt)
     d strClassDesc    s                   Like(RW5desc)
     d intClass        s                   Like(RW5clas)
     d strExt          s                   Like(RW5ext)
     d isHeadNoSplit   s              1
     d dtFrmExp        s              7s 0
     d flgGenAllDiv    s              1
     d flgNoOutput     s              1
     d flgGen301En     s              1
     d flgGen311En     s              1
     d flgListClms     s              1
     d*
     d srPayrollHist   ds            38    Qualified Dim(100) Inz
     d  intEfDt                       7s 0
     d  intTrdt                       7s 0
     d  intPfDt                       7s 0
     d  intPtDt                       7s 0
     d  strPlan                       1
     d  strDoWk                       2
     d  strPayToDt                    7s 0
     d*
     d srPayrollDs     ds            33    Qualified Dim(100) Inz
     d  intRpMy                       5S 0
     d  intToDt                       7s 0
     d  strType                      10
     d  flgRpt                        1
     d  strHdg                       10
     d*** End Add *** 10/28/20 **************************************************
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D AD              S            100    DIM(5)
     d                 ds
     d pad                     1    120        DIM(3)
     d  pad1                   1     40
     d  pad2                  41     80
     d  pad3                  81    120

     d mad                     1    420        DIM(10)
     d  mad1                   1     42
     d  mad2                  43     84
     d  mad3                  85    126
     d  mad4                 127    168
     d  mad5                 169    210
     d  mad6                 211    252
     d  mad7                 253    294
     d  mad8                 295    336
     d  mad9                 337    378
     d  mad10                379    420
     D ADDR            S             40    DIM(200)                             name/address array
     D HCN             S             20    DIM(200)                             name/address array
     D PDAT            S              7  0 DIM(8) ASCEND                        ANNIV DATES
     D RDAT            S              7  0 DIM(8) ASCEND                        RATING DATES
     D*  ARRAYS TO HANDLE EFF/CANX DTS FOR NON-COMMOM POLICY YEARS
     D EFF             S              7  0 DIM(5)                               EFF.DATE
     D CNX             S              7  0 DIM(5)                               CXL.DATE
     D NM              S             25    DIM(999)
     D IE              S              1    DIM(999)
     D XXAR30          S              3    DIM(10)                              AUDIT REP.
     D XXMON           S              9    DIM(12) CTDATA PERRCD(1)             MONTH
     D XXMMM           S              9    DIM(12) CTDATA PERRCD(1)             month
     D MTH             S              9    DIM(12) CTDATA PERRCD(1)             Months - alpha
     D CLS             S              5  0 DIM(999)                             Class code
     D SPC             S              1    DIM(999)                             Special des

     D COUT            S              5  0 DIM(5)                               Class code output
     D SOUT            S              1    DIM(5)                               Spec des output
     D CAMT            S              9  0 DIM(5)                               Class amount
     D CMIS            S              7    DIM(5)                               payroll missing
     d strForm         s             15
     d strCo#          s              3
     d strFnd#         s              3
     d strFyr          s              3
     d strEmp#         s              9
     d strOutq         s             10
     d strPrt          s              1
     d strDiv          s              5
      *
     d strSplf         s             10
     d strque          s             10
     d intRrn          s              9  0
      *
     d intSeq#         s              5  0
     d strKey          s             40
     D*
     D DSCLAS          DS                  OCCURS(8)
     D  A1CL1                  1      5
     D  A1SD1                  7      7
     D  A1DE1                  8     37
     D ELFDS4          DS
     D  ELRRN4               397    400B 0
     D*** Beg Add *** 10/28/20 ****************************************
     D EL5fDs          ds
     D  EL5Rrn               397    400B 0
     D*** End Add *** 10/28/20 ****************************************
     D cmfds           ds
     D  cmrrn                397    400B 0
     D*
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)                               CXL.DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS                  INZ
     D  LN                     1    700
     D                                     DIM(10)
     D  RSLN01                 1     70
     D  RSLN02                71    140
     D  RSLN03               141    210
     D  RSLN04               211    280
     D  RSLN05               281    350
     D  RSLN06               351    420
     D  RSLN07               421    490
     D  RSLN08               491    560
     D  RSLN09               561    630
     D  RSLN10               631    700
     D                 DS
     D  AD1                    1    500
     D                                     DIM(5)
     D  A1CNAM                 1    100
     D  A1ENAM               101    200
     D  A1AD1                201    300
     D  A1AD2                301    400
     D  A1AD3                401    500
     D                 DS
     D  AD2                    1    500
     D                                     DIM(5)
     D  A2CNAM                 1    100
     D  A2ENAM               101    200
     D  A2AD1                201    300
     D  A2AD2                301    400
     D  A2AD3                401    500
     D                 DS
     D  AD3                    1    500
     D                                     DIM(5)
     D  A3CNAM                 1    100
     D  A3ENAM               101    200
     D  A3AD1                201    300
     D  A3AD2                301    400
     D  A3AD3                401    500

     D  COUTDS         DS
     D    COUT1                            INZ
     D    COUT2                            INZ
     D    COUT3                            INZ
     D    COUT4                            INZ
     D    COUT5                            INZ
     D    @COUT                       5S 0 DIM(5) OVERLAY(COUTDS)

     D  SOUTDS         DS
     D    SOUT1
     D    SOUT2
     D    SOUT3
     D    SOUT4
     D    SOUT5

     D  CAMTDS         DS
     D    CAMT1                            INZ
     D    CAMT2                            INZ
     D    CAMT3                            INZ
     D    CAMT4                            INZ
     D    CAMT5                            INZ
     D    @CAMT                       9S 0 DIM(5) OVERLAY(CAMTDS)

     D  CMISDS         DS
     D    CMIS1
     D    CMIS2
     D    CMIS3
     D    CMIS4
     D    CMIS5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EMPHN#                 1      7  0
     D  XXPH1                  1      3
     D  XXPH2                  4      7
     D                 DS
     D  ECPHN#                 1      7  0
     D  X2PH1                  1      3
     D  X2PH2                  4      7
     D                 DS
     D  AGPHN1                 1      7  0
     D  ZZPH1                  1      3
     D  ZZPH2                  4      7
     D                 DS
     D  ZZDATE                 1      7  0
     D  ZZYYMM                 1      5  0
     D  ZZMM                   4      5  0
     D  ZZDD                   6      7  0
     D  ZZDAY                  6      7  0
     D*
     D MISSNG          C                   CONST('Missing')
     D                 DS
     D  UPPHN#                 1      7  0
     D  YYPH1                  1      3
     D  YYPH2                  4      7
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS
     D  NAME                   1    100
     D  NAME1                  1     40
     D  NAME2                 41    100
     D                 DS
     D  SNAME                  1    100
     D  SNAM1                  1     50
     D  SNAM2                 51    100
     D                 DS
     D  XXDATE                 1      8  0
     D  XXM                    1      2
     D  XXD                    3      4
     D  XXY                    5      8
     D*
     D HCNH            C                   CONST('HCN Credit:')
     D EFFDH           C                   CONST('Eff Dt:')
     D EXPDH           C                   CONST('Exp Dt:')
     D*
     D TITLE1          C                   CONST('AUDIT REPORT')
     D USESRP          C                   CONST('USE S/R PAYROLL:')
     D ACCTTP          C                   CONST('ACCOUNT TYPE   :')
     D ACCTST          C                   CONST('ACCOUNT STATUS :')
     D ADTTYP          C                   CONST('AUDIT TYPE     :')
     D ADTREP          C                   CONST('AUDIT REPS     :')
     D GRP15C          C                   CONST('GROUP 1 & 5    :')
     D PRDFRM          C                   CONST('PRODUCE FORMS  :')
     D PART            C                   CONST('PARTIAL YEAR')
     D FULL            C                   CONST('FULL YEAR')
     D CXLD            C                   CONST('CANCELLED')
     D ACTV            C                   CONST('ACTIVE')
     D INC             C                   CONST('INCLUDED')
     D EXC             C                   CONST('EXCLUDED')
     D ECM             C                   CONST('ECM REPORTS ONLY')
     D BASE            C                   CONST('BASE REPORTS ONLY')
     D BOTH            C                   CONST('BASE & ECM REPORTS')
     D EXCREP          C                   CONST('EXCLUDE POLICIES -
     D                                     WITH AN ACCOUNT REP -
     D                                     ASSIGNED')
     D*
     D WKMON           C                   CONST('SEPTEMBER')
     D WKFLD1          C                   CONST('deadline is vital -
     D                                     to the continuation -
     D                                     of your')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     d*** Beg Del *** 10/28/20 *******************************************************************
     D*** PrtPolEnd       pr             1    ExtProc('PRTPOLEND')
     d*** strForm                       15
     d*** strCo#                         3
     d*** strFnd#                        3
     d*** strFyr                         3
     d*** strEmp#                        9
     d*** strDiv                         5
     d*** strOutq                       10
     d*** strPrt                         1
     d*** End Del *** 10/28/20 *******************************************************************
      *
     D ResSplDDoc      pr                  ExtProc('RESSPLDDOC')
     d strsplf                       10
     d strque                        10
     d intrrn                         9  0
     d @@co#           s                   like(ELCO#)
     d @@fnd           s                   like(ELFND)
     d @@emp#          s                   like(ELEMP#)
     d @@fyr           s                   like(ELFYR)
     d ##err           s             50
     d ##info          s             30
     d ##nrbs          s              1
     d svtypa          s              2
     D ApplyRateSplit  pr              n
     D inState                        2a
     D inDate                         7  0
     d sv1stPyEfdt     s                   like(EPefdt)
     d xxTnPyFnd       s               n
     d xxTn14          s               n
     d xxType          s              1
     d xxArFl          s              1
     d xxDesc          s             19
     d wlDesc          s             30
     d isDiscounted    pr              n
     d  state                         2    const
     d  class                         5  0 const
     d  spec                          1    const
     d* Effective Date
     d SrEffd          pr             7  0
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d inFYR                          3  0 const
     d inEMP#                         9  0 const
     d inDIV                          5  0 const
     d inDate                         7  0
     d inType                         1    options(*nopass)
     d inClas                         5  0 options(*nopass)
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
     d xxOKPyFnd       s               n
     d xxOK14          s               n
     d chAudRProc      s              1
     d chUpdAudTR      s              1
     d QQRegion        s              3
     d strRegion       s              3
     d intRegion       s              3  0
       //*** Beg Add *** 10/28/20 *********************************************
     d* Numeric Day of Week
     d DayofWeek       pr            10i 0
     d intDate                        8  0 Const
     d*
     DCvtNumToDate     PR              d
     D intDate                        8  0 Const
     d*
     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   XmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     d*
     d GenScriptOut    pr                  ExtPgm('S4401C')
     d inXml_                        61
     d inXml           s             61
     d*
     d GetFutureDate   pr                  ExtPgm('SRFUTR8')
     d   inDate8                      8  0
     d   inDays                       5  0
     d   outError                     1
     d   inDate8       s              8  0
     d   inDays        s              5  0
     d   outError      s              1
     d processType     c                   const('Audit Request')
     d isUseSpoolOut   s              1n   Inz
     d isUseScript     s              1n   Inz
     d isUseXML        s              1n   Inz
     d isMassReq       s              1n   Inz
     d isGrpSlct       s              1n   Inz
     d flgMass         s              1
     d strXmlLink      s                   Like(XPlink)
     d strXmlFileName  s                   Like(XPxml)
     d strXmlFileBlk   s                   Like(XPxml)
     d strXmlFile      s                   Like(XPxml)
     d message         s            250    Inz(*blanks)
     d string          s             10    Inz(*blanks)
     d xxPre           s              2    Inz('AU')
     d intEl5Rrn       s              9  0 Inz(*zeros)
     d oneFnd          s                   Like(ELfnd)
     d svElRr          s                   Like(ELrrn4)
     d sys_Date        s               d   datfmt(*ISO)
     d MthlyWkly       s              1
       //*** End Add *** 10/28/20 *********************************************
      *** Beg Add *** 03/18/23 **********************************************************
     d proctype        s            100    Inz(*blanks)
      *** End Add *** 03/18/23 **********************************************************
     C*
     C     *LIKE         DEFINE    PAGE          XXPAGE
     C     *LIKE         DEFINE    PAGE          PAGE#0
     C     *LIKE         DEFINE    PAGE          PAGE#1
     C     *LIKE         DEFINE    PAGE          PAGE#2
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Excluded Workplaces
      *
     C     EWKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPYR
      *
     C*
     C*  KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   EMPLOYER PAYROLL FILE
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFTDT
      *
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFTDT
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY22        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C     ELKY23        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR ADDITIONAL DIVISIONS
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE OWNER/OFFICERS FILE.
     C*
     C*
     C*   KEY LIST FOR THE OWNER/OFFICERS FILE, INCLUDING LOCATION ID
     C*
     C     OOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C*
     C*
     C     OOKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*   KEY LIST FOR THE FUND MASTER FILE.
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE PAYROLL CLASS FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    X1EFDT            7 0
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     AHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*   KEY LIST FOR THE BILLING ADDRESS FILE
     C*
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      * Claims Master File
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * Audit Information File
      *
     C     AIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER CONTACTS FILE
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPCNT
     C*
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*
     C*  REMARK TYPE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    QQDRMK
     C*
     C*   AUDIT TRACKING FILE
     C*
     C     AUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C                   KFLD                    EPEFDT
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * Office Master
      *
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*
     C*  Self-reporting payroll file
     C     SRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C     SRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C                   KFLD                    XXRPMY
      * Key List for file SDCMLA
     c     keySDCMLA     klist
     c                   kfld                    strKey
     c                   kfld                    intSeq#
     C*
     C     *LIKE         DEFINE    CNDESC        XXCNDS
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELFYR         XXPYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELEMP#        RSEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDIV         RSDIV
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C     *LIKE         DEFINE    EPSPEC        XXSPEC
     C     *LIKE         DEFINE    ELANDT        XXANDT
     C     *LIKE         DEFINE    ELANDT        SVANDT
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    EMDT          XXEFDT
     C     *LIKE         DEFINE    ELPFDT        XXFTDT
     C     *LIKE         DEFINE    ELPTDT        XXTTDT
     C     *LIKE         DEFINE    ELPTDT        XXTODT
     C     *LIKE         DEFINE    EMMST         XXST
     C     *LIKE         DEFINE    EMCONT        XXCONT
     C     *LIKE         DEFINE    EMMCTY        XXCITY
     C     *LIKE         DEFINE    EMMAD1        XXADR1
     C     *LIKE         DEFINE    EMMAD2        XXADR2
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    OOELID        XXELID
     C     *LIKE         DEFINE    AGMCTY        ZZCITY
     C     *LIKE         DEFINE    AGMAD1        ZZADR1
     C     *LIKE         DEFINE    AGMST         ZZST
     C     *LIKE         DEFINE    ELPTDT        XXCXDT
     C     *LIKE         DEFINE    EMDIV         ZZDIV
     C     *LIKE         DEFINE    SRRPMY        XXFRMY
     C     *LIKE         DEFINE    SRRPMY        XXTOMY
     C     *LIKE         DEFINE    SRRPMY        XXRPMY
     C     *LIKE         DEFINE    ECPCNT        XXPCNT
     C     *LIKE         DEFINE    elhcn         svhcn
     C     *LIKE         DEFINE    ELMRFL        hldELMRFL
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFTDT            8
     C                   PARM                    QQTTDT            8
     C                   PARM                    QQPYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQRPFL            1
     C                   PARM                    QQDRMK            5
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQCOM1           30
     C                   PARM                    QQCOM2           30
     C                   PARM                    QQCOM3           30
     C                   PARM                    QQCOM4           30
     C                   PARM                    QQCOM5           30
     C                   PARM                    QQCDE1            2
     C                   PARM                    QQCDE2            2
     C                   PARM                    QQCDE3            2
     C                   PARM                    QQCDE4            2
     C                   PARM                    QQCDE5            2
     C                   PARM                    QQSRFL            1
     C                   PARM                    QQNRBS            1
     C                   PARM                    QQAUTY            2
     C                   PARM                    QQATIE            1
     C                   PARM                    QQAR             30
     C                   PARM                    QQACTY            1
     C                   PARM                    QQACST            1
     C                   PARM                    QQG15C            1
     C                   PARM                    QQAFTY            1
     C                   PARM                    QQDDUE            8
     C                   PARM                    QQPRDT            8
     C                   PARM                    QQASSF            1
     C                   PARM                    QQRFLG            1
     C                   PARM                    QQAudRProc        1
     C                   PARM                    QQUpdAudTR        1
     C                   PARM                    QQRegion
     C*** Beg Add *** 10/28/20 ************************************************
     C                   PARM                    QQgenAllDiv       1
     C                   PARM                    QQnoOutput        1
     C                   PARM                    QQgen301En        1
     C                   PARM                    QQgen311En        1
     C                   PARM                    QQlistClms        1
     C*** End Add *** 10/28/20 ************************************************
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
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
       //*** Beg Add *** 10/28/20 ***********************************
       // Setup Scriptura indicators
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);

       If Q1prms > 37;
          flgGenAllDiv = QQgenAllDiv;
       Else;
          flgGenAllDiv = ' ';
       Endif;
       If Q1prms > 38;
          flgNoOutput = QQnoOutput;
       Else;
          flgNoOutput = ' ';
       Endif;
       If Q1prms > 39;
          flgGen301En = QQgen301En;
       Else;
          flgGen301En = ' ';
       Endif;
       If Q1prms > 40;
          flgGen311En = QQgen311En;
       Else;
          flgGen311En = ' ';
       Endif;
       If Q1prms > 41;
          flgListClms = QQListClms;
       Else;
          flgListClms = ' ';
       Endif;

       // If division is not passed in or GenAllDiv = Y then this is a Mass Request
       If qqDiv = *blanks or flgGenAllDiv = 'Y';
          isMassReq = *on;
          flgMass = 'Y';
       Endif;
       // Setup XML Prep, Etc.
       //*** begin add ***  06/15/21  ****************************************
       Clear strXmlFileName;
       Clear strXmlLink;
       isCXConfirm = *off;
       confirmStRpt = *off;
       stRptDone = *off;
       xsDiv = 0;
       If isUseScript = *On and PgmInStack('W0171R') = *On and
        flgGenAllDiv = ' ';
          confirmStRpt = *on;
          flgGenAllDiv = 'Y';
       Endif;
       //*** end add *****  06/15/21  ****************************************
       //*** End Add *** 10/28/20 ***********************************
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C                   SETOFF                                       53
     C     XXCLT#        IFEQ      '102'
     C                   SETON                                        53
     C                   ENDIF                                                  XXCLT#='102'
     C*
     C*
      /free
       Open W7231O;
       qq1COM5 = qqCOM5;
      /end-free
     C     QQMODE        IFEQ      '*'
     C                   MOVE      'D'           QQMODE
     C                   MOVE      '*'           QQMOD1            1
     C                   ENDIF
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        SYSTM6            6 0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT7
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFTDT        X1FTDT            8 0
     C*
     C                   Z-ADD     X1FTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFTDT
       //*** Beg Add *** 10/28/20 ******************************************
       dtFrmExp = xxFtDt;
       //*** End Add *** 10/28/20 ******************************************
     C*
     C                   MOVE      QQTTDT        X1TTDT            8 0
     C*
     C                   Z-ADD     X1TTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTTDT
     C*
     C                   MOVE      QQPYR         XXPYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C*
     C*  BATCH OR DEMAND  MODE
     C*
     C     QQMODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09                          DEMAND MODE
     C                   ELSE
     C                   MOVE      *ON           *IN08                          BATCH MODE
     C                   ENDIF
     C*
     C                   MOVE      *ON           MASSRN            1
     C     QQEMP#        IFEQ      *ZERO
     C     QQEMP#        OREQ      *BLANK
     C                   MOVE      *OFF          *IN24                          EXP RANGE
     C                   ELSE
     C                   MOVE      *ON           *IN24                          SELECT EMPLR
     C                   MOVE      *OFF          MASSRN
     C                   MOVE      9999999       XXTTDT
     C                   ENDIF
     C*
     C                   MOVE      QQCO#         RSCO#             3 0
     C                   MOVE      QQFND         RSFND             3 0
     C                   MOVE      QQFTDT        R1FTDT            8 0
      /Free
       If q1Prms > 36;
          strRegion = qqRegion;
       Else;
          strRegion = *Blank;
       EndIf;
       If strRegion = *Blank;
          RSRegion = '*ALL';
       Else;
          intRegion = %int(strRegion);
          Chain (xxCo#:xxFnd:intRegion) pre00;
          If %found(wmrel);
             RSRegion = %editc(RErgn#:'X') + ' - ' + %trim(REdesc);
          Else;
             RSRegion = strRegion;
          Endif;
       Endif;
      /End-Free
     C*
     C                   Z-ADD     R1FTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         RSFTDT            8 0
     C*
     C                   MOVE      QQTTDT        R1TTDT            8 0
     C*
     C                   Z-ADD     R1TTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         RSTTDT            8 0
     C*
     C                   MOVE      QQPYR         RSPYR             3 0
     C                   MOVE      QQEMP#        RSEMP#
     C                   MOVE      QQDIV         RSDIV
     C                   MOVE      QQRPFL        RSRPFL            1
     C     QQDRMK        IFEQ      *BLANK
     C                   MOVE      'N'           RSDRMK            1
     C                   ELSE
     C                   MOVE      'Y'           RSDRMK            1
     C                   ENDIF
     C*
     C                   MOVE      RSDRMK        CRTPDF            1
     C                   Z-ADD     1             I                 3 0
     C     USESRP        CAT(P)    QQSRFL:2      LN(I)
     C*
     C                   CLEAR                   XXAUTY
     C                   CLEAR                   XXATIE
     C     Q1PRMS        IFGE      24
     C                   MOVE      QQAUTY        XXAUTY            2
     C                   MOVE      QQATIE        XXATIE            1
     C     XXCLT#        IFEQ      '071'
     C                   ADD       1             I
     C     ACCTTP        CAT(P)    QQAUTY:2      LN(I)
     C                   SELECT
     C     QQATIE        WHENEQ    'I'
     C                   CAT       INC:2         LN(I)
     C     QQATIE        WHENEQ    'E'
     C                   CAT       EXC:2         LN(I)
     C                   OTHER
     C                   CAT(P)    '*ALL':2      LN(I)
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C*
     C     *LIKE         DEFINE    QQAR          XXAR
     C                   CLEAR                   XXAR
     C                   CLEAR                   XXAR30
     C     Q1PRMS        IFGE      26
     C                   MOVE      QQAR          XXAR
     C                   MOVEA     QQAR          XXAR30
     C                   SORTA     XXAR30
     C     XXCLT#        IFEQ      '071'
     C                   ADD       1             I
     C                   MOVEL(P)  ADTREP        LN(I)
     C                   SELECT
     C     XXAR          WHENEQ    *BLANKS
     C                   CAT(P)    '*ALL':2      LN(I)
     C     XXAR30(1)     WHENEQ    '***'
     C                   CAT(P)    EXCREP:2      LN(I)
     C                   OTHER
     C                   DO        10            X
     C                   MOVEL(P)  XXAR30(X)     HD3               3
     C                   CAT(P)    HD3:2         LN(I)
     C                   ENDDO
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CLEAR                   XXACTY
     C     Q1PRMS        IFGE      27
     C                   MOVE      QQACTY        XXACTY            1
     C     XXCLT#        IFEQ      '071'
     C                   ADD       1             I
     C     ADTTYP        CAT(P)    QQACTY:2      LN(I)
     C                   SELECT
     C     QQACTY        WHENEQ    'P'
     C                   CAT(P)    PART:2        LN(I)
     C     QQACTY        WHENEQ    'F'
     C                   CAT(P)    FULL:2        LN(I)
     C                   OTHER
     C                   CAT(P)    '*ALL':2      LN(I)
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CLEAR                   XXACST
     C     Q1PRMS        IFGE      28
     C                   MOVE      QQACST        XXACST            1
     C     XXCLT#        IFEQ      '071'
     C                   ADD       1             I
     C     ACCTST        CAT(P)    QQACST:2      LN(I)
     C                   SELECT
     C     QQACST        WHENEQ    'C'
     C                   CAT(P)    CXLD:2        LN(I)
     C     QQACST        WHENEQ    'A'
     C                   CAT(P)    ACTV:2        LN(I)
     C                   OTHER
     C                   CAT(P)    '*ALL':2      LN(I)
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CLEAR                   XXG15C
     C     Q1PRMS        IFGE      29
     C                   MOVE      QQG15C        XXG15C            1
     C     XXCLT#        IFEQ      '071'
     C                   ADD       1             I
     C     GRP15C        CAT(P)    QQG15C:2      LN(I)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CLEAR                   XXAFTY
     C                   CLEAR                   X1DDUE
     C     Q1PRMS        IFGE      30
     C                   MOVE      QQAFTY        XXAFTY            1
     C                   MOVE      QQDDUE        X1DDUE            8 0
     C     XXCLT#        IFEQ      '071'
     C                   ADD       1             I
     C     PRDFRM        CAT(P)    QQAFTY:2      LN(I)
     C                   SELECT
     C     QQAFTY        WHENEQ    'E'
     C                   CAT(P)    ECM:2         LN(I)
     C     QQAFTY        WHENEQ    'B'
     C                   CAT(P)    BASE:2        LN(I)
     C     QQAFTY        WHENEQ    '2'
     C                   CAT(P)    BOTH:2        LN(I)
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     X1DDUE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDDUE            7 0
     C*
     C                   CLEAR                   X1PRDT
     C     Q1PRMS        IFGE      32
     C                   MOVE      QQPRDT        X1PRDT            8 0
     C                   ENDIF
     C*
     C     X1PRDT        IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        X1PRDT
     C                   ENDIF
     C*
     C                   Z-ADD     X1PRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT            7 0
     C*
     C*  MOVE THE CODES TO ROUTING PAGE FIELDS
     C*
     C* Perform this when client id is NE '093'
     C     XXCLT#        IFNE      '093'
     C                   MOVE      QQCDE1        RSCDE1            2
     C                   MOVE      QQCDE2        RSCDE2            2
     C                   MOVE      QQCDE3        RSCDE3            2
     C                   MOVE      QQCDE4        RSCDE4            2
     C                   MOVE      QQCDE5        RSCDE5            2
     C*
     C*  SET UP INDICATORS FOR BILLING COMMENTS
     C*
     C                   MOVEA     '00000'       *IN(80)
     C*
     C     QQCOM1        COMP      *BLANKS                            8080
     C     QQCOM2        COMP      *BLANKS                            8181
     C     QQCOM3        COMP      *BLANKS                            8282
     C     QQCOM4        COMP      *BLANKS                            8383
     C     QQCOM5        COMP      *BLANKS                            8484
     C                   ENDIF                                                  XXCLT#NE '093'
     C*
     C*  COUNT THE NUMBER OF COMMENTS
     C*
     C                   Z-ADD     0             CNT1              2 0
     C*
     C     80            DO        84            X
     C*
     C     *IN(X)        IFEQ      *ON
     C*
     C                   ADD       1             CNT1
     C*
     C                   ENDIF                                                  *IN,X=ON
     C*
     C                   ENDDO
     C*
     C*  SET THE COMMENT INDICATOR
     C*
     C     CNT1          IFEQ      *ZERO
     C*
     C                   MOVE      *OFF          *IN75
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN75
     C*
     C                   ENDIF                                                  Y = 0
     C*
     C     CRTPDF        IFEQ      'Y'
     C                   EXSR      OPEN2
     C                   ENDIF
     C*
     C                   MOVE      '0'           XXPCNT
     C                   MOVE      '0'           *IN65
      *
      * SET UP THE PRIMARY CONTACT KEY FIELD
     C     XXCLT#        IFEQ      '114'
     C     XXCLT#        OREQ      '152'
     C                   MOVE      '1'           XXPCNT
     C                   MOVE      '1'           *IN65
     C                   ENDIF
      /Free
       If isContri = '1';
          *in56 = '0';
       Else;
          *in56 = '1';
       Endif;
      /End-free
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C* Perform this when client id is NE '093'
     C     XXCLT#        IFNE      '093'
     C     UPKY          CHAIN     PUP01                              60
     C                   ENDIF                                                  XXCLT#NE '093'
     C*
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*  CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
       //*** Beg Add *** 10/28/20 ******************************************
       // Read from the Group Select file for multiple groups or oneFund if
       // called from Policy toolbar
       oneFnd = 0;
       If xxFnd = 0;
          Setll xxCo# pgs01;
          Reade xxCo# pgs01;
          xxFnd = GSfnd;
          isGrpSlct = *on;
       Else;
          oneFnd = xxFnd;
          GSfnd = 0;
       Endif;

       Dow GSfnd <> 0 or oneFnd <> 0;
          *in50 = *off;
          If GSiEfl = 'I' or oneFnd <> 0;
             If oneFnd = 0;
                xxFnd = GSfnd;
             Endif;
          // Populate the region based on group if region is passed in
          If strRegion <> *blanks;
             intRegion = %int(strRegion);
             Chain (xxCo#:xxFnd:intRegion) pre00;
             If %found(wmrel);
                RSRegion = %editc(RErgn#:'X') + ' - ' + %trim(REdesc);
             Else;
                RSRegion = strRegion;
             Endif;
          Endif;
       //*** End Add *** 10/28/20 ******************************************
     C*   CHAIN TO THE FUND MASTER FILE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
      /Free
       isContri = GetPrmContflg(xxCo#:xxFnd);
      /End-free
     C*
     C*
     C                   MOVEL     FNMZIP        XXZIP5            5
     C                   MOVE      FNMZIP        XXZIP4            4
     C     FNMCTY        CAT(P)    ',':0         FNCS             43
     C                   CAT       FNMST:1       FNCS
     C                   CAT       XXZIP5:1      FNCS
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         FNCS
     C                   CAT       XXZIP4:0      FNCS
     C                   ENDIF
      *
      *   CENTER FUND INFORMATION
      *
     C                   MOVEL(P)  FNNAME        XXFNAM           50
     C                   MOVEL(P)  FNMAD1        XXMAD1           50
     C                   MOVEL(P)  FNMAD2        XXMAD2           50
     C                   MOVEL(P)  FNCS          XXFNCS           50
      *
     C     XXMAD2        IFEQ      *BLANKS
     C                   MOVEL(P)  FNCS          XXMAD2
     C                   MOVEL(P)  *BLANKS       XXFNCS
     C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3

      /free
       Clear F2blog;
          *in78 = *Off;
       Chain fnky PF201;
       If F2blog <> ' ';
          *in78 = *On;
       Endif;
       Select;
       When (cltid = '152' or cltid = '114') and F2blog = 'OCTABILL';
            LOGHRZ =  0.2;
            LOGVRT = 0.3;
       Other;
            LOGHRZ = 0.2;
            LOGVRT = 0.3;
       Endsl;
      /end-free
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C                   ENDIF
     C                   MOVE      *ON           *IN97
     C*
     C*  PROCESS AUDIT REQUESTS FOR ONE OR ALL EMPLOYERS
     C*
       //*** Beg Del *** 10/28/20 *********************************************
     C***  *IN24         IFEQ      *ON                                          SPECIFIC EMP#
     C***  ELKY          CHAIN     PEL01                              50
     C***                ELSE
     C***  ELKY1         SETLL     PEL02                                        EXP DT RANGE
     C***  ELKY2         READE     PEL02                                  50
     C***  *LIKE         DEFINE    ELRRN4        SVELRR
     C***                Z-ADD     ELRRN4        SVELRR
     C***                ENDIF
       //*** End Del *** 10/28/20 *********************************************
       //*** Beg Add *** 10/28/20 *********************************************
       xxFtDt = dtFrmExp;
       Select;
       //*** begin add ***  06/15/21  *****************************************
       //* CX Confirmations generating all div to State Rpt Division -
       //* Process state rpt division first, then rest of divisions.
       When *in24 = *on and confirmStRpt;
          Setll (xxCo#:xxFnd:xxEmp#) pem01;
          Reade (xxCo#:xxFnd:xxEmp#) pem01;
          Dow not %eof;
             If EMStrp = 'Y';
                xsDiv = EMDiv;
                Leave;
             Endif;
             Reade (xxCo#:xxFnd:xxEmp#) pem01;
          Enddo;
          Chain (xxCo#:xxFnd:xxPyr:xxEmp#:xsDiv) pel01q;
          If not %Found(wdell1q);
              *in50 = *on;
          Endif;
       //*** end add *****  06/15/21  *****************************************
       // Single Employer/Division
       When *in24 = *on and flgGenAllDiv <> 'Y' and qqDiv <> *blanks;
          Chain elKy pel01;
          If not %Found(wdell);
             *in50 = *on;
          Endif;
       // Single Employer all divisions
       When *in24 = *on and (flgGenAllDiv = 'Y' or qqDiv = *blanks);
          Chain (xxCo#:xxFnd:xxPyr:xxEmp#) pel01q;
          If not %Found(wdell1q);
              *in50 = *on;
          Endif;
       // All employers
       When *in24 = *off;
          Setll elKy1 pel02;
          Reade elKy2 pel02;
          If %Eof(wdell4);
             *in50 = *on;
          Endif;
       Endsl;
       svElRr = ELrrn4;
       //*** End Add *** 10/28/20 *********************************************
     C                   MOVE      *OFF          *IN73                          USE EST PAYROLL
     C* ESTIMATED PAYROLL IS THE DEFAULT SETTING, *IN73 = *OFF
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60BSSEFL        COMP      'S'                                    71
     C     Q1PRMS        IFGE      22
     C     QQSRFL        IFEQ      'Y'
       //*** Beg Del *** 10/28/20 ******************************************
     C**** *IN71         OREQ      *ON
       //*** End Del *** 10/28/20 ******************************************
     C                   MOVE      *ON           *IN73                          USE S/R PAYROLL
     C                   ENDIF
     C                   ENDIF                                                  Q1PRMS
      /Free
       If q1Prms > 34;
          chAudRProc = qqAudRProc;
       Else;
          chAudRProc = 'N';
       EndIf;

       If q1Prms > 35;
          chUpdAudTR = qqUpdAudTR;
       Else;
          chUpdAudTR = *Blank;
       EndIf;

      /End-Free
     C*
     C* Perform this when client id is NE '093'
     C     *IN50         DOWEQ     *OFF
     C     XXCLT#        ANDNE     '093'
     C     ELPTDT        ANDLE     XXTTDT
       //*** End Add *** 10/28/20 *********************************************
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfYr) pel04;
       If %Found(WDELL5);
          intEL5Rrn = EL5Rrn;
       Endif;
       //*** End Add *** 10/28/20 *********************************************
     C     ELEMP#        IFNE      XXEMP#
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C     ELEMP#        IFEQ      XXEMP#
     C     ELDIV         ANDNE     SVDIV
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN96
     C     ELPROM        IFEQ      'M'
     C                   MOVE      *ON           *IN96
     C                   ENDIF
     C                   MOVE      ELDIV         ZZDIV
      /free
       zzDIV1 = zzDIV;
      /end-free
     C*
     C*  Determine if employer is self-reporting
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60BSSEFL        COMP      'S'                                    71
     C*
      *
      *   If a carrier pol# exists then format it for display on report
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72
     C     'POLICY #'    CAT(P)    ':':0         XXCAP#           37
     C                   CAT(P)    RPCAP#:1      XXCAP#           37
      *
     C                   ELSE
      *
     C                   MOVE      *OFF          *IN72
      *
     C                   SELECT
      *
     C     XXCLT#        WHENEQ    '093'
     C     'POLICY #'    CAT(P)    ':':0         XXCAP#           37
     C                   MOVEL(P)  XXFND         XXTEMP           10
     C                   CAT(P)    XXTEMP:1      XXCAP#
     C                   CAT(P)    '-':0         XXCAP#
     C                   MOVEL(P)  XXEMP#        XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXCAP#
     C                   CAT(P)    '-':0         XXCAP#
     C                   MOVEL(P)  XXDIV         XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXCAP#
     C                   CAT(P)    '-':0         XXCAP#
     C                   MOVEL(P)  XXFYR         XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXCAP#
      *
     C                   OTHER
     C     'POLICY #'    CAT(P)    ':':0         XXCAP#           37
     C                   MOVEL(P)  XXFND         XXTEMP           10
     C                   CAT(P)    XXTEMP:1      XXCAP#
     C                   MOVEL(P)  XXEMP#        XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXCAP#
     C                   MOVEL(P)  XXFYR         XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXCAP#
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60=*OFF
      /free
       xxCAP1# = xxCAP#;
      /end-free
      *
     C                   SELECT
      *
     C     XXCLT#        WHENEQ    '093'
     C                   MOVEL(P)  'FILE #:'     XXFIL#           37
     C                   MOVEL(P)  XXFND         XXTEMP           10
     C                   CAT(P)    XXTEMP:1      XXFIL#
     C                   CAT(P)    '-':0         XXFIL#
     C                   MOVEL(P)  XXEMP#        XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXFIL#
     C                   CAT(P)    '-':0         XXFIL#
     C                   MOVEL(P)  XXDIV         XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXFIL#
     C                   CAT(P)    '-':0         XXFIL#
     C                   MOVEL(P)  XXFYR         XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXFIL#
      *
     C                   OTHER
     C                   MOVEL(P)  'FILE #:'     XXFIL#           37
     C                   MOVEL(P)  XXFND         XXTEMP           10
     C                   CAT(P)    XXTEMP:1      XXFIL#
     C                   MOVEL(P)  XXEMP#        XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXFIL#
     C                   MOVEL(P)  XXFYR         XXTEMP           10
     C                   CAT(P)    XXTEMP:0      XXFIL#
     C                   ENDSL
      /free
       XXFIL1# = XXFIL#;
      /end-free
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY2         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZEROS        ZZFEIN            9 0
     C                   ELSE
     C                   Z-ADD     EMFEIN        ZZFEIN
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C     *IN60         IFEQ      *ON
     C                   GOTO      NEXTEL
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C     ELFAF         IFEQ      ' '
     C                   GOTO      NEXTEL
     C                   ENDIF
     C*
     c                   if        ELNWRW <> *blank
     c                   eval      ##nrbs = ELNWRW
     c                   else
     c                   exsr      srauto
     c                   endif
     C*
     C     QQNRBS        IFNE      ##NRBS
     C     QQNRBS        ANDNE     'B'
     C                   GOTO      NEXTEL
     C                   ENDIF
     C*
      /Free
          If (chAudRProc = 'N' and ELMRFL  = 'Y') Or
             (chAudRProc = 'R' and ELMRFL <> 'Y');
      /End-Free
     C                   GOTO      NEXTEL
      /Free
          EndIf;
      /End-Free
      /Free
       hldELMRFL = ELMRFL;

       If strRegion <> *Blank and %int(strRegion) <> EMRgn#;
      /End-Free
     C                   GOTO      NEXTEL
      /Free
       EndIf;
      /End-Free
     C*
     C     CLTID         IFEQ      '071'
     C                   EXSR      ADLSEL
     C*
     C     I#ASEL        IFEQ      'N'
     C                   GOTO      NEXTEL
     C                   ENDIF
     C*
     C                   MOVE      '0'           WAFTY             1
     C                   EXSR      SRPECM
     C                   ENDIF
     C     QQRPFL        IFEQ      'Y'
     C*
     C                   MOVE      *OFF          *IN19
     C*
     C* Subroutines found in copybook (WADTTRKCSR)
     C     ELBCTG        IFEQ      *BLANK
     C                   EXSR      ASGNBC
     C                   ENDIF
     C                   EXSR      ASGNAT
     C                   EXSR      ASGNAC
     C*
     C     WAFTY         IFNE      'E'
     C     XXPRT1        IFNE      'OPEN'
     C                   EXSR      OPEN1
     C                   ENDIF
     C*
     C*** NO AUTO ASSIGNMENT FOUND
     C* Perform this when client id is NE '093'
     C     XXACO         IFEQ      *BLANKS
     C* Entire block is for clt <> 093 so this is redundant
     C                   MOVE      *ON           *IN19
     C     1             DO        I9            I8                3 0
     C                   MOVEL     MSG(I8)       ##ERR
     C                   MOVEL     INF(I8)       ##INFO
     c   94              ADD       1             PAGE#1
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c   94              Write     HEAD1
     c   94              WRITE     HDLOGO1
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c   94              MOVE      *OFF          *IN94
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and  flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     DET1
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDDO
     C                   ELSE
     C*
     c   94              ADD       1             PAGE#1
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c   94              Write     HEAD1
     c   94              WRITE     HDLOGO1
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c   94              MOVE      *OFF          *IN94
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     DET1
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF                                                  XXACO = ' '
     C     SVELRR        CHAIN     PEL0P                              60
     C                   MOVE      'Y'           ELMRFL
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             SYSTM6
     C                   MOVEL     Q1PGM         ELPGM
     C                   MOVEL     Q1USER        ELUSER
     C                   Z-ADD     SYSTM6        ELTIME
     C                   Z-ADD     SYSDT7        ELDT
     C                   UPDATE    PEL0P
     C                   ENDIF
     C*
     C** This ENDIF is moved up, update to WDELP should execute regardless of
     C** XXACO value
     C                   ENDIF                                                  WAFTY<>E
     C*
     C                   ENDIF
      *
      *  GET OFFICE ADDRESS AND PHONE
      *
     C                   MOVEL(P)  *BLANKS       X3ONAM           40
     C                   MOVEL(P)  *BLANKS       X3OAD1           40
     C                   MOVEL(P)  *BLANKS       X3OAD2           40
     C                   MOVEL(P)  *BLANKS       X3OCSZ           40
     C                   MOVEL(P)  *BLANKS       X3OPHN           40
     C*
      * IF NOT Client 109, then leave office fields blank
     C     CLTID         IFEQ      '109'
      *
     C     OFKY          CHAIN     POF01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        X3ONAM
     C                   MOVEL(P)  OFADR1        X3OAD1
     C                   MOVEL(P)  OFADR2        X3OAD2
      *
     C                   MOVEL     OFZIP         X3ZIP5            5
     C                   MOVE      OFZIP         X3ZIP4            4
     C     OFCTY         CAT(P)    ',':0         X3OCSZ
     C                   CAT       OFST:1        X3OCSZ
     C                   CAT       X3ZIP5:1      X3OCSZ
      *
     C     X3ZIP4        IFNE      *ZERO
     C     X3ZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         X3OCSZ
     C                   CAT       X3ZIP4:0      X3OCSZ
     C                   ENDIF
      *
     C     OFPHN#        IFNE      *ZERO
      *
     C                   MOVEL     OFPHN#        X3PH1             3
     C                   MOVE      OFPHN#        X3PH2             4
      *
     C     OFAREA        IFNE      *ZERO
     C                   MOVE      OFAREA        X3AREA            3
     C     '('           CAT(P)    X3AREA:0      X3OPHN
     C                   CAT(P)    ') ':0        X3OPHN
     C                   CAT(P)    X3PH1:1       X3OPHN
      *
     C                   ELSE
      *
     C                   MOVEL(P)  X3PH1         X3OPHN
     C                   ENDIF
      *
     C                   CAT       '-':0         X3OPHN
     C                   CAT       X3PH2:0       X3OPHN
     C                   ENDIF
      *
     C     X3OAD2        IFEQ      *BLANKS
     C                   MOVEL(P)  X3OCSZ        X3OAD2
     C                   MOVEL(P)  X3OPHN        X3OCSZ
     C                   MOVEL(P)  *BLANKS       X3OPHN
     C                   ENDIF                                                  XXOAD2
      *
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN45 = *ON
     C*
     C*
     C*  GET FUND DETAIL RECORD
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   GOTO      NEXTEL
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C*   LOOK THRU EACH OCCURRENCE IN ORDER TO FIND ONE OF THE
     C*   FOLLOWING:
     C*          1. THE EFFECTIVE DATE PRESENT WITHOUT A CANCEL DATE.
     C*   -OR-   2. THE EFFECTIVE DATE PRESENT WITH THE CANCEL DATE
     C*             BEING GREATER THAN THE POLICY TO DATE.
     C*
     C                   MOVE      *OFF          *IN60
     C     1             DO        5             X                 2 0
     C*
     C     EF(X)         IFNE      *ZERO                                        ACTIVE
     C     CX(X)         ANDEQ     *ZERO
     C     EF(X)         ORNE      *ZERO                                        CX AFT CURR
     C     CX(X)         ANDGE     ELPTDT                                       POL TO DATE
     C                   MOVE      *ON           *IN60
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  IF EMPLOYER WAS CANCELLED IN A PRIOR YEAR, READ THE NEXT
     C*  EMPLOYER RECORD
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C* Client 071 cancellations will be handled in a different area of code
     C                   OTHER
     C     *IN60         IFEQ      *OFF
     C     *IN24         ANDEQ     *OFF                                         EXP DT RANGE
     C                   GOTO      NEXTEL
     C                   ENDIF
     C                   ENDSL
     C*
     C*  REPOSITION TO LAST ACTIVE BUCKET FOR SINGLE EMPLOYER
     C*
     C     *IN60         IFEQ      *OFF
     C     *IN24         ANDEQ     *ON                                          SINGLE EMP
     C     CLTID         OREQ      '071'
     C                   Z-ADD     5             X
     C                   DO        5
     C*
     C     EF(X)         IFNE      *ZERO
     C                   LEAVE
     C                   ENDIF
     C*
     C                   SUB       1             X
     C                   ENDDO
     C                   ENDIF
     C                   Z-ADD     X             W                 1 0
     C*
     C*  IF AN EXPIRATION DATE RANGE RUN, DO NOT INCLUDE CANCELLED
     C*  CANCELLED ACCTS
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C* Client 071 cancellations will be handled in a different area of code
     C                   OTHER
     C     *IN24         IFEQ      *OFF                                         EXP DT RANGE
     C     CX(X)         ANDLE     XXTTDT                                       CANCELLED
     C     CX(X)         ANDNE     *ZERO
     C*    Include if (CX dt=pol to dt) and policy is not audited
     C     CX(X)         IFLT      ELPTDT
     C     ELFAF         OREQ      'A'
     C                   GOTO      NEXTEL
     C                   ENDIF                                                  CX,X<>ELPTDT
     C                   ENDIF
     C                   ENDSL
     C*
     C*  GET THE CANCELLATION DATE IF PRESENT
     C*
     C                   MOVE      *OFF          *IN99
     C*
     C     CX(X)         IFNE      *ZERO
     C*
     C                   MOVE      CX(X)         XXCXDT
     C*
     C     CX(X)         IFLE      ELPTDT
     C                   MOVE      *ON           *IN99
     C*
     C                   ENDIF                                                  CX,X<=ELPTDT
     C                   ENDIF                                                  Z IFEQ 0
     C*
     C                   Z-ADD     XXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZCXDT            8 0
      /free
       zzCXDT1 = zzCXDT;
      /end-free
     C*
     C*
     C*  SET CANCELLED DURING CURRENT YEAR FLAG
     C*
     C     CX(X)         IFNE      *ZERO
     C     CX(X)         ANDLE     ELPTDT
     C                   MOVE      *ON           XXCXCY            1
     C                   ELSE
     C                   MOVE      *OFF          XXCXCY
     C                   ENDIF
     C*
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD1            8 0
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD2            8 0
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD3            8 0
     C*
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD4            6 0
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD5            6 0
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD6            6 0
     C*  LOAD MAILING ADDRESS
     C*
     C                   CLEAR                   MAD
     C                   CLEAR                   PAD
     C                   CLEAR                   XXCONT
     C                   Z-ADD     *ZERO         PAGE
     C                   Z-ADD     *ZERO         PAGE#0
     C                   Z-ADD     *ZERO         PAGE#2
     C                   MOVE      *OFF          *IN90
     C*
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      XXEMP#        ##EMP#            9
     C                   MOVE      XXDIV         ##DIV             5
     C                   MOVE      ' '           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C*
     C*
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C*
     C                   MOVE      ##EMN1        XXEMN1           50
     C                   MOVE      ##EMN2        XXEMN2           50
     C                   MOVE      ##EMN3        XXEMN3           50
     C                   MOVE      ##EMN4        XXEMN4           50
     C                   MOVEL     ##EMN5        XXEMN5           40
     C                   MOVEL     ##EMN6        XXEMN6           40
     C                   MOVEL     ##EMN7        XXEMN7           40
      *
      *  LOAD MAD ARRAY AND FIND THE FIRST BLANK INDEX.
      *
     C     1             DO        7             X
      *
     C                   SELECT
     C     X             WHENEQ    1
     C                   MOVEL     XXEMN1        MAD(X)
     C     X             WHENEQ    2
     C                   MOVEL     XXEMN2        MAD(X)
     C     X             WHENEQ    3
     C                   MOVEL     XXEMN3        MAD(X)
     C     X             WHENEQ    4
     C                   MOVEL     XXEMN4        MAD(X)
     C     X             WHENEQ    5
     C                   MOVEL     XXEMN5        MAD(X)
     C     X             WHENEQ    6
     C                   MOVEL     XXEMN6        MAD(X)
     C     X             WHENEQ    7
     C                   MOVEL     XXEMN7        MAD(X)
     C                   ENDSL
      *
     C                   ENDDO
      *
     C                   Z-ADD     1             X
     C     MAD(X)        DOWNE     *BLANKS
     C                   ADD       1             X
     C                   ENDDO
     C*
     C*  LOAD PHONE NUMBER
     C*
     C     XXCLT#        IFEQ      '129'
     C                   MOVE      '1'           XXPCNT
     C     ECKY          CHAIN     PEC01                              61
     C     *IN61         IFEQ      *OFF
     C     ECPHN#        IFNE      *ZERO
     C                   MOVE      ECAREA        EMAREA
     C                   MOVE      ECPHN#        EMPHN#
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EMPHN#        IFNE      *ZERO
      *
     C     EMAREA        IFNE      *ZERO
     C                   MOVE      EMAREA        XXAREA            3
     C     '('           CAT       XXAREA:0      MAD(8)
     C                   CAT       ') ':0        MAD(8)
     C                   CAT       XXPH1:1       MAD(8)
      *
     C                   ELSE
      *
     C                   MOVEL     XXPH1         MAD(8)
     C                   ENDIF
      *
     C                   CAT       '-':0         MAD(8)
     C                   CAT       XXPH2:0       MAD(8)
     C                   ENDIF
      *
     C*
     C*  LOAD CONTACT NAME
     C*
     C                   SELECT
      *--------------------------------------------------------------
     C     XXCLT#        WHENEQ    '114'
     C     XXCLT#        OREQ      '152'
      *--------------------------------------------------------------
      *  Check for primary contact and use if available
     C     ECKY          CHAIN     PEC01                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL(P)  ECFSN         XXCONT
      * IF HAVE MIDDLE INITAL USE IT
     C     ECMID         IFNE      *BLANK
     C     XXCONT        CAT       ECMID:1       XXCONT
     C     XXCONT        CAT       '.':0         XXCONT
     C                   ENDIF
     C     XXCONT        CAT       ECLSN:1       XXCONT
     C                   MOVEL     XXCONT        MAD(X)
     C                   ADD       1             X
     C*
     C*  LOAD CONTACT PHONE NUMBER
     C*
     C     ECPHN#        IFNE      *ZERO
     C     ECAREA        IFNE      *ZERO
     C                   MOVE      ECAREA        XXAREA            3
     C     '('           CAT       XXAREA:0      MAD(X)
     C                   CAT       ') ':0        MAD(X)
     C                   CAT       X2PH1:1       MAD(X)
     C                   ELSE
     C                   MOVEL     X2PH1         MAD(X)
     C                   ENDIF
     C                   CAT       '-':0         MAD(X)
     C                   CAT       X2PH2:0       MAD(X)
     C     ECEXT#        IFNE      *ZERO
     C                   CAT       ' ext':1      MAD(X)
     C                   MOVE      ECEXT#        XXEXT             3
     C                   CAT       XXEXT:1       MAD(X)
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDIF
     C                   ENDIF                                                  ECKY *IN64
      *
      *--------------------------------------------------------------
     C     XXCLT#        WHENEQ    '129'
      *--------------------------------------------------------------
     C                   MOVE      '1'           XXPCNT
     C     ECKY          CHAIN     PEC01                              61
     C     *IN61         IFEQ      *OFF
     C     ECFSN         CAT(P)    ECMID:1       XXCONT
     C     XXCONT        CAT(P)    ECLSN:1       XXCONT
     C                   ENDIF
      *
     C     XXCONT        IFNE      *BLANKS
     C                   MOVEL     XXCONT        MAD(X)
     C                   ADD       1             X
     C                   ENDIF
      *
      *--------------------------------------------------------------
     C                   OTHER
      *--------------------------------------------------------------
     C     A3KY          CHAIN     PA301                              61
     C     *IN61         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C                   MOVE(P)   A3CONT        XXCONT
     C                   ENDIF
     C     XXCONT        IFNE      *BLANKS
     C                   MOVEL     XXCONT        MAD(X)
     C                   ADD       1             X
     C                   ENDIF
     C                   ENDSL                                                              9'
      *
      *
      *
     C*  LOAD COUNTY CODE AND DESCRIPTION
     C*
     C     EMCNTY        IFNE      *BLANK
     C     EMPST         IFNE      *BLANKS
     C*
     C                   MOVEL     EMPST         XXSTCO            4
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMMST         XXSTCO
     C*
     C                   ENDIF                                                  EMPST
     C*
     C                   MOVE      EMCNTY        XXSTCO
     C*
     C                   MOVEL(P)  XXSTCO        XXKEY
     C*
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     CNDESC        XXCNDS
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXCNDS
     C*
     C                   ENDIF
     C*
     C     'COUNTY'      CAT       XXSTCO:1      MAD(7)
     C                   CAT       ',':0         MAD(7)
     C                   CAT       XXCNDS:1      MAD(7)
     C*
     C                   ENDIF
     C*
     C     1             DO        7             X
      *
     C                   SELECT
     C     X             WHENEQ    1
     C     XXEMN1        ANDEQ     *BLANKS
     C                   MOVEL     MAD(X)        XXEMN1
     C     X             WHENEQ    2
     C     XXEMN2        ANDEQ     *BLANKS
     C                   MOVEL     MAD(X)        XXEMN2
     C     X             WHENEQ    3
     C     XXEMN3        ANDEQ     *BLANKS
     C                   MOVEL     MAD(X)        XXEMN3
     C     X             WHENEQ    4
     C     XXEMN4        ANDEQ     *BLANKS
     C                   MOVEL     MAD(X)        XXEMN4
     C     X             WHENEQ    5
     C     XXEMN5        ANDEQ     *BLANKS
     C                   MOVEL     MAD(X)        XXEMN5
     C     X             WHENEQ    6
     C     XXEMN6        ANDEQ     *BLANKS
     C                   MOVEL     MAD(X)        XXEMN6
     C     X             WHENEQ    7
     C     XXEMN7        ANDEQ     *BLANKS
     C                   MOVEL     MAD(X)        XXEMN7
     C                   ENDSL
      *
     C                   ENDDO
     C                   Z-ADD     1             X
     C*
     C*  LOAD PHYSICAL ADDRESS IF IT WASN'T USED IN MAILING AREA
     C*
     C     *IN61         IFEQ      *OFF
     C     EMMAD1        ORNE      *BLANK
     C     EMMAD1        ORNE      *BLANK
     C                   CLEAR                   PAD1
     C                   CLEAR                   PAD2
     C                   CLEAR                   PAD3
     C*
     C     EMPAD1        IFNE      *BLANK
     C                   MOVEL     EMPAD1        PAD(X)
     C                   ADD       1             X
     C                   ENDIF
     C     EMPAD2        IFNE      *BLANK
     C                   MOVEL     EMPAD2        PAD(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C*  LOAD CITY, STATE AND ZIP
     C*
     C                   MOVEL     EMPZIP        XXZIP5
     C                   MOVE      EMPZIP        XXZIP4
     C     EMPCTY        IFNE      *BLANK
     C     EMPCTY        CAT       ',':0         PAD(X)
     C                   CAT       EMPST:1       PAD(X)
     C                   CAT       XXZIP5:1      PAD(X)
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         PAD(X)
     C                   CAT       XXZIP4:0      PAD(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  *IN61
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  50
     C     *IN50         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY1         READPE    PAH01                                  50
     C                   ENDDO
     C*
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     AHAGT#        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZERO         XXFEIN
     C                   MOVE      *ZERO         XXMOD
     C                   ENDIF
     C*
     C     AGKY          CHAIN     PAG01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     AGNAME        ANAME            40
     C                   Z-ADD     3             CNT               2 0
     C                   MOVEL(P)  AGMAD1        ZZADR1
     C                   MOVEL(P)  AGMCTY        ZZCITY
     C                   MOVEL(P)  AGMST         ZZST
     C                   MOVEL(P)  AGMZIP        ZZZIP5            5
     C                   MOVE(P)   AGMZIP        ZZZIP4            4
     C*
     C*  BUILD CITY, STATE AND ZIP LINE
     C*
     C     ZZCITY        CAT(P)    ',':0         ZZCSZP           40
     C                   CAT       ZZST:1        ZZCSZP
     C                   CAT       ZZZIP5:1      ZZCSZP
     C*
     C     ZZZIP4        IFNE      *ZERO
     C     ZZZIP4        ANDNE     *BLANK
     C*
     C                   CAT       '-':0         ZZCSZP
     C                   CAT       ZZZIP4:0      ZZCSZP
     C*
     C                   ENDIF
     C*
     C*  LOAD PHONE NUMBER
     C*
     C                   MOVE      *OFF          *IN76
     C*
     C     AGPHN1        IFNE      *ZERO
     C*
     C                   Z-ADD     4             CNT
     C                   MOVE      *ON           *IN76
     C*
     C     AGARE1        IFNE      *ZERO
     C*
     C                   MOVE      AGARE1        ZZARE1            3
     C     '('           CAT(P)    ZZARE1:0      ZZARPH           14
     C                   CAT       ') ':0        ZZARPH
     C                   CAT       ZZPH1:1       ZZARPH
     C*
     C                   ELSE
     C                   MOVEL     ZZPH1         ZZARPH
     C*
     C                   ENDIF                                                  AGARE1 NE 0
     C*
     C                   CAT       '-':0         ZZARPH
     C                   CAT       ZZPH2:0       ZZARPH
      /free
       zzARPH1 = zzARPH;
      /end-free
     C*
     C                   ENDIF                                                  AGPHN1 NE 0
     C                   ELSE
     C                   MOVE      *BLANK        ANAME
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *BLANK        AHAGT#
     C                   MOVE      *BLANK        ANAME
     C                   ENDIF
     C*
     C*  LOAD POLICY PERIOD DATES
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C     CX(W)         IFNE      *ZERO
     C*
     C*  If cancellation date is less than ELPFDT, then load ELPFDT
     C*     in XXPTDT field.
     C*
     C                   SELECT
     C*
     C     CX(W)         WHENLE    ELPFDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C                   Z-ADD     ELPFDT        XXTODT
     C*
     C*
     C     CX(W)         WHENLE    ELPTDT
     C*
     C                   Z-ADD     CX(W)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C                   Z-ADD     CX(W)         XXTODT
     C*
     C     CX(W)         WHENGT    ELPTDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C                   Z-ADD     ELPTDT        XXTODT
     C*
     C                   ENDSL
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C                   Z-ADD     ELPTDT        XXTODT
     C                   ENDIF
     C*
     C*  LOAD ANNIVERSARY RATING PERIOD DATES
     C*
     C                   Z-ADD     *ZERO         XXFRAV
     C                   Z-ADD     *ZERO         XXTOAV
     C*
     C     ELANDT        ADD       10000         XXANDT
     C     ELANDT        IFEQ      *ZERO
     C     ELANDT        OREQ      ELPFDT
     C     XXANDT        ORLE      ELPFDT
     C     XXANDT        ORGE      ELPTDT
     C                   Z-ADD     *ZERO         SVANDT
     C                   MOVE      *ON           *IN88
     C                   ELSE
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRAV            8 0
     C*
     C                   Z-ADD     XXANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTOAV            8 0
     C*
     C                   Z-ADD     XXANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRAV            8 0
     C*
     C                   MOVE      *ON           *IN91
     C                   Z-ADD     XXANDT        SVANDT
     C                   MOVE      *OFF          *IN88
     C                   ENDIF
     C*
     C*  SEARCH OUT THE FIRST ACTIVE EMP. P/R FILE.
     C*
     C     EPKY1         CHAIN     PEP01                              55
     C     EPSTAT        DOWEQ     'I'
     C     *IN55         ANDEQ     *OFF
     C     EPKY1         READE     PEP01                                  55
     C                   ENDDO                                                  EPSTAT DOWEQ
      /free
       If *in55 = *off;
          sv1stPyEfdt = EPefdt;
       Else;
          sv1stPyEfdt = 0;
       Endif;
      /end-free
     C*
     C     *IN55         IFEQ      *ON
     C     *IN24         ANDEQ     *OFF
     C                   GOTO      NEXTEL
     C                   ENDIF                                                  *IN55 IFEQ
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
          If isUseXML = *on and flgNoOutput <> 'Y';
             isHeadNoSplit = 'N';
             Exsr ScriptXML;
             If flgListClms = 'Y';
                Exsr ScriptWxRw5p;
             Endif;
          Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C* If there are isn't payroll and a single employer is being run, allow
     C* one cycle through the payroll loop in order to except the report.
     C*
     C     *IN55         IFEQ      *ON
     C     *IN24         ANDEQ     *ON
     C                   MOVE      *ON           *IN85
     C                   ENDIF
     C                   MOVE      *BLANKS       XXJRST            2
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXJRST
     C                   ENDIF
     C*
     C                   Z-ADD     9999999       RDAT
     C                   MOVE      *OFF          *IN86
     C                   MOVE      *OFF          *IN87
     C                   Z-ADD     0             SVRTDT
     C                   Z-ADD     0             SVRTD2
     C                   Z-ADD     2             X
     C                   Z-ADD     ELPFDT        RDAT(1)
     C     *IN88         IFEQ      *OFF
     C                   Z-ADD     ELANDT        RDAT(1)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ADD       1             X
     C                   ENDIF
     C                   Z-ADD     XXTODT        RDAT(X)
     C                   ADD       1             X
     C*
     C     XXJRST        IFEQ      'FL'
     C*
     C     1             DO        2             Y
     C*
     C     1             ADD       Y             Z
     C     RDAT(Y)       IFGE      1030101
     C     RDAT(Y)       ANDLE     1030331
     C     ELPFDT        ANDLT     1030401
     C     RDAT(Z)       ANDGT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           *IN86
     C                   ENDIF
     C     RDAT(Y)       IFLT      1031001
     C     ELPFDT        ANDLT     1031001
     C     RDAT(Z)       ANDGT     1031001
     C                   Z-ADD     1031001       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           *IN87
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXJRST IFEQ
     C*
     C                   MOVE      *OFF          XXDEFL            1
     C     XXJRST        IFEQ      'DE'
     C*DE rate split flag
     C     EXDE08        ANDNE     'Y'
     C*
     C     1             DO        2             Y
     C*
     C     1             ADD       Y             Z
     C     RDAT(Y)       IFLT      1081001
     C     ELPFDT        ANDLT     1081001
     C     RDAT(Z)       ANDGT     1081001
     C                   Z-ADD     1081001       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XXDEFL
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXJRST IFEQ
     C                   MOVE      *OFF          XX0709            1
     C     XXJRST        IFEQ      'FL'
     C     XXCLT#        ANDEQ     '139'
     C*
     C     1             DO        2             Y
     C*
     C     1             ADD       Y             Z
     C     RDAT(Y)       IFGE      1090401
     C     RDAT(Y)       ANDLT     1090701
     C     ELPFDT        ANDLT     1090701
     C     RDAT(Z)       ANDGT     1090701
     C                   Z-ADD     1090701       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XX0709
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXJRST IFEQ
     C                   MOVE      *OFF          XXAZ10            1
     C     XXJRST        IFEQ      'AZ'
     C*
     C     1             DO        2             Y
     C*
     C     1             ADD       Y             Z
     C     RDAT(Y)       IFLT      1100101
     C     ELPFDT        ANDLT     1100101
     C     RDAT(Z)       ANDGT     1100101
     C                   Z-ADD     1100101       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XXAZ10
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXJRST IFEQ
     C* OK RATE SPLIT FOR 09/01/2010
     C*
     C                   move      *off          xxOK10            1
     C                   if        xxjrst = 'OK'
     C* Check opt out flag for OK 1100901 rate change
     C                   z-add     1100901       xxSpltDt          7 0
     C                   if        ApplyRateSplit(xxjrst:xxSpltDt)
     C     1             do        2             Y
     C                   eval      z = y + 1
     C*
     C                   if        rdat(y) < xxSpltDt and
     C                             ELPFDT  < xxSpltDt and
     C                             rdat(z) > xxSpltDt
     C*
     C                   Z-ADD     xxSpltDt      rdat(x)
     C                   ADD       1             X
     C                   eval      xxOK10 = *on
     C                   endif                                                  RDAT,Y IFGE
     C*
     C                   enddo                                                  DO 2 Y
     C                   endif                                                  ApplyRateSplit
     C*
     C                   endif                                                  xxjrst
      * TN Rate Split FOR 07/01/2014
      *
     C                   eval      xxTn14    = *off
     C                   eval      xxTnPyFnd = *off
     C                   if        xxjrst = 'TN'
      * Look in WMEPP for existence of a TN split payroll. Don't add
      * split date to RDAT if it doesn't exist
     C                   z-add     1140701       xxEfdt
     C     epky          setll     pep01
     C     epky          reade     pep01
     C                   dow       not(%eof)
     C                   if        EPstat <> 'I'
     C                   eval      xxTnPyFnd = *on
     C                   leave
     C                   endif
     C     epky          reade     pep01
     C                   enddo
     C* Check opt out flag for TN 1140701 rate change
     C                   z-add     1140701       xxSpltDt          7 0
     C                   if        ApplyRateSplit(xxjrst:xxSpltDt)
     C     1             do        2             y
     C                   eval      z = y + 1
     C                   if        rdat(y) < xxSpltDt and
     C                             ELPFDT  < xxSpltDt and
     C                             rdat(z) > xxSpltDt
     C                   if        xxTnPyfnd and F1spla <> 'Y'
     C                   z-add     xxSpltDt      rdat(x)
     C                   endif                                                  %found
     C                   add       1             x
     C                   eval      xxTN14 = *on
     C                   endif                                                  RDAT,Y IFGE
     C                   enddo                                                  DO 2 Y
     C                   endif                                                  ApplyRateSplit
     C                   endif                                                  xxjrst
      * OK Rate Split FOR 02/01/2014
      *
     C                   eval      xxOK14    = *off
     C                   eval      xxOKPyFnd = *off
     C                   if        xxjrst = 'OK'
      * Look in WMEPP for existence of a OK split payroll. Don't add
      * split date to RDAT if it doesn't exist
     C                   z-add     1140201       xxEfdt
     C     epky          setll     pep01
     C     epky          reade     pep01
     C                   dow       not(%eof)
     C                   if        EPstat <> 'I'
     C                   eval      xxOKPyFnd = *on
     C                   leave
     C                   endif
     C     epky          reade     pep01
     C                   enddo
     C* Check opt out flag for TN 1140701 rate change
     C                   z-add     1140201       xxSpltDt          7 0
     C                   if        ApplyRateSplit(xxjrst:xxSpltDt)
     C     1             do        2             y
     C                   eval      z = y + 1
     C                   if        rdat(y) < xxSpltDt and
     C                             ELPFDT  < 1140101  and
     C                             rdat(z) > xxSpltDt
     C                   if        xxOKPyfnd and F1spla <> 'Y'
     C                   z-add     xxSpltDt      rdat(x)
     C                   endif                                                  %found
     C                   add       1             x
     C                   eval      xxOK14 = *on
     C                   endif                                                  RDAT,Y IFGE
     C                   enddo                                                  DO 2 Y
     C                   endif                                                  ApplyRateSplit
     C                   endif                                                  xxjrst
     C*
     C                   SORTA     RDAT
     C*
     C                   DO        8             X
     C*
     C     RDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         RDAT(X)
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     *IN86         IFEQ      *ON
     C     *IN87         OREQ      *ON
     C     XXDEFL        OREQ      *ON
     C     XX0709        OREQ      *ON
     C     XXAZ10        OREQ      *ON
     C     XXOK10        OREQ      *ON
     C     xxTN14        oreq      *on
     C     xxOK14        oreq      *on
     C*
     C     *IN87         IFEQ      *ON
     C                   Z-ADD     1031001       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTD2            8 0
     C                   Z-ADD     1031001       SVRTD2            7 0
     C                   ENDIF
     C*
     C     *IN86         IFEQ      *ON
     C                   Z-ADD     1030401       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTDT            8 0
     C                   Z-ADD     1030401       SVRTDT            7 0
     C                   ENDIF
     C*
     C     XXDEFL        IFEQ      *ON
     C                   Z-ADD     1081001       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTDT            8 0
     C                   Z-ADD     1081001       SVRTDT            7 0
     C                   ENDIF
     C*
     C     XX0709        IFEQ      *ON
     C                   Z-ADD     1090701       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTDT            8 0
     C                   Z-ADD     1090701       SVRTDT            7 0
     C                   ENDIF
     C*
     C     XXAZ10        IFEQ      *ON
     C                   Z-ADD     1100101       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTDT            8 0
     C                   Z-ADD     1100101       SVRTDT            7 0
     C                   ENDIF
     C*
     C     XXOK10        IFEQ      *ON
     C                   Z-ADD     xxSpltDt      SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTDT            8 0
     C                   Z-ADD     xxSpltDt      SVRTDT            7 0
     C                   ENDIF
     C*
     C     XXTN14        IFEQ      *ON
     C     XXOK14        OREQ      *ON
     C                   Z-ADD     xxSpltDt      SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTDT            8 0
     C                   Z-ADD     xxSpltDt      SVRTDT            7 0
     C                   ENDIF

     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRAV            8 0
     C*
     C     RDAT(2)       IFNE      *ZERO
     C                   Z-ADD     RDAT(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTOAV            8 0
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN91
     C                   ENDIF                                                  *IN86 IFEQ
     C*
     C*
     C                   MOVE      *OFF          *IN95
     C                   Z-ADD     17            XXLINE
     C*
     C*  IF SPLIT REQUESTED AND THE EXPERIENCE MODS ARE SPLIT, LOAD THE
     C*  DATE ARRAY.
     C*
     C     F1SPLA        IFEQ      'Y'
     C     ELEMD2        ANDNE     *ZERO
     C     ELEMD2        ANDGT     ELPFDT
     C     ELEMD2        ANDLT     XXTODT
     C     F1SPLA        OREQ      'Y'
     C     ELEMD3        ANDNE     *ZERO
     C     ELEMD3        ANDGT     ELPFDT
     C     ELEMD3        ANDLT     XXTODT
     C                   MOVE      *ON           *IN90
     C*
     C*  CHECK FOR SPLIT ON ANNIVERSARY DATE
     C*
     C     *IN88         IFEQ      *OFF
     C     ELEMD2        ANDEQ     XXANDT
     C     ELEMD3        ANDEQ     *ZERO
     C     *IN88         OREQ      *OFF
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     XXANDT
     C     *IN86         OREQ      *ON
     C     ELEMD2        ANDEQ     1030401
     C     ELEMD3        ANDEQ     *ZERO
     C     *IN86         OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     1030401
     C     *IN87         OREQ      *ON
     C     ELEMD2        ANDEQ     1031001
     C     ELEMD3        ANDEQ     *ZERO
     C     *IN87         OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     1031001
     C     XXDEFL        OREQ      *ON
     C     ELEMD2        ANDEQ     1081001
     C     ELEMD3        ANDEQ     *ZERO
     C     XXDEFL        OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     1081001
     C     XX0709        OREQ      *ON
     C     ELEMD2        ANDEQ     1090701
     C     ELEMD3        ANDEQ     *ZERO
     C     XX0709        OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     1090701
     C     XXAZ10        OREQ      *ON
     C     ELEMD2        ANDEQ     1100101
     C     ELEMD3        ANDEQ     *ZERO
     C     XXAZ10        OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     1100101
     C     XXOK10        OREQ      *ON
     C     ELEMD2        ANDEQ     xxSpltDt
     C     ELEMD3        ANDEQ     *ZERO
     C     XXOK10        OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     xxSpltDt
     C     XXTN14        OREQ      *ON
     C     ELEMD2        ANDEQ     xxSpltDt
     C     ELEMD3        ANDEQ     *ZERO
     C     XXTN14        OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     xxSpltDt
     C     XXOK14        OREQ      *ON
     C     ELEMD2        ANDEQ     xxSpltDt
     C     ELEMD3        ANDEQ     *ZERO
     C     XXOK14        OREQ      *ON
     C     ELEMD2        ANDLE     ELPFDT
     C     ELEMD3        ANDEQ     xxSpltDt
     C                   MOVE      *OFF          *IN90
     C                   ELSE
     C*
     C*  LOAD THE DATES INTO ARRAY PDAT
     C*
     C                   Z-ADD     9999999       PDAT
     C                   Z-ADD     1             Z                 3 0
     C                   Z-ADD     ELPFDT        PDAT(Z)
     C                   ADD       1             Z
     C*
     C     *IN86         IFEQ      *ON
     C     1030401       ANDNE     ELEMD2
     C     1030401       ANDNE     ELEMD3
     C     1030401       ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     *IN86         OREQ      *ON
     C     1030401       ANDNE     ELEMD2
     C     1030401       ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     1030401       PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C     *IN87         IFEQ      *ON
     C     1031001       ANDNE     ELEMD2
     C     1031001       ANDNE     ELEMD3
     C     1031001       ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     *IN87         OREQ      *ON
     C     1031001       ANDNE     ELEMD2
     C     1031001       ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     1031001       PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C     XXDEFL        IFEQ      *ON
     C     1081001       ANDNE     ELEMD2
     C     1081001       ANDNE     ELEMD3
     C     1081001       ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     XXDEFL        OREQ      *ON
     C     1081001       ANDNE     ELEMD2
     C     1081001       ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     1081001       PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C     XX0709        IFEQ      *ON
     C     1090701       ANDNE     ELEMD2
     C     1090701       ANDNE     ELEMD3
     C     1090701       ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     XX0709        OREQ      *ON
     C     1090701       ANDNE     ELEMD2
     C     1090701       ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     1090701       PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C     XXAZ10        IFEQ      *ON
     C     1100101       ANDNE     ELEMD2
     C     1100101       ANDNE     ELEMD3
     C     1100101       ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     XXAZ10        OREQ      *ON
     C     1100101       ANDNE     ELEMD2
     C     1100101       ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     1100101       PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C     XXOK10        IFEQ      *ON
     C     xxSpltDt      ANDNE     ELEMD2
     C     xxSpltDt      ANDNE     ELEMD3
     C     xxSpltDt      ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     XXOK10        OREQ      *ON
     C     xxSpltDt      ANDNE     ELEMD2
     C     xxSpltDt      ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     xxSpltDt      PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C     XXTN14        IFEQ      *ON
     C     xxSpltDt      ANDNE     ELEMD2
     C     xxSpltDt      ANDNE     ELEMD3
     C     xxSpltDt      ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     XXTN14        OREQ      *ON
     C     xxSpltDt      ANDNE     ELEMD2
     C     xxSpltDt      ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     xxSpltDt      PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C     XXOK14        IFEQ      *ON
     C     xxSpltDt      ANDNE     ELEMD2
     C     xxSpltDt      ANDNE     ELEMD3
     C     xxSpltDt      ANDNE     XXANDT
     C     *IN88         ANDEQ     *OFF
     C     XXOK14        OREQ      *ON
     C     xxSpltDt      ANDNE     ELEMD2
     C     xxSpltDt      ANDNE     ELEMD3
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     xxSpltDt      PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C*
     C     *IN88         IFEQ      *OFF
     C     XXANDT        ANDLT     XXTODT
     C     XXANDT        ANDNE     ELEMD2
     C     XXANDT        ANDNE     ELEMD3
     C                   Z-ADD     XXANDT        PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF
      /free
       If ELemd2 > sv1stPyEfdt
        and ELemd2 < xxtodt;
           pDat(z) = ELemd2;
           z+=1;
       Endif;
       //
       If ELemd3 > sv1stPyEfdt
        and ELemd3 < xxToDt;
           pDat(z) = ELemd3;
           z+=1;
       Endif;
      /end-free
     C*
     C                   Z-ADD     XXTODT        PDAT(Z)
     C                   SORTA     PDAT
     C                   Z-ADD     2             Z
     C                   Z-ADD     1             Y                 3 0
     C                   MOVE      *ON           *IN91
     C                   ENDIF
     C*
     C                   ELSE

     C                   If         xxTn14  =  *on and
     c                              xxSpltDt <> xxAndt
     c                              or xxOK14 = *on and
     c                                 xxSpltDt <> xxAnDt
     C                   eval      *in90 = *on

     C                   Z-ADD     9999999       PDAT
     C                   Z-ADD     1             Z                 3 0
     C                   Z-ADD     ELPFDT        PDAT(Z)
     C                   ADD       1             Z

     C     *IN88         IFEQ      *OFF
     C     XXTN14        OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C     xxOK14        OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C                   Z-ADD     xxSpltDt      PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF

     C     *IN88         IFEQ      *OFF
     C     XXANDT        ANDLT     XXTODT
     C                   Z-ADD     XXANDT        PDAT(Z)
     C                   ADD       1             Z
     C                   ENDIF

     C                   Z-ADD     XXTODT        PDAT(Z)
     C                   SORTA     PDAT
     C                   Z-ADD     2             Z
     C                   Z-ADD     1             Y                 3 0
     C                   MOVE      *ON           *IN91

     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         XXEFDT
     C*
     C     NEXPER        TAG
     C*    Initialize fields for self reporting payrolls
     C                   Z-ADD     0             I                 3 0          INDEX VAR
     C                   CLEAR                   CLS
     C                   CLEAR                   SPC
     C
       //*** Beg Add *** 10/28/20 ******************************************
       intSeq#Rw1 = 0;
       //*** End Add *** 10/28/20 ******************************************
     C*
     C     EPKY          SETLL     PEP01
     C     EPKY1         READE     PEP01                                  55
     C     *IN55         DOWEQ     *OFF
     C     *IN85         OREQ      *ON
       //*** Beg Add *** 10/28/20 ******************************************
       isHeadRw1 = *off;
       //*** End Add *** 10/28/20 ******************************************
     C*
     C                   If        *in85
     C                   Exsr      ClrPayroll
     C                   ENDIF                                                  WAFTY <> 'E'
     C*
     C     EPSTAT        IFNE      'I'
     C*** begin add ***  06/10/21  ******************************************
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEPDT            8 0
     C*** end add *****  06/10/21  ******************************************
     C*
     C     *IN71         IFEQ      *ON                                          self-reporting
     C*  Check if class/special designation  already exists in array
     C                   MOVE      *OFF          *IN43
     C*
     C                   DO        I             M
     C     EPCLAS        IFEQ      CLS(M)
     C     EPSPEC        ANDEQ     SPC(M)
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Store class code and special designation in arrays
     C     *IN43         IFEQ      *OFF
     C*
     C                   ADD       1             I
     C                   Z-ADD     EPCLAS        CLS(I)
     C                   MOVE      EPSPEC        SPC(I)
     C*
     C                   ENDIF                                                  *IN43=*OFF
     C*
     C                   ENDIF                                                  *IN71=*ON
     C*  LOAD SECOND ANNIVERSARY RATING PERIOD IF CURRENT PAYROLL
     C*  EFFECTIVE DATES MATCH ANNVERSARY RATING DATE PLUS ONE YEAR
     C*
     C     *IN88         IFEQ      *OFF
     C     SVANDT        ANDEQ     EPEFDT
     C     *IN86         OREQ      *ON
     C     SVRTDT        ANDEQ     EPEFDT
     C     *IN87         OREQ      *ON
     C     SVRTD2        ANDEQ     EPEFDT
     C     XXDEFL        OREQ      *ON
     C     SVRTDT        ANDEQ     EPEFDT
     C     XX0709        OREQ      *ON
     C     SVRTDT        ANDEQ     EPEFDT
     C     XXAZ10        OREQ      *ON
     C     SVRTDT        ANDEQ     EPEFDT
     C     XXOK10        OREQ      *ON
     C     SVRTDT        ANDEQ     EPEFDT
     C     XXTN14        OREQ      *ON
     C     SVRTDT        ANDEQ     EPEFDT
     C     XXOK14        OREQ      *ON
     C     SVRTDT        ANDEQ     EPEFDT
     C                   Z-ADD     XXTOAV        XXFRAV
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    RDAT(X)                            60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     RDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTOAV            8 0
     C                   ENDIF                                                  *IN60 IFEQ
     C*
      /free
       If svAndt = EPefdt;
          svAndt = 0;
       Endif;
       If svRtdt = EPefdt;
          svRtdt = 0;
       Endif;
       If svRtd2 = EPefdt;
          svRtd2 = 0;
       Endif;
      /end-free
     C*
     C                   MOVE      *ON           *IN91
     C                   ENDIF                                                  *IN88 IFEQ
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C*
     C*  LOAD THE PAYROLL CLASS CODE DESCRIPTION
     C*
      /free
       xxType = 'P';
       x1EfDt = SrEffd(EPco#:EPfnd:EPfyr:EPemp#:EPdiv:EPefdt:xxType:EPclas);
       Setgt prKy1 ppr01;
       Readpe prKy ppr01;
       If not(%eof);
          xxDesc = PRdesc;
          xxArfl = PRarfl;
       Else;
          xxDesc = *blanks;
          xxArfl = *blanks;
       Endif;
      /end-free
     C*
     C*
      *  CONVERT PAYROLL CX DATE TO 8 DIGIT FORMAT AFTER CHAIN TO
      *  EMPLOYER PAYROLL FILE
     C*
     C                   Z-ADD     EPCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCADT            8 0
     C*
      *  IF PAYRL CX DATE IS ZERO, SETON *IN34 SO UNDERLINE IS PRINTED
     C*
     C                   MOVE      *OFF          *IN34
     C     EPCADT        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN34
     C                   ENDIF                                                  ENDIF EPCADT
     C*
     C*
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        XXDESC
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        XXDESC
     C                   ENDIF                                                  EPEXT IFNE
     C*
     C     *IN71         IFEQ      *OFF
     C     *IN73         OREQ      *OFF
     C                   Z-ADD     EPEAPR        XXAMT             9 0          Estimated
     C                   ELSE
     C                   Z-ADD     *LOVAL        XXFRMY
     C                   Z-ADD     *HIVAL        XXTOMY
     C                   Z-ADD     EPSAPR        XXAMT             9 0          Estimated
     C                   ENDIF                                                  *IN71=*OFF
     C*
     C                   Z-ADD     XXFRAV        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCOMP            7 0
      *
     C     EPEFDT        IFGT      XXCOMP
      *
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEPDT            8 0
     C                   MOVE      *ON           *IN18
      /free
       xxEPDT1= xxEPDT;
      /end-free
      *
     C                   ELSE
     C                   MOVE      *OFF          *IN18
     C                   ENDIF                                                  EPEFDT IFGT XXCOMP
     C     WAFTY         IFNE      'E'
     C*
     C*  PRINT DETAIL LINE
     C*
     C     *IN97         IFEQ      *ON
     C                   Z-ADD     17            XXLINE
     C     *IN95         IFEQ      *ON
     C                   Add       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ENDIF
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEAD
     c                   WRITE     HDLOGO
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   ADD       1             XXLINE
     C                   ENDIF
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEADDT
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEADD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       3             XXLINE
     C                   Move      *OFF          *IN97
     C                   Endif
     C                   Endif                                                  WAFTY <> 'E'
     C*
     C     *IN95         IFEQ      *OFF
     C                   MOVE      *ON           *IN95
     C                   ENDIF
     C*
     C     WAFTY         IFNE      'E'
     C*
     C*  PRINT SPLIT HEADING, IF IT IS DUE
     C*
     C     *IN91         IFEQ      *ON
     C     XXLINE        IFGE      40
     C                   Z-ADD     17            XXLINE
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2

       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
     c                   WRITE     HDLOGO
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   ADD       1             XXLINE
     C                   ENDIF
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEADDT
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEADD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       3             XXLINE
     C                   MOVE      *OFF          *IN97
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *ON
     C     ELPFDT        IFEQ      PDAT(Y)
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     POLHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Policy Start Date';
          strHddesc = 'Policy Start Date';
          Exsr ScriptWxrw1p;
          isHeadNoSplit = 'Y';
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     POLHD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C*
     C     XXANDT        IFEQ      PDAT(Y)
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     ANNHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Anniversary Rated';
          strHddesc = 'Anniversary Rated';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     ANNHD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C*
     C     1030401       IFEQ      PDAT(Y)
     C     *IN86         ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     FLRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'FL Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     FLRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C     1031001       IFEQ      PDAT(Y)
     C     *IN87         ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRHD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'FL Rate Decrease';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRH22
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C     1081001       IFEQ      PDAT(Y)
     C     XXDEFL        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     DERHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'DE Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DERH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C     1090701       IFEQ      PDAT(Y)
     C     XX0709        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'FL Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     FLRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C     1100101       IFEQ      PDAT(Y)
     C     XXAZ10        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     AZRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'AZ Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     AZRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C     xxSpltDt      IFEQ      PDAT(Y)
     C     XXOK10        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'OK Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C     xxSpltDt      IFEQ      PDAT(Y)
     C     XXTN14        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     TNRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'TN Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     TNRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C     xxSpltDt      IFEQ      PDAT(Y)
     C     XXOK14        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'OK Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C*
     C     ELEMD3        IFNE      *ZERO
     C     ELEMD3        ANDLE     PDAT(Y)
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     XMOD3
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Experience Mod';
          If *in96 = *off;
             strHddesc   = 'Experience Mod';
          Else;
             strHddesc   = 'Merit Rating';
          Endif;
          //*** Begin Chg *** 07/30/23 *************************************
          //*** decExpMod      = ELexM3;
          If ELnoExM3 = 'Y';
             decExpMod = 0;
          Else;
             decExpMod = ELexM3;
          Endif;
          //*** End   Chg *** 07/30/23 *************************************
          expModDate  = xxEmD3;
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     XMOD32
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ELSE
     C     ELEMD2        IFLE      PDAT(Y)
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     XMOD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Experience Mod';
          If *in96 = *off;
             strHddesc   = 'Experience Mod';
          Else;
             strHddesc   = 'Merit Rating';
          Endif;
          //*** Begin Chg *** 07/30/23 *************************************
          //*** decExpMod      = ELexM2;
          If ELnoExM2 = 'Y';
             decExpMod = 0;
          Else;
             decExpMod = ELexM2;
          Endif;
          //*** End   Chg *** 07/30/23 *************************************
          expModDate  = xxEmD2;
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     XMOD22
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C*
     C                   ELSE
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     XMOD1
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Experience Mod';
          If *in96 = *off;
             strHddesc   = 'Experience Mod';
          Else;
             strHddesc   = 'Merit Rating';
          Endif;
          If ELnoExM1 = 'Y';
             decExpMod = 0;
          Else;
             decExpMod = ELexM1;
          Endif;
          expModDate  = xxEmd1;
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     XMOD12
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     *IN88         IFEQ      *OFF
     C     *IN86         OREQ      *ON
     C
     C     *IN87         OREQ      *ON
     C     XXDEFL        OREQ      *ON
     C     XX0709        OREQ      *ON
     C     XXAZ10        OREQ      *ON
     C     XXOK10        OREQ      *ON
     C     XXTN14        OREQ      *ON
     C     XXOK14        OREQ      *ON
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    RDAT(X)                              6060
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SELECT
     C     RDAT(X)       WHENEQ    XXANDT
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     ANNHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Anniversary Rated';
          strHddesc   = 'Anniversary Rated';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     ANNHD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    1030401
     C     *IN86         ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'FL Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    1031001
     C     *IN87         ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRHD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'FL Rate Decrease';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRH22
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    1081001
     C     XXDEFL        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DERHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'DE Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DERH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    1090701
     C     XX0709        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'FL Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     FLRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    1100101
     C     XXAZ10        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     AZRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'AZ Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     AZRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    xxSpltDt
     C     XXOK10        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'OK Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    xxSpltDt
     C     XXTN14        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     TNRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'TN Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     TNRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     RDAT(X)       WHENEQ    xxSpltDt
     C     XXOK14        ANDEQ     *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'State Rate Change';
          strHddesc = 'OK Rate Change';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OKRH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   OTHER
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     POLHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Policy Start Date';
          strHddesc = 'Policy Start Date';
          Exsr ScriptWxrw1p;
          isHeadNoSplit = 'Y';
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     POLHD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ELSE
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     POLHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          isHeadRw1  = *on;
          strHdTypRw1 = 'Policy Start Date';
          strHddesc = 'Policy Start Date';
          Exsr ScriptWxrw1p;
          isHeadNoSplit = 'Y';
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     POLHD2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  *IN88 IFEQ
     C                   ENDIF                                                  *IN90 IFEQ
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DATLIN
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DATLI2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       2             XXLINE
     C                   MOVE      *OFF          *IN91
     C                   ENDIF
     C*
     C*  FOR SPLIT, EXECUTE SRSPLT, OTHERWISE, JUST WRITE THE DETAIL LINE.
     C*
     C     *IN90         IFEQ      *ON
     C     *IN71         IFEQ      *OFF
     C     QQSRFL        OREQ      ' '
     C*** Beg Add *** 10/28/20 ***********************************************************
     C     QQSRFL        OREQ      'N'
     C*** End Add *** 10/28/20 ***********************************************************
     C                   EXSR      SRSPLT                                       Estimating
     C                   ELSE
     C                   EXSR      SRSPL2                                       Self-reporting
     C                   ENDIF                                                    *IN71=*OFF
     C                   ELSE
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseScript = *on and flgNoOutput <> 'Y' and isHeadNoSplit <> 'Y';
          isHeadRw1 = *on;
          strHdTypRw1 = 'Policy Start Date';
          strHddesc = 'Policy Start Date';
          Exsr ScriptWxrw1p;
          isHeadNoSplit = 'Y';

       //*** begin del ***  06/10/21  **************************************************
       //*** Beg Undelete *** 06/16/21 ************************************
       //Print Exp mod1 heading
       isHeadRw1  = *on;
       strHdTypRw1 = 'Experience Mod';
       If *in96 = *off;
          strHddesc   = 'Experience Mod';
       Else;
          strHddesc   = 'Merit Rating';
       Endif;
       If ELnoExM1 = 'Y';
          decExpMod = 0;
       Else;
          decExpMod = ELexM1;
       Endif;
       expModDate  = xxEmd1;
       Exsr ScriptWxrw1p;
       //*** End Undelete *** 06/16/21 ************************************
       //*** end del *****  06/10/21  **************************************************
       Endif;
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       2             XXLINE            3 0
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  WAFTY <> 'E'
     C*
     C     EPKY1         READE     PEP01                                  55
     C*
     C     *IN90         IFEQ      *ON
     C     *IN55         ANDEQ     *OFF
     C     EPEFDT        ANDGE     PDAT(Z)
     C                   MOVE      *ON           *IN55
     C                   ENDIF
     C*
     C     *IN85         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  IF SPLIT FOR EXPERIENCE MODS, GO BACK THROUGH PAYROLLS FOR NEXT
     C*  PERIOD.
     C*
     C     *IN90         IFEQ      *ON
     C                   ADD       1             Z
     C                   ADD       1             Y
     C*
     C     PDAT(Z)       IFNE      9999999
     C*
     C*  FOR ANNIVERSARY RATING, POSITION PAYROLL FILE PAST ANNIVERSARY
     C*  DATE IF WE ARE RATING PERIODS BEYOND THAT DATE
     C*
     C     *IN88         IFEQ      *OFF
     C     *IN86         OREQ      *ON
     C     *IN87         OREQ      *ON
     C     XXDEFL        OREQ      *ON
     C     XX0709        OREQ      *ON
     C     XXAZ10        OREQ      *ON
     C     XXOK10        OREQ      *ON
     C     XXTN14        OREQ      *ON
     C     XXOK14        OREQ      *ON
     C*
     C                   Z-ADD     1             X
     C     PDAT(Y)       LOOKUP    RDAT(X)                              6060
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     RDAT(X)       XXEFDT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN88 IFEQ
     C*
     C                   MOVE      *ON           *IN91
     C                   GOTO      NEXPER
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  CHAIN TO OWNER/OFFICERS FILE
     C*
     C                   MOVE      *BLANKS       XXELID
     C     OOKY1         CHAIN     POO01                              60
     C                   MOVEA     *BLANKS       NM
      *
     C*  IF CHAIN IS UNSUCCESSFUL, SET *IN29 ON SO OWN/OFF HEADING ISN'T PRINTED
     C*
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      *ON           *IN29
      *
     C                   ELSE
      *
     C                   MOVE      *OFF          *IN29
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C     *IN95         IFEQ      *ON
     C     WAFTY         IFNE      'E'
     C     XXLINE        IFGE      40
     C                   Z-ADD     17            XXLINE
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     PAGLIN
     c                   Write     HEAD
     c                   WRITE     HDLOGO
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   ADD       1             XXLINE
     C                   ENDIF
     C                   MOVE      *OFF          *IN97
     C                   ENDIF
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     BOT
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     BOT2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     ** ADD THE CORRECT NUMBER OF LINES TO XXLINE BASED ON HOW MANY
     ** COMMENTS WERE PRINTED AND IF THE AGENT PHONE # WAS PRINTED
     **
     C     *IN42         IFEQ      *OFF
     C*
     C     CNT1          IFEQ      5
     C                   ADD       4             XXLINE
     C*
     C                   ELSE
     C                   ADD       CNT           XXLINE
     C*
     C                   ENDIF                                                  CNT1=5
     C*
     C                   ENDIF                                                  *IN42=OFF
     C*
     C                   ADD       6             XXLINE
     C*
     C*  CHECK TO SEE WHICH EXP MODS TO PRINT FOR REPORT
     C*
     C                   MOVE      *ZERO         XCOUNT            3 0
     C                   MOVE      *OFF          *IN41
     C                   MOVE      *OFF          *IN35
     C                   MOVE      *OFF          *IN36
     C                   MOVE      *OFF          *IN37
     C*
     C                   SELECT
     C*
     C     ELEXM3        WHENNE    *ZERO
     C     ELEMD3        ORNE      *ZERO
     C                   MOVE      *ON           *IN37
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN35
     C*
     C     ELEXM2        WHENNE    *ZERO
     C     ELEMD2        ORNE      *ZERO
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN35
     C*
     C     ELEXM1        WHENNE    *ZERO
     C     ELEMD1        ORNE      *ZERO
     C                   MOVE      *ON           *IN35
     C*
     C*  IF NO EXPERIENCE MODIFIERS ARE PRESENT
     C*
     C                   OTHER
     C                   MOVE      *ON           *IN41
     C*
     C                   ENDSL
     C*
     C*
     C     *IN60         IFEQ      *OFF
      *
     C*                    Z-ADD1         X
     C                   Z-ADD     1             X
      *
       //*** Beg Add *** 10/28/20 ****************************************
       intSeq#Rw2 = 0;
       //*** End Add *** 10/28/20 ****************************************
     C     *IN60         DOWEQ     *OFF
      *
      * If Owner/Officer record has an Expiration Date which is LT or EQ
      * policy from date, bypass the record and read next record in OO file.
     C     OOEXDT        IFGT      *ZEROS
     C     OOEXDT        ANDLE     ELPFDT
     C     OOEFDT        ORGT      *ZEROS
     C     OOEFDT        ANDGE     ELPTDT
     C                   GOTO      NEXOOR
     C                   ENDIF
     C*
     C*  COMBINE THE OFFICER'S FIRST & LAST NAMES
     C*
     C     OOOFSN        IFEQ      *BLANK
     C                   MOVEL(P)  OOOLSN        XXNAM            25
     C                   ELSE
     C     OOOFSN        CAT(P)    OOOLSN:1      XXNAM
     C                   ENDIF
     C*
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEDAT             8 0
     C*
      *
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XEDAT6            6 0
      *
     C                   Z-ADD     OOEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDAT6            6 0
      *
     C                   SETOFF                                       43
     C     OOEXDT        IFGT      0
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C*  CONTROL OUTPUT OF EXP MODS AND EFFECTIVE DATES
     C*
     C*  IF 3 EXP MODS ARE PRESENT
     C*
     C                   SETOFF                                       383940
     C*
     C                   SELECT
     C*
     C     *IN37         WHENEQ    *ON
     C     X             ANDEQ     1
     C                   MOVE      *ON           *IN38
     C     *IN37         WHENEQ    *ON
     C     X             ANDEQ     2
     C                   MOVE      *OFF          *IN38
     C                   MOVE      *ON           *IN39
     C     *IN37         WHENEQ    *ON
     C     X             ANDEQ     3
     C                   MOVE      *OFF          *IN39
     C                   MOVE      *ON           *IN40
     C*
     C                   ENDSL
     C*
     C*  IF 2 EXP MODS ARE PRESENT
     C*
     C                   SELECT
     C*
     C     *IN36         WHENEQ    *ON
     C     X             ANDEQ     1
     C                   MOVE      *ON           *IN38
     C     *IN36         WHENEQ    *ON
     C     X             ANDEQ     2
     C                   MOVE      *OFF          *IN38
     C                   MOVE      *ON           *IN39
     C*
     C                   ENDSL
     C*
     C*  IF 1 EXP MOD IS PRESENT
     C*
     C     *IN35         IFEQ      *ON
     C     X             ANDEQ     1
      *
     C                   MOVE      *ON           *IN38
      *
     C                   ENDIF                                                  *IN35 IFEQ *ON
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OWNEXP
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       if isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptWxRw2p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OWNEX2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C*
     C                   ADD       1             XXLINE
     C*
     C     XXLINE        IFGE      55
     C                   Z-ADD     17            XXLINE
     C*
     C                   MOVE      *ON           *IN42
     C*
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
     c                   WRITE     HDLOGO
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   ADD       1             XXLINE
     C                   ENDIF
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     BOT
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     BOT2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     ** ADD THE CORRECT NUMBER OF LINES TO XXLINE BASED ON HOW MANY
     ** COMMENTS WERE PRINTED AND IF THE AGENT PHONE # WAS PRINTED
     **
     C     *IN42         IFEQ      *OFF
     C*
     C     CNT1          IFEQ      5
     C                   ADD       4             XXLINE
     C*
     C                   ELSE
     C                   ADD       CNT           XXLINE
     C*
     C                   ENDIF                                                  CNT1=5
     C*
     C                   ENDIF                                                  *IN42=OFF
     C                   ADD       4             XXLINE
     C*
     C                   ENDIF                                                  XXLINE IFEQ 58
     C*
     C                   ADD       1             X
     C*
     C     X             IFGT      3
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  X IFGT 3
      *
     C     NEXOOR        TAG
      *
     C     OOKY2         READE     POO01                                  60
     C*
     C                   ENDDO                                                  *IN60 EQ *OFF
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C                   ENDIF                                                  WAFTY=E PRT ECM ONLY
     C*
     C*  IF DARTS REMARK CODE IS FILLED, WRITE DARTS RECORD
     C*
     C*
     C*  IF UPDATE AUDIT LOG IS "Y"ES, PERFORM AUDIT LOG SUBROUTINE
     C*
     C     QQRPFL        IFEQ      'Y'
     C                   EXSR      SRAUDT
     C                   ENDIF
     C*
     C*  LIST ADDITIONAL INSUREDS (OTHER DIVISIONS AND LOCATIONS)
     C*
     C                   MOVE      *OFF          *IN23
     C                   Z-ADD     EMDIV         SVDIV             3 0
     C                   MOVE      ELPTDT        SVPTDT            7 0
     C                   MOVE      ELFYR         SVFYR             3 0
     c                   z-add     elhcn         svhcn
     C     EMKY1         CHAIN     PEM01                              51
     C     WAFTY         IFNE      'E'                                          WAFTY=E PRT ECM ONLY
     C*
     C*  PRINT ANY ADDITIONAL EMPLOYER DIVISION ADDRESSES
     C*
     C                   MOVE      *ON           *IN52
     C                   SETON                                        26
     C*
     C     *IN51         DOWEQ     *OFF
     C*
     C                   MOVE      EMDIV         XXDIV
     C*
     C     XXDIV         IFNE      SVDIV
     c     XXCLT#        OREQ      '139'
     c     SVHCN         ANDNE     *zero
     C*
     C                   MOVE      ELFYR         XXPYR
     C*
     C                   MOVE      *OFF          *IN63
     C*
     C*    Determine if pol yr for div exists and is not flat cancelled
     C     ELKY          CHAIN     PEL01                              62
     C     *IN62         IFEQ      *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   MOVE      *ON           *IN63
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C                   ENDIF                                                  *IN62=*OFF
     C*
     C     *IN63         IFEQ      *ON
     C*
     C                   MOVE      *OFF          *IN33
     C                   Z-ADD     5             XL                1 0
     C*
     C                   DO        5
     C*
     C     CX(XL)        IFNE      0
     C     EF(XL)        ANDNE     0
     C*
     C     CX(XL)        IFLE      ELPFDT
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  CX,XL IFLE
     C*
     C                   ELSE
     C                   SUB       1             XL
     C                   ENDIF                                                  CX,XL IFNE
     C*
     C                   ENDDO                                                  DO 5
     C*
     C     *IN33         IFEQ      *OFF
     C*
     C*  FORMAT ADDITIONAL DIVISION TO PRINT
     C*
     C                   MOVE      *BLANKS       XXZIP5
     C                   MOVE      *BLANKS       XXZIP4
     C*
     C     EMPAD1        IFNE      *BLANK
     C                   MOVEL(P)  EMNAME        NAME1
     C                   MOVEL(P)  EMNAM2        NAME2
     C                   MOVE      NAME          SNAME
     C                   MOVEL(P)  SNAM1         XXONAM
     C                   MOVEL(P)  SNAM2         XXONM2
     C                   MOVEL     EMPAD1        XXOAD1
     C                   MOVEL     EMPAD2        XXOAD2
     C                   MOVEL     EMPZIP        XXZIP5
     C                   MOVE      EMPZIP        XXZIP4
     C     EMPCTY        CAT(P)    ',':0         XXOCSZ
     C                   CAT       EMPST:1       XXOCSZ
     C                   CAT       XXZIP5:2      XXOCSZ
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXOCSZ
     C                   CAT       XXZIP4:0      XXOCSZ
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  EMNAME        NAME1
     C                   MOVEL(P)  EMNAM2        NAME2
     C                   MOVE      NAME          SNAME
     C                   MOVEL(P)  SNAM1         XXONAM           50
     C                   MOVEL(P)  SNAM2         XXONM2           50
     C                   MOVEL     EMMAD1        XXOAD1           40
     C                   MOVEL     EMMAD2        XXOAD2           40
     C                   MOVEL     EMMZIP        XXZIP5
     C                   MOVE      EMMZIP        XXZIP4
     C     EMMCTY        CAT(P)    ',':0         XXOCSZ           40
     C                   CAT       EMMST:1       XXOCSZ
     C                   CAT       XXZIP5:2      XXOCSZ
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXOCSZ
     C                   CAT       XXZIP4:0      XXOCSZ
     C                   ENDIF
     C*
     C                   ENDIF                                                  EMPAD1<>*BLANK
       //*** Beg Add *** 10/28/20 ****************************************
       // Load Scriptura Mailing and physical Addresses
       If isUseScript = *on and flgNoOutput <> 'Y';
          Clear prw301;
          intSeq#Rw3 = 0;
          RW3Mad1 = EMmAd1;
          RW3Mad2 = EMmAd2;
          RW3Mcty = EMmCty;
          RW3Mst  = EMmSt;
          RW3Mzip = EMmZip;
          RW3pAd1 = EMpAd1;
          RW3pAd2 = EMpAd2;
          RW3pCty = EMpCty;
          RW3pSt  = EMpSt;
          RW3pZip = EMpZip;
          RW3aDiv = EMdiv;
          RW3name = EMname + EMnam2;
          RW3fdNm = EMfdNm;
          Exsr ScriptWxrw3p;
       Endif;
       //*** End Add *** 10/28/20 ****************************************
     C*
     C     XXOAD2        IFNE      *BLANK
     C                   MOVE      *ON           *IN22
     C                   ELSE
     C                   MOVE      *OFF          *IN22
     C                   ENDIF
     C*
      *  IF XXOCSZ=BLANK, DON'T PRINT
     C*
     C     XXOCSZ        IFNE      *BLANK
     C                   MOVE      *ON           *IN25
     C                   ELSE
     C                   MOVE      *OFF          *IN25
     C                   ENDIF
     C                   MOVE      *OFF          *IN21
     c                   clear                   addr
     c                   clear                   hcn
     c                   z-add     1             v                 3 0

     c                   if        elhcn <> 0
     C                   MOVE      *ON           *IN21

     c                   movel     xxoad1        addr(v)
     c                   add       1             v
     C     XXOAD2        IFNE      *BLANK
     c                   movel     xxoad2        addr(v)
     c                   add       1             v
     C                   ENDIF
     C     XXOCSZ        IFNE      *BLANK
     c                   movel     xxocsz        addr(v)
     c                   add       1             v
     C                   ENDIF

     c                   z-add     1             v                 3 0
     c     elky          chain     phc01                              60
     c     *in60         doweq     *off

     C                   Z-ADD     HCIN          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     c                   movel     effdh         hcn(v)
     c                   cat       xxm:2         hcn(v)
     c                   cat       '/':0         hcn(v)
     c                   cat       xxd:0         hcn(v)
     c                   cat       '/':0         hcn(v)
     c                   cat       xxy:0         hcn(v)
     c                   add       1             v

     c                   if        hcout <> 0
     C                   Z-ADD     HCOUT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     c                   movel     expdh         hcn(v)
     c                   cat       xxm:2         hcn(v)
     c                   cat       '/':0         hcn(v)
     c                   cat       xxd:0         hcn(v)
     c                   cat       '/':0         hcn(v)
     c                   cat       xxy:0         hcn(v)
     c                   add       1             v
     c                   endif

     c     elky          reade     phc01                                  60
     c                   enddo

     c                   endif
     C*
     C     *IN97         IFEQ      *ON
     C                   Z-ADD     17            XXLINE
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
     c                   WRITE     HDLOGO
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   ADD       1             XXLINE
     C                   ENDIF
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     OTHPHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     OTHPH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   SETOFF                                       26
     C                   ADD       4             XXLINE
     C                   MOVE      *OFF          *IN97
     C*                    MOVE *ON       *IN23
     C                   ENDIF
     C*
     C     *IN23         IFEQ      *OFF
     C     XXLINE        IFGT      50
     C                   Z-ADD     17            XXLINE
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
     c                   WRITE     HDLOGO
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   ADD       1             XXLINE
     C                   ENDIF
     C                   Z-ADD     17            XXLINE
     C                   ADD       4             XXLINE
     C                   ENDIF                                                  XXLINE IFGT 54
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHPHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHPH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   SETOFF                                       26
     C                   ENDIF                                                  *IN23 IFEQ *OFF
     C*
     C                   EXSR      SROVFW
     c                   exsr      PrintAddIns
     c                   if        *in21 = *on

     c                   z-add     1             v
     c                   dow       addr(v) <> *blanks
     c                               or hcn(v) <> *blanks

     c                   move      *blanks       xxaddr           40
     c                   move      *blanks       xxhcnh           11
     c                   move      *blanks       xxhcnd           19
     c                   z-add     0             xxhcn             5 4

     c                   if        addr(v) <> *blanks
     c                   movel     addr(v)       xxaddr
     c                   endif
     c
     c                   if        v = 1
     c                   z-add     elhcn         xxhcn
     c                   movel     hcnh          xxhcnh
     c                   endif

     c                   if        hcn(v) <> *blanks
     c                   movel     hcn(v)        xxhcnd
     c                   endif

       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     OTHHCN
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************

     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHHC2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF

     c                   add       1             v
     c                   enddo

       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     OTHHC3
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHHC4
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF

     c                   endif                                                  *in21 = *on
     C     XXONAM        IFNE      *BLANKS
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  XXONAM IFNE *BLANKS
     C     XXONM2        IFNE      *BLANKS
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  XXONM2 IFNE *BLANKS
     C     XXOFDNM       IFNE      *BLANKS
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  XXOFDNM <> *BLANKS

     C     XXOFDNM2      IFNE      *BLANKS
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  XXOFDNM2 <> *BLANKS
     C*
     C     XXOAD1        IFNE      *BLANKS
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  XXOAD1 IFNE *BLANKS
     C*
     C     XXOAD2        IFNE      *BLANKS
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  XXOAD2 IFNE *BLANKS
     C     XXOCSZ        IFNE      *BLANKS
     C                   ADD       1             XXLINE
     C                   ENDIF                                                  XXOCSZ IFNE *BLANKS
     C                   EXSR      CLROTH
     C*
     C                   MOVE      *ON           *IN23
     C                   ENDIF                                                  *IN33 IFEQ
     C                   ENDIF                                                  *IN63=*ON
     C                   ENDIF                                                  XXDIV NE SVDIV
     C*
     C     EMKY1         READE     PEM01                                  51
     C                   ENDDO                                                  *IN51 = *OFF
     C                   ENDIF                                                  WAFTY=E PRT ECM ONLY
     C                   MOVE      *OFF          *IN52
     C                   MOVE      *OFF          *IN29
     C*
     C*  RESET POINTER TO CURRENT EMPLOYER
     C*
     C                   Z-ADD     SVDIV         XXDIV
     C     EMKY2         CHAIN     PEM01                              51
     C*
     C     *IN24         IFEQ      *ON
     C     ELKY          CHAIN     PEL01                              62
     C*
     C                   ELSE
     C*
     C                   MOVE      SVPTDT        XXFTDT
     C     ELKY3         SETLL     PEL02                              62
     C     ELKY2         READE(N)  PEL02                                  62
     C*
     C     *IN62         DOWEQ     *OFF
     C     ELFYR         ANDNE     SVFYR
     C     ELKY2         READE(N)  PEL02                                  62
     C                   ENDDO                                                  *IN62=*OFF
     C*
     C                   ENDIF                                                  *IN24=*ON
     C     WAFTY         IFNE      'E'                                          WAFTY=E PRT ECM ONLY
     C*
     C*  PRINT ANY ADDITIONAL ENTITY/LOCATION ADDRESSES
     C*
     C     EMKY2         SETLL     PAL01
       //*** Beg Add *** 10/28/20 ****************************************
       intSeq#Rw3 = 0;
       //*** End Add *** 10/28/20 ****************************************
      *
     C     *IN51         DOUEQ     *ON
     C                   MOVE      EMDIV         XXDIV
     C     EMKY2         READE     PAL01                                  51
     C*
     C     *IN51         IFEQ      *OFF
     C     ALEFDT        ANDLT     ELPTDT
     C     ALDLDT        IFGT      ELPFDT
     C     ALDLDT        OREQ      *ZERO
      *
      *
      *  Print Entity Location Mailing Address if not blank
      *
     C                   SETOFF                                       28
     C                   MOVEL(P)  ALELID        XXELID
      *
     C     ALMAD1        IFNE      *BLANK
     C                   MOVEL(P)  ALNAME        NAME1
     C                   MOVEL(P)  ALNAM2        NAME2
     C                   MOVE      NAME          SNAME
     C                   MOVEL(P)  SNAM1         XXONAM
     C                   MOVEL(P)  SNAM2         XXONM2
     c                   movel(p)  alfdnm        name1
     c                   movel(p)  alfdn2        name2
     c                   move      name          sname
     c                   movel(p)  snam1         xxofdnm          50
     c                   movel(p)  snam2         xxofdnm2         50
     C                   MOVEL     ALMAD1        XXOAD1           40
     C                   MOVEL     ALMAD2        XXOAD2           40
     C     ALMAD2        COMP      *BLANK                             2222      Don't prt blank
     C                   MOVEL(P)  ALMZIP        XXZIP5
     C                   MOVE      ALMZIP        XXZIP4
      *
      *  IF CITY, STATE AND ZIP ARE ALL BLANK DO NOT CONCATENATE
      *  AS THIS WILL ADD AN UNNECESSARY COMMA
      *
     C     ALMCTY        IFNE      *BLANK
     C     ALMST         ORNE      *BLANK
     C     XXZIP5        ORNE      *ZERO
     C     ALMCTY        CAT(P)    ',':0         XXOCSZ           40
     C                   CAT       ALMST:1       XXOCSZ
     C                   CAT       XXZIP5:1      XXOCSZ
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        XXOCSZ
     C                   ENDIF                                                  ENDIF ALMCTY
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXOCSZ
     C                   CAT       XXZIP4:0      XXOCSZ
     C                   ENDIF
      *
     C     XXOCSZ        COMP      *BLANKS                            2525      Don't prt blank
     c                   move      *off          *in21
      *
     C     *IN97         IFEQ      *ON
     C     XXLINE        ORGE      49
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
     c                   WRITE     HDLOGO
     C                   Write     OTHPHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
     C                   Write     OTHPH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   MOVE      *OFF          *IN97
     C                   Z-ADD     23            XXLINE
     C                   SETOFF                                       26
     C                   ENDIF                                                  *inof
      *
     C     *IN26         IFEQ      *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     OTHPHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHPH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       5             XXLINE
     C                   SETOFF                                       26
     C                   ENDIF                                                  *inof
      *
     C                   EXSR      SROVFW
     c                   exsr      PrintAddIns
     C                   EXSR      CLROTH
     C                   SETON                                        28
     C                   ADD       3             XXLINE
     C   22              ADD       1             XXLINE
     C   25              ADD       1             XXLINE
       //*** Beg Add *** 10/28/20 *********************************************************
       // Load Scriptura Mailing Address
       If isUseScript = *on and flgNoOutput <> 'Y';
          Clear prw301;
          RW3Mad1 = ALmAd1;
          RW3Mad2 = ALmAd2;
          RW3Mcty = ALmCty;
          RW3Mst  = ALmSt;
          RW3Mzip = ALmZip;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************

     C                   ENDIF                                                  ALMAD1 <> BLANK
      *
      *  Print Entity Location Physical Address if not blank
      *
     C     ALPAD1        IFNE      *BLANK
     C                   MOVEL(P)  ALNAME        NAME1
     C                   MOVEL(P)  ALNAM2        NAME2
     C                   MOVE      NAME          SNAME
     C                   MOVEL(P)  SNAM1         XXONAM
     C                   MOVEL(P)  SNAM2         XXONM2
     c                   movel(p)  alfdnm        name1
     c                   movel(p)  alfdn2        name2
     c                   move      name          sname
     c                   movel(p)  snam1         xxofdnm          50
     c                   movel(p)  snam2         xxofdnm2         50
     C                   MOVEL     ALPAD1        XXOAD1           40
     C                   MOVEL     ALPAD2        XXOAD2           40
     C     ALPAD2        COMP      *BLANK                             2222      Don't prt blank
     C                   MOVEL(P)  ALPZIP        XXZIP5
     C                   MOVE      ALPZIP        XXZIP4
      *
      *  IF CITY, STATE AND ZIP ARE ALL BLANK DO NOT CONCATENATE
      *  AS THIS WILL ADD AN UNNECESSARY COMMA
      *
     C     ALPCTY        IFNE      *BLANK
     C     ALPST         ORNE      *BLANK
     C     XXZIP5        ORNE      *ZERO
     C     ALPCTY        CAT(P)    ',':0         XXOCSZ           40
     C                   CAT       ALPST:1       XXOCSZ
     C                   CAT       XXZIP5:1      XXOCSZ
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        XXOCSZ
     C                   ENDIF                                                  ENDIF ALPCTY
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXOCSZ
     C                   CAT       XXZIP4:0      XXOCSZ
     C                   ENDIF
      *
     C     XXOCSZ        COMP      *BLANKS                            2525      Don't prt blank
     c                   move      *off          *in21
      *
     C     *IN97         IFEQ      *ON
     C     XXLINE        ORGE      49
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
     c                   WRITE     HDLOGO
     C                   Write     OTHPHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
     C                   Write     OTHPH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   MOVE      *OFF          *IN97
     C                   Z-ADD     23            XXLINE
     C                   SETOFF                                       26
     C                   ENDIF                                                  *inof
      *
     C     *IN26         IFEQ      *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHPHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHPH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       5             XXLINE
     C                   SETOFF                                       26
     C                   ENDIF                                                  *inof
      *
     C                   EXSR      SROVFW
     c                   exsr      PrintAddIns
     C                   EXSR      CLROTH
     C                   ADD       3             XXLINE
     C   22              ADD       1             XXLINE
     C   25              ADD       1             XXLINE
       //*** Beg Add *** 10/28/20 *********************************************************
       // Load Scriptura Physical Address
       If isUseScript = *on and flgNoOutput <> 'Y';
          RW3Pad1 = ALpAd1;
          RW3Pad2 = ALpAd2;
          RW3Pcty = ALpCty;
          RW3Pst  = ALpSt;
          RW3Pzip = ALpZip;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF                                                  ALPAD1 <> BLANK
       //*** Beg Add *** 10/28/20 *********************************************************
       // Write to Additional Location workfile unless NCCI = N.
       If isUseScript = *on and flgNoOutput <> 'Y' and ALncci <> 'N';
          RW3Adiv = ALdiv;
          RW3Aloc = ALelId;
          RW3Name = ALname + ALnam2;
          RW3FdNm = ALfdNm;
          Exsr ScriptWxrw3p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
      *
      *  If Entity, list Owners/Officers for each additional location.
      *
     C     ALELTP        IFEQ      'E'
     C                   MOVEL(P)  ALELID        XXELID
     C     OOKY1         CHAIN     POO01                              60
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  ALELTP = E
      *
      *  LIST OWNER/OFFICERS FOR EACH ADDITIONAL LOCATION
      *
     C                   MOVE      *ON           *IN27
      *
     C     *IN60         DOWEQ     *OFF
      *
      *  COMBINE THE OFFICER'S FIRST & LAST NAMES
      *
     C     OOOFSN        IFEQ      *BLANK
     C                   MOVEL(P)  OOOLSN        XXNAM            25
     C                   ELSE
     C     OOOFSN        CAT(P)    OOOLSN:1      XXNAM
     C                   ENDIF
      *
      *    EFFECTIVE DATE
      *
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEDAT             8 0
      *
      *
     C                   Z-ADD     OOEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDAT             8 0
      *
     C                   SETOFF                                       43
     C     OOEXDT        IFGT      0
     C                   MOVE      '1'           *IN43
     C                   ENDIF
      *  PRINT OWNER / OFFICER LINE
      *
     C     *IN97         IFEQ      *ON
     C     XXLINE        ORGE      55
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
     c                   WRITE     HDLOGO
     C                   Write     OTHPHD
     C                   Write     HEDOFF
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
     C                   Write     OTHPH2
     C                   Write     HEDOF2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   MOVE      *OFF          *IN97
     C                   MOVE      *OFF          *IN27
     C                   Z-ADD     25            XXLINE
     C                   ENDIF                                                  *inof
      *
     C     *IN27         IFEQ      *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEDOFF
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEDOF2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   MOVE      *OFF          *IN27
     C                   ADD       2             XXLINE
     C                   ENDIF                                                  *IN27 IFEQ *ON
      *
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     ADDOFF
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     ADDOF2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       1             XXLINE
      *
     C     OOKY1         READE     POO01                                  60
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C   60              Write     BLANK
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C   60              Write     BLANK2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C   60              ADD       1             XXLINE
     C                   ENDDO                                                  *IN60 DOWEQ*OFF
      *
     C                   ENDIF                                                  NOT DELETED
     C                   ENDIF                                                  *IN51 = *OFF
      *
     C                   ENDDO                                                  *IN51 = *ON
     C*
     C     *IN71         IFEQ      *ON                                          *IN71=*ON
     C     Q1PRMS        ANDGT     33
     C     QQRFLG        ANDEQ     'Y'                                          *IN71=*ON
       //*** Beg Add *** 10/28/20 *********************************************************
       // If Scriptura output then there will be a new format for periods, classes, etc.
       If isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptSrPayroll;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C** Here, we want to start a new page and print the monthly payroll
     C** history.  5 classes across page and 12 months down at a time.
     C**             Class   Class   Class   Class  Class
     C** (mth 1)
     C** (mth 2) ...
     C*
     C*  Get range of months for self reporting payrolls
     C                   EXSR      SRRNGE
     C*
     C*  Get # of lines for each set of class codes
     C                   EXSR      SR#LNS
     C                   Z-ADD     99            ZZLNS             3 0
     C*
     C*  Split class codes into groups of 5 (XTIMES)
     C     I             DIV       5             XTIMES            3 0
     C                   MVR                     REM               3 0
     C     REM           IFNE      0
     C                   ADD       1             XTIMES
     C                   ENDIF
     C*
     C                   DO        XTIMES        J                 3 0
     C*
     C                   ADD       #LNS          ZZLNS
     C     ZZLNS         IFGT      42
     C*
     C*    Start a new page and print header
     C     *IN95         IFEQ      *ON
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN95
     C                   ENDIF
     c                   move      MAD8          MAD8_1
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEAD
     c                   WRITE     HDLOGO
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C*
     C                   Z-ADD     #LNS          ZZLNS
     C*
     C                   ENDIF                                                  ZZLNS>40
     C*
     C*      Print header line for class codes
     C     J             SUB       1             K                 3 0
     C                   MULT      5             K
     C                   MOVEA     '00000'       *IN(45)
     C*
     C                   DO        5             N                 3 0
     C*
     C     K             ADD       N             M                 3 0
     C                   Z-ADD     CLS(M)        COUT(N)
     C                   MOVE      SPC(M)        SOUT(N)
     C*
     C     CLS(M)        IFEQ      *ZERO
     C     N             ADD       44            XX                3 0
     C                   MOVE      *ON           *IN(XX)
     C                   ENDIF                                                  CLS,M=*ZERO
     C*
     C                   ENDDO                                                  DO 5 N
     C                   MOVEA     COUT          @COUT
     C                   MOVEA     SOUT          SOUTDS
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     CLSHD
     C                   Write     CLSUND
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     CLSH2
     C                   Write     CLSUN2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C*
     C                   EXSR      SRRNGE
     C*
     C*    Process all Months for class codes
     C     ZZYYMM        DOWLT     TORPMY
     C     ZZYYMM        OREQ      TORPMY
     C     TODD          ANDNE     1
     C                   MOVEA     '00000'       *IN(45)
     C                   Z-ADD     ZZYYMM        XXRPMY
     C*
     C                   CLEAR                   CAMT
     C                   CLEAR                   CMIS
     C                   CLEAR                   @CAMT
     C                   CLEAR                   CMISDS
     C*
     C*    Print detail lines for class codes
     C                   DO        5             N
     C     K             ADD       N             M
     C*
     C*      Get amounts for class codes for processing month (ZZYYMM)
     C                   Z-ADD     CLS(M)        XXCLAS
     C                   MOVE      SPC(M)        XXSPEC
     C*
     C                   CLEAR                   CAMT(N)
     C                   MOVE      *OFF          SRFLG             1
     C     SRKY1         CHAIN     PSR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       SRAMTP        CAMT(N)
     C                   MOVE      *ON           SRFLG
     C     SRKY1         READE     PSR01                                  60
     C                   ENDDO
     C     SRFLG         IFEQ      *OFF
     C     N             ADD       44            XX                3 0
     C                   MOVE      *ON           *IN(XX)
     C     CLS(M)        IFNE      *ZERO
     C                   MOVE      MISSNG        CMIS(N)
     C                   ENDIF                                                  CLS,M<>*ZERO
     C                   ENDIF                                                  SRFLG = *OFF
     C*
     C                   ENDDO                                                  DO 5 N
     C                   MOVEA     CAMT          @CAMT
     C                   MOVEA     CMIS          CMISDS
     C                   MOVEA     MTH(MM)       MTHMM
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     CLSDTL
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     CLSDT2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C*
     C*      Get next month
     C                   ADD       1             ZZYYMM
     C     ZZMM          IFGT      12
     C                   Z-ADD     1             ZZMM
     C                   ADD       100           ZZYYMM
     C                   ENDIF
     C                   MOVE      ZZYYMM        MM
     c*
     C                   ENDDO                                                  ZZYYMM<=TORPMY
     C*
     C                   ENDDO                                                  DO XTIMES J
     C*
     C                   ENDIF                                                  *IN71=*ON
     C     PAGE          IFGT      *ZERO
     C                   EXSR      SRDEW
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     SIGLIN
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     SIGLI2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   SETON                                        77
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *ON           *IN97
     C                   ENDIF                                                  WAFTY=E PRT ECM ONLY
     C                   ENDIF
     C     WAFTY         IFNE      'E'                                          WAFTY=E PRT ECM ONLY
     C  N77              EXSR      SRDEW
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C  N77              Write     SIGLIN
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C  N77              Write     SIGLI2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ENDIF                                                  WAFTY=E PRT ECM ONLY
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on;
       //*** End Add *** 10/28/20 *********************************************************
     C     QQDRMK        IFNE      *BLANK
     C                   EXSR      DARTS
     C                   ENDIF
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C     NEXTEL        TAG
     C*
       //*** Beg Del *** 10/28/20 *********************************************************
     C**** *IN24         IFEQ      *ON                                          SPECIFIC EMP#
     C****               MOVE      *ON           *IN50
     C****               ELSE
     C**** ELKY2         READE(N)  PEL02                                  50    EXP DT RANGE
     C****               Z-ADD     ELRRN4        SVELRR
     C****               SETOFF                                       77
     C****               ENDIF
       //*** End Del *** 10/28/20 *********************************************
       //*** Beg Add *** 10/28/20 *********************************************
       Select;
       //*** begin add ***  06/15/21  *****************************************
       //* CX Confirmations generating all div to State Rpt Division -
       //* Process state rpt division first, then rest of divisions.
       When *in24 = *on and confirmStRpt;

          If stRptDone;
             Reade (xxCo#:xxFnd:xxPyr:xxEmp#) pel01q;
             If not %eof(wdell1q) and ELDiv = xsDiv;
                Reade (xxCo#:xxFnd:xxPyr:xxEmp#) pel01q;
             Endif;
          Else;
             stRptDone = *on;
             Setll (xxCo#:xxFnd:xxPyr:xxEmp#) pel01q;
             Reade (xxCo#:xxFnd:xxPyr:xxEmp#) pel01q;
             If not %eof(wdell1q) and ELDiv = xsDiv;
                Reade (xxCo#:xxFnd:xxPyr:xxEmp#) pel01q;
             Endif;
          Endif;

          If %Eof(wdell1q);
              *in50 = *on;
          Else;
             *in50 = *off;
          Endif;
       //*** end add *****  06/15/21  *****************************************
       // Single Employer/Division
       When *in24 = *on and flgGenAllDiv <> 'Y' and qqDiv <> *blanks;
          *in50 = *on;
       // Single Employer all divisions
       When *in24 = *on and (flgGenAllDiv = 'Y' or qqDiv = *blanks);
          Reade (xxCo#:xxFnd:xxPyr:xxEmp#) pel01q;
          If %Eof(wdell1q);
              *in50 = *on;
          Else;
             *in50 = *off;
          Endif;
       // All employers
       When *in24 = *off;
          Reade(n) elKy2 pel02;
          If %Eof(wdell4);
             *in50 = *on;
          Else;
             *in50 = *off;
          Endif;
       Endsl;
       svElRr = ELrrn4;
       *in77 = *off;
       //*** End Add *** 10/28/20 *********************************************
     C                   ENDDO                                                  *IN50
     C*
     C     QQMOD1        IFEQ      '*'
     C                   MOVE      '*'           QQMODE
     C                   ENDIF
     C*
      /Free
       If %Open(W7231O);
          xxPrt = '0';
          Chain orKy POR01;
          If %found;
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
             Write Route;
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
          Endif;
       Endif;
       If %Open(W7231O1);
          xxPrt = '1';
          Chain orKy POR01;
          If %found;
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
             Write Route1;
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
          Endif;
       Endif;
      /End-free
       //*** Beg Add *** 10/28/20 ******************************************
       // Read from the Group Select file for multiple groups.  If there was
       // only one group (called from policy toolbar) then we are done.
       Endif;
          If oneFnd = 0;
             Reade xxCo# pgs01;
             If %Eof(WXGSL);
                Leave;
             Endif;
          Else;
             oneFnd = 0;
          Endif;
       Enddo;

       Exsr SrCloseFiles;

       // Call process to generate Scriptura output/Darts if requested.
       If strXmlFileName <> ' ';
          inXml = strXmlFileName + '$';
          Callp GenScriptOut(inXml);
       Endif;
       //*** End Add *** 10/28/20 ******************************************
     C                   MOVE      *ON           *INLR
     C*

     C***  Begin Add  ***  01/22/18  **********************************
     C*================================================================
     C* Clear payroll values for single audit request without payrolls
     C*================================================================
     C     ClrPayroll    Begsr
     C                   Clear                   EPstat
     C                   Clear                   EPclas
     C                   Clear                   EPspec
     C                   Clear                   xxDesc
     C                   Clear                   EPaRat
     C                   Clear                   EPeAPr
     C                   Clear                   EPsAPr
     C                   Endsr
     C***  End   Add  ***  01/22/18  **********************************
      *
      *====================================================================
      * SRDEW - Print Excluded Workplaces
      *====================================================================
     C     SRDEW         BEGSR
      *
       //*** Beg Add *** 10/28/20 ********************************************************
       intSeq#Rw6 = 0;
       //*** End Add *** 10/28/20 ********************************************************
     C                   SETON                                        66
     C     EWKY          CHAIN     PEW01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     XXLINE        IFGE      45
     C                   SETON                                        66
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DEWH
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DEWH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   Z-ADD     4             XXLINE
     C                   ENDIF
      *
     C   66              ADD       4             XXLINE
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DEW
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptWxRw6p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ADD       6             XXLINE
      *
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DEW2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
      *
     C   66              SETOFF                                       66
      *
     C     EWKY          READE     PEW01                                  60
     C                   ENDDO                                                  EWKY
      *
     C                   ENDSR                                                  SRDEW
     C*
     C*====================================================================
     C* Open QSYSPRT1 and print routing page
     C*====================================================================
     C     OPEN1         BEGSR
     C*
     C                   OPEN      W7231O1
     C                   MOVEL     'OPEN'        XXPRT1            5
     C*
     C     *LIKE         DEFINE    ORTITL        SVTITL
     C                   MOVEL     ORTITL        SVTITL
     C*
     C                   MOVEL(P)  TITLE1        ORTITL
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C     *LIKE         DEFINE    ORTITL        XXDATA
     C                   MOVE(P)   ORTITL        XXDATA                         FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C                   SUBST(P)  XXDATA:N      X1TITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  X1TITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        X1TITL
     C*
     C                   MOVE      *ON           *IN94
     C*
     C                   MOVEL     SVTITL        ORTITL
     C*
     C                   Z-ADD     *ZERO         PAGE#1
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Open QSYSPRT2
     C*====================================================================
     C     OPEN2         BEGSR
     C*
     C     XXPRT2        IFNE      'OPEN'
     C                   OPEN      W7231O2
     C                   MOVEL(P)  'OPEN'        XXPRT2            5
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Open AUDTFRM
     C*====================================================================
     C     OPENA         BEGSR
     C*
     C     XXPRTA        IFNE      'OPEN'
     C                   OPEN      AUDTFRM
     C                   MOVEL(P)  'OPEN'        XXPRTA            5
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Close QSYSPRT2
     C*====================================================================
     C     CLOSE2        BEGSR
     C*
     C     XXPRT2        IFEQ      'OPEN'
     C                   CLOSE     W7231O2
     C                   MOVEL     'CLOSE'       XXPRT2
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Clear fields used for RCDFMT OTHPDT/OTHPD2
     C*====================================================================
     C     CLROTH        BEGSR
     C*
     C                   CLEAR                   XXONAM
     C                   CLEAR                   XXONM2
     C                   CLEAR                   XXDIV
     C                   CLEAR                   XXELID
     C                   CLEAR                   XXOAD1
     C                   CLEAR                   XXOAD2
     C                   CLEAR                   XXOCSZ
     c                   clear                   xxofdnm
     c                   clear                   xxofdnm2
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRSPLT SUBROUTINE
     C**********************************************************************
     C     SRSPLT        BEGSR
     C*
     C     EPEFDT        IFLT      PDAT(Z)
     C*
     C*  FIRST, CHECK TO SEE IF THIS PAYROLL RECORD ONLY COVERS ONE EXP
     C*  MOD PERIOD.  IF SO, NO SPLIT NEEDS TO BE DONE.
     C*
     C                   Z-ADD     1             X
     C     PDAT(Z)       LOOKUP    RDAT(X)                                60
     C*
     C     *IN88         IFEQ      *OFF
     C     *IN60         ANDEQ     *ON
     C     *IN86         OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     *IN87         OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXDEFL        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XX0709        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXAZ10        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXOK10        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXTN14        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     xxTnPyFnd     ANDEQ     *ON
     C     XXOK14        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     xxOKPyFnd     ANDEQ     *ON
     C*
     C     PDAT(Y)       IFEQ      ELPFDT
     C     PDAT(Y)       OREQ      XXANDT
     C     *IN88         ANDEQ     *OFF
     C     PDAT(Y)       OREQ      1030401
     C     *IN86         ANDEQ     *ON
     C     PDAT(Y)       OREQ      1031001
     C     *IN87         ANDEQ     *ON
     C     PDAT(Y)       OREQ      1081001
     C     XXDEFL        ANDEQ     *ON
     C     PDAT(Y)       OREQ      1090701
     C     XX0709        ANDEQ     *ON
     C     PDAT(Y)       OREQ      1100101
     C     XXAZ10        ANDEQ     *ON
     C     PDAT(Y)       OREQ      xxSpltDt
     C     XXOK10        ANDEQ     *ON
     C     PDAT(Y)       OREQ      xxSpltDt
     C     XXTN14        ANDEQ     *ON
     C     xxTnPyFnd     ANDEQ     *ON
     C     PDAT(Y)       OREQ      xxSpltDt
     C     xxOK14        ANDEQ     *ON
     C     xxOKPyFnd     ANDEQ     *ON
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDP            8 0
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODP            8 0
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       2             XXLINE            3 0
     C                   GOTO      SPLTEN
     C                   ENDIF                                                  PDAT,Y IFEQ
     C                   ENDIF
     C*
     C*  DETERMINE THE NUMBER OF DAYS IN THE PAYROLL
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     1             X
     C     PDAT(Z)       LOOKUP    RDAT(X)                            60  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     RDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM      *BLANK        QQERR             1
     C                   PARM      *ON           QQTERM            1
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C*
     C*  PRORATE AND PRINT THE PAYROLL AMOUNT FOR THE EXP MOD PERIOD
     C*
     C     EPEFDT        IFGT      PDAT(Y)
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FRDP            8 0
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     PDAT(Y)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FRDP
     C*
     C                   Z-ADD     PDAT(Y)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDP
     C*
     C                   Z-ADD     X1FRDP        XXEPDT
      /free
       xxEPDT1= xxEPDT;
      /end-free
     C*
     C                   ENDIF
     C*
      /free
       // Discount rate if TN Rate split is in effect
       // and payrolls have not been split yet
       If xxTN14 = *on and xxTNPyFnd = *off and pDat(y) >= 1140701;
          If isDiscounted('TN':EPclas:EPspec);
             //**** Beg Chg **** 06/17/24 **********************************
             //***EParat = %dech(EParat*.941:7:2);
             EParat = %dech(EParat*.941:8:3);
             //**** End Chg **** 06/17/24 **********************************
          Endif;
       Endif;
      /end-free
      /free
       // Discount rate if OK Rate split is in effect
       // and payrolls have not been split yet
       If xxOK14 = *on and xxOKPyFnd = *off and pDat(y) >= 1140201;
          If isDiscounted('OK':EPclas:EPspec);
             //**** Beg Chg **** 06/17/24 **********************************
             //***EParat = %dech(EParat*.871:7:2);
             EParat = %dech(EParat*.941:8:3);
             //**** End Chg **** 06/17/24 **********************************
          Endif;
       Endif;
      /end-free
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TODP            8 0
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODP            8 0
     C*
     C                   MOVE      X1FRDP        QQFROM
     C                   MOVE      X1TODP        QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM      *BLANK        QQERR             1
     C                   PARM      *ON           QQTERM            1
     C*
     C     PDAYS         IFNE      *ZERO
     C     QQDAYS        DIV       PDAYS         XXPRO            11 9
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPRO
     C                   ENDIF
     C*
     C     EPEAPR        MULT(H)   XXPRO         XXWK70            7 0
     C                   Z-ADD     XXWK70        XXAMT             9 0
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       2             XXLINE
     C*
     C                   ENDIF
     C*
     C     SPLTEN        ENDSR
     C**********************************************************************
     C*  DARTS SUBROUTINE  -  CREATE DARTS RECORD
     C********************************************************************
     C     DARTS         BEGSR
     C*
     C                   CLEAR                   #1DCID
     C                   CLEAR                   #1FOLD
     C*
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
     C*** Beg Add *** 10/28/20 ******************************************************
     C     flgNoOutput   Andne     'Y'
     C*** End Add *** 10/28/20 ******************************************************
     C                   EXSR      SRPDF
     C                   ENDIF
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      ZZDIV         QCDIV
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C     CTKY          CHAIN     PCT01                              60
     C                   MOVEL(P)  CTDESC        Q#COMM          256
     C                   CAT(P)    'Policy':2    Q#COMM
     C                   CAT(P)    'Year:':1     Q#COMM
     C                   MOVE      ELFYR         X1FYR             3
     C                   CAT(P)    X1FYR:1       Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  #1DCID        Q#DCID           12
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZERO         Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  #1FOLD        Q#FOLD           80
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
     C                   MOVEL(P)  'O'           Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
       //*** Beg Add *** 10/28/20 ***********************************************************
       If flgNoOutput = 'Y';
          q#DcId = *blanks;
          Q#Fold = *blanks;
       Endif;
       //*** End Add *** 10/28/20 ***********************************************************
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
      /free

       strForm = 'WC000311';
       //*** Beg Add *** 10/28/20 *****************************************
       If flgGen311En = 'Y';
          In *Lock *dtaara;
          xxXML = *blanks;
          Out *dtaara;
        Endif;
       //*** End Add *** 10/28/20 *****************************************

        strCo#  = %EditC(qcCo#:'X');
        strFnd# = %EditC(qcFnd:'X');
        strFyr  = x1Fyr;
        strEmp# = %EditC(qcEmp#:'X');

        strOutq = *blanks;
        strPrt  = '0';

        //*** Beg Chg *** 10/28/20 ***************************************
        //***strDiv  = QQDIV;
        strDiv  = %EditC(ELdiv:'X');
        //*** End Chg *** 10/28/20 ***************************************
        //*** Beg Add *** 10/28/20 ***************************************
        strPfDt = %EditC(ELpFDt:'X');
        //*** End Add *** 10/28/20 ***************************************
        //*** Beg Chg *** 10/28/20 **************************************
        //***PrtPolEnd(strForm:strCo#:strFnd#:
        //***           strFyr:strEmp#:strDiv:strOutq:strPrt);
        PrtPolEnd(strForm:strCo#:strFnd#:
         strFyr:strEmp#:strDiv:strPfDt:strOutq:strPrt);
        //*** End Chg *** 10/28/20 ***************************************

       // If the spool is printed, than qqprt value will be '1'.

       If strPrt = '1';

       //  Chain SDCMP to retrieve the latest record number

           intSeq# = %dec(q#seq#:5:0);
           strKey = q#key;

           Chain keySDCMLA pcm02;

           If %found(sdcmla);

              intRrn = CMrrn;

           Endif;

        Endif;

      /end-free
     C*
     C                   ENDSR
       //*** Beg Add *** 10/28/20 **************************************
        //===================================================================
       // Get Class Code Description
       //===================================================================
       BegSr GetClassDesc;

       // Check Payroll Master then if extension check extension file with
       // blank state first then with Claim Jurisdiction State.
       Setgt (ELco#:ELrtCd:intClass:ELpFdt) ppr01;
       Readpe (ELco#:ELrtCd:intClass) ppr01;
       If not %eof;
          strClassdesc = PRDesc;
          If strExt <> *blanks;
             Clear strPst;
             Setgt (ELco#:intClass:strExt:strPst:ELpFdt) ppx01;
             Readpe (Elco#:intClass:strExt:strPst) ppx01;
             If not %Eof(wmpxl1);
                strClassDesc = PXDesc;
             Else;
                strPst = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ElfYr);
                Setgt (ELco#:intClass:strExt:strPst:ELpFdt) ppx01;
                Readpe (ELco#:intClass:strExt:strPst) ppx01;
                If not %Eof(wmpxl1);
                   strClassDesc = PXDesc;
                Endif;
             Endif;
          Endif;
       Endif;

       EndSr;
       //***************************************************************
       // SrCloseFiles - Close workfiles
       //***************************************************************
       Begsr SrCloseFiles;
          If %Open(WXRWP);
             Close WXRWP;
             Close WXRW1P;
             Close WXRW2P;
             Close WXRW3P;
             Close WXRW4P;
             Close WXRW5P;
             Close WXRW6P;
          Endif;
       Endsr;
       //***************************************************************
       // SrOpenFiles - Open workfiles
       //***************************************************************
       Begsr SrOpenFiles;
          If Not %Open(WXRWP);
             Open WXRWP;
             Open WXRW1P;
             Open WXRW2P;
             Open WXRW3P;
             Open WXRW4P;
             Open WXRW5P;
             Open WXRW6P;
          Endif;
       Endsr;
       //***************************************************************
       // ScriptXML - Reserve/Update XML record for Scriptura Output
       //***************************************************************
       Begsr ScriptXML;

       strXmlFile = ' ';
       message = %trim(processType);
       string = ' ';

       // Determine if called from Cancellation Confirmation program
       If isUseScript = *On and PgmInStack('W0171R') = *On;
          isMassReq = *off;
          flgMass = 'Y';

       //*** begin add ***  06/15/21  **********************************************
          In *Lock *dtaara;
          Out *dtaara;
          strXmlLink = %trim(ldMassFile);
          isCXConfirm = *on;
       //*** end add *****  06/15/21  **********************************************

       Endif;

       If isMassReq;

          StLetter('XMLREQUEST':strXmlFile:message:' ':'000000000':'  ':
           'Audit_Req':'000':'000':flgMass:strXmlLink);

          isMassReq = *off;
          strXmlFileName = strXmlFile;
          strXmlLink = strXmlFile;
          Chain (strXmlFile) xp100;
          XPCat = 'PS';
          Update xp100;
       Endif;

       //*** begin add ***  06/15/21  *****************************************
       //* Process as individual document unless all division document on state
       //* reporting division.
       If not confirmStRpt or not stRptDone;
       //*** end add *****  06/15/21  *****************************************

          StLetter('XMLREQUEST':strXmlFile:message:'E':%editc(intEl5Rrn:'X'):
           xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):flgMass:strXmlLink);

          //*** begin chg ***  06/15/21  *******************************************
          //*****If strXmlFileName = ' ';
          If strXmlFileName = ' ' and not isCXConfirm;
          //*** end chg *****  06/15/21  *******************************************
             strXmlFileName = strXmlFile;
          Endif;

          //* The template XML file name must be pulled from record for work files for a
          //* single request.  Otherwise it has been returned from the XML Request call.
          If strXmlLink = ' ';
             Chain (strXmlFile) xp100;
             strXmlFile = XPXmlT;
          Else;
             Chain (strXmlFileBlk:strXmlLink:strXmlFile) xp100;
          Endif;

          XPData = 'Y';

          // If multiple groups are selected LDA indicates to Group
          // remark code or called from Cancellation Confirmation
          // then check Admin DARTS file for defautl remark code.
          If (isGrpSlct = *on and xxDefDart = 'Y') or
            PgmInStack('W0171R') = *on;
             Chain (xxCo#:xxFnd) pex01;
               If %Found(WMEXL) and EXauRq <> *blanks;
                  XPrmCd = EXauRq;
                  XPdart = 'Y';
                  //*** Beg Add ***  03/18/23  *******************************************
                  // Load Email Subject and Email Body for Audit Requet Process
                  If PgmInStack('W0171R') = *on;
                     proctype = processType;
                     Chain proctype psp00;
                     If %Found(STSPL);
                        XPsubj = SPEmlSub;
                        XPbody = SPEmlBody;
                        If SPEmlSubM <> ' ';
                           XPsubj = SPEmlSubM;
                           XPbody = SPEmlBodyM;
                        Endif;
                     EndIf;
                  EndIf;
                  //*** End Add ***  03/18/23  *******************************************
               Endif;
          Endif;

          sys_Date = %Date();
          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPtime = %dec(%time():*HMS);
          XPdt   = %dec(%char(sys_Date:*cymd0):7:0);

          Update xp100;

       //*** begin add ***  06/15/21  *****************************************
          outputFile = XPFile;
       //* Process additional divisions for a state reporting div only document
       //* as additional template requests.
       Else;

          templateFile = outputFile;
          StLetter('TEMPLRQST':templateFile:message:'E':%editc(intEL5rrn:'X'):
           xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'));

          strXmlFile = templateFile;

       Endif;
       //*** end add *****  06/15/21  *****************************************

       //*** Write main audit tracking record and initialize detail files
       Exsr SrOpenFiles;
       Exsr ScriptMain;
       Clear prw101;

       // Add the 301 Endorsement as additional template
       If flgGen301En = 'Y' and EMStrp = 'Y';
          In *Lock *dtaara;
          xxXML = %Trim(XPfile);
          Out *dtaara;

          strForm = 'WC000301';
          strCo#  = %EditC(ELco#:'X');
          strFnd# = %EditC(ELfnd:'X');
          strFyr  = %EditC(ELfYr:'X');
          strEmp# = %EditC(ELemp#:'X');

          strOutq = *blanks;
          strPrt  = '0';

          strDiv  = %EditC(ELdiv:'X');
          strPfDt = %EditC(ELpFDt:'X');
          PrtPolEnd(strForm:strCo#:strFnd#:
           strFyr:strEmp#:strDiv:strPfDt:strOutq:strPrt);
       Endif;

       // Add the 311 Endorsement as additional template
       If flgGen311En = 'Y' and EMStrp = 'Y';
          In *Lock *dtaara;
          xxXML = %Trim(XPfile);
          Out *dtaara;

          strForm = 'WC000311';
          strCo#  = %EditC(ELco#:'X');
          strFnd# = %EditC(ELfnd:'X');
          strFyr  = %EditC(ELfYr:'X');
          strEmp# = %EditC(ELemp#:'X');

          strOutq = *blanks;
          strPrt  = '0';

          strDiv  = %EditC(ELdiv:'X');
          strPfDt = %EditC(ELpFDt:'X');

          PrtPolEnd(strForm:strCo#:strFnd#:
           strFyr:strEmp#:strDiv:strPfDt:strOutq:strPrt);
       Endif;

       Endsr;

       // *=================================================================
       // *  ScriptMain - Write the main workfile record for Scriptura outpu
       // *=================================================================
       Begsr ScriptMain;

       If not isUseXML;
          Leavesr;
       Endif;

       //* Write the main work file record.
       Clear prw01;

       //* Write the criteria fields
       If QQemp# <> *blanks and QQemp# <> *zeros;
          Monitor;
             RWcEmp#  = %Dec(QQemp#:9:0);
          On-Error;
          Endmon;
          RWcEname = EMname + EMnam2;
       Endif;
       If QQdiv <> *blanks and QQdiv <> *zeros;
          Monitor;
             RWcDiv = %Dec(QQdiv:5:0);
          On-Error;
          Endmon;
       Endif;
       If (QQdiv = *blanks and QQemp# <> *blanks and qqEmp# <> *zeros)
        or flgGenAllDiv = 'Y';
          RWcAllDiv = 'Y';
       Endif;
       If QQpYr <> *blanks and QQpYr <> *zeros;
          Monitor;
             RWcFyr = %Dec(QQpYr:3:0);
          On-Error;
          Endmon;
       Endif;
       RWfrmPexD = x1FtDt;
       RWtoPexD  = x1TtDt;
       If QQnRbs = 'N' or QQnRbs = 'B';
          RWincNb   = 'Y';
       Endif;
       If QQnRbs = 'R' or QQnRbs = 'B';
          RWincRnB  = 'Y';
       Endif;
       If QQAudRProc = 'R' or QQAudRProc = 'A';
          RWprvRun  = 'Y';
       Endif;
       If QQAudRProc = 'N' or QQAudRProc = 'A';
          RWnotPvRn = 'Y';
       Endif;
       RWupdAudT = QQrpFl;
       RWUpdAuPrv= QQUpdAudTR;
       RWrmCd    = XPrmCd;
       RWuseSrPa = QQsrFl;
       RWprtSrPa = QQRFlg;
       RWcom1   = qqCom1;
       RWcom2   = qqCom2;
       RWcom3   = qqCom3;
       RWcom4   = qqCom4;
       RWcom5   = qqCom5;

       //* Write other criteria fields

       RWxml  = strXmlFile;
       RWco#  = ELco#;
       RWfnd  = ELfnd;
       RWemp# = ELemp#;
       RWdiv  = ELdiv;
       RWfYr  = ELfYr;

       Monitor;
       RWpolEfD = xxPfDt;
       RWpolExD = xxPtDt;
       RWannDt  = xxPrav;
       RWcxDt   = zzCxDt;
       On-error;
       Endmon;

       If *in55 = *on;
          RWnoPay = 'Y';
       Endif;
       If ELnoExM1 = 'Y';
          RWexM1 = 0;
       Else;
          RWexM1  = ELexM1;
       Endif;
       //*** Begin Chg *** 07/30/23 ****************************************
       //*** RWexM2 = ELexM2;
       //*** RWexM3 = ELexM3;
       If ELnoExM2 = 'Y';
          RWexM2 = 0;
       Else;
          RWexM2  = ELexM2;
       Endif;
       If ELnoExM3 = 'Y';
          RWexM3 = 0;
       Else;
          RWexM3  = ELexM3;
       Endif;
       //*** End   Chg *** 07/30/23 ****************************************
       RWemD1 = xxEmD1;
       RWemD2 = xxEmD2;
       RWemD3 = xxEmD3;
       If *in96 = *on;
         RWmerit = 'Y';
       Endif;
       RWstRp = EMstRp;
       sys_Date = %Date();
       RWiUsr = Q1user;
       RWiPgm = Q1pgm;
       RWiDt  = %dec(%char(sys_Date:*cymd0):7:0);
       RWiTim = %dec(%time():*HMS);
       RWuser = Q1user;
       RWPgm  = Q1pgm;
       RWDt   = RWiDt;
       RWtime = RWiTim;

       Write prw01;

       Endsr;
       // *=================================================================
       // *  ScriptWxrw1p- Write the Payroll workfile record for Scriptura
       // *=================================================================
       Begsr ScriptWxrw1p;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear prw101;

       //* Write the payroll work file record.
       RW1xml  = strXmlFile;
       RW1co#  = ELco#;
       RW1fnd  = ELfnd;
       RW1emp# = ELemp#;
       RW1div  = ELdiv;
       RW1fYr  = ELfYr;
       intSeq#Rw1 = intSeq#Rw1 + 1;
       RW1Seq  = intSeq#Rw1;

       // Load fields based on whether this is a header record
       Select;
       When isHeadRw1 = *on;
          RW1Head = 'Y';
          RW1HdgT = strHdTypRw1;
          RW1Desc = strHddesc;
          If strHdTypRw1 = 'State Rate Change';
             RW1hSt = xxJrSt;
          Endif;
          Monitor;
          Select;
             When strHdDesc = 'FL Rate Decrease';
                RW1efDt = xxRtD2;
             When strHdDesc = 'Policy Start Date';
                RW1efDt = xxPfDt;
             Other;
                RW1EfDt = xxRtDt;
          Endsl  ;
          On-error;
          Endmon;
          If Rw1HdgT = 'Experience Mod';
             RW1Mod = decExpMod;
             Monitor;
                RW1EfDt = expModDate;
             On-error;
             Endmon;
          Endif;
       When isHeadRw1 = *off;

          RW1Mod = decExpMod;
          Monitor;
             RW1EfDt = expModDate;
          On-error;
          Endmon;
          RW1clas = EPclas;
          RW1Spec = EPspec;
          RW1Ext  = EPext;
          intClass = EPclas;
          strExt = EPext;
          Exsr GetClassDesc;
          RW1desc = strClassDesc;
          RW1Rate = EPaRat;
          Monitor;
          RW1Epay =  xxAmt;
          On-error;
          Endmon;
          RW1NumE = EPeCnt;
          RW1Apay = 0;
          Select;
             // Payroll Effective Date
             When *in18 = *off and *in88 = *off and *in90 = *off;
             Monitor;
                RW1PefD = xxFrAv;
             On-error;
             Endmon;
             When *in18 = *on and *in88 = *off and *in90 = *off;
             Monitor;
                RW1PefD = xxEpDt;
             On-error;
             Endmon;
             When *in18 = *off and *in90 = *on;
             Monitor;
                RW1PefD = xxFrDp;
             On-error;
             Endmon;
             When *in18 = *on and *in90 = *on;
             Monitor;
                RW1PefD = xxEpDt;
             On-error;
             Endmon;
          //*** begin add ***  06/10/21  **************************************
             Other;
             If EPEfdt > ELPfdt;
                RW1PefD = xxEpdt;
             Else;
                RW1PefD = xxPfdt;
             Endif;
          //*** end add *****  06/10/21  **************************************
          Endsl;

             // Payroll Expiration Date
          Select;
             When *in88 = *off and *in90 = *off;
             Monitor;
                RW1PexD = xxToAv;
             On-error;
             Endmon;
             When *in90 = *on;
             Monitor;
                RW1PexD = xxToDp;
             On-error;
             Endmon;
          //*** begin add ***  06/10/21  **************************************
             Other;
             Monitor;
                RW1PexD = xxPtdt;
             On-error;
             Endmon;
          //*** end add *****  06/10/21  **************************************
          Endsl;

          // Cancellation Date
          If *in34 = *off;
             RW1CxDt = xxCaDt;
          Endif;

       Endsl;


       sys_Date = %Date();
       RW1iUsr = Q1user;
       RW1iPgm = Q1pgm;
       RW1iDt  = %dec(%char(sys_Date:*cymd0):7:0);
       RW1iTim = %dec(%time():*HMS);
       RW1user = Q1user;
       RW1Pgm  = Q1pgm;
       RW1Dt   = RW1iDt;
       RW1time = RW1iTim;

       Write pRW101;

       isHeadRw1  = *off;
       strHdTypRw1 = *blank;

       Endsr;
       // *=================================================================
       // *  ScriptWxrw2p- Write the Officer workfile record for Scriptura
       // *=================================================================
       Begsr ScriptWxrw2p;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear prw201;

       //* Write the payroll work file record.
       RW2xml  = strXmlFile;
       RW2co#  = ELco#;
       RW2fnd  = ELfnd;
       RW2emp# = ELemp#;
       RW2div  = ELdiv;
       RW2fYr  = ELfYr;
       intSeq#Rw2 = intSeq#Rw2 + 1;
       RW2Seq  = intSeq#Rw2;
       RW2Odiv = OOdiv;
       RW2Oloc = OOeLid;
       RW2ExFl = OOexFl;
       RW2Clas = OOclas;
       If OOoFsN <> *blanks;
          RW2Name = %Trim(OOofsN) + ' ' + %Trim(OOolsN);
       Else;
          RW2Name = OOolsN;
       Endif;
       RW2Titl = OOtitl;

       Monitor;
       RW2OefD = xeDat;
       RW2OexD = %dec(%char(%date(OOexDt:*cymd):*usa0):8:0);
       On-error;
       Endmon;

       sys_Date = %Date();
       RW2iUsr = Q1user;
       RW2iPgm = Q1pgm;
       RW2iDt  = %dec(%char(sys_Date:*cymd0):7:0);
       RW2iTim = %dec(%time():*HMS);
       RW2user = Q1user;
       RW2Pgm  = Q1pgm;
       RW2Dt   = RW2iDt;
       RW2time = RW2iTim;

       Write pRW201;

       Endsr;
       // *=================================================================
       // *  ScriptWxrw3p- Write the Location workfile record for Scriptura
       // *=================================================================
       Begsr ScriptWxrw3p;

       If not isUseXML;
          Leavesr;
       Endif;

       //* Write the Location work file record.
       RW3xml  = strXmlFile;
       RW3co#  = ELco#;
       RW3fnd  = ELfnd;
       RW3emp# = ELemp#;
       RW3div  = svDiv;
       RW3fYr  = ELfYr;
       intSeq#Rw3 = intSeq#Rw3 + 1;
       RW3Seq  = intSeq#Rw3;

       sys_Date = %Date();
       RW3iUsr = Q1user;
       RW3iPgm = Q1pgm;
       RW3iDt  = %dec(%char(sys_Date:*cymd0):7:0);
       RW3iTim = %dec(%time():*HMS);
       RW3user = Q1user;
       RW3Pgm  = Q1pgm;
       RW3Dt   = RW3iDt;
       RW3time = RW3iTim;

       Write pRW301;

       Endsr;
       // *===================================================================
       // *  ScriptWxrw4p- Write the S/R Payroll workfile record for Scriptura
       // *===================================================================
       Begsr ScriptWxrw4p;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear prw401;

       //* Write the payroll work file record.
       RW4xml  = strXmlFile;
       RW4co#  = ELco#;
       RW4fnd  = ELfnd;
       RW4emp# = ELemp#;
       RW4div  = ELdiv;
       RW4fYr  = ELfYr;
       intSeq#Rw4 = intSeq#Rw4 + 1;
       RW4Seq  = intSeq#Rw4;
       Select;
          When flgSrTotEmp = 'Y';
             RW4Pay = decTotAmtP;
          Other;
             RW4Type = srPayrollDs(t).strType;

             Select;

             // Weekly
             When srPayrollDs(t).strType = 'Weekly';
                Monitor;
                RW4Todt = %dec(%char(%date(S8toDt:*cymd):*usa0):8:0);
                On-Error;
                Endmon;
                RW4Clas = S8Clas;
                RW4Spec = S8Spec;
                RW4Ext  = S8Ext;
                RW4Pay  = S8AmtP;

             // Monthly
             When srPayrollDs(t).strType = 'Monthly';
                Monitor;
                intDate7 = %Dec(%Editc(srPayrollDs(t).intRpMy:'X') + '01':7:0);
                RW4ToDt = %dec(%char(%date(intDate7:*cymd):*usa0):8:0);
                On-Error;
                Endmon;
                RW4Clas = EPClas;
                RW4Spec = EPSpec;
                RW4Ext  = EPExt;
                RW4Pay  = S15AmtP;
             Endsl;
             // If the no report flag = Y then dont load class or payroll.
             If RW4Clas <> 0 and flgSumPeriod <> 'Y';

                xxRtCd = ELrtCd;
                xxType = 'P';

                // Weekly
                If srPayrollDs(t).strType = 'Weekly';
                   x1EfDt = SrEffd(S8co#:S8fnd:S8fyr:S8emp#:
                     S8div:S8efdt:xxType:S8clas);
                Endif;

                // Monthly
                If srPayrollDs(t).strType = 'Monthly';
                   x1EfDt = SrEffd(S15co#:S15fnd:S15fyr:S15emp#:
                     S15div:S15efdt:xxType:S15clas);
                Endif;
                Setgt prKy1 ppr01;
                Readpe prKy ppr01;
                If not %eof (WMPRL1);
                   RW4Desc = PRdesc;
                Endif;
                RW4Rate = EPaRat;

             Endif;

             If flgSumPeriod = 'Y';
                RW4Pay = decAmtP;
                RW4Clas = 0;
                flgSumPeriod = ' ';
                decAmtP = 0;
             Endif;

             RW4NoRpt = srPayrollDs(t).flgRpt;
             RW4Hdg   = srPayrollDs(t).strHdg;
       Endsl;

       sys_Date = %Date();
       RW4iUsr = Q1user;
       RW4iPgm = Q1pgm;
       RW4iDt  = %dec(%char(sys_Date:*cymd0):7:0);
       RW4iTim = %dec(%time():*HMS);
       RW4user = Q1user;
       RW4Pgm  = Q1pgm;
       RW4Dt   = RW4iDt;
       RW4time = RW4iTim;

       Write pRW401;

       Endsr;
       //===============================================================
       // List all claims associated with audit request
       //===============================================================
       Begsr ScriptWxRw5p;
          Clear prw501;
          Setll (ELCo#:ELFnd:ELemp#:Eldiv:ELfYr) pcm01;
          Reade(n) (ELCo#:ELFnd:ELemp#:Eldiv:ELfYr) pcm01;
            Dow Not %Eof(WMCML);
               RW5xml  = strXmlFile;
               RW5Co#  = CMco#;
               RW5Fnd  = CMfnd;
               RW5Emp# = CMemp#;
               RW5Div  = CMdiv;
               RW5fYr  = CMfYr;
               RW5Case = CMcase;
               RW5Clas = CMclas;
               RW5Spec = CMspec;
               RW5Ext  = CMext;
               intClass = CMclas;
               strExt = RW5ext;
               Exsr GetClassDesc;
               RW5Desc = strClassDesc;
               Chain CMss# pcd01;
               If %Found(WMCDL);
                  If CDfsN <> *blanks;
                     RW5Name = %Trim(CDfsN) + ' ' + %Trim(CDlsN);
                  Else;
                     RW5Name = %Trim(CDlsN);
                  Endif;
                  RW5FsN = CDfsN;
                  RW5LsN = CDLsN;
                  RW5Mid = CDMid;
                  RW5Adr1 = CDadr1;
                  RW5Adr2 = CDadr2;
                  RW5Cty  = CDcty;
                  RW5St   = CDst;
                  RW5Zip  = CDzip;
                  If CDcnty <> *blanks;
                     Chain (CMco#:CDcnty) pcn01;
                     If %Found(WTABL);
                        RW5Cnty = CNdesc;
                     Endif;
                  Endif;
                  strGtKey = CDctry;
                  Chain (CMco#:'CTRY':strGtKey) pgt01;
                  If %Found(WTGTL);
                     RW5Ctry = GTdesc;
                  Endif;
                  If CDctry <> *blanks and CDctry <> 'USA';
                     RW5oCty  = CDcty;
                     RW5oAdr1 = CDadr1;
                     RW5oAdr2 = CDadr2;
                     RW5oAdr3 = CDadr3;
                     RW5Adr1 = *blanks;
                     RW5Adr2 = *blanks;
                     RW5Cty  = *blanks;
                     RW5St   = *blanks;
                     RW5Zip  = *zeros;
                  Else;
                     RW5Ctry = *blanks;
                  Endif;
                  Monitor;
                     RW5DoB = %dec(%char(%date(CDdtoB:*cymd):*usa0):8:0);
                  On-error;
                  Endmon;
                  RW5Msts = CDmSts;
                  RW5Email = CDemAd;
                  RW5ph#  = CDwPh#;
                  RW5ara  = CDwAra;
                  RW5Sex  = CDsex;
               Endif;
               RW5AcDt = %dec(%char(%date(CMacDt:*cymd):*usa0):8:0);

               sys_Date = %Date();
               RW5iUsr = Q1user;
               RW5iPgm = Q1pgm;
               RW5iDt  = %dec(%char(sys_Date:*cymd0):7:0);
               RW5iTim = %dec(%time():*HMS);
               RW5user = Q1user;
               RW5Pgm  = Q1pgm;
               RW5Dt   = RWiDt;
               RW5time = RWiTim;

               Write prw501;

               Reade(n) (ELCo#:ELFnd:ELemp#:Eldiv:ELfYr) pcm01;
            Enddo;
       Endsr;
       // *===================================================================
       // *  ScriptWxrw6p- Write the Excluded Workplace Workfile for Scriptura
       // *===================================================================
       Begsr ScriptWxrw6p;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear prw601;

       //* Write the Excluded Workplacerecord.
       RW6xml  = strXmlFile;
       RW6co#  = ELco#;
       RW6fnd  = ELfnd;
       RW6emp# = ELemp#;
       RW6div  = ELdiv;
       RW6fYr  = ELfYr;
       intSeq#Rw6 = intSeq#Rw6 + 1;
       RW6Seq  = intSeq#Rw6;
       RW6Num  = EWwpl#;
       RW6Wrk1 = EWwrk1;
       RW6Wrk2 = EWwrk2;
       RW6Wrk3 = EWwrk3;
       RW6Wrk4 = EWwrk4;
       RW6Wrk5 = EWwrk5;

       sys_Date = %Date();
       RW6iUsr = Q1user;
       RW6iPgm = Q1pgm;
       RW6iDt  = %dec(%char(sys_Date:*cymd0):7:0);
       RW6iTim = %dec(%time():*HMS);
       RW6user = Q1user;
       RW6Pgm  = Q1pgm;
       RW6Dt   = RW6iDt;
       RW6time = RW6iTim;

       Write pRW601;

       Endsr;
       // *=================================================================
       // *  ScriptSRPayroll- Process the S/R Payroll Data
       // *=================================================================
       Begsr ScriptSrPayroll;

          // Determine From CYYMM for S/R Payroll

          // Read through Employer Detail History to find S/R Payroll information
          // and load into an array for use in the next step.  This will be an
          // array of billing plans and dates.  May contain one or many records
          intBsCd = *zeros;
          t = 0;
          xxFreq = ' ';
          MthlyWkly = ' ';
          isFoundSr = *off;
          Clear srPayrollHist;
          intSeq#Rw4 = 0;
          Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
          Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
          Dow not %Eof(WDEHL6);
             If intBsCd <> EHbScd;
                xxKey = %Editc(EHbScd:'X');
                Chain (ELCo#:xxKey) pbs01;

                // Read until you find first S/R record
                if isFoundSr = *off and BSseFl <> 'S';
                   Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
                   Iter;
                Endif;

                isFoundSr = *on;

                // If Frequency changes determine if frequency is Monthly, Weekly or possibly Both
                If BSfreq <> xxFreq;
                   If BSfreq = 'M';
                      MthlyWkly = 'M';
                   Endif;
                   If BSfreq = 'W';
                      MthlyWkly = 'W';
                   Endif;
                   xxFreq = BSfreq;
                   t = t + 1;
                   srPayrollHist(t).intEfdt = EHefDt;
                   srPayrollHist(t).intTrDt = EHtrDt;
                   srPayrollHist(t).intPfDt = EHpFdt;
                   srPayrollHist(t).intPtdt = EHpTdt;
                   If BSfreq = 'M';
                      srPayrollHist(t).strPlan = 'M';
                   Else;
                      srPayrollHist(t).strPlan = 'W';
                      srPayrollHist(t).strDoWk = BSdOwk;
                   Endif;
                Else;
                   Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
                   Iter;
                Endif;
                intBsCd = EHbScd;
             Endif;
             Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) peh01;
          Enddo;

          // Read through payroll history data structure and create reporting
          // Periods whether Monthly, Weekly or Combination
          intN2= 1;
          Dow srPayrollHist(intN2).intEfDt > 0;
             Select;
                When MthlyWkly = 'M';
                   Exsr SrMonthly;
                When MthlyWkly = 'W';
                   Exsr SrWeekly;
             Endsl;
             intN2= intN2+ 1;
          Enddo;

       Endsr;
       //*****************************************************************
       //  SrNoRpt  - Determine if no payrolls in WASRP for this month
       //*****************************************************************
       Begsr SrNoRpt;

          // Read through employer payroll to process for each class code
          srPayrollDs(t).flgRpt = 'Y';
          Setll (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
          Reade (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
          Dow not %Eof(WMEPL7E);
             // Find the SR record for each class code
             Chain (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv:srPayrollDs(t).intRpMy   :
              EPclas:EPspec) psr15;
             If %Found(WASRL15);
                srPayrollDs(t).flgRpt = 'N';
             Endif;
             Reade (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
         Enddo;
       Endsr;
       //*****************************************************************
       //  SrMonthly - Write S/R for each month
       //*****************************************************************
       Begsr SrMonthly;
          flgRptType = 'M';
          strFmRpMy = %Subst(%Editc(ELpFdt:'X'):1:5);
          intFmRpMy = %Dec(strFmRpMy:5:0);

          // Determine to MonthYear for S/R Payroll
          If Cx(w) <> 0 and Cx(w) <= ELpTdt;
             intPtDt = Cx(w);
          Else;
             intPtDt = ELpTdt;
          Endif;
          strToRpMy = %Subst(%Editc(intpTdt:'X'):1:5);
          intToRpMy = %Dec(strToRpMy:5:0);

          // Load each month into and array
          xxRpMy = IntFmRpMy;
          t = 1;
          decAmtP  = 0;
          decTotAmtP = 0;
          flgSumPeriod = ' ';
          flgSrTotEmp = ' ';
          Clear srPayrollDs;

          Dow xxRpMy <= intToRpMy;
             srPayrollDs(t).intRpMy = xxRpMy;
             srPayrollDs(t).strType = 'Monthly';
             Exsr GetMonth;
             // Set the No reporting flag for this employer and month
             Exsr SrNoRpt;

             // Read through employer payroll to process for each class code
             Setll (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
             Reade (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
             Dow not %Eof(WMEPL7E);

                // Find the SR record for each class code
                Chain (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv:srPayrollDs(t).intRpMy:
                 EPclas:EPspec) psr15;
                If %Found(WASRL15);
                   decAmtP = decAmtP + S15amtP;
                   decTotAmtP = decTotAmtP + S15amtP;
                   Exsr ScriptWxrw4p;
                Else;
                   Clear psr15Ds;
                   Exsr ScriptWxrw4P;
                Endif;
                Reade (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
             Enddo;
             flgSumPeriod = 'Y';
             Exsr ScriptWxrw4p;
             // When 12th month revert to 1st month of next year else add one to month
             If %Subst(%Char(xxRpMy):4:2) = '12';
                xxRpMy = (xxRpMy - 11) + 100;
             Else;
                xxRpMy = xxRpMy + 1;
             Endif;

             t = t + 1;

          Enddo;

          flgSrTotEmp = 'Y';
          Exsr ScriptWxrw4P;

       Endsr;
       //*****************************************************************
       //  GetMonth - Determine the name of the month
       //*****************************************************************
       Begsr GetMonth;
          Select;
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '01';
                srPayrollDs(t).strHdg = 'January';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '02';
                srPayrollDs(t).strHdg = 'February';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '03';
                srPayrollDs(t).strHdg = 'March';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '04';
                srPayrollDs(t).strHdg = 'April';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '05';
                srPayrollDs(t).strHdg = 'May';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '06';
                srPayrollDs(t).strHdg = 'June';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '07';
                srPayrollDs(t).strHdg = 'July';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '08';
                srPayrollDs(t).strHdg = 'August';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '09';
                srPayrollDs(t).strHdg = 'September';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '10';
                srPayrollDs(t).strHdg = 'October';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '11';
                srPayrollDs(t).strHdg = 'November';
             When %Subst(%Editc(srPayrollDs(t).intRpMy:'X'):4:2) = '12';
                srPayrollDs(t).strHdg = 'December';

          Endsl;
       Endsr;
       //*****************************************************************
       //  SrCalcPayRptDt - Determine the payroll reporting date
       //*****************************************************************
       Begsr SrCalcPayRptDt;
          j = 1;
          Select;
             When srPayrollHist(j).strDoWk = 'MO';
                intNumDay = 1;
             When srPayrollHist(j).strDoWk = 'TU';
                intNumDay = 2;
             When srPayrollHist(j).strDoWk = 'WE';
                intNumDay = 3;
             When srPayrollHist(j).strDoWk = 'TH';
                intNumDay = 4;
             When srPayrollHist(j).strDoWk = 'FR';
                intNumDay = 5;
             When srPayrollHist(j).strDoWk = 'SA';
                intNumDay = 6;
             When srPayrollHist(j).strDoWk = 'SU';
                intNumDay = 7;
          Endsl;
          Monitor;
            inDate8 = %dec(%char(%date(intDateWkly:*cymd):*usa0):8:0);
             x = 1;
             inDays = 1;
             outError = *blank;

             // Cycle through the week to match the date with the SR Day of Week
             Dow x <= 7;
                strDayOfWeek = DayOfWeek(inDate8);
                If intNumDay = strDayOfWeek;
                   intDateWkly = %dec(%char(%date(inDate8:*usa):*cymd0):7:0);
                   Leave;
                Else;
                   Callp GetFutureDate(inDate8:inDays:outError);
                   x = x + 1;
                Endif;
             Enddo;
          On-Error;
          Endmon;
       Endsr;
       //*****************************************************************
       //  SrDatePlus7 - Add 7 days to the date
       //*****************************************************************
       Begsr SrDatePlus7;

       Monitor;
         inDate8 = %dec(%char(%date(intDateWkly:*cymd):*usa0):8:0);
       On-Error;
       Endmon;
       inDays = 7;
       outError = *blank;
       Monitor;
       Callp GetFutureDate(inDate8:inDays:outError);
       intDateWkly = %dec(%char(%date(inDate8:*usa):*cymd0):7:0);
       On-error;
       Endmon;

       Endsr;
       //*****************************************************************
       //  SrWeekly - Write S/R for each week
       //*****************************************************************
       Begsr SrWeekly;
          flgRptType = 'W';

          // Determine policy to date if cancelled
          If Cx(w) <> 0 and Cx(w) <= ELpTdt;
             intPtDt = Cx(w);
          Else;
             intPtDt = ELpTdt;
          Endif;

          // Load each week into and array
          t = 1;
          decAmtP  = 0;
          decTotAmtP = 0;
          flgSumPeriod = ' ';
          flgSrTotEmp = ' ';
          Clear srPayrollDs;
          intDateWkly = ELpFDt;
          Exsr SrCalcPayRptDt;

          Dow intDateWkly <= intPtDt;
             srPayrollDs(t).strType = 'Weekly';
             srPayrollDs(t).strHdg = 'Week ' + %Char(t);
             Exsr SrNoRpt;

             // Read through employer payroll to process for each class code
             Setll (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
             Reade (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
             Dow not %Eof(WMEPL7E);
                //*** Beg Chg ***** 05/09/25 **************************
                //***Chain (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv:intDateWkly:
                //***EPclas:EPspec) psr08;
                Chain(n) (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv:intDateWkly:
                EPclas:EPspec) psr08;
                //*** End Chg ***** 05/09/25 **************************
                If %Found(wasrl8);
                   srPayrollDs(t).intToDt = s8toDt;
                   decAmtP = decAmtP + S8amtP;
                   decTotAmtP = decTotAmtP + S8amtP;
                   Exsr ScriptWxrw4p;
                Else;
                   Clear psr08Ds;
                   Exsr ScriptWxrw4P;
                Endif;
                Reade (xxCo#:xxFnd:xxFyr:EMemp#:EMdiv) pep07e;
             Enddo;
             flgSumPeriod = 'Y';
             Exsr ScriptWxrw4p;

             // Calculate the date for the next week
             Exsr SrDatePlus7;

             t = t + 1;

          Enddo;

          flgSrTotEmp = 'Y';
          Exsr ScriptWxrw4P;

       Endsr;
       //*** End Add *** 10/28/20 **************************************
     C******************************************************************
     C*  SRAUDT - SUBROUTINE TO WRITE AUDIT LOG RECORDS
     C******************************************************************
     C     SRAUDT        BEGSR
      /Free
       If (chUpdAudTR <> 'Y' And hldELMRFL = 'Y');
          LeaveSr;
       EndIf;
      /End-Free
     C                   MOVEA     EF            EFF
     C                   MOVEA     CX            CNX
     C*  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EFF(XI)                                70
     C*
     C     EFF(XI)       IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
     C*
     C     XI            DOWGT     *ZERO
     C     EFF(XI)       IFGT      ELPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Determine if cancellation date exists, set canx flag
     C     XI            IFEQ      *ZERO
     C                   MOVE      'N'           XXCXFL            1
     C                   ELSE
     C     CNX(XI)       IFEQ      *ZEROS
     C     CNX(XI)       ORGT      ELPTDT
     C                   MOVE      'N'           XXCXFL
     C                   ELSE
     C                   MOVE      'Y'           XXCXFL
     C                   ENDIF                                                  CX,XI=*ZEROS
     C                   ENDIF                                                  XI IFEQ *ZERO
     C*
     C*  DONT UPDATE AUDIT TRACKING RECORD IF EMPLOYER IS CANCELLED
     C*
     C     XXPFDT        IFNE      XXPTDT
     C*
     C*  FIND AUDIT TRACKING RECORD IF IT EXISTS
     C*
     C*
     C                   SETON                                        63
     C     AUKY          SETGT     PAU01
     C     AUKY          READPE    PAU01                                  60
     C     *IN60         IFEQ      *OFF
     C     AUSTAT        ANDEQ     'A'
     C                   SETOFF                                       63
     C                   ENDIF                                                   Active Audit trck
     C*
     C*
     C*  FOR A NEW RECORD, CLEAR OUT ENTIRE RECORD FIRST
     C*
     C     *IN63         IFEQ      *ON
     C                   CLEAR                   PAU01
     C                   ENDIF
     C*
     C*  FILL FIELDS COMMON TO WRITE AND UPDATE ACTIVITY
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AURQDT
     C*
     C*  DEFAULT AUSTAT IF NECESSARY
     C     AUSTAT        IFEQ      *BLANK
     C                   MOVE      'A'           AUSTAT
     C                   ENDIF
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
     C                   Z-ADD     DSTIME        AUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C*
     C*  HANDLE NEW RECORD
     C*
     C     *IN63         IFEQ      *ON
     C                   Z-ADD     ELCO#         AUCO#
     C                   Z-ADD     ELFND         AUFND
     C                   Z-ADD     ELFYR         AUFYR
     C                   Z-ADD     ELEMP#        AUEMP#
     C                   Z-ADD     ELDIV         AUDIV
     C                   MOVE      'N'           AURVFL
     C                   MOVE      'N'           AURSGN
     C                   MOVE      'N'           AUREFL
     C                   MOVE      XHTYPA        AUTYPA
     C                   MOVE      XHACO         AUACO
     C                   MOVE      *ZERO         AURCVD
     C                   MOVE      *ZERO         AUPRDT
     C                   MOVE      *ZERO         AUMAIL
     C                   MOVE      *ZERO         AUCNTD
     C                   MOVE      *ZERO         AUATTM
     C                   MOVE      *BLANK        AUSTAF
     C                   MOVE      *BLANK        AUCOM1
     C                   MOVE      *BLANK        AUCOM2
     C                   MOVE      *BLANK        AUCOM3
     C                   MOVE      *BLANK        AUCOM4
     C*  IF CANCELLED DURING CURRENT YEAR
     C*
     C                   MOVE      'A'           AUSTAT
     C                   MOVE      XXCXFL        AUCXFL
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUPTDT
     C                   Z-ADD     *ZERO         AUATDT
     C                   MOVE      'N'           AUCNFL
     C*
     C* Move for CLT102
     C   53              MOVE      EMLREP        AUREP
     C*
     C*  WRITE A NEW RECORD
     C                   WRITE     PAU01
     C*
     C                   ELSE
     C*
     C*  UPDATE AN EXISTING RECORD
     C*
     C*  IF AUCXFL = 'N' AND REQUESTED EMPLOYER HAS A CANX DATE W/IN
     C*  REQUESTED POLICY YEAR, INACTIVATE CURRENT RECORD AND WRITE
     C*  A NEW ONE
     C*
     C     AUCXFL        IFEQ      'N'
     C*
     C     XXCXFL        IFEQ      'Y'
     C                   MOVE      'I'           AUSTAT
     C*
     C                   UPDATE    PAU01
     C*
     C*    Save audit company for assignation to new adt trk record
     C                   MOVE      AUACO         SVACO             3
     c                   eval      svtypa = AUTYPA
     C*
     C*  WRITE NEW RECORD
     C                   CLEAR                   PAU01
     C*
     C                   Z-ADD     ELCO#         AUCO#
     C                   Z-ADD     ELFND         AUFND
     C                   Z-ADD     ELFYR         AUFYR
     C                   Z-ADD     ELEMP#        AUEMP#
     C                   Z-ADD     ELDIV         AUDIV
     C                   MOVE      'N'           AURVFL
     C                   MOVE      'N'           AURSGN
     C                   MOVE      'N'           AUREFL
     C                   MOVE      SVACO         AUACO
     C                   MOVE      *ZERO         AURCVD
     C                   MOVE      *ZERO         AUPRDT
     C                   MOVE      *ZERO         AUMAIL
     C                   MOVE      *ZERO         AUCNTD
     C                   MOVE      *ZERO         AUATTM
     C                   MOVE      *BLANK        AUSTAF
     C                   MOVE      *BLANK        AUCOM1
     C                   MOVE      *BLANK        AUCOM2
     C                   MOVE      *BLANK        AUCOM3
     C                   MOVE      *BLANK        AUCOM4
     C*
     C                   MOVE      'A'           AUSTAT
     C                   MOVE      XXCXFL        AUCXFL
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUPTDT
     C                   Z-ADD     *ZERO         AUATDT
     C                   MOVE      'N'           AUCNFL
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AURQDT
     C*
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
     C                   Z-ADD     DSTIME        AUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C*
      *
      * Auto assign audit co and type if not already assigned.
      *
     c                   if        xhtypa <> *blank and
     c                             svtypa =  *blank
     c                   eval      AUTYPA = XHTYPA
     c                   else
     c                   eval      AUTYPA = svtypa
     c                   endif

     c                   if        xhaco  <> *blank and
     c                             svaco  =  *blank
     c                   eval      AUACO  = XHACO
     c                   else
     c                   eval      AUACO  = svaco
     c                   endif
     C* Move for CLT102
     C   53              MOVE      EMLREP        AUREP
     C*
     C                   WRITE     PAU01
     C*
     C                   ELSE                                                   XXCXFL='Y'
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AURQDT
     C*
     C                   MOVE      XXCXFL        AUCXFL
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUPTDT
     C*
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
     C                   Z-ADD     DSTIME        AUTIME
     C*
     C*
      *
      * If auto assign was successful, move to type audit and audit co# fields
      * else, let the previous values carry thru
      *
     c                   if        xhtypa <> *blank
     c                   eval      AUTYPA = XHTYPA
     c                   endif

     c                   if        xhaco  <> *blank
     c                   eval      AUACO  = XHACO
     c                   endif
     C* Move for CLT102
     C   53              MOVE      EMLREP        AUREP
     C*
     C                   UPDATE    PAU01
     C*
     C                   ENDIF                                                  XXCXFL=*ON
     C                   ENDIF                                                  AUCXFL='N'
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  ENDIF XXPFDT
     C*
     C                   ENDSR
     C*
     C*
     C*================================================================
     C*  SRSRPR - Calculate self-reporting payroll to date within from
     C*           and to months (XXFRMY, XXTOMY).  These will be *loval
     C*           and *hival respectively, if calc for all months.
     C*================================================================
     C     SRSRPR        BEGSR
     C*
     C                   Z-ADD     0             XXAMT
     C*
     C     SRKY          CHAIN     PSR01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     SRRPMY        IFGE      XXFRMY
     C     SRRPMY        ANDLT     XXTOMY
     C                   ADD       SRAMTP        XXAMT
     C                   ENDIF                                                  SRRPMY>=XXFRMY
     C*
     C     SRKY          READE     PSR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  SRSRPR
     C*================================================================
     C*  SRSPL2 - Calculates self-rptg payroll if pol yr is split
     C*================================================================
     C     SRSPL2        BEGSR
     C*
     C     EPEFDT        IFLT      PDAT(Z)
     C*
     C*     Initialize from and to MM/CCYY for calculating payroll
     C                   MOVEL     EPEFDT        XXFRMY
     C                   MOVEL     PDAT(Z)       XXTOMY
     C*
     C*  FIRST, CHECK TO SEE IF THIS PAYROLL RECORD ONLY COVERS ONE EXP
     C*  MOD PERIOD.  IF SO, NO SPLIT NEEDS TO BE DONE.
     C*
     C                   Z-ADD     1             X
     C     PDAT(Z)       LOOKUP    RDAT(X)                                60
     C*
     C     *IN88         IFEQ      *OFF
     C     *IN60         ANDEQ     *ON
     C     *IN86         OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     *IN87         OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXDEFL        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XX0709        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXAZ10        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXOK10        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     XXTN14        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C     xxOK14        OREQ      *ON
     C     *IN60         ANDEQ     *ON
     C*
     C     PDAT(Y)       IFEQ      ELPFDT
     C     PDAT(Y)       OREQ      XXANDT
     C     *IN88         ANDEQ     *OFF
     C     PDAT(Y)       OREQ      1030401
     C     *IN86         ANDEQ     *ON
     C     PDAT(Y)       OREQ      1031001
     C     *IN87         ANDEQ     *ON
     C     PDAT(Y)       OREQ      1081001
     C     XXDEFL        ANDEQ     *ON
     C     PDAT(Y)       OREQ      1090701
     C     XX0709        ANDEQ     *ON
     C     PDAT(Y)       OREQ      1100101
     C     XXAZ10        ANDEQ     *ON
     C     PDAT(Y)       OREQ      xxSpltDt
     C     XXOK10        ANDEQ     *ON
     C     PDAT(Y)       OREQ      xxSpltDt
     C     XXTN14        ANDEQ     *ON
     C     PDAT(Y)       OREQ      xxSpltDt
     C     xxOK14        ANDEQ     *ON
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDP            8 0
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODP            8 0
     C*
     C*    Call subroutine to calc self-rptg payroll
     C                   EXSR      SRSRPR
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       2             XXLINE            3 0
     C                   GOTO      SPL2EN
     C                   ENDIF                                                  PDAT,Y IFEQ
     C                   ENDIF
     C*
     C*  Get from and to month/year values
     C     EPEFDT        IFGT      PDAT(Y)
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDP
     C*
     C                   ELSE
     C*
     C                   MOVEL     PDAT(Y)       XXFRMY
     C                   Z-ADD     PDAT(Y)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDP
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODP
     C*
     C*    Call subroutine to calc self-rptg payroll
     C                   EXSR      SRSRPR
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       If isUseScript = *on and flgNoOutput <> 'Y';
          Exsr ScriptWxrw1p;
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     DETL2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C                   ADD       2             XXLINE
     C*
     C                   ENDIF
     C*
     C     SPL2EN        ENDSR
     C*================================================================
     C*  SR#LNS - Gets the # of months that will be printed for self-
     C*           reporting payroll amounts and adds 3 to get the
     C*           total lines needed
     C*================================================================'
     C     SR#LNS        BEGSR
     C*
     C                   Z-ADD     0             #LNS              3 0
     C     ZZYYMM        DOWLE     TORPMY
     C                   ADD       1             #LNS
     C*
     C                   ADD       1             ZZYYMM
     C     ZZMM          IFGT      12
     C                   Z-ADD     1             ZZMM
     C                   ADD       100           ZZYYMM
     C                   ENDIF                                                  ZZMM>12
     C*
     C                   ENDDO
     C*
     C                   ADD       3             #LNS
     C                   Z-ADD     ELPFDT        ZZDATE
     C*
     C                   ENDSR                                                  SR#LNS
     C*================================================================
     C*  SRRNGE - Gets range of months to process
     C*================================================================'
     C     SRRNGE        BEGSR
     C*
     C*    Get ending month
     C     CX(W)         IFNE      0
     C     CX(W)         ANDLE     ELPTDT
     C                   Z-ADD     CX(W)         ZZDATE
     C                   ELSE
     C                   Z-ADD     ELPTDT        ZZDATE
     C                   ENDIF                                                  CX,W<>0
     C*
     C                   Z-ADD     ZZYYMM        TORPMY            5 0
     C                   Z-ADD     ZZDD          TODD              3 0
     C*
     C*    Get starting month
     C                   Z-ADD     ELPFDT        ZZDATE
     C                   MOVE      ZZYYMM        MM                2 0
     C*
     C                   ENDSR                                                  SRRNGE
     C*================================================================
     C*  SRAUTO - Handle auto assignment functions
     C*================================================================
     C     SRAUTO        BEGSR
     C*
     C* * ALREADY RUN
     c                   call      'WA069R'
     c                   parm      ELCO#         @@co#
     c                   parm      ELFND         @@fnd
     c                   parm      ELEMP#        @@emp#
     c                   parm      ELFYR         @@fyr
     c                   parm      *blank        ##nrbs
     C*
     C     ENAUTO        ENDSR
     C*
     C*================================================================
     C*  SRPDF  - Handle DARTS and PDF creation
     C*================================================================
     C     SRPDF         BEGSR
     C*
     C                   EXSR      CLOSE2
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #1FOLD           80
     C                   PARM      *BLANKS       #1DCID           12
     C                   PARM      'AU'          #1PRE             2
     C                   PARM      QQCO#         #1CO#             3
     C                   PARM      QQFND         #1FND             3
     C                   PARM      'P'           #1RTCD            1
      *
     C                   CALL      'W7231C1'
     C                   PARM                    #1FOLD           80
     C                   PARM                    #1DCID           12
      *
     C*
     C                   EXSR      OPEN2
     C*
     C                   ENDSR
     C*
     C     ADLSEL        BEGSR
     C*
     C                   MOVE      'Y'           I#ASEL            1
     C*
     C* Get required data for checks
     C                   CLEAR                   AUTYPA
     C                   CLEAR                   AUREP
     C     AUKY          CHAIN     PAU01                              60
     C*
     C*  Find if cancellation date exists
     C                   Z-ADD     1             X
     C     *ZERO         LOOKUP    CX(X)                                  70
     C*
     C     CX(X)         IFNE      *ZERO
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF
     C*
     C     X             DOWGT     *ZERO
     C     CX(X)         IFGT      ELPTDT
     C                   SUB       1             X
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Set canx flag
     C     X             IFEQ      *ZERO
     C                   MOVE      'N'           XXCXFL            1
     C                   ELSE
     C                   MOVE      'Y'           XXCXFL
     C                   ENDIF                                                  X  IFEQ *ZERO
     C*
     C* CHECK AUDIT TYPE
     C*
     C     XXAUTY        IFNE      *BLANKS
     C                   SELECT
     C     XXATIE        WHENEQ    'I'
     C     XXAUTY        ANDNE     AUTYPA
     C                   MOVE      'N'           I#ASEL
     C     XXATIE        WHENEQ    'E'
     C     XXAUTY        ANDEQ     AUTYPA
     C                   MOVE      'N'           I#ASEL
     C                   ENDSL
     C                   ENDIF
     C*
     C* CHECK ACCOUNT TYPE
     C*
     C                   CALL      'SRDDIF7'
     C                   PARM      ELPFDT        SRFRDC            7 0
     C                   PARM      ELPTDT        SRTODC            7 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRDAYS        DIV       365           WKYR              2 1
     C*
     C                   SELECT
     C     XXACTY        WHENEQ    'F'
     C     WKYR          ANDLT     1
     C                   MOVE      'N'           I#ASEL
     C     XXACTY        WHENEQ    'P'
     C     WKYR          ANDGE     1
     C                   MOVE      'N'           I#ASEL
     C                   ENDSL
     C*
     C* CHECK ACCOUNT STATUS
     C*
     C                   SELECT
     C     XXACST        WHENEQ    'C'
     C     XXCXFL        ANDEQ     'N'
     C                   MOVE      'N'           I#ASEL
     C     XXACST        WHENEQ    'A'
     C     XXCXFL        ANDEQ     'Y'
     C                   MOVE      'N'           I#ASEL
     C                   ENDSL
     C*
     C* CHECK FOR AUDIT REPRESENTATIVE
     C*
     C     XXAR          IFNE      *BLANKS
     C*
     C     AUREP         LOOKUP    XXAR30                                 60
     C*
     C                   SELECT
     C     XXAR30(1)     WHENEQ    '***'
     C     AUREP         ANDNE     *BLANKS
     C                   MOVE      'N'           I#ASEL
     C     XXAR30(1)     WHENEQ    '***'
     C     AUREP         ANDEQ     *BLANKS
     C* This condition should not change the selection status
     C     *IN60         WHENEQ    *OFF
     C                   MOVE      'N'           I#ASEL
     C     AUREP         WHENEQ    *BLANKS
     C                   MOVE      'N'           I#ASEL
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C* CHECK FOR GROUP 1 & 5 COVERAGE
     C*
     C                   SELECT
     C     XXG15C        WHENEQ    'Y'
     C     EMSPAC        ANDNE     'Y'
     C                   MOVE      'N'           I#ASEL
     C     XXG15C        WHENEQ    'N'
     C     EMSPAC        ANDEQ     'Y'
     C                   MOVE      'N'           I#ASEL
     C                   ENDSL
     C*
     C                   ENDSR
     C     SRPECM        BEGSR
     C*
     C                   EXSR      OPENA
     C*
     C                   EXSR      GETINF
     C                   Z-ADD     5             X
     C                   DO        5
     C*
     C     EF(X)         IFNE      *ZERO
     C                   LEAVE
     C                   ENDIF
     C*
     C                   SUB       1             X
     C                   ENDDO
     C                   Z-ADD     X             W                 1 0
     C*
     C     QQAFTY        IFEQ      'E'
     C     QQAFTY        OREQ      '2'
     C*
     C                   SELECT
     C     XXCXFL        WHENEQ    'Y'
     C                   EXSR      AUDFM3
     C     WKYR          WHENGE    1
     C                   EXSR      AUDFM1
     C     WKYR          WHENLT    1
     C                   EXSR      AUDFM2
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C     QQAFTY        IFEQ      'E'
     C     CLTID         ANDEQ     '071'
     C                   MOVE      'E'           WAFTY                          PRINT ECM ONLY
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* * SUBROUTINE TO PRINT ECM AUDIT FORM 1
     C*****************************************************************
     C     AUDFM1        BEGSR
     C*
     C* * SERVICED BY ADDRESS INFORMATION
     C*
     C     OFKY          CHAIN     POF01                              60
     C                   MOVEL     OFADR1        A1ADR1
     C     OFADR2        IFNE      *BLANKS
     C                   MOVEL(P)  OFADR2        A1ADR2
     C     A1ADR2        CAT       ',':0         A1ADR2
     C                   ENDIF
     C     A1ADR2        CAT(P)    OFCTY:1       A1ADR2
     C     A1ADR2        CAT       ',':0         A1ADR2
     C     A1ADR2        CAT       OFST:1        A1ADR2
     C*
     C                   MOVEL     OFZIP         WKZIP             9
     C     5             SUBST     WKZIP:1       WZIPSS            5
     C     A1ADR2        CAT       WZIPSS:1      A1ADR2
     C     4             SUBST     WKZIP:6       WZIPS1            4
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A1ADR2        CAT       '-':0         A1ADR2
     C     A1ADR2        CAT       WZIPS1:0      A1ADR2
     C                   ENDIF
     C*
     C                   MOVEL     OFAREA        A1PHN            10
     C                   MOVEL     OFPHN#        A1PHN#            7
     C     A1PHN         CAT       A1PHN#:0      A1PHN
     C                   MOVEL     A1PHN         A1PHN1
     C*
     C                   MOVEL     OFFARA        A1FAX            10
     C                   MOVEL     OFFAX#        A1FAX#            7
     C     A1FAX         CAT       A1FAX#:0      A1FAX
     C                   MOVEL     A1FAX         A1FAX1
     C* PRINT DATE
     C                   Z-ADD     XXPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C** **                MOVELSRDTO     A1PDAT
     C                   MOVEL     XXPRDT        Q1MMM
     C                   EXSR      SRDATE
     C                   MOVEL     WDATE1        A1DAT1
     C* SYSTEM DATE
     C                   Z-ADD     Q1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7
     C*
     C                   MOVEL     SYSDT         Q1MMM             7
     C                   EXSR      SRDATE
     C** **                MOVELWDATE1    A1DAT1
     C                   MOVEL     WNCYR         A1YR3
     C*
     C* EMPLOYER NAME AND ADDRESS
     C*  Get employer contact information
     C                   EXSR      GTCTCT
     C*
     C*  EMPLOYER NAME
     C     EMKY2         CHAIN     PEM01                              61
     C                   MOVEL     EMNAME        A1ENAM
     C*
     C* * EMPLOYER # AND DIVISION
     C*
     C                   CLEAR                   A1MEM#
     C     EMKY2         CHAIN     PEM01                              61
     C                   MOVEL     EMEMP#        WKMEM#            9
     C                   MOVEL     EMDIV         WKDIV             5
     C*
     C     '0'           CHECK     WKMEM#        WFL               2 0
     C                   SUBST     WKMEM#:WFL    A1MEM#
     C*
     C     WKDIV         IFNE      *ZEROS
     C     A1MEM#        CAT       '/':0         A1MEM#
     C*
     C     '0'           CHECK     WKDIV         WFL
     C                   SUBST(P)  WKDIV:WFL     WKDIV1            5
     C     A1MEM#        CAT       WKDIV1:0      A1MEM#
     C*
     C                   ENDIF
     C*
     C* * POLICY PERIOD FROM DATE - TO DATE
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1PFDT            8 0
     C*
     C*  LOAD POLICY PERIOD DATES
     C*
     C     CX(W)         IFNE      *ZERO
     C*
     C*  If cancellation date is less than ELPFDT, then load ELPFDT
     C*     in XXPTDT field.
     C*
     C                   SELECT
     C*
     C     CX(W)         WHENLE    ELPFDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1PTDT
     C*
     C     CX(W)         WHENLE    ELPTDT
     C*
     C                   Z-ADD     CX(W)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1PTDT
     C*
     C     CX(W)         WHENGT    ELPTDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1PTDT
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1PTDT
     C*
     C                   ENDIF
     C*
     C*
     C* ** MOVE AUDIT YEAR
     C                   Z-ADD     A1PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT            7 0
     C*
     C                   MOVE      XXTDAT        Q1MON             7
     C                   EXSR      SRDATE
     C                   MOVEL     WNCYR         A1YEAR
     C*
     C                   MOVE      XXDDUE        Q1MON             7
     C                   EXSR      SRDATE
     C                   MOVE      WDATE         A1DDUE
     C*
     C     A1DDUE        CAT       WKFLD1:1      A1DTTX
     C*
     C     A1DDUE        CAT       '.':0         A1DDUE
     C*
     C* * MAIN PAGE
     C*
     C                   MOVE      XXDDUE        Q1MMM             7
     C                   EXSR      SRDATE
     C                   MOVEL     Q1MMDD        A1MMDD           12
     C*
     C*
     C* PLEASE MAIL, OVERNIGHT, EMAIL, OR FAX TO:
     C     OFKY          CHAIN     POF01                              60
     C*
     C                   MOVEL     OFADR1        A1ADD1
     C                   MOVEL     OFADR2        A1ADD2
     C     OFADR2        IFNE      *BLANKS
     C     A1ADD2        CAT       ',':0         A1ADD2
     C                   ENDIF
     C     A1ADD2        CAT       OFCTY:1       A1ADD2
     C     A1ADD2        CAT       ',':0         A1ADD2
     C     A1ADD2        CAT       OFST:1        A1ADD2
     C*
     C                   MOVEL     OFZIP         WKZIP
     C     5             SUBST     WKZIP:1       WZIPSS
     C     A1ADD2        CAT       WZIPSS:1      A1ADD2
     C     4             SUBST     WKZIP:6       WZIPS1
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A1ADD2        CAT       '-':0         A1ADD2
     C     A1ADD2        CAT       WZIPS1:0      A1ADD2
     C                   ENDIF
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL     COADR1        A1ADD3
     C     A1ADD3        CAT       COADR2:1      A1ADD3
     C     A1ADD3        CAT       ',':0         A1ADD3
     C     A1ADD3        CAT       COCTY:1       A1ADD3
     C     A1ADD3        CAT       ',':0         A1ADD3
     C     A1ADD3        CAT       COST:1        A1ADD3
     C*
     C                   MOVEL     COZIP         WKZIP
     C     5             SUBST     WKZIP:1       WZIPSS
     C     A1ADD3        CAT       WZIPSS:1      A1ADD3
     C     4             SUBST     WKZIP:6       WZIPS1
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A1ADD3        CAT       '-':0         A1ADD3
     C     A1ADD3        CAT       WZIPS1:0      A1ADD3
     C                   ENDIF
     C*
     C                   MOVEL     COFARA        A1FAX            10
     C                   MOVEL     COFAX#        A1FAX#            7
     C     A1FAX         CAT       A1FAX#:0      A1FAX
     C                   MOVEL     A1FAX         A1FAXX
     C*
     C* * MAIN PAGE
     C*
     C                   MOVE      A1YEAR        A1YR1
     C                   MOVEL     A1MMDD        A1MMD
     C     A1MMD         CAT       ',':0         A1MMD
     C                   MOVE      WNCYR         A1YR2
     C     A1YR1         ADD       1             A1YR3
     C*
     C                   EXSR      ADDLIN
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR1
     C                   WRITE     QHEAD1
     C                   WRITE     QMEMAU
     C                   WRITE     QEMPAD
     C                   WRITE     PAGE_1
     C*
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C* ** PAGE 2
     C* MEMBER # AND DIVISION
     C                   CLEAR                   A1MEM2
     C                   MOVEL     A1MEM#        A1MEM2
     C* POLICY PERIOD FROM DATE - TO DATE
     C*
     C*  LOAD POLICY PERIOD DATES
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1FDT2
     C*
     C     CX(W)         IFNE      *ZERO
     C*
     C*  If cancellation date is less than ELPFDT, then load ELPFDT
     C*     in XXPTDT field.
     C*
     C                   SELECT
     C*
     C     CX(W)         WHENLE    ELPFDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT2
     C*
     C     CX(W)         WHENLE    ELPTDT
     C*
     C                   Z-ADD     CX(W)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT2
     C*
     C     CX(W)         WHENGT    ELPTDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT2
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT2
     C*
     C                   ENDIF
     C*
     C                   MOVEL     A1ADD1        A1AD21
     C                   MOVEL     A1ADD2        A1AD22
     C                   MOVEL     A1ADD3        A1AD23
     C                   MOVEL     A1FAX         A1FAX2
     C*
     C* * CLASS CODE AND DESCRIPTION
     C*
     C                   MOVE      0             CN                1 0
     C                   MOVE      '0'           WPRT              1
     C                   Z-ADD     1             CN
     C     CN            DOWLE     8
     C     CN            OCCUR     DSCLAS
     C                   CLEAR                   DSCLAS
     C                   ADD       1             CN
     C                   ENDDO
     C                   Z-ADD     0             CN
     C     EPKY          SETLL     PEP01
     C     EPKY1         READE     PEP01                                  55
     C     *IN55         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C     ELKY23        CHAIN     PEL04                              60
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C*
      /free
       xxType = 'P';
       x1EfDt = SrEffd(EPco#:EPfnd:EPfyr:EPemp#:EPdiv:EPefdt:xxType:EPclas);
       Setgt prKy1 ppr01;
       Readpe prKy ppr01;
       If not(%eof);
          wlDesc = PRdesc;
          xxArfl = PRarfl;
       Else;
          wlDesc = *blanks;
          xxArfl = *blanks;
       Endif;
      /end-free
     C*
     C                   Z-ADD     EPCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCADT            8 0
     C*
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C*
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        WLDESC
     C*
     C   60RSKY          CHAIN     PRS01                              61
     C*                                                                   *
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*
     C  N61
     CANN60              MOVEL(P)  PXDESC        WLDESC
     C                   ENDIF                                                  EPEXT IFNE
     C*
     C                   ADD       1             CN
     C     CN            OCCUR     DSCLAS
     C                   MOVEL     EPCLAS        A1CL1
     C                   MOVE      EPSPEC        A1SD1
     C                   MOVEL     WLDESC        A1DE1
     C     CN            IFEQ      8
     C* * * PRINT THIS PAGE
     C     1             OCCUR     DSCLAS
     C                   MOVEL     A1CL1         A1CL11
     C                   MOVE      A1SD1         A1SD11
     C                   MOVE      A1DE1         A1DE11
     C*
     C     2             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL12
     C                   MOVE      A1SD1         A1SD12
     C                   MOVE      A1DE1         A1DE12
     C*
     C     3             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL13
     C                   MOVE      A1SD1         A1SD13
     C                   MOVE      A1DE1         A1DE13
     C*
     C     4             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL14
     C                   MOVE      A1SD1         A1SD14
     C                   MOVE      A1DE1         A1DE14
     C*
     C     5             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL15
     C                   MOVE      A1SD1         A1SD15
     C                   MOVE      A1DE1         A1DE15
     C*
     C     6             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL16
     C                   MOVE      A1SD1         A1SD16
     C                   MOVE      A1DE1         A1DE16
     C*
     C     7             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL17
     C                   MOVE      A1SD1         A1SD17
     C                   MOVE      A1DE1         A1DE17
     C*
     C     8             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL18
     C                   MOVE      A1SD1         A1SD18
     C                   MOVE      A1DE1         A1DE18
     C*
     C                   MOVEL     A1YR1         A1YR4
     C*
     C                   MOVEL     A1DDUE        A1DUE1
     C*
     C* * CLEAR DATA STRUCTURE FOR NEXT CLASS CODES
     C     CN            DOWGT     0
     C     CN            OCCUR     DSCLAS
     C                   CLEAR                   DSCLAS
     C     CN            SUB       1             CN
     C                   ENDDO
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR2
     C                   WRITE     QHEAD2
     C                   WRITE     QCLAS
     C                   WRITE     PAGE_2
     C*
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   MOVE      0             N
     C                   MOVE      '1'           WPRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  EPSTAT    IFNE 'I'
     C*
     C     EPKY1         READE     PEP01                                  55
     C                   ENDDO
     C*
     C     CN            IFGT      0
     C* * * PRINT THIS PAGE
     C     1             OCCUR     DSCLAS
     C                   MOVEL     A1CL1         A1CL11
     C                   MOVE      A1SD1         A1SD11
     C                   MOVE      A1DE1         A1DE11
     C*
     C     2             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL12
     C                   MOVE      A1SD1         A1SD12
     C                   MOVE      A1DE1         A1DE12
     C*
     C     3             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL13
     C                   MOVE      A1SD1         A1SD13
     C                   MOVE      A1DE1         A1DE13
     C*
     C     4             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL14
     C                   MOVE      A1SD1         A1SD14
     C                   MOVE      A1DE1         A1DE14
     C*
     C     5             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL15
     C                   MOVE      A1SD1         A1SD15
     C                   MOVE      A1DE1         A1DE15
     C*
     C     6             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL16
     C                   MOVE      A1SD1         A1SD16
     C                   MOVE      A1DE1         A1DE16
     C*
     C     7             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL17
     C                   MOVE      A1SD1         A1SD17
     C                   MOVE      A1DE1         A1DE17
     C*
     C     8             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A1CL18
     C                   MOVE      A1SD1         A1SD18
     C                   MOVE      A1DE1         A1DE18
     C*
     C                   MOVEL     A1YR1         A1YR4
     C*
     C                   MOVEL     A1DDUE        A1DUE1
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR2
     C                   WRITE     QHEAD2
     C                   WRITE     QCLAS
     C                   WRITE     PAGE_2
     C*
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   MOVE      '1'           WPRT
     C*
     C                   ENDIF
     C     WPRT          IFEQ      '0'
     C                   MOVEL     A1DDUE        A1DUE1
     C                   MOVEL     A1YR1         A1YR4
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR2
     C                   WRITE     QHEAD2
     C                   WRITE     QCLAS
     C                   WRITE     PAGE_2
     C*
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C                   ENDIF
     C*
     C* * PAGE 3
     C* MEMBER # AND DIVISION
     C                   CLEAR                   A1MEM3
     C                   MOVEL     A1MEM#        A1MEM3
     C*
     C*  LOAD POLICY PERIOD DATES
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1FDT3
     C*
     C     CX(W)         IFNE      *ZERO
     C*
     C*  If cancellation date is less than ELPFDT, then load ELPFDT
     C*     in XXPTDT field.
     C*
     C                   SELECT
     C*
     C     CX(W)         WHENLE    ELPFDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT3
     C*
     C     CX(W)         WHENLE    ELPTDT
     C*
     C                   Z-ADD     CX(W)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT3
     C*
     C     CX(W)         WHENGT    ELPTDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT3
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT3
     C*
     C                   ENDIF
     C* POLICY PERIOD FROM DATE - TO DATE
     C* FOOTER INFORMATION
     C                   MOVEL     A1ADD1        A1AD31
     C                   MOVEL     A1ADD2        A1AD32
     C                   MOVEL     A1ADD3        A1AD33
     C                   MOVEL     A1FAX         A1FAX3
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR3
     C                   WRITE     QHEAD3
     C                   WRITE     FOOT
     C*
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C* * PAGE 4
     C* MEMBER # AND DIVISION
     C                   CLEAR                   A1MEM4
     C                   MOVEL     A1MEM#        A1MEM4
     C*
     C*  LOAD POLICY PERIOD DATES
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1FDT4
     C*
     C     CX(W)         IFNE      *ZERO
     C*
     C*  If cancellation date is less than ELPFDT, then load ELPFDT
     C*     in XXPTDT field.
     C*
     C                   SELECT
     C*
     C     CX(W)         WHENLE    ELPFDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT4
     C*
     C     CX(W)         WHENLE    ELPTDT
     C*
     C                   Z-ADD     CX(W)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT4
     C*
     C     CX(W)         WHENGT    ELPTDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT4
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1TDT4
     C*
     C                   ENDIF
     C* POLICY PERIOD FROM DATE - TO DATE
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR4
     C                   WRITE     QHEAD4
     C                   WRITE     PAGE_4
     C                   WRITE     FOOT
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C* * SUBROUTINE TO PRINT ECM AUDIT FORM 2
     C*****************************************************************
     C     AUDFM2        BEGSR
     C*
     C* * SERVICED BY ADDRESS INFORMATION
     C*
     C     OFKY          CHAIN     POF01                              60
     C                   MOVEL     OFADR1        A2ADR1
     C     OFADR2        IFNE      *BLANKS
     C                   MOVEL(P)  OFADR2        A2ADR2
     C     A2ADR2        CAT       ',':0         A2ADR2
     C                   ENDIF
     C     A2ADR2        CAT(P)    OFCTY:1       A2ADR2
     C     A2ADR2        CAT       ',':0         A2ADR2
     C     A2ADR2        CAT       OFST:1        A2ADR2
     C*
     C                   MOVEL     OFZIP         WKZIP             9
     C     5             SUBST     WKZIP:1       WZIPSS            5
     C     A2ADR2        CAT       WZIPSS:1      A2ADR2
     C     4             SUBST     WKZIP:6       WZIPS1            4
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A2ADR2        CAT       '-':0         A2ADR2
     C     A2ADR2        CAT       WZIPS1        A2ADR2
     C                   ENDIF
     C*
     C                   MOVEL     OFAREA        A2PHN            10
     C                   MOVEL     OFPHN#        A2PHN#            7
     C     A2PHN         CAT       A2PHN#:0      A2PHN
     C                   MOVEL     A2PHN         A2PHN1
     C*
     C                   MOVEL     OFFARA        A2FAX            10
     C                   MOVEL     OFFAX#        A2FAX#            7
     C     A2FAX         CAT       A2FAX#:0      A2FAX
     C                   MOVEL     A2FAX         A2FAX1
     C* PRINT DATE
     C                   Z-ADD     XXPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     XXPRDT        Q1MMM             7
     C                   EXSR      SRDATE
     C                   MOVEL     WDATE1        A2DAT1
     C* SYSTEM DATE
     C                   Z-ADD     Q1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7
     C*
     C                   MOVEL     SYSDT         Q1MMM             7
     C                   EXSR      SRDATE
     C                   MOVE      WNCYR         A2YR3
     C*
     C* * EMPLOYER NAME AND ADDRESS
     C*  Get employer contact information
     C                   EXSR      GTCTCT
     C*
     C* EMPLOYER NAME
     C     EMKY2         CHAIN     PEM01                              61
     C                   MOVEL     EMNAME        A2ENAM
     C                   MOVEL(P)  A2ENAM        A1ENAM
     C*
     C* EMPLOYER # AND DIVISION
     C*
     C                   CLEAR                   A2MEM#
     C     EMKY2         CHAIN     PEM01                              61
     C                   MOVEL     EMEMP#        WKMEM#
     C                   MOVEL     EMDIV         WKDIV
     C*
     C     '0'           CHECK     WKMEM#        WFL               2 0
     C                   SUBST     WKMEM#:WFL    A2MEM#
     C*
     C     WKDIV         IFNE      *ZEROS
     C     A2MEM#        CAT       '/':0         A2MEM#
     C*
     C     '0'           CHECK     WKDIV         WFL
     C                   SUBST(P)  WKDIV:WFL     WKDIV1            5
     C     A2MEM#        CAT       WKDIV1:0      A2MEM#
     C*
     C                   ENDIF
     C*
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A2PFDT            8 0
     C*
     C*  LOAD POLICY PERIOD DATES
     C*
     C     CX(W)         IFNE      *ZERO
     C*
     C*  If cancellation date is less than ELPFDT, then load ELPFDT
     C*     in XXPTDT field.
     C*
     C                   SELECT
     C*
     C     CX(W)         WHENLE    ELPFDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A2PTDT
     C*
     C     CX(W)         WHENLE    ELPTDT
     C*
     C                   Z-ADD     CX(W)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A2PTDT
     C*
     C     CX(W)         WHENGT    ELPTDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A2PTDT
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A2PTDT
     C*
     C                   ENDIF
     C*
     C*
     C* ** MOVE AUDIT YEAR
     C                   Z-ADD     A2PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT            7 0
     C*
     C                   MOVE      XXTDAT        Q1MON             7
     C                   EXSR      SRDATE
     C                   MOVEL     WNCYR         A2YEAR
     C*
     C                   MOVE      XXDDUE        Q1MON
     C                   EXSR      SRDATE
     C                   MOVE      WDATE         A2DDUE
     C*
     C     A2DDUE        CAT       WKFLD1:1      A2DTTX
     C*
     C     A2DDUE        CAT       '.':0         A2DDUE
     C*
     C* MAIN PAGE
     C*
     C                   MOVE      XXDDUE        Q1MMM
     C                   EXSR      SRDATE
     C                   MOVE      Q1MMDD        A2MMDD           12
     C*
     C* PLEASE MAIL, OVERNIGHT, EMAIL, OR FAX TO
     C     OFKY          CHAIN     POF01                              60
     C*
     C                   MOVEL     OFADR1        A2ADD1
     C                   MOVEL     OFADR2        A2ADD2
     C     A2ADD2        IFNE      *BLANKS
     C     A2ADD2        CAT       ',':0         A2ADD2
     C                   ENDIF
     C     A2ADD2        CAT       OFCTY:1       A2ADD2
     C     A2ADD2        CAT       ',':0         A2ADD2
     C     A2ADD2        CAT       OFST:1        A2ADD2
     C*
     C                   MOVEL     OFZIP         WKZIP
     C     5             SUBST     WKZIP:1       WZIPSS
     C     A2ADD2        CAT       WZIPSS:1      A2ADD2
     C     4             SUBST     WKZIP:6       WZIPS1
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A2ADD2        CAT       '-':0         A2ADD2
     C     A2ADD2        CAT       WZIPS1:0      A2ADD2
     C                   ENDIF
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL     COADR1        A2ADD3
     C                   CAT       COADR2:1      A2ADD3
     C     A2ADD3        CAT       ',':0         A2ADD3
     C     A2ADD3        CAT       COCTY:1       A2ADD3
     C     A2ADD3        CAT       ',':0         A2ADD3
     C     A2ADD3        CAT       COST:1        A2ADD3
     C*
     C                   MOVEL     COZIP         WKZIP
     C     5             SUBST     WKZIP:1       WZIPSS
     C     A2ADD3        CAT       WZIPSS:1      A2ADD3
     C     4             SUBST     WKZIP:6       WZIPS1
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A2ADD3        CAT       '-':0         A2ADD3
     C     A2ADD3        CAT       WZIPS1:0      A2ADD3
     C                   ENDIF
     C*
     C                   MOVEL     COFARA        A2FAX            10
     C                   MOVEL     COFAX#        A2FAX#            7
     C     A2FAX         CAT       A2FAX#:0      A2FAX
     C                   MOVEL     A2FAX         A2FAXX
     C*
     C* * MAIN PAGE
     C*
     C                   MOVE      A2YEAR        A2YR1
     C                   MOVEL     A2MMDD        A2MMD
     C     A2MMD         CAT       ',':0         A2MMD
     C                   MOVE      WNCYR         A2YR2
     C     A2YR1         ADD       1             A2YR3
     C*
     C                   EXSR      ADDLIN
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR1
     C                   WRITE     QHEAD21
     C                   WRITE     Q2MEAU
     C                   WRITE     QEMPAD
     C                   WRITE     PAGE21
     C*
     C                   WRITE     QNPG2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C* * PAGE 2
     C* EMPLOYER # AND DIVISION
     C                   CLEAR                   A2MEM2
     C                   MOVEL     A2MEM#        A2MEM2
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   MOVEL     A2PFDT        A2FDT2
     C                   MOVEL     A2PTDT        A2TDT2
     C*
     C                   MOVEL     A2ADD1        A2AD21
     C                   MOVEL     A2ADD2        A2AD22
     C                   MOVEL     A2ADD3        A2AD23
     C                   MOVEL     A2FAX         A2FAX2
     C*
     C* CLASS CODE AND DESCRIPTION
     C*
     C                   MOVE      0             CN                1 0
     C                   MOVE      '0'           WPRT              1
     C                   Z-ADD     1             CN
     C     CN            DOWLE     8
     C     CN            OCCUR     DSCLAS
     C                   CLEAR                   DSCLAS
     C                   ADD       1             CN
     C                   ENDDO
     C                   Z-ADD     0             CN
     C     EPKY          SETLL     PEP01
     C     EPKY1         READE     PEP01                                  55
     C     *IN55         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
      *
     C     ELKY23        CHAIN     PEL04                              60
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C*
     C                   Z-ADD     EPEFDT        X1EFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY          READPE    PPR01                                  60
     C     *IN60         IFEQ      *ON
     C     PRKY          CHAIN     PPR01                              60
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     EPCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCADT            8 0
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        WLDESC
     C                   ELSE
     C                   MOVEL     PRDESC        WLDESC
     C                   ENDIF
     C*
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C*
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        WLDESC
     C*
     C   60RSKY          CHAIN     PRS01                              61
     C*                                                                * **
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*
     C  N61
     CANN60              MOVEL(P)  PXDESC        WLDESC
     C                   ENDIF                                                  EPEXT IFNE
     C*
     C                   ADD       1             CN
     C     CN            OCCUR     DSCLAS
     C                   MOVEL     EPCLAS        A1CL1
     C                   MOVE      EPSPEC        A1SD1
     C                   MOVEL     WLDESC        A1DE1
     C     CN            IFEQ      8
     C* * * PRINT THIS PAGE
     C     1             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL11
     C                   MOVE      A1SD1         A2SD11
     C                   MOVE      A1DE1         A2DE11
     C     2             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL12
     C                   MOVE      A1SD1         A2SD12
     C                   MOVE      A1DE1         A2DE12
     C     3             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL13
     C                   MOVE      A1SD1         A2SD13
     C                   MOVE      A1DE1         A2DE13
     C     4             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL14
     C                   MOVE      A1SD1         A2SD14
     C                   MOVE      A1DE1         A2DE14
     C     5             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL15
     C                   MOVE      A1SD1         A2SD15
     C                   MOVE      A1DE1         A2DE15
     C     6             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL16
     C                   MOVE      A1SD1         A2SD16
     C                   MOVE      A1DE1         A2DE16
     C     7             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL17
     C                   MOVE      A1SD1         A2SD17
     C                   MOVE      A1DE1         A2DE17
     C     8             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL18
     C                   MOVE      A1SD1         A2SD18
     C                   MOVE      A1DE1         A2DE18
     C*
     C                   MOVEL     A2YR1         A2YR4
     C*
     C                   MOVEL     A2DDUE        A2DUE1
     C*
     C* * CLEAR DATA STRUCTURE FOR NEXT CLASS CODES
     C     CN            DOWGT     0
     C     CN            OCCUR     DSCLAS
     C                   CLEAR                   DSCLAS
     C     CN            SUB       1             CN
     C                   ENDDO
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR22
     C                   WRITE     QHEAD22
     C                   WRITE     QCLAS2
     C                   WRITE     PAGE22
     C*
     C                   WRITE     QNPG2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   MOVE      0             N
     C                   MOVE      '1'           WPRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  EPSTAT    IFNE 'I'
     C*
     C     EPKY1         READE     PEP01                                  55
     C                   ENDDO
     C*
     C     CN            IFGT      0
     C* * * PRINT THIS PAGE
     C     1             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL11
     C                   MOVE      A1SD1         A2SD11
     C                   MOVE      A1DE1         A2DE11
     C     2             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL12
     C                   MOVE      A1SD1         A2SD12
     C                   MOVE      A1DE1         A2DE12
     C     3             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL13
     C                   MOVE      A1SD1         A2SD13
     C                   MOVE      A1DE1         A2DE13
     C     4             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL14
     C                   MOVE      A1SD1         A2SD14
     C                   MOVE      A1DE1         A2DE14
     C     5             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL15
     C                   MOVE      A1SD1         A2SD15
     C                   MOVE      A1DE1         A2DE15
     C     6             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL16
     C                   MOVE      A1SD1         A2SD16
     C                   MOVE      A1DE1         A2DE16
     C     7             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL17
     C                   MOVE      A1SD1         A2SD17
     C                   MOVE      A1DE1         A2DE17
     C     8             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A2CL18
     C                   MOVE      A1SD1         A2SD18
     C                   MOVE      A1DE1         A2DE18
     C*
     C                   MOVEL     A2YR1         A2YR4
     C*
     C                   MOVEL     A2DDUE        A2DUE1
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR22
     C                   WRITE     QHEAD22
     C                   WRITE     QCLAS2
     C                   WRITE     PAGE22
     C*
     C                   WRITE     QNPG2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   MOVE      '1'           WPRT
     C*
     C                   ENDIF
     C*
     C     WPRT          IFEQ      '0'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR22
     C                   WRITE     QHEAD22
     C                   WRITE     QCLAS2
     C                   WRITE     PAGE22
     C*
     C                   WRITE     QNPG2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C*
     C* * PAGE 3
     C* MEMBER # AND DIVISION
     C                   CLEAR                   A2MEM3
     C                   MOVEL     A2MEM#        A2MEM3
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   MOVEL     A2PFDT        A2FDT3
     C                   MOVEL     A2PTDT        A2TDT3
     C* FOOTER INFORMATION
     C                   MOVEL     A2ADD1        A2AD31
     C                   MOVEL     A2ADD2        A2AD32
     C                   MOVEL     A2ADD3        A2AD33
     C                   MOVEL     A2FAX         A2FAX3
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR23
     C                   WRITE     QHEAD23
     C                   WRITE     FOOT2
     C*
     C                   WRITE     QNPG2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C* * PAGE 4
     C* EMPLOYER # AND DIVISION
     C                   CLEAR                   A2MEM4
     C                   MOVEL     A2MEM#        A2MEM4
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   MOVEL     A2PFDT        A2FDT4
     C                   MOVEL     A2PTDT        A2TDT4
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR24
     C                   WRITE     QHEAD24
     C                   WRITE     PAGE24
     C                   WRITE     FOOT2
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C* * SUBROUTINE TO PRINT ECM AUDIT FORM 3
     C*****************************************************************
     C     AUDFM3        BEGSR
     C*
     C* SERVICED BY ADDRESS INFORMATION
     C*
     C     OFKY          CHAIN     POF01                              60
     C                   MOVEL     OFADR1        A3AR1
     C     OFADR2        IFNE      *BLANKS
     C                   MOVEL(P)  OFADR2        A3AR2
     C     A3AR2         CAT       ',':0         A3AR2
     C                   ENDIF
     C                   CAT(P)    OFCTY:1       A3AR2
     C     A3AR2         CAT       ',':0         A3AR2
     C     A3AR2         CAT       OFST:1        A3AR2
     C*
     C                   MOVEL     OFZIP         WKZIP             9
     C     5             SUBST     WKZIP:1       WZIPSS            5
     C     A3AR2         CAT       WZIPSS:1      A3AR2
     C     4             SUBST     WKZIP:6       WZIPS1            4
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A3AR2         CAT       '-':0         A3AR2
     C     A3AR2         CAT       WZIPS1        A3AR2
     C                   ENDIF
     C*
     C                   MOVEL     OFAREA        A3PHNS           10
     C                   MOVEL     OFPHN#        A3PHN#            7
     C     A3PHNS        CAT       A3PHN#:0      A3PHNS
     C                   MOVEL     A3PHNS        A3PHN1
     C*
     C                   MOVEL     OFFARA        A3FAX            10
     C                   MOVEL     OFFAX#        A3FAX#            7
     C     A3FAX         CAT       A3FAX#:0      A3FAX
     C                   MOVEL     A3FAX         A3FAX1
     C* PRINT DATE
     C                   Z-ADD     XXPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     XXPRDT        Q1MMM
     C                   EXSR      SRDATE
     C                   MOVEL     WDATE1        A3DAT1
     C* SYSTEM DATE
     C                   Z-ADD     Q1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7
     C*
     C                   MOVEL     SYSDT         Q1MMM
     C                   EXSR      SRDATE
     C*
     C* EMPLOYER NAME AND ADDRESS
     C*  Get employer contact information
     C                   EXSR      GTCTCT
     C*
     C* EMPLOYER NAME
     C     EMKY2         CHAIN     PEM01                              61
     C                   MOVEL     EMNAME        A3ENAM
     C                   MOVEL(P)  A3ENAM        A1ENAM
     C**IF*BILLING ADDRESS USED
     C**IF*MAILING ADDRESS USED
     C**IF*PHYSICAL ADDRESS USED
     C* EMPLOYER # AND DIVISION
     C                   CLEAR                   A3MEM#
     C     EMKY2         CHAIN     PEM01                              61
     C                   MOVE      EMEMP#        WKMEM#
     C                   MOVEL     EMDIV         WKDIV
     C*
     C     '0'           CHECK     WKMEM#        WFL               2 0
     C                   SUBST     WKMEM#:WFL    A3MEM#
     C     WKDIV         IFNE      *ZEROS
     C     A3MEM#        CAT       '/':0         A3MEM#
     C*
     C     '0'           CHECK     WKDIV         WFL
     C                   SUBST(P)  WKDIV:WFL     WKDIV1            5
     C     A3MEM#        CAT       WKDIV1:0      A3MEM#
     C*
     C                   ENDIF
     C*
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3PFDT            8 0
     C*
     C*  LOAD POLICY PERIOD DATES
     C*
     C     CX(W)         IFNE      *ZERO
     C*
     C*  If cancellation date is less than ELPFDT, then load ELPFDT
     C*     in XXPTDT field.
     C*
     C                   SELECT
     C*
     C     CX(W)         WHENLE    ELPFDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3PTDT
     C*
     C     CX(W)         WHENLE    ELPTDT
     C*
     C                   Z-ADD     CX(W)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3PTDT
     C*
     C     CX(W)         WHENGT    ELPTDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3PTDT
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3PTDT
     C*
     C                   ENDIF
     C*
     C*
     C* ** MOVE AUDIT YEAR
     C                   Z-ADD     A3PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT            7 0
     C*
     C                   MOVE      XXTDAT        Q1MON             7
     C                   EXSR      SRDATE
     C                   MOVEL     WNCYR         A3YEAR
     C*
     C                   MOVEL     A3YEAR        A3YR1
     C                   MOVEL     A3YEAR        A3YR2
     C*
     C* * PLEASE MAIL, FAX - OR - EMAIL TO
     C*
     C     OFKY          CHAIN     POF01                              60
     C*
     C                   MOVEL     OFADR1        A3ADD1
     C                   MOVEL     OFCTY         WKSTZ1           30
     C     WKSTZ1        CAT       ',':0         WKSTZ1
     C     WKSTZ1        CAT       OFST:1        WKSTZ1
     C*
     C                   MOVEL     OFZIP         WKZIP
     C     5             SUBST     WKZIP:1       WZIPSS
     C     WKSTZ1        CAT       WZIPSS:1      WKSTZ1
     C     4             SUBST     WKZIP:6       WZIPS1
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     WKSTZ1        CAT       '-':0         WKSTZ1
     C     WKSTZ1        CAT       WZIPS1:1      WKSTZ1
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN82
     C     OFADR2        IFNE      *BLANKS
     C                   MOVEL     OFADR2        A3ADD2
     C                   MOVEL     WKSTZ1        A3ADD3
     C                   MOVE      '1'           *IN82
     C                   ELSE
     C                   MOVEL     WKSTZ1        A3ADD2
     C                   ENDIF
     C*
     C                   MOVEL     OFAREA        WKPHN            10
     C                   MOVEL     OFPHN#        WKPHN#            7
     C     WKPHN         CAT       WKPHN#:0      WKPHN
     C*
     C                   MOVEL     COFARA        A3FAX            10
     C                   MOVEL     COFAX#        A3FAX#            7
     C     A3FAX         CAT       A3FAX#:0      A3FAX
     C                   MOVEL     A3FAX         A3FAXX
     C                   MOVEL     A3FAX         A3FA1X
     C*
     C                   EXSR      ADDLIN
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR31
     C                   WRITE     QHEAD31
     C                   WRITE     QMEAU3
     C                   WRITE     QEMPAD
     C                   WRITE     PAGE31
     C*
     C                   WRITE     QNPG3
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C* * PAGE 2
     C* EMPLOYER # AND DIVISION
     C                   CLEAR                   A3MEM2
     C                   MOVEL     A3MEM#        A3MEM2
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   MOVEL     A3PFDT        A3FDT2
     C                   MOVEL     A3PTDT        A3TDT2
     C**
     C     OFKY          CHAIN     POF01                              60
     C*
     C                   MOVEL     OFADR1        A3AD31
     C                   MOVEL     OFADR2        A3AD32
     C     OFADR2        IFNE      *BLANKS
     C     A3AD32        CAT       ',':0         A3AD32
     C                   ENDIF
     C     A3AD32        CAT       OFCTY:1       A3AD32
     C     A3AD32        CAT       ',':0         A3AD32
     C     A3AD32        CAT       OFST:1        A3AD32
     C*
     C                   MOVEL     OFZIP         WKZIP
     C     5             SUBST     WKZIP:1       WZIPSS
     C     A3AD32        CAT       WZIPSS:1      A3AD32
     C     4             SUBST     WKZIP:6       WZIPS1
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A3AD32        CAT       '-':0         A3AD32
     C     A3AD32        CAT       WZIPS1:0      A3AD32
     C                   ENDIF
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL     COADR1        A3AD33
     C     A3AD33        CAT       COADR2:1      A3AD33
     C     A3AD33        CAT       ',':0         A3AD33
     C     A3AD33        CAT       COCTY:1       A3AD33
     C     A3AD33        CAT       ',':0         A3AD33
     C     A3AD33        CAT       COST:1        A3AD33
     C*
     C                   MOVEL     COZIP         WKZIP
     C     5             SUBST     WKZIP:1       WZIPSS
     C     A3AD33        CAT       WZIPSS:1      A3AD33
     C     4             SUBST     WKZIP:6       WZIPS1
     C*
     C     WZIPS1        IFNE      *ZEROS
     C     A3AD33        CAT       '-':0         A3AD33
     C     A3AD33        CAT       WZIPS1:0      A3AD33
     C                   ENDIF
     C*
     C                   MOVEL     COFARA        A3FAX            10
     C                   MOVEL     COFAX#        A3FAX#            7
     C     A3FAX         CAT       A3FAX#:0      A3FAX
     C                   MOVEL     A3FAX         A3FAX2
     C*
     C* CLASS CODE DESCRIPTION
     C                   MOVE      0             CN                1 0
     C                   MOVE      '0'           WPRT              1
     C                   Z-ADD     1             CN
     C     CN            DOWLE     8
     C     CN            OCCUR     DSCLAS
     C                   CLEAR                   DSCLAS
     C                   ADD       1             CN
     C                   ENDDO
     C                   Z-ADD     0             CN
     C     EPKY          SETLL     PEP01
     C     EPKY1         READE     PEP01                                  55
     C     *IN55         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C     ELKY23        CHAIN     PEL04                              60
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C*
     C                   Z-ADD     EPEFDT        X1EFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY          READPE    PPR01                                  60
     C     *IN60         IFEQ      *ON
     C     PRKY          CHAIN     PPR01                              60
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     EPCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCADT            8 0
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        WLDESC
     C                   ELSE
     C                   MOVEL     PRDESC        WLDESC
     C                   ENDIF
     C*
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C*
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        WLDESC
     C*
     C   60RSKY          CHAIN     PRS01                              61
     C*                                                                * **
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*
     C  N61
     CANN60              MOVEL(P)  PXDESC        WLDESC
     C                   ENDIF                                                  EPEXT IFNE
     C*
     C                   ADD       1             CN
     C     CN            OCCUR     DSCLAS
     C                   MOVEL     EPCLAS        A1CL1
     C                   MOVE      EPSPEC        A1SD1
     C                   MOVEL     WLDESC        A1DE1
     C     CN            IFEQ      8
     C* * * PRINT THIS PAGE
     C     1             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL11
     C                   MOVE      A1SD1         A3SD11
     C                   MOVE      A1DE1         A3DE11
     C     2             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL12
     C                   MOVE      A1SD1         A3SD12
     C                   MOVE      A1DE1         A3DE12
     C     3             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL13
     C                   MOVE      A1SD1         A3SD13
     C                   MOVE      A1DE1         A3DE13
     C     4             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL14
     C                   MOVE      A1SD1         A3SD14
     C                   MOVE      A1DE1         A3DE14
     C     5             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL15
     C                   MOVE      A1SD1         A3SD15
     C                   MOVE      A1DE1         A3DE15
     C     6             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL16
     C                   MOVE      A1SD1         A3SD16
     C                   MOVE      A1DE1         A3DE16
     C     7             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL17
     C                   MOVE      A1SD1         A3SD17
     C                   MOVE      A1DE1         A3DE17
     C     8             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL18
     C                   MOVE      A1SD1         A3SD18
     C                   MOVE      A1DE1         A3DE18
     C*
     C* * CLEAR DATA STRUCTURE FOR NEXT CLASS CODES
     C     CN            DOWGT     0
     C     CN            OCCUR     DSCLAS
     C                   CLEAR                   DSCLAS
     C     CN            SUB       1             CN
     C                   ENDDO
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR32
     C                   WRITE     QHEAD32
     C                   WRITE     QCLAS3
     C                   WRITE     PAGE32
     C*
     C                   WRITE     QNPG3
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   MOVE      0             N
     C                   MOVE      '1'           WPRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  EPSTAT    IFNE 'I'
     C*
     C     EPKY1         READE     PEP01                                  55
     C                   ENDDO
     C*
     C     CN            IFGT      0
     C* * * PRINT THIS PAGE
     C     1             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL11
     C                   MOVE      A1SD1         A3SD11
     C                   MOVE      A1DE1         A3DE11
     C     2             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL12
     C                   MOVE      A1SD1         A3SD12
     C                   MOVE      A1DE1         A3DE12
     C     3             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL13
     C                   MOVE      A1SD1         A3SD13
     C                   MOVE      A1DE1         A3DE13
     C     4             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL14
     C                   MOVE      A1SD1         A3SD14
     C                   MOVE      A1DE1         A3DE14
     C     5             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL15
     C                   MOVE      A1SD1         A3SD15
     C                   MOVE      A1DE1         A3DE15
     C     6             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL16
     C                   MOVE      A1SD1         A3SD16
     C                   MOVE      A1DE1         A3DE16
     C     7             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL17
     C                   MOVE      A1SD1         A3SD17
     C                   MOVE      A1DE1         A3DE17
     C     8             OCCUR     DSCLAS
     C                   MOVE      A1CL1         A3CL18
     C                   MOVE      A1SD1         A3SD18
     C                   MOVE      A1DE1         A3DE18
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR32
     C                   WRITE     QHEAD32
     C                   WRITE     QCLAS3
     C                   WRITE     PAGE32
     C*
     C                   WRITE     QNPG3
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   MOVE      '1'           WPRT
     C*
     C                   ENDIF
     C*
     C     WPRT          IFEQ      '0'
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR32
     C                   WRITE     QHEAD32
     C                   WRITE     QCLAS3
     C                   WRITE     PAGE32
     C                   WRITE     QNPG3
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
     C*
     C* * PAGE 3
     C* EMPLOYER # AND DIVISION
     C                   CLEAR                   A3MEM3
     C                   MOVEL     A3MEM#        A3MEM3
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   MOVEL     A3PFDT        A3FDT3
     C                   MOVEL     A3PTDT        A3TDT3
     C* FOOTER INFORMATION
     C                   MOVEL     A3AD31        A3A31
     C                   MOVEL     A3AD32        A3A32
     C                   MOVEL     A3AD33        A3A33
     C                   MOVEL     A3FAX2        A3FAX3
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR33
     C                   WRITE     QHEAD33
     C                   WRITE     FOOT3
     C*
     C                   WRITE     QNPG3
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C* * PAGE 4
     C* EMPLOYER # AND DIVISION
     C                   CLEAR                   A3MEM4
     C                   MOVEL     A3MEM#        A3MEM4
     C* POLICY PERIOD FROM DATE - TO DATE
     C                   MOVEL     A3PFDT        A3FDT4
     C                   MOVEL     A3PTDT        A3TDT4
     C*
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QOVR43
     C                   WRITE     QHEAD43
     C                   WRITE     PAGE43
     C                   WRITE     FOOT3
     C                   WRITE     QNPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C*
     C                   ENDSR
     C*====================================================================
     C* Get employer contact information
     C*====================================================================
     C     GTCTCT        BEGSR
     C*
     C                   CLEAR                   A1CNAM
     C*
     C     ECKY1         CHAIN     PEC01                              60
     C     *IN60         DOWEQ     *OFF
     C     ECPCNT        ANDNE     '3'
     C     ECKY1         READE     PEC01                                  60
     C                   ENDDO
     C*
     C* If contact name found
     C     *IN60         IFEQ      '0'
     C     ECPCNT        ANDEQ     '3'
     C*
     C                   MOVEL     ECFSN         A1CNAM
     C*
     C     ECMID         IFNE      *BLANKS
     C     A1CNAM        CAT       ECMID:1       A1CNAM
     C                   CAT       '.':0         A1CNAM
     C                   ENDIF
     C*
     C     A1CNAM        CAT       ECLSN:1       A1CNAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  A1CNAM        A2CNAM
     C                   MOVEL(P)  A1CNAM        A3CNAM
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* * SUBROUTINE FOR DATE IN MONTH DD, YEAR FORMAT
     C*****************************************************************
     C     SRDATE        BEGSR
     C*
     C                   CLEAR                   WDATE
     C                   CLEAR                   WDATE1
     C                   CLEAR                   Q1MMDD
     C*
     C     Q1MON         IFNE      *BLANKS
     C                   MOVEL     Q1MON         Q1DDT             7            UPPER CASE DATE
     C                   ELSE
     C                   MOVEL     Q1MMM         Q1DDT                          LOWER CASE DATE
     C                   ENDIF
     C*
     C     1             SUBST     Q1DDT:1       WCENT             1
     C     2             SUBST     Q1DDT:2       WYR               2
     C     2             SUBST     Q1DDT:4       WMON              2
     C     2             SUBST     Q1DDT:6       WDAY              2
     C*
     C     WCENT         IFEQ      '1'
     C                   MOVEL     '20'          WCYR              4
     C                   ELSE
     C                   MOVEL     '19'          WCYR
     C                   ENDIF
     C*
     C                   MOVE      WYR           WCYR
     C                   MOVEL     WCYR          WNCYR             4 0
     C     WNCYR         SUB       1             WNPYR             4 0
     C*
     C                   MOVEL     WMON          MN                2 0
     C*
     C     Q1MMM         IFNE      *BLANKS
     C     XXMMM(MN)     CAT       WDAY:1        WDATE1           19
     C     WDATE1        CAT       ',':0         WDATE1
     C     WDATE1        CAT       WCYR:1        WDATE1
     C*
     C     XXMMM(MN)     CAT       WDAY:1        Q1MMDD           12
     C*
     C                   ELSE
     C     XXMON(MN)     CAT       WDAY:1        WDATE            19
     C     WDATE         CAT       ',':0         WDATE
     C     WDATE         CAT       WCYR:1        WDATE
     C     XXMON(MN)     CAT       WDAY:1        Q1MMDD           12
     C*
     C                   ENDIF
     C*
     C                   CLEAR                   Q1MON
     C                   CLEAR                   Q1MMM
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Get information needed for ALL forms
     C*====================================================================
     C     GETINF        BEGSR
     C*
     C     *LIKE         DEFINE    *IN60         USEA3
     C*
     C                   MOVE      *IN60         SVIN60
     C*
     C*  Read from Billing Address file
     C                   MOVE      *OFF          USEA3
     C     A3KY          CHAIN     PA301                              60
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C                   MOVE      *ON           USEA3
     C                   ENDIF
     C*
     C* Gather address information
     C                   Z-ADD     2             A1                2 0
     C                   CLEAR                   AD
     C*
     C                   SELECT
     C*
     C*  If Billing Address used
     C     USEA3         WHENEQ    *ON
     C                   ADD       1             A1
     C                   MOVEL(P)  A3ADR1        AD(A1)
     C*
     C     A3ADR2        IFNE      *BLANKS
     C                   ADD       1             A1
     C                   MOVEL(P)  A3ADR2        AD(A1)
     C                   ENDIF
     C*
     C                   ADD       1             A1
     C     A3CTY         CAT(P)    ',':0         AD(A1)
     C                   CAT       A3ST:1        AD(A1)
     C                   CAT       A3ZIP1:2      AD(A1)
     C     A3ZIP2        IFNE      *ZEROS
     C                   CAT       '-':0         AD(A1)
     C                   CAT       A3ZIP2:0      AD(A1)
     C                   ENDIF
     C*
     C*  If Mailing Address used
     C     EMMPFL        WHENEQ    'M'
     C                   ADD       1             A1
     C                   MOVEL(P)  EMMAD1        AD(A1)
     C*
     C     EMMAD2        IFNE      *BLANKS
     C                   ADD       1             A1
     C                   MOVEL(P)  EMMAD2        AD(A1)
     C                   ENDIF
     C*
     C                   ADD       1             A1
     C                   MOVEL     EMMZIP        WKZIP5            5
     C                   MOVE      EMMZIP        WKZIP4            4
     C     EMMCTY        CAT(P)    ',':0         AD(A1)
     C                   CAT       EMMST:1       AD(A1)
     C                   CAT       WKZIP5:2      AD(A1)
     C     WKZIP4        IFNE      *ZEROS
     C                   CAT       '-':0         AD(A1)
     C                   CAT       WKZIP4:0      AD(A1)
     C                   ENDIF
     C*
     C*  If Physical Address used
     C     EMMPFL        WHENEQ    'P'
     C                   ADD       1             A1
     C                   MOVEL(P)  EMPAD1        AD(A1)
     C*
     C     EMPAD2        IFNE      *BLANKS
     C                   ADD       1             A1
     C                   MOVEL(P)  EMPAD2        AD(A1)
     C                   ENDIF
     C*
     C                   ADD       1             A1
     C                   MOVEL     EMPZIP        WKZIP5            5
     C                   MOVE      EMPZIP        WKZIP4            4
     C     EMPCTY        CAT(P)    ',':0         AD(A1)
     C                   CAT       EMPST:1       AD(A1)
     C                   CAT       WKZIP5:2      AD(A1)
     C     WKZIP4        IFNE      *ZEROS
     C                   CAT       '-':0         AD(A1)
     C                   CAT       WKZIP4:0      AD(A1)
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   MOVEA     AD            AD1
     C                   MOVEA     AD            AD2
     C                   MOVEA     AD            AD3
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR
     C*
     C*
     C*====================================================================
     C* Address lines setup
     C*====================================================================
     C     ADDLIN        BEGSR
     C* The following are the positioning variables for the address lines.
     C*  There is a new request to update the forms.  We should allow for
     C*  the user to select the positioning at that point.  INADV/H will be
     C*  the user defined variable.  At this point, it will always be zero
     C*  and therefore the defaults we assigned are used.  The IN*
     C*  variables will be used only for the cover letter.
     C     *LIKE         DEFINE    ADH           INADH
     C     *LIKE         DEFINE    ADV1          INADV
     C     *LIKE         DEFINE    ADV1          ADSPC
     C     *LIKE         DEFINE    ADV1          ADSPCH
     C     *LIKE         DEFINE    ADH           WDWSID
     C     *LIKE         DEFINE    ADV1          WDWTOP
     C     *LIKE         DEFINE    ADV1          WDWLEN
     C*
     C                   Z-ADD     2.0           WDWTOP
     C                   Z-ADD     1.0           WDWSID
     C                   Z-ADD     1.75          WDWLEN
     C                   Z-ADD     0.167         ADSPC
     C     ADSPC         DIV       2             ADSPCH
     C*
     C* The client was in need of an address only page to print for the
     C*  envelope window.  This may not be needed in the future.
     C     *LIKE         DEFINE    *IN60         USEADP
     C                   MOVE      *ON           USEADP
     C*
     C     WDWSID        ADD       0.125         ADH
     C     WDWLEN        SUB       0.835         ADV1
     C                   DIV       2             ADV1
     C                   ADD       WDWTOP        ADV1
     C*
     C*
     C     ADV1          ADD       ADSPC         ADV2
     C     ADV2          ADD       ADSPC         ADV3
     C     ADV3          ADD       ADSPC         ADV4
     C     ADV4          ADD       ADSPC         ADV5
     C*
     C                   SELECT
     C     USEADP        WHENEQ    *ON
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   WRITE     QEMPAD
     C                   WRITE     NEWPG
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDSL
     C*
     C* The following is used for address lines on the cover page.
     C                   SELECT
     C     INADH         WHENEQ    *ZERO
     C                   Z-ADD     0.625         ADH
     C                   OTHER
     C                   Z-ADD     INADH         ADH
     C                   ENDSL
     C*
     C                   SELECT
     C     INADV         WHENEQ    *ZERO
     C     2.0           ADD       ADSPCH        ADV1
     C                   OTHER
     C                   Z-ADD     INADV         ADV1
     C                   ENDSL
     C*
     C     AD(5)         IFEQ      *BLANKS
     C                   ADD       ADSPCH        ADV1
     C     AD(4)         IFEQ      *BLANKS
     C                   ADD       ADSPCH        ADV1
     C     AD(3)         IFEQ      *BLANKS
     C                   ADD       ADSPCH        ADV1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     ADV1          ADD       ADSPC         ADV2
     C     ADV2          ADD       ADSPC         ADV3
     C     ADV3          ADD       ADSPC         ADV4
     C     ADV4          ADD       ADSPC         ADV5
     C*
     C                   ENDSR
     C*====================================================================
     C     SROVFW        BEGSR
     C*
     C     *IN97         IFEQ      *ON
     C                   Z-ADD     17            XXLINE
     C                   ADD       1             PAGE
     C                   ADD       1             PAGE#0
     C                   ADD       1             PAGE#2
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLIN
     C                   Write     HEAD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     PAGLI2
     C                   Write     HEAD2
     c                   WRITE     HDLOGO2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
      *
     C     *IN99         IFEQ      *ON
     C                   ADD       1             XXLINE
     C                   ENDIF
      *
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHPHD
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
      *
     C     CRTPDF        IFEQ      'Y'
     C     XXPRT2        ANDEQ     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     OTHPH2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   ENDIF
      *
     C                   ADD       4             XXLINE
     C                   MOVE      *OFF          *IN97
     C                   ENDIF
     C*
     C                   ENDSR
     c*====================================================================
     c     PrintAddIns   begsr
     c
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     othpdt
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************

     c                   if        xxonm2 <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     othpdtnm2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c                   endif                                                  xxonm2 <> *blank

     c                   if        not *in21
     c                   if        xxofdnm <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     othpdtfd
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   endif                                                  xxofdnm <> *blank
     C                   if        xxofdnm2 <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     othpdtfd2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   endif                                                  xxofdnm2 <> *blank
     C                   if        xxoad1 <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     othpdtad
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   endif                                                  xxoad1 <> *blank
     C                   endif                                                  not *in21
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     othpdtex
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c
     C     crtpdf        ifeq      'Y'
     C     xxprt2        andeq     'OPEN'
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     othpd2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   if        xxonm2 <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     othpd2nm2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     C                   endif                                                  xxonm2 <> *blank
     c
     C                   IF        not *in21
     C                   IF        xxofdnm <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     C                   Write     othpd2fd
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c                   endif                                                  xxofdnm <> *blank
     c                   if        xxofdnm2 <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     othpd2fd2
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c                   endif                                                  xxofdnm2 <> *blank
     c                   if        xxoad1 <> *blank
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     othpd2ad
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c                   endif                                                  xxoad1 <> *blank
     c                   endif                                                  not *in21
       //*** Beg Add *** 10/28/20 *********************************************************
       If isUseSpoolOut = *on and flgNoOutput <> 'Y';
       //*** End Add *** 10/28/20 *********************************************************
     c                   Write     othpd2ex
       //*** Beg Add *** 10/28/20 *********************************************************
       Endif;
       //*** End Add *** 10/28/20 *********************************************************
     c                   endif                                                  crtpdf = 'Y'

     c                   endsr                                                  PrintAddIns
     C*
     C/COPY WADTTRKCSR
     O*
      *=============================================================
      * ApplyRateSplit - Check option flag to apply rate split
      *=============================================================
     p ApplyRateSplit  b
     d ApplyRateSplit  pi              n
     d   inState                      2a
     d   inDate                       7  0
      *
     d xxState         s              2a
     d xxSpltDt        s              7  0
     d xxPrcRate       s               n
      *
     C     rtky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxstate
     C                   kfld                    xxSpltDt          7 0
     C*
     C                   eval      xxPrcRate = *on
     C                   eval      xxState = inState
     C                   eval      xxSpltDt = inDate
     C     rtky          chain     prt0x
     C                   if        %found and rtopt = 'Y'
     C                   eval      xxPrcRate = *off
     C                   endif
     C*
     C                   return    xxPrcRate
     C*
     p ApplyRateSplit  e
      *===============================================================
      * Apply discount if not A-rated, not L&H, not maritime exposure
      *===============================================================
     p isDiscounted    b
     d isDiscounted    pi              n
     d  state                         2    const
     d  class                         5  0 const
     d  spec                          1    const
      /free
       Select;
       When ELstat = 'Y' and xxArfl = 'Y';
          Return *off;
       When state = 'TN' and xxTNPyFnd = *off and pDat(y) >= 1140701
        or  state = 'OK' and xxOKPyFnd = *off and pDat(y) >= 1140201;
       // Don't discount LH exposure or maritime
          If spec = 'F' or
           class = 6703 or
           class = 7047 or
           class = 7050 or
           class = 7099 or
           class = 7152 or
           class = 7337 or
           class = 7398 or
           class = 8738 or
           class = 8815;
             Return *off;
          Else;
             Return *on;
          Endif;
       Other;
          Return *off;
       Endsl;
      /end-free
     p isDiscounted    e
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0 const
     d   inFND                        3  0 const
     d   inFYR                        3  0 const
     d   inEMP#                       9  0 const
     d   inDIV                        5  0 const
     d   inDate                       7  0
     d   inType                       1    options(*nopass)
     d   inClas                       5  0 options(*nopass)
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
       //*** Beg Add *** 10/28/20 ******************************************
     P*=====================================================================
     P* dow - returns numeric value for day of the week
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 1 numeric  (1=Sunday ... 7=Saturday)
     P*=====================================================================
     PDayOfWeek        B                   export
     DDayOfWeek        PI            10i 0
     D intDate                        8  0 Const
     D
     D baseDate        s               d   static inz(d'1900-01-01')
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
     D nDays           s              7  0
     D answer          s              7  0
     D remain          s              1  0
     C
     C                   eval      isoDate1 = CvtNumToDate(intDate)
     C                   if        isoDate1 <> isoDate2
     C                   if        isoDate1 > baseDate
     C     isoDate1      subdur    baseDate      nDays:*days
     C     nDays         div       7             answer
     C                   mvr                     remain
     C                   add       1             remain
     C                   endif
     C                   endif
     C*
     C                   return    remain
     PDayOfWeek        E
     P*=====================================================================
     P* CvtNumToDate - convert number to date data type
     P*  Parameter 1 - 8 numeric date
     P*  Returns - date data type (date will be 01/01/0001 if invalid)
     P*=====================================================================
     PCvtNumToDate     B                   Export
     DCvtNumToDate     PI              d
     D intDate8                       8  0 Const
     D
     D date1           s               d
     D date2           s               d   datfmt(*ISO)
     D year            s              4  0
     C*
     C                   if        intDate8 < 1000000
     C     *mdy          test(d)                 intDate8               99
     C  n99*mdy          move      intDate8      date1
     C                   else
     C     *usa          test(d)                 intDate8               99
     C  n99*usa          move      intDate8      date1
     C                   If        Not *in99
     C     *usa          move      intDate8      date1
     C                   Else
     C     *iso          test(d)                 intDate8               99
     C  n99*iso          move      intDate8      date2
     C                   move      date2         date1
     C                   EndIf
     C                   endif
     C
     C  n99              extrct    date1:*y      year
     C
     C                   if        *in99 or year < 1900
     C     *cymd         test(d)                 intDate8               99
     C  n99*cymd         move      intDate8      date1
     C                   endif
     C
     C                   return    date1
     PCvtNumToDate     E
       //*** End Add *** 10/28/20 ******************************************
**  XXMON
JANUARY
FEBRUARY
MARCH
APRIL
MAY
JUNE
JULY
AUGUST
SEPTEMBER
OCTOBER
NOVEMBER
DECEMBER
**  XXMMM
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
** Months
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
