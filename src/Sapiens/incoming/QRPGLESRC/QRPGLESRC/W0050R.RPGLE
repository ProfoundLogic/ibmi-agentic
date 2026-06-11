     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry  dftactgrp(*no) actgrp('QILE')
     H DFTACTGRP(*NO) ACTGRP(*Caller) BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0050R - WORK WITH EMPLOYERS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                EMPLOYERS BY DBA NAME, EMP #, FEDERAL NAME OR  *
     H*                STATE EMP #.  EACH EMPLOYER CAN BE ACCESSED    *
     H*                FOR UPDATE, DISPLAY OR SELECTION DEPENDING UPON*
     H*                THE CALLING PROGRAM.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0050R02 - EMPLOYER MASTER INFORMATION (OPT 2/4/5/6)       *
     H*    W1057R   - OWNER/OFFICER INFORMATION (OPT 7)               *
     H*    W0051R   - WORK WITH POLICY YEARS (OPT 8)                  *
     H*    W0553R   - AGENT HISTORY (OPT 9)                           *
     H*    W0076R   - ENTITY/LOCATION (OPT 10)                        *
     H*    W1050R   - DEPARTMENTS (OPT 11)                            *
     H*    SC010R   - DARTS (OPT 14)                                  *
     H*    W0050R15 - OWNER/OFFICER INFORMATION (OPT 15)              *
     H*    W0050R17 - TRANSFER/DELETE QUOTES (OPT 17)                 *
     H*             - STATE CERT (OPT 58)                             *
     H*    W0168R   - CX HISTORY (OPT 62)                             *
     H*    W0070R   - CLAIMS INQUIRY (OPT 80)                         *
     H*    W0051R1  - MID TERM ENDORSE (OPT 55)                       *
     H*                                                               *
     H*  CREATED: 03/03/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
    * 11/08/13 C:191 Redmine: 2210                                  *
    *                Modified to add wrapper call for employer      *
    *                search.                                        *
    *                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H*                                                               *
     H* 09/27/12 C:283 Support WA099 80572                            *
     H*                Lock the Employer for any updates in case      *
     H*                FastQuote lock is ON.                          *
     H*                                                               *
     H* 03/23/12 C:180 Support 79400
     H*                Prevent creating a dup division on an
     H*                existing pol.
     H*
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Add opt 93 for HCN Program (CLT139 only)       *
     H*                * * Clean-up comment lines for OFD             *
     H*                                                               *
     H*  09/09/10 C:190 PRISS :159                                    *
     H*                 WHILE TRANSFERRING THE POLICY, IF  F12 IS     *
     H*                 PRESSED ON THE AGENT HISTORY EDIT WINDOW      *
     H*                 THEN TRANSFER CONFIRMATION SCREEN SHOULD NOT  *
     H*                 BE DISPLAYED                                  *
     H*                                                               *
     H*  01/04/10 C:190 PROJECT: 25WRE                                *
     H*                 BACK TO BASE                                  *
     H*                 ADDED OPTION 90-CAMPUS CODES FOR CLT#25       *
     H*                                                               *
     H* 10/05/09 C:062 PROJECT: 139WMJ                                *
     H*                ADD TRIAGE FOR CLIENT 139.                     *
     H*                                                               *
     H* 07/08/09 C:62  Project: 133WHN                                *
     H*                 Add search to quote type subset.              *
     H*                                                               *
     H* 06/29/09 C:191 WC1806                                         *
     H*                EFT - DISABLE OPTION 26 - DIRECT DEBIT INFO    *
     H*                      IF WC1806 IS INSTALLED, ELSE CALL        *
     H*                      OPTION 70                                *
     H*                    - ADD OPTION 70 - EFT ACCOUNT INFO         *
     H*                    - OPT 26 REMOVED FROM COMPILE TIME ARRAY   *
     H*                                                               *
     H* 06/01/09  C:62  Project: 133WGX                               *
     H*                 Add FQ/WC flag processing.                    *
     H*                                                               *
     H* 05/28/09  C:33  Project: 133WGV                               *
     H*                 Use a new file for validation.                *
     H*                                                               *
     H* 05/14/09 C:136 PROJECT: 93WHZ                                 *
     H*                BACK TO BASE - IMPLEMENT CLIENT SPECIFIC OPTION*
     H*                91                                             *
     H*                                                               *
     H* 02/25/09 C:189 67339/ 71WJN                                   *
     H*                W/W Diary item not showing 'Q' for QUOTE       *
     H*                                                               *
     H* 02/18/09 C:220 SUPPORT: 67179                                 *
     H*                -Issue with WwEmployer headings                *
     H*                'State Emplr #' need to replace 'State Risk    *
     H*                 ID #'                                         *
     H*                                                               *
     H* 11/14/08 C:220 Project 71WJN                                  *
     H*                 Add opt to complete diary in DARTS            *
     H*                                                               *
     H* 08/21/08 C:191 Project 102WEP                                 *
     H*                 The mailing state will be displayed in View 1 *
     H*                 unless blank, then the physical addr. state   *
     H*                 will be used.                                 *
     H*                                                               *
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                ENABLE OPTION 26 FOR ACH IF ONLINE PROCESSING. *
     H*                                                               *
     H* 10/19/07 C:193 PROJECT: WC1561                                *
     H*                ADD A NEW QUOTE TRACKING TYPE                  *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491                                *
     H*                ADD OPTION TO CALL RETURN FUNDS                *
     H*                                                               *
     H* 08/22/07  C:33  Project: 71WHI (Back to Base)                 *
     H*        04/05/02 PROJECT 71WEK                                 *
     H*                 New Quote Type 'S'=Closed                     *
     H*                                                                   *
     H* 07/09/07  C:189  Support: WA045 Seq: 54503                        *
     H*          Quote to Live transfer should not be editing master level*
     H*          information on policies that were  renewed from LIVE     *
     H*          Also The Validations of SIC,UNEMPLOYMENT, FEIN Taking    *
     H*          Place in W0050R17 so  no need to validate again       *
     H*                                                               *
     H* 05/22/07 C:191 SUPPORT: 53093                                 *
     H*                - Include second DBA and Fed name line in sub  *
     H*                  setting                                      *
     H*                                                               *
     H* 05/21/07 C:191 SUPPORT: 57498                                 *
     H*                - Change mode value from U to C while calling  *
     H*                  W0050R15                                     *
     H*                                                               *
     H* 05/02/07 C:132 132WFT                                         *
     H*              - Change @@CALL for an "M" passed in from W4700R *
     H*                to allow F12=Cancel to display. This will be   *
     H*                changed to a "2" to allow update for client    *
     H*                132, so they can modify audit payrolls. This   *
     H*                will be done in PGMSTR subroutine.             *
     H*                                                               *
     H* 12/20/06 C:185 Project: 132WGJ                                *
     H*                Producer's Email Address.                      *
     H*                                                               *
     H* 12/12/06 C:33  Project: WC1299                                *
     H*                Add processing for F12 being returned from     *
     H*                 W0050R17.                                     *
     H*                                                               *
     H* 11/27/06 C:15  Project: 139WFS                                *
     H*                Added code to process the new GIC PEO type 'G' *
     H*                the same as PEO type 'S'.                      *
     H*
     H* 10/30/06 C:61  SEQUENCE 55588 132WFM
     H*                Remove option 92, clients request.
     H*                                                               *
     H* 09/06/06 C:61  PROJECT 132WFM
     H*                Add option 92=MPN Provider
     H*                                                               *
     H* 08/21/06 C:33  Seq#: WA135/52534                              *
     H*                Moved editing for Opt 4=Delete to W0050R04.    *
     H*                                                               *
     H* 08/18/06 C:180 PROJECT: 114WMF                                *
     H*                                                               *
     H* 08/17/06 C:53  Support: 54064                                 *
     H*                Correct option issue.                          *
     H*                                                               *
     H* 07/06/06 C:15  Support: WC099/53478                           *
     H*                Changed the Maint flag in the option table to  *
     H*                'N' for option 17.  This flag will be changed  *
     H*                to 'Y' in the PGMSTR routine if the "quote"    *
     H*                parm = 'Q'.                                    *
     H*                                                               *
     H* 04/07/06 C:15  Support: WC132/52179                           *
     H*                Removed code that loaded options after they    *
     H*                had been removed due to various conditions.    *
     H*                                                               *
     H* 03/06/06 C:168 PROJECT: WC1416                                *
     H*              - Removed edit preventing option 6 if the record *
     H*                is a Renewal Quote                             *
     H*              - Added code to check if flag EMRDFL (other div) *
     H*                = *blanks, to allow update to a division       *
     H*                created through Quote.                         *
     H*              - Changed all update options (2,7,9,etc) to only *
     H*                be a display option if fields EMRQFL='Y' and   *
     H*                EMRDFL=*blanks, all others variations will now *
     H*                allow the user to update the employer/division *
     H*                                                               *
     H* 02/22/06 C:15  Support: WC065/51122                           *
     H*                Added code so the 8/2/05 mod will not be       *
     H*                executed in search mode.                       *
     H*                                                               *
     H* 01/05/06 C:53  SUPPORT # 50587                                *
     H*                Qualify F6 restriction on the quote flag       *
     H*                                                               *
     H* 10/12/05 C:132 SUPPORT # 49183  WA099                         *
     H*                Prevent Option 55 and option 58 when in Display*
     H*                mode - reset flags at bottom for these options *
     H*                                                               *
     H* 08/05/05 C:132 PROJECT: SUPP# 47865                           *
     H*                Correct the READE at the bottom of DO loop.    *
     H*                DO NOT ALLOW A SUMMARIZED DIVISION TO BE       *
     H*                DELETED WHEN THE STATE REPORTING DIVISION HAS  *
     H*                ANY BILLED A/R.                                *
     H*                                                               *
     H* 08/02/05 C:53  SUPPORT # 48340                                *
     H*                Prevent F6 for FUNDS w/PEO = S and Emp 1 div 0 *
     H*                already existing.                              *
     H*                                                               *
     H* 06/23/05 C:33  PROJECT: WC1287/135WCB                         *
     H*                Moved edits to W0050R17.                       *
     H*                                                               *
     H* 06/09/05 C:61  PROJECT 114QIK                                 *
     H*                Prevent access to options (2, 4, 6, 17) if     *
     H*                if record locked by FASTQUOTE.  DISPLAY mode   *
     H*                only if record locked by FASTQUOTE for         *
     H*                options (7, 8, 9, 10, 13, 15, 26, 42).         *
     H*                                                               *
     H* 04/27/05 C:132 PROJECT: WC1285/135WCY                         *
     H*                Allow entry of Named Insureds (Opt 12) from    *
     H*                the Quote Environment.                         *
     H*                This completes the 04/18/05 changes.           *
     H*                                                               *
     H* 04/18/05 C:09  PROJECT: WC1285/135WCY                         *
     H*                Allow entry of Named Insureds (Opt 12) from    *
     H*                the Quote Environment.                         *
     H*                                                               *
     H* 03/31/05 C:53  PROJECT: WC1287/135WCB                         *
     H*                Added processing for NAICS code requirements
     H*                                                               *
     H* 12/01/04 C:65  PROJECT: SUPP#43540                            *
     H*                DO NOT ALLOW A SUMMARIZED DIVISION TO BE       *
     H*                DELETED WHEN THE STATE REPORTING DIVISION HAS  *
     H*                ANY BILLED A/R.                                *
     H*                                                               *
     H* 11/18/04 C:33  Project: 132WCB                                *
     H*                Add processing for 'S' quote type              *
     H*                                                               *
     H* 06/22/04 C:61  PROJECT 114WHL                                 *
     H*                Add new option "42=New Business Tracking".     *
     H*                                                               *
     H* 06/03/04 C:136 PROJECT: WC1208                                *
     H*                Add option to transfer all divisions of an     *
     H*                employer at once.                              *
     H*                                                               *
     H* 01/13/04 C:136 PROJECT: WC1116                                *
     H*                Change Opt 26 to call W/W Direct Debit         *
     H*                Information.                                   *
     H*                                                               *
     H* 10/10/03 C:33  PROJECT: WC1017                                *
     H*               -Add new quote types - RENEWAL PENDING          *
     H*                                      RENEWAL QUOTE            *
     H*               -Move edits to W0050R17                         *
     H*                                                               *
     H* 10/10/03 C:136 PROJECT: WC1017                                *
     H*                Add subset options for:                        *
     H*                 (1) New business vs. renewal                  *
     H*                 (2) Rating state (primary and/or incidental)  *
     H*                 (3) Underwriting staff code                   *
     H*                 (4) Mailing address                           *
     H*                 (5) Quote type                                *
     H*                Inhibit opt 2 and 6 if in Quote and policy     *
     H*                is a renewal quote.                            *
     H*                Add search function to subset.                 *
     H*                                                               *
     H* 09/30/03 C:61  SUPPORT WA129 39275 WC1119                     *
     H*                Correct Unemployment # edit.                   *
     H*                                                               *
     H* 08/12/03 C:079 Support: WA126 #38462                          *
     H*                On delete only check for MC claims if not      *
     H*                in Quote                                       *
     H*                                                               *
     H* 07/16/03 C:136 Support: WA129 #38008                          *
     H*                Edit for state unemployment # invoked          *
     H*                incorrectly.                                   *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116                                *
     H*                Add option 26 - Direct Debit Info              *
     H*                                                               *
     H* 03/04/03 C:132 PROJECT WC1119                                 *
     H*                For Option 17 Transfer Quote to live, check    *
     H*                unemployment # flag in WDRPP/RRURFL to see if  *
     H*                state uemployment # is required. If so, issue  *
     H*                error message if missing and do not allow the  *
     H*                transfer to live.                              *
     H*                                                               *
     H* 06/05/02 C:136 SUPPORT: WA099 32883                           *
     H*                As a result of WC1054, WAQOL is opened but not *
     H*                closed if SNGEMP isn't turned on.              *
     H*                                                               *
     H* 04/17/02 C:136 WC577                                          *
     H*                Refer to Support item 26692. Adds Info Message *
     H*                that Entity/Location records exist for employer*
     H*                when attempting to delete.                     *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Add option 13=Contacts.                        *
     H*                                                               *
     H* 12/26/01 C:90  PR0JECT: WC1054                                *
     H*                DO NOT ALLOW QUOTE TRANSFER IF THE NEXT AVAIL. *
     H*                CARRIER # FALLS OUTSIDE THE VALID CARRIER #    *
     H*                RANGE.                                         *
     H*                                                               *
     H* 06/08/01 C:61  PROJECT WC1034                                 *
     H*                Add Batch Lockout, OPT17-Transfer Quote policy.*
     H*                                                               *
     H* 01/16/01 C:61  PROJECT WC996                                  *
     H*                Add option "12=Named insureds", avail if not   *
     H*                QUOTE and flag SWCPF/WCFL16 = 'Y'.             *
     H*                                                               *
     H* 11/01/00  C:33  PROJECT: WC1008                               *
     H*                 USE TRANSFER TYPE (QTXFER) IF VALUED.         *
     H*                                                               *
     H* 06/09/00  C:33  PROJECT: WC696                                *
     H*                 ADD OPT 60=A/R HISTORY                        *
     H*                                                               *
     H* 04/12/00  C:53  PROJECT: MC189                                *
     H*                 ADD EDIT TO GIVE AN ERROR MESSAGE AND PREVENT *
     H*                 EMPLOYER DELETION IF WCMC CLAIMS EXIST        *
     H*                                                               *
     H* 03/24/00  C:33  SEQ#: WA048/22223                             *
     H*                 EDIT EMPLOYER AGENT FOR EFFECTIVENESS.        *
     H*                                                               *
     H* 02/09/00  C:33  PROJECT: WC919                                *
     H*                 ADD ADDITIONAL EDITS FOR OPT 17=TRANSFER.     *
     H*                                                               *
     H* 01/10/00  C:33  SEQ#: WA091/21311                             *
     H*                 DO NOT EDIT AGAINST A/R FOR OPT 4=DELETE.     *
     H*                                                               *
     H* 01/05/00  C:09  PROJECT: SUPPORT (CLT #109)                   *
     H*                 OPT58-STATE CERTIFICATES, ENSURE OUTPUT GOES  *
     H*                 TO QUEUE OF USER REQUESTING THE CERTIFICATE   *
     H*                                                               *
     H* 01/03/00  C:33  SEQ#: WA095/21201                             *
     H*                 IF THE USER IS RESTRICTED AND THE GROUP IS    *
     H*                 PASSED IN, USE THAT GROUP AND DO NOT CHECK FOR*
     H*                 ANY OTHER VALID GROUPS.                       *
     H*                                                               *
     H* 12/21/99  C:35   PROJECT: SUPPORT WA101 #21089                *
     H*                  WHEN DELETING, DO NOT CHECK FOR NCCI RECORDS *
     H*                  OR NON-TRANSFERRED A/R BATCH RECORDS IF IN   *
     H*                  QUOTE.                                       *
     H*                                                               *
     H* 09/21/99 C:33  PROJECT: WC577                                 *
     H*                OVERRIDE CORRECT FILE.                         *
     H*                                                               *
     H* 08/26/99 C:33  PROJECT: WC577                                 *
     H*                DEFAULT @@QTE TO 'W' IF NOT PASSED.            *
     H*                                                               *
     H* 08/11/99 C:33  PROJECT: WC577                                 *
     H*                ADD FEIN SEARCH.                               *
     H*                                                               *
     H* 07/07/99 C:113 PROJECT: WC895                                 *
     H*                Per JJ, comment out copy option.  This is not  *
     H*                going on base tape at this time.               *
     H*                                                               *
     H* 06/23/99 C:113 PROJECT: WC895                                 *
     H*                Add copy option in Quote environment.          *
     H*                Use subroutine UPDOFA for option & function    *
     H*                key changes.                                   *
     H*                                                               *
     H*  06/02/99 C:61 PROJECT 101AT                                  *
     H*                Add code for Group / Employer restriction.     *
     H*                                                               *
     H*  05/10/99 C:33 SEQ#: WC051/17998                              *
     H*                ALLOW FOR DIVISION RESTRICTION.                *
     H*                                                               *
     H*  03/30/99 C:33 PROJECT: WC577                                 *
     H*                RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*                                                               *
     H*  12/22/98 C:78 PROJECT: CM027                                 *
     H*                OPT55: MID-TERM ENDORSEMENTS SHOULD NOT BE     *
     H*                AVAILABLE FROM QUOTE.                          *
     H*                                                               *
     H*  08/11/98 C:33 SEQ#: WA060/12968                              *
     H*                IF THE CALLED TO DISPLAY A SINGLE EMPLOYER, DO *
     H*                NOT OVERRITE SCDBRR.                           *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                ADD PARAMETER ##QTE WHEN CALLING W0076R, W1057R*
     H*                FOR QUOTE MODE.                                *
     H*                                                               *
     H*  08/05/98 C:33 SEQ#: WA091/12885                              *
     H*                ALLOW EMPLOYER TO ALSO BE TRANSFERED IF TYPE IS*
     H*                BOUND OR BOUND W/MONEY.                        *
     H*                                                               *
     H*  08/04/98 C:90 PROJECT: WC577                                 *
     H*                EMPLOYER SEARCH - ALLOW OPTION 8=DETAIL INQUIRY*
     H*                                                               *
     H*  07/15/98 C:33 PROJECT: WC577                                 *
     H*                READ ALL RECORDS IN SUBMISSION HISTORY FILE AND*
     H*                A/R BATCH FILE FOR EMPLOYER TO BE DELETED      *
     H*                                                               *
     H*  06/30/98 C:57 PROJECT: SUPPORT 12278                         *
     H*                CLEAR THE *LDA AFTER PROCESSING OPTION 55.     *
     H*                                                               *
     H*  06/26/98 C:33 PROJECT: WC849                                 *
     H*               -IF THE ADMIN/DARTS CONTROL FLAG REQUIRES THE   *
     H*                ENTRY OF THE FEIN, DO NOT ALLOW A QUOTED EMP   *
     H*                TO BE TRANSFERRED TO LIVE WITHOUT IT.          *
     H*               -APPLY SAME EDIT FOR SIC CODE.                  *
     H*                                                               *
     H*  06/02/98 C:33 SEQ#: WA095/11832                              *
     H*                CHANGE SCPSTA TO SCPST9 FOR REPOSITIONING.     *
     H*                                                               *
     H*  03/23/98 C:85 PROJECT: WC773                                 *
     H*                DETERMINE IF CLIENT IS ADMIN-ONLY,             *
     H*                OR CLAIMS-ONLY.                                *
     H*                                                               *
     H*  03/22/98  C:33  PROJECT: WC395                               *
     H*                  ADD QUOTE STATUS' - BOUND, BOUND W/MONEY,    *
     H*                  TRANSFER                                     *
     H*                                                               *
     H*  03/09/98  C:33  PROJECT: WC577                               *
     H*                  ALLOW DELETION IF A/R TRANS DATE IS IN       *
     H*                  CURRENT MONTH                                *
     H*                                                               *
     H*  03/07/98  C:33  PROJECT: WC577   WA065/10506                 *
     H*                 -ONLY DISPLAY OPT 17 MESSAGE IF AUTO ASSIGN   *
     H*                  FLAG IS 'Y'                                  *
     H*                 -USE OVERRIDE FOR TRANSFER PROGRAM            *
     H*                                                               *
     H*  02/24/98  C:33  PROJECT: WC577   WC095/10414                 *
     H*                  ALLOW CORRECT FUNCTIONS TO BE USED WHEN THIS *
     H*                  PROGRAM IS ACCESSED FROM W/W CLAIMS (OPT 80).*
     H*                                                               *
     H*  02/12/98  C:33  PROJECT: WC577                               *
     H*                 -CHECK NCCI SUBMISSION FOR STATUS OF '4'.     *
     H*                 -CHECK FOR NON-TRANSFERRED A/R BATCH RECORDS. *
     H*                                                               *
     H*  01/29/98  C:27  PROJECT: CM027                               *
     H*                  ADD OPT 55 = MID TERM ENDORSEMENTS           *
     H*                                                               *
     H*  01/22/98  C:33  PROJECT: WC577                               *
     H*                  REMOVE OPT 58=STATE CERT AND OPT 62=CX INFO  *
     H*                  IN QUOTE ENVIRONMENT                         *
     H*                                                               *
     H*  01/20/98  C:33  PROJECT: WC577                               *
     H*                  USE DIFFERENT LOGICALS FOR GROUP SUBSETS     *
     H*                                                               *
     H*  12/22/97  C:33  PROJECT: WC577                               *
     H*                 -USE DIFFERENT API FOR EDIT PROCESSOR         *
     H*                 -DISPLAY CONFIRMATION MESSAGE FOR OPT 17      *
     H*                                                               *
     H*  12/16/97  C:33  PROJECT: WC577                               *
     H*                  PHONE # SUBSET NEEDS TO WORK FOR PARTIAL #'S *
     H*                                                               *
     H*  11/18/97  C:33  PROJECT: WC577                               *
     H*                  RECOMPILE DUE TO ADDING ERROR MESSAGE        *
     H*                                                               *
     H*  11/06/97  C:33  PROJECT: WC577                               *
     H*                 -ALLOW REINSTATED QUOTE STATUS                *
     H*                 -ONLY ALLOW OPT 17-TRANSFER TO LIVE IF QUOTE  *
     H*                  TYPE IS 'P' OR 'Q'                           *
     H*                                                               *
     H*  10/30/97  C:33  PROJECT: WC577                               *
     H*                 -REMOVE OPTION(S) IN QUOTE ENVIRONMENT        *
     H*                 -ADD OPT 17-TRANSFER TO LIVE IN QUOTE ENV     *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  20 - Employer DBA Search                                     *
     H*  21 - Fund/Emp#/Div Search                                    *
     H*  22 - Employer Name Search                                    *
     H*  24 - State ID # Search                                       *
     H*  30 - Called as a search                                      *
     H* N30 - Called as Fund Master Maintenance                       *
     H*  31 - Called as a search w/Fyr                                *
     H*  32 - Called as a search w/Entity/Location                    *
     H*  33 - Called with Group                                       *
     H*  34 - Called with Employer                                    *
     H*  35 - Option 17 is invalid when batch is running.             *
     H*  36 - Option 17 not allowed if Unemployment# required,missing *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0050F    CF   E             WORKSTN
     F                                     usropn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PFRCDF)
     FWMEML9    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEM01:DBRCDF)
     FWMEML     IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEM01:DBRCDF2)
     FWMEML8    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PEM01:DBRCDF3)
     FWMEML7    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PEM01:DBRCDF4)
     FWMEML12   IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS5)
     F                                     RENAME(PEM01:DBRCDF5)
     FWMEML1    IF   E           K DISK
     F                                     INFDS(DBFDB)
     F                                     RENAME(PEM01:DBRCDFB)
     FWMEML11   IF   E           K DISK
     F                                     INFDS(DBFD3B)
     F                                     RENAME(PEM01:DBRCDF3B)
     FWMEML10   IF   E           K DISK
     F                                     INFDS(DBFD4B)
     F                                     RENAME(PEM01:DBRCDF4B)
     FWMEML14   IF   E           K DISK
     F                                     INFDS(DBFD5B)
     F                                     RENAME(PEM01:DBRCDF5B)
     FWMEMLQ    IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:PEM01Q)
     FWMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDB)
     FWAQTL7    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMEXL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL0P)
     FWMRSL     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAHL3    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*
      * User parms (subset subroutine)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP04)
      *
      * MPN Provider
     FWMMPL     IF   E           K DISK
     F                                     INFDS(MPFDS)
     FWTQSL1    IF   E           K DISK
     FWTQSP     IF   E             DISK
     F                                     RENAME(PQS01:PQS02)
     FWXRIL1    IF A E           K DISK
     F*
      /COPY SCOPYWW01D
     D*
     D QTET            S              1    DIM(15) CTDATA PERRCD(1)             QUOTE
     D QTED            S             12    DIM(15) ALT(QTET)
     D HD              S             74    DIM(30) CTDATA PERRCD(1)             HEADERS
     D HD139           S             74    DIM(30) CTDATA PERRCD(1)             HEADERS
     D OF              S              1    DIM(50) CTDATA PERRCD(1)             OPTIONS,FUNCTIONS
     D OFD             S             35    DIM(50) ALT(OF)
     D DTA1            S              1    DIM(74)
     D DTA2            S              1    DIM(68)
     D PH              S              1    DIM(14)
     D CSZ             S              1    DIM(20)
      *
     D GRP             S              3    DIM(999)                             GROUP RESTRICTIONS
     D EMP             S             14  0 DIM(999)                             EDIT EMP/DIV     S
     D*
     D DBFDB           DS
     D  DBRRB                397    400B 0
     D DBFD3B          DS
     D  DBRR3B               397    400B 0
     D DBFD4B          DS
     D  DBRR4B               397    400B 0
     D DBFD5B          DS
     D  DBRR5B               397    400B 0
     D FNFDB           DS
     D  FNRRN                397    400B 0
     D MPFDS           DS
     D  MPRRN                397    400B 0
     D                 DS
     D  INFO6                  1     54
     D  X6PNAM                 2     24
     D  X6PEML                25     54
     D*
     D OPTNM           C                   CONST('Employers')
     D DELETD          C                   CONST('*Deleted')
     D NOVALU          C                   CONST('* No Value Entered *')
     D XFER            C                   CONST('will be transferred')
     D ALLDIV          C                   CONST('All divisions')
     D ACDPGM          C                   CONST('W0050R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D GRPAPI          C                   CONST('WA016R')
     D QRYPGM          C                   CONST('W0050C')
     D OVRDB1          C                   CONST('OVRDBF FILE(')
     D OVRDB2          C                   CONST(') SHARE(*YES)')
     D SF1PAG          C                   CONST(12)
     D VIEWMX          C                   CONST(5)
     D INFOMX          C                   CONST(6)
     D COMMA           C                   CONST(',')
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D FQIUSR          C                   CONST('WCEXPRESS')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  FED                    1     20
     D  XXFND                  1      3
     D  XXEMP#                 5     13
     D  XXDIV                 15     19
     D                 DS
     D  PHN                    1      7
     D  PHN1                   1      3
     D  PHN2                   4      7
     D                 DS
     D  SEMPNM                 1     20
     D  XXSEMP                 1     20
     D                 DS
     D  INFO1                  1     33
     D  XXQTE                  1     12
     D  ZZAGT#                14     19
     D  XXANAM                21     33
     D                 DS
     D  INFO2                  1     50
     D  XXEFDT                 2     11
     D  XXCXDT                16     25
     D  XXFQFL                30     31
     D                 DS
     D  INFO3                  1     50
     D  EMUWTR                 1      3
     D  XXUEXT                 5      8
     D  EMAMGR                14     16
     D  XXMEXT                18     21
     D  EMPSRV                27     29
     D  XXCEXT                31     34
     D  EMARVW                40     42
     D  XXREXT                44     47
     D                 DS
     D  INFO4                  1     50
     D  YYAGT#                 1      6
     D  YYANAM                 8     27
     D  YYPNAM                31     50
     D                 DS
     D  INFO5                  1     50
     D  XZAGT#                 1      6
     D  XZANAM                 8     27
     D  XXPHON                31     44
     D                 DS
     D  EATRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D                 DS
     D  EAPODT                 1      7  0
     D  XXPOYM                 1      5  0
     D                 DS
     D  XXDATE                 1      7  0
     D  XXCUYM                 1      5  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  XXDATA                 1     80
     D  XXTYPE                 1      1
     D  XXCO#                  2      4  0
     D  SCFND                  5      7  0
     D  SCEMP#                 8     16  0
     D  SCDIV                 17     21  0
     D  UPOUTQ                22     31
     D                 DS
     D  XSDTTM                 1     12  0
     D  XSTIME                 1      6  0
     D  XSDATE                 7     12  0
     D SVLDA           DS          1024
     D*
     D/copy wPrEmplr
     D/copy WTSEARCHDS
     D*
     DWTSEARCH         pr                  extPgm('WTSEARCH')
     D ##Mode                         1    const
     D ##Call                         1    const
     D ##RRN                          9
     D ##Criteria                          likeds(dsSrchCrit)
     D*
     D srchRRN         s              9    inz(*zeros)
     D empRRN          s              9  0
     D empYrRRN        s              9  0
     D empLocRRN       s              9  0
     D useWTSEARCH     s              1n
      *
      *=========================================================
      *  *LIKE DEFN
      *=========================================================
     C     *LIKE         DEFINE    MPELFL        KKELFL
     C     *LIKE         DEFINE    MPELID        KKELID
     C     *LIKE         DEFINE    EMFND         K4FND
     C     *LIKE         DEFINE    EMEMP#        K4EMP#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    EMNAME        KKNAME
     C     *LIKE         DEFINE    EMFDNM        KKFDNM
     C     *LIKE         DEFINE    EMSEMP        KKSEMP
     C     *LIKE         DEFINE    EMFEIN        KKFEIN
     C     *LIKE         DEFINE    EMMAD1        XXADR1
     C     *LIKE         DEFINE    EMNAME        U2PDSC
     C* Extend U1PDSC length to 100 chars to accept employer name part 1 and 2
     C                   MOVE      *BLANKS       U1PDSC          100
     C     *LIKE         DEFINE    EMEMP#        ZZEMP#
     C     *LIKE         DEFINE    EMDIV         ZZDIV
     C     *LIKE         DEFINE    EMNAME        ZZNAME
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    UPEXT#        XXEXT#
     C     *LIKE         DEFINE    UPLVL         SVLVL
     C     *LIKE         DEFINE    FNFND         SVFND
     C     *LIKE         DEFINE    FNFND         SVFND2
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C     *LIKE         DEFINE    @@CALL        S1CALL
     C     *LIKE         DEFINE    @@CALL        S2CALL
     C     *LIKE         DEFINE    @@CALL        S3CALL
     C     *LIKE         DEFINE    *IN09         SVIN09
     C     *LIKE         DEFINE    EMFND         #@FND
     C     *LIKE         DEFINE    EMEMP#        ##EMP
     C     *LIKE         DEFINE    UPDIV         SVDIV
     C     *LIKE         DEFINE    @@RRN3        SVRRN3
     C     *LIKE         DEFINE    AHEFDT        X7DATE
     C     *LIKE         DEFINE    EMP           XXEMP
     C     *LIKE         DEFINE    DBRRN         SVRRN
     C     *LIKE         DEFINE    EMFND         K1FND
     C     *LIKE         DEFINE    EMEMP#        K1EMP#
     C     *LIKE         DEFINE    EMDIV         K1DIV
     C     *LIKE         DEFINE    ELFYR         K1FYR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@QTE         ##QTE
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM      @@QTE         ##QTE             1
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      XXCALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      ' '           ##ELFL            1
     C                   PARM      @@QTE         ##QTE
     C*
     C     ACDPL3        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C     ACDPL4        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      XXCALL        ##CALL
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      @@QTE         ##QTE
     C                   PARM      'E'           ##MODE
     C*
     C     ACDPL5        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@CALL        ##CALL
     C                   PARM      SCDBRR        ##RRN
     C*
     C     ACDPL6        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM      @@QTE         ##QTE
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
     C                   PARM                    ##XEMP            1
     C                   PARM                    ##XDIV            1
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C                   PARM                    ##AREA            3 0
     C                   PARM                    ##PHON            7 0
     C                   PARM                    ##PLAN            1
     C                   PARM                    ##DATE            8 0
     C                   PARM                    ##NWRT            5 2
     C                   PARM                    ##RNRT            5 2
     C                   PARM                    ##@RT#            3 0
     C                   PARM                    ##PEML           60
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*  Group expired API parameter list
     C*
     C     GRPPL         PLIST
     C                   PARM      FNFND         ##FND
     C                   PARM      FNNAME        ##NAME           60
     C                   PARM      FNEXDT        ##EXDT            7 0
     C*
     C*  Quote employer program parameter list
     C*
     C     QTEPL         PLIST
     C                   PARM                    ##MODE
     C                   PARM                    ##CO#
     C                   PARM      SCFND         ##FND
     C                   PARM      SCEMP#        ##EMP#
     C                   PARM      SCDIV         ##DIV
     C                   PARM      *ON           ##ERR             1
     C*
     C     QTEPL1        PLIST
     C                   PARM                    ##MODE
     C                   PARM                    ##CO#
     C                   PARM      SCFND         ##FND
     C                   PARM      SCEMP#        ##EMP#
     C                   PARM      SCDIV         ##DIV
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     RSTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
      *
     C     RSTPL1        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'W'           ##APPL
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM                    ##AUTH            1
     C                   PARM      EMFND         #@FND
     C                   PARM      EMEMP#        ##EMP
      *
     C     RSTPLF        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'W'           ##APPL
     C                   PARM      *OFF          ##RTCD
     C                   PARM                    ##AUTH
     C                   PARM      FNFND         #@FND
      *
     C     QRYPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##GRP1          255
     C                   PARM                    ##GRP2          255
     C                   PARM                    ##GRP3          255
     C                   PARM                    ##GRP4          255
     C                   PARM                    ##GRP5          255
     C                   PARM                    ##GRP6          255
     C                   PARM                    ##GRP7          255
     C                   PARM                    ##GRP8          255
     C                   PARM                    ##GRP9          255
     C                   PARM                    ##GRPA          255
     C                   PARM                    ##GRPB          255
     C                   PARM                    ##GRPC           64
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      3             ##VIEW            1 0
      *
      *==========================================================
      *  KLIST
      *==========================================================
      *
      *  MPN Provider
      *
     C     MPKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKELFL
     C                   KFLD                    KKELID
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNAME
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFDNM
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1D         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSEMP
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1E         KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY1F         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1X         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKNAME
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY2B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY2C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKFDNM
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY2D         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKSEMP
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY2E         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C*
     C     KEY2F         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     EMKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
      *
      *  EMPLOYER
      *
     C     EMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K4FND
     C                   KFLD                    K4EMP#
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C     GPKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  QUOTE MASTER FILE
     C*
     C     QTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    @@QTE
     C                   KFLD                    ##AGT#
     C*
     C     QTKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    @@QTE
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    XXSTAF
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  NCCI SUBMISSINON HISTORY FILE
     C*
     C     SHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     EAKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    K1DIV
     C                   KFLD                    ELFYR
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1FND
     C                   KFLD                    K1EMP#
     C*
     C     ELKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1FND
     C                   KFLD                    K1EMP#
     C                   KFLD                    K1DIV
     C*
     C     ELKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1EMP#
     C*
     C*   PAYROLL CLASSIFICATION FILE (FOR RATE SET CODE) WMRSP
     C*
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C*
     C*  KEY LIST FOR ADMIN/DARTS FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C*  ENTITY/LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     AAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCAGT#
     C*
     C     AHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCAGT#
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  DARTS REMARK DETAIL FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  GROUP CARRIER INFORMATION FILE
     C*
     C     GCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  QUOTE TRANSFER FILE
     C*
     C     QOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      *
     C     RRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    ##RRST            2
     C*
     C     QSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C     RIUKY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM                    @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C*  NOTE: -@@RRN WILL RETURN THE RELETIVE RECORD NUMBER OF WMEMP
     C*        -@@RRN2 WILL RETURN THE RELETIVE RECORD NUMBER OF
     C*         WDELP OR WMALP DEPENDING ON @@CALL (Y/L)
     C*        -@@RRN2 IS USED FOR DISPLAYING A PARTICULAR GROUP
     C*        -@@RRN3 IS USED FOR DISPLAYING A PARTICULAR EMPLOYER
     C*
     C                   MOVE      'Y'           XX1ST             1
      *
     C/COPY SCOPYWW01C
      /Free
       //==============================================================
       // Determine if WTSEARCH should be called and use it
       //==============================================================
       Begsr *Inzsr;

       Select;
       When @@call = '2';
          useWTSEARCH = *on;
       When @@call = 'Y';
          useWTSEARCH = *on;
       When @@call = 'L';
          useWTSEARCH = *on;
       //**************** Beg Add ************** 11/08/13 *******************
       When @@call = '1' or @@call = 'Z';
          useWTSEARCH = *on;
       //**************** End Add ************** 11/08/13 *******************
       Endsl;

       Select;
       When useWTSEARCH;
          Exsr CallWTSEARCH;
          // WTSEARCH will bypass the rest of the program
       Other;
          Open W0050F;
       Endsl;

       Endsr;

       //==============================================================
       // Call WTSEARCH instead of using W/W
       //==============================================================
       Begsr CallWTSEARCH;

       if %parms >= 5;
          Clear FNfnd;
          Chain(n) @@rrn2 pfn02;
          dsEmpGrp  = FNfnd;
          Clear @@rrn2;
       Endif;

       if %parms >= 8;
          Clear EMfnd;
          Clear EMemp#;
          Clear EMdiv;
          Chain(n) @@rrn3 pfrcdf;
          dsEmpGrp  = EMfnd;
          dsEmpEmp# = EMemp#;
        //dsEmpDiv  = EMdiv;
       Endif;

       Callp WTSEARCH ('E':'2':srchRRN:dsSrchCrit);

       empYrRRN = %int(srchRRN);
       Clear ELemp#;
       Chain(n) empYrRRN pel0p;
       empRRN = GetEmpRRN(ELemp#:ELdiv:ELfnd:ELco#);
       @@rrn = empRRN;

       Select;
       When @@Call = 'L'; //Use employer location;
          Exsr LocationSrch;
          @@rrn2 = empLocRRN;
       When @@Call = '$'; //Use employer location;
          Exsr LocationSrch;
          @@rrn2 = empLocRRN;
       When @@Call = 'Y'; //Use employer detail;
          @@rrn2 = empYrRRN;
       Endsl;

       *inlr = *on;
       Return;

       Endsr;
      /End-free
      *
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C     *IN34         IFEQ      *OFF
     C*
B01  C     SSFND         IFNE      *ZEROS
     C     SSNAME        ORNE      *BLANKS
     C     SSFDNM        ORNE      *BLANKS
     C     SSPAD1        ORNE      *BLANKS
     C***  BEGIN CHG  ***  12/16/97  *********************************
     C********** SSPHN#    ORNE *ZEROS
     C     SSPHN#        ORNE      *BLANKS
     C***  END   CHG  ***  12/16/97  *********************************
     C     SSAGT#        ORNE      *ZEROS
      *** BEG ADD ***** 010/10/03 ************************************
     C     SSNRA         ORNE      *BLANKS
     C     SSQTYP        ORNE      *BLANKS
     C     SSJRST        ORNE      *BLANKS
     C     SSUWTR        ORNE      *BLANKS
     C     SSMAD1        ORNE      *BLANKS
      *** END ADD ***** 010/10/03 ************************************
     C*** BEGIN ADD *** 06/01/09 *************************************
     C     SSFQFL        ORNE      *BLANKS
     C*** END   ADD *** 06/01/09 *************************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSFND<>*ZEROS
     C*
E01  C                   ENDIF
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      ***** BEG ADD ******* 06/09/05 **********************************
      *
      *  IF Employer record locked by FASTQUOTE.
      *
     C  N90EMFQLK        IFEQ      'Y'
      *
      *     Do not allow options
      *
     C  N90SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C     SCSEL         OREQ      '6'
     C     SCSEL         OREQ      '17'
     C                   SETON                                        90  50
     C                   ENDIF                                                  SCSEL
      *
      *    "DISPLAY Only" message window
      *
     C  N90SCSEL         IFEQ      '7'
     C     SCSEL         OREQ      '8'
     C     SCSEL         OREQ      '9'
     C     SCSEL         OREQ      '10'
     C     SCSEL         OREQ      '11'
     C     SCSEL         OREQ      '12'
     C     SCSEL         OREQ      '13'
     C     SCSEL         OREQ      '15'
     C     SCSEL         OREQ      '26'
     C*** BEG ADD ***** 06/29/09 ***************************************
     C     SCSEL         OREQ      '70'
     C*** END ADD ***** 06/29/09 ***************************************
     C                   EXFMT     WDW01
     C   KL              SETON                                        90
     C                   SETOFF                                       KL
     C                   ENDIF
      *
     C                   ENDIF                                                  EMFQLK
      *
      *  IF Employer record locked by FASTQUOTE,
      *     Do not allow option
      *
     C  N90SCSEL         IFEQ      '6'
     C     SCSEL         OREQ      '17'
     C     SCSEL         OREQ      '42'
     C                   Z-ADD     EMFND         K4FND
     C                   Z-ADD     EMEMP#        K4EMP#
     C     EMKY1         CHAIN     WMEML                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EMFQLK        IFEQ      'Y'
     C                   SETON                                        90  50
     C                   LEAVE
     C                   ENDIF
      *
     C     EMKY1         READE     WMEML                                  60
     C                   ENDDO
      *
     C     SCDBRR        CHAIN     PFRCDF                             99
     C                   ENDIF
      *
      ***** END ADD ******* 06/09/05 **********************************
     C*
      *** BEG ADD ***** 10/10/03 **************************************
     C*
     C* If employer is renewal quote, prevent following options from being run.
     C*
     C     @@QTE         IFEQ      'Q'
     C     SCRQFL        ANDEQ     'Y'
      *** BEG CHG ***** 03/06/06 **************************************
     C     SCRDFL        ANDEQ     *BLANKS                                      other divisions
      *** END CHG ***** 03/06/06 **************************************
     C*
     C     SCSEL         IFEQ      '2'                                          change
     C                   SETON                                        90  71
     C                   ENDIF
     C*
     C                   ENDIF
      *** END ADD ***** 10/10/03 **************************************
      ***** BEG ADD ******* 06/08/01 **********************************
      *
      *  Check for Batch process running
      *
     C     SCSEL         IFEQ      '17'
     C                   CALL      'WA044R'
     C                   PARM      *BLANK        #1RTCD            1
      *
     C     #1RTCD        IFEQ      'F'
     C                   SETON                                        90  35
     C                   ENDIF                                                  #1RTCD
      *
     C***  Begin Del  ***  10/10/03  ***  C:33  ***************************
      ******BEG*ADD ***** 03/04/03 ************************************
      **Check*State Unemployment Flag in WDRRP/RRURFL to see if required
      **to*have*Unemployment # before allowing transfer
     C********** *IN90     IFEQ *OFF
      ******BEG*ADD ******* 09/30/03 **********************************
      **********
     C********** EMPST     IFNE *BLANKS
     C**********           MOVE EMPST     ##RRST    P
      **********
     C**********           ELSE
      **********
     C**********           MOVE EMMST     ##RRST    P
     C**********           ENDIF
      **********
      ******END*ADD ******* 09/30/03 **********************************
     C********** RRKY      CHAINPRR01                60
      **********
     C********** *IN60     IFEQ *OFF
     C********** RRURFL    ANDEQ'Y'
     C********** EMUE#     ANDEQ*BLANKS
     C**********           SETON                     90  36
     C**********           ENDIF                           END 60=OFF
      **********
     C**********           ENDIF                           END 90=OFF
      ******END*ADD ***** 03/04/03 ************************************
      **********
      *****BEGIN ADD **** 12/26/01 ************************************
      **********
      **DO*NOT*ALLOW QUOTE TRANSFER IF CARRIER # FALLS OUTSIDE RANGE
      **********
     C**N90***** GCKY      CHAINPGC01                60
     C**N90***** *IN60     IFEQ *OFF
      **********
     C********** GCFRCN    IFNE *ZEROS
     C********** GCTOCN    ANDNE*ZEROS
      **********
     C********** QOKY      SETLLPQO01
     C********** QOKY      READEPQO01                    61
     C********** *IN61     DOWEQ*OFF
     C**********           ADD  1         XXTRF#  90
     C********** QOKY      READEPQO01                    61
     C**********           ENDDO
      **********
     C********** GCNACN    ADD  XXTRF#    XXNACN  90
     C********** XXNACN    IFLT GCFRCN
     C********** XXNACN    ORGT GCTOCN
     C**********           SETON                     90  86
     C**********           ENDIF                           XXNACN
     C**********           ENDIF                           GCFRCN
      **********
     C**********           ENDIF                           *IN60
      *****END** ADD **** 12/26/01 ************************************
     C***  End   Del  ***  10/10/03  ***  C:33  ***************************
      *
     C                   ENDIF                                                  SCSEL = 17
      *
      ***** END ADD ******* 06/08/01 **********************************
     C***  BEGIN ADD  ***  01/20/98  *********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  01/20/98  *********************************
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
      ***** BEG CHG ******* 06/09/05 **********************************
B01  C*********            SELEC
B01  C  N90              SELECT
      ***** END CHG ******* 06/09/05 **********************************
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C     SCSEL         WHENEQ    '2'
     C     SCAUTH        ANDEQ     'I'
     C     ##RCDS        ANDEQ     *ON
     C                   SETON                                        88  90
      *
     C     SCSEL         WHENEQ    '4'
     C     SCAUTH        ANDEQ     'I'
     C                   SETON                                        77  90
      ***** END ADD ******* 06/02/99 **********************************
     C*
S01  C     SCSEL         WHENEQ    '4'
     C*
     C***  Begin Chg  ***  08/21/06  **************************************
     C********** EXKY      CHAINPEX01                60
     C**********
B02  C********** SVLVL     IFLT EXDLTL
     C********** *IN60     OREQ *ON
     C**********
     C**********           SETON                     77  90
     C**********
X02  C**********           ELSE
     C**********
     C***GOTO*THE CLAIMS FILE AND CHECK FOR A CLAIM
     C**********
     C********** CMKY      CHAINPCM01                60
     C**********
B03  C********** *IN60     IFEQ *ON
     C**********
     C***CHECK*FOR PAYMENTS IN ACCOUNTS RECEIVABLE FILE, LIVE ONLY
     C**********
     C*****BEGIN ADD  ***  01/10/00  *********************************
     C********** @@QTE     IFNE 'Q'
     C*****END** ADD  ***  01/10/00  *********************************
     C**********
     C********** EAKY      CHAINPEA01                60
     C**********
B04  C********** *IN60     DOWEQ*OFF
     C**********
B05  C**********           SELEC
     C**********
     C**********
S01  C********** EAARTY    WHEQ 'P'
     C********** EAARTY    OREQ 'I'
     C********** EAARTY    OREQ 'Q'
     C**********           SETON                     726090
     C**********
     C**********
S02  C********** XXTRYM    WHGT XXPOYM
     C*****BEGIN CHG  ***  03/09/98  *********************************
     C********** XXTRYM    ANDLEXXCUYM
     C********** XXTRYM    ANDLTXXCUYM
     C*****END** CHG  ***  03/09/98  *********************************
     C**********           SETON                     746090
     C**********
     C**********
S03  C********** XXTRYM    WHLT XXCUYM
     C********** XXPOYM    ANDLTXXCUYM
     C**********           SETON                     746090
     C**********
     C**********
S04  C**********           OTHER
     C********** EAKY      READEPEA01                    60
     C**********
E05  C**********           ENDSL
     C**********
E04  C**********           ENDDO                           *IN60=*OFF
     C**********
     C*****BEGIN ADD ****** 12/01/04 **********************************
     C********** *IN90     IFEQ *OFF
     C**********
     C***Get*state reporting division
     C**********
     C**********           Z-ADDEMFND     K1FND
     C**********           Z-ADDEMEMP#    K1EMP#
     C********** ELKY1     SETLLWMEML
     C********** ELKY1     READEWMEML                    60
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** EMSTRP    IFEQ 'Y'
     C**********           Z-ADDEMDIV     K1DIV
     C**********           LEAVE
     C**********           ENDIF                           EMSTRP = 'Y'
     C**********
     C********** ELKY1     READEWMEML                    60
     C**********           ENDDO                           *IN60
     C**********
     C********** ELKY      CHAINPEL01                60
     C**N90***** *IN60     DOWEQ*OFF
     C**********
     C********** ELARSM    IFEQ 'Y'
     C********** ELDIV     ANDNEK1DIV
     C**********
     C***CHECK*FOR BILLINGS IN STATE REPORTING DIVISION.
     C**********
     C********** EAKY1     CHAINPEA01                60
     C**********
     C**N90***** *IN60     DOWEQ*OFF
     C**********
     C**********           SELEC
     C**********
     C********** EAARTY    WHEQ 'H'
     C********** EAARTY    OREQ 'F'
     C********** EAARTY    OREQ 'B'
     C********** EAARTY    OREQ 'A'
     C********** EAARTY    OREQ 'D'
     C**********           SETON                     696090
     C**********
     C**********           OTHER
     C******BEG*CHG ***** 08/05/05 ******************************
     C**Need*to*read with Key having State Reporting Division
     C********** EAKY1     READEPEA01                    60
     C********** EAKY      READEPEA01                    60
     C******END*CHG ***** 08/05/05 ******************************
     C**********
     C**********           ENDSL
     C**********
     C**********           ENDDO                           *IN60=*OFF
     C**********
     C**********           ENDIF                           ELARSM IFEQ
     C**********
     C**N90***** ELKY      READEPEL01                    60
     C**********           ENDDO                           *IN60=*OFF
     C**********
     C**********           ENDIF                           *IN90 IFEQ
     C*****END*ADD ******** 12/01/04 **********************************
     C**********
     C*****BEGIN ADD  ***  01/10/00  *********************************
     C**********           ENDIF                           @@QTE<>Q
     C*****END** ADD  ***  01/10/00  *********************************
     C**********
X03  C**********           ELSE
     C**********
     C***ONLY*PERFORM EDIT FOR CLAIMS IF THIS IS NOT THE QUOTE ENV
     C**********
B04  C********** @@QTE     IFNE 'Q'
     C**********           SETON                     73  90
E04  C**********           ENDIF                           @@QTE<>Q
     C**********
E03  C**********           ENDIF                           *IN60=*OFF
     C*****BEGIN ADD  ***  02/12/98  ********************************
     C*****BEGIN ADD **** 12/21/99 *********************************************
B04  C********** @@QTE     IFNE 'Q'
     C*****END** ADD **** 12/21/99 *********************************************
     C**********
     C***CHECK*TO BE SURE NO NCCI SUBMISSION HAS A STATUS OF '4'
     C**********
     C*****BEGIN ADD  ***  07/15/98  *********************************
     C********** SHKY      SETLLPSH01
     C*****END** ADD  ***  07/15/98  *********************************
     C**********
     C**N90***** *IN60     DOUEQ*ON
     C**********
     C*****BEGIN CHG  ***  07/15/98  *********************************
     C********** SHKY      CHAINPSH01                60
     C********** SHKY      READEPSH01                    60
     C*****END** CHG  ***  07/15/98  *********************************
     C**********
     C********** SHSTAT    IFEQ '4'
     C********** *IN60     ANDEQ*OFF
     C**********           SETON                     81  90
     C**********           LEAVE
     C**********           ENDIF                           SHSTAT=A
     C**********
     C**********           ENDDO                           *IN60=*ON
     C**********
     C***CHECK*FOR NON-TRANSFERRED A/R BATCH RECORDS
     C**********
     C*****BEGIN ADD  ***  07/15/98  *********************************
     C********** EAKY      SETLLPEA02
     C*****END** ADD  ***  07/15/98  *********************************
     C**********
     C**N90***** *IN60     DOUEQ*ON
     C**********
     C********** EAKY      READEPEA02                    60
     C**********
     C********** EASTAT    IFNE 'T'
     C********** *IN60     ANDEQ*OFF
     C**********           SETON                     82  90
     C**********           LEAVE
     C**********           ENDIF                           EASTAT=A
     C**********
     C**********           ENDDO                           *IN60=*ON
     C*****END** ADD  ***  02/12/98  ********************************
     C*****BEGIN ADD **** 12/21/99 *********************************************
B04  C**********           ENDIF                           @@QTE IFNE 'Q'
     C*****END** ADD **** 12/21/99 *********************************************
     C**********
B03  C********** *IN90     IFEQ *OFF
     C**********
     C***EXECUTE THIS CODE ONLY IF DARTS IS INSTALLED
     C**********
     C**********           MOVEL'SY'      XXSYK
     C**********
     C********** SYKY      CHAINSSY01                60
     C**********
B04  C********** *IN60     IFEQ *OFF
     C********** SYSCIF    ANDEQ'Y'
     C**********
     C***CHECK*FOR ACTIVE DARTS REMARKS
     C**********
     C*****ESTABLISH KEY
     C**********
     C**********           CLEARQCKEY
     C**********
     C**********           MOVE ##CO#     QCCO#
     C**********           MOVE SCFND     QCFND
     C**********           MOVE SCEMP#    QCEMP#
     C**********           MOVE SCDIV     QCDIV
     C**********
     C***CHAIN*TO DARTS REMARKS DETAIL FILE
     C**********
     C********** C3KY      CHAINPCM03                60
     C**********
B05  C********** *IN60     DOWEQ*OFF
     C**********
B06  C********** C3DIFL    IFEQ 'Y'
     C********** C3STAT    ANDNE'I'
     C**********
     C**********           MOVEL'N'       SCOVR
     C**********
     C**********           EXFMTERRWDW01
     C**********
B07  C********** SCOVR     IFNE 'Y'
     C**********           SETON                     75  90
E07  C**********           ENDIF                           SCERFL<>Y
     C**********
     C**********           MOVE *ON       *IN60
     C**********
E06  C**********           ENDIF                           C3DIFL=Y
     C**********
     C********** C3KY      READEPCM03                    60
     C**********
E05  C**********           ENDDO                           *IN60=*OFF
     C**********
E04  C**********           ENDIF                           *IN60=*OFF/SYSCIF=Y
     C**********
E03  C**********           ENDIF                           *IN90=*OFF
     C**********
E02  C**********           ENDIF                           SVLVL<EXDLTL
     C**********
     C****BEG*ADD *** 04/12/00 **********************************
     C**********
     C****CHECK*FOR ANY VALID CLAIMS FOR THIS EMPLOYER IF NOT IN QUOTE
     C**********
     C********** XXMANC    IFEQ *ON
     C****BEG*ADD *** 08/12/03 ****************************************
     C********** @@QTE     ANDNE'Q'
     C****END*ADD *** 08/12/03 ****************************************
     C**********           MOVE ##CO#     #@CO#
     C**********           MOVE SCFND     #2FND
     C**********           MOVE SCEMP#    #@EMP#
     C**********           MOVE SCDIV     #@DIV
     C**********
     C**********           CALL 'FA701R'
     C**********           PARM *BLANKS   ##RTCD
     C**********           PARM           #@CO#
     C**********           PARM *ZEROS    #@SUB#
     C**********           PARM           #2FND
     C**********           PARM *BLANKS   ##OUTS  1
     C**********
     C********** ##RTCD    IFNE 'F'
     C********** ##OUTS    ANDEQ'N'
     C**********
     C**********           CALL 'FA207R'
     C**********           PARM *BLANKS   ##RTCD
     C**********           PARM           #@CO#   3
     C**********           PARM *ZEROS    #@SUB#  5
     C**********           PARM           #2FND   3
     C**********           PARM           #@EMP#  9
     C**********           PARM           #@DIV   5
     C**********
     C********** ##RTCD    IFEQ *ON
     C**********           SETON                     9070
     C**********           ENDIF
     C**********           ENDIF                           LINKED
     C**********           ENDIF                           MANC ON
     C**********
     C****END*ADD *** 04/12/00 **********************************
     C*
     C*IN61 - S/R Billings Exist in St Rpt Div
     C*IN69 - State Reporting Div Billings Exist
     C*IN70 - MC Claims Exist
     C*IN72 - Payments Exist
     C*IN73 - Claims Exist
     C*IN74 - A/R Exists in prior month
     C*IN75 - DARTS records Exist
     C*IN77 - Not Authorized
     C*IN81 - NCCI Submission Status '4' Exist
     C*IN82 - Non x-fer'd A/R Batch Rcds Exist
     C*IN86 - Payroll Exist
     C                   CALL      'W0050R04'
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      @@QTE         ##QTE
     C                   PARM      *ZEROS        ##IND             2 0
     C*
     C     *LIKE         DEFINE    ##IND         IN
     C*
     C     ##IND         IFNE      *ZEROS
     C                   Z-ADD     ##IND         IN
     C                   MOVE      *ON           *IN(IN)
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C***  End   Chg  ***  08/21/06  **************************************
      *** BEG ADD **** 04/17/02 ***********************************************
      *
      * DISPLAY INFORMATIONAL MESSAGE IF ENTITY/LOCATIONS EXIST
      *
      *
      ***** BEG ADD ******* 06/09/05 **********************************
     C     *IN90         IFEQ      *OFF
      ***** END ADD ******* 06/09/05 **********************************
     C     ALKY          CHAIN     PAL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   EXFMT     LOCMSG
     C                   ENDIF
      *** END ADD **** 04/17/02 ***********************************************
      ***** BEG ADD ******* 06/09/05 **********************************
     C                   ENDIF
      ***** END ADD ******* 06/09/05 **********************************
      *
      ***** BEG ADD ******* 06/02/99 **********************************
     C*
     C     SCSEL         WHENEQ    '6'
     C     SCAUTH        ANDEQ     'I'
     C                   SETON                                        89  90
      ***** END ADD ******* 06/02/99 **********************************
     C**
S05  C     SCSEL         WHENEQ    '6'
     C*
B02  C     FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGT     FNEXDT
     C                   SETON                                        78  90
E02  C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
      ***** BEG ADD ******* 06/02/99 **********************************
     C     SCSEL         WHENEQ    '17'
     C     SCAUTH        ANDEQ     'I'
      ***** BEG ADD ******* 06/08/01  **********************************
     C     *IN90         ANDEQ     *OFF
      ***** END ADD ******* 06/08/01  **********************************
     C                   SETON                                        89  90
      ***** END ADD ******* 06/02/99 **********************************
     C**
     C***  BEGIN ADD  ***  10/30/97  *********************************
S06  C     SCSEL         WHENEQ    '17'
      ***** BEG ADD ******* 06/08/01  **********************************
     C     *IN90         ANDEQ     *OFF
      ***** END ADD ******* 06/08/01  **********************************
      *
      ***** BEG ADD ******* 06/03/04  **********************************
      *
      * IF DIV COUNT > 1, OFFER TRANSFER OF ALL DIVISIONS
      *
     C                   Z-ADD     0             XXDVCT            1 0
     C                   OPEN      WMEMLQ
     C     EMKY          CHAIN     PEM01Q                             60
     C     *IN60         DOWEQ     *OFF
      *
     C                   ADD       1             XXDVCT
     C     XXDVCT        IFGT      1
     C                   LEAVE
     C                   ENDIF
      *
     C     EMKY          READE     PEM01Q                                 60
     C                   ENDDO
     C                   CLOSE     WMEMLQ
      *
      * Reposition cursor, find out if user wants to transfer all divisions,
      * and pass @@mode = 'W' to validate all divisions, or 'V' for just
      * one.
     C     SCDBRR        CHAIN     PFRCDF                             60
      *
     C                   MOVE      'N'           SCTFRD
      *
     C     XXDVCT        IFGT      1
     C                   EXFMT     TFRDIV
     C                   ENDIF                                                  XXDVCT > 1
      *
     C     *INKL         IFEQ      *OFF
      *
     C     SCTFRD        IFEQ      'Y'
     C                   MOVE      'W'           ##MODE
     C                   ELSE
      ***** END ADD ******* 06/03/04  **********************************
     C     SCTFRD        IFEQ      'N'
     C                   MOVE      'V'           ##MODE
     C                   ENDIF
      ***** BEG ADD ******* 06/03/04  **********************************
     C                   ENDIF
      ***** END ADD ******* 06/03/04  **********************************
     C*
     C***  Begin Chg  ***  06/23/05  **************************************
     C**********           CALL 'W0050R17'QTEPL        99
     C                   CALL      'W0050C17'    QTEPL                  99
     C***  End   Chg  ***  06/23/05  **************************************
     C*
     C***  BEGIN ADD  ***  09/09/10  *********************************
     C     ##ERR         IFEQ      'L'
     C                   SETON                                            90
     C                   ENDIF
     C*
     C***  END   ADD  ***  09/09/10  *********************************
     C     ##ERR         IFEQ      *OFF
     C                   SETON                                        79  90
     C                   ENDIF                                                  ##ERR=*OFF
     C*
     C***  BEGIN ADD  ***  02/09/00  *********************************
     C     ##ERR         IFEQ      '2'
     C                   SETON                                        85  90
     C                   ENDIF                                                  ##ERR=*OFF
     C***  END   ADD  ***  02/09/00  *********************************
     C***  BEGIN ADD  ***  11/06/97  *********************************
     C*
     C*  QUOTE TYPE MUST BE PENDED OR QUOTED TO BE TRANSFERED LIVE
     C*
     C  N90SCQTTP        IFNE      'P'
     C     SCQTTP        ANDNE     'Q'
     C***  BEGIN ADD  ***  08/05/98  *********************************
     C     SCQTTP        ANDNE     'B'
     C     SCQTTP        ANDNE     'M'
     C***  END   ADD  ***  08/05/98  *********************************
     C***  Begin Add  ***  10/10/03  **************************************
     C     SCQTTP        ANDNE     'F'
     C     SCQTTP        ANDNE     'G'
     C***  End   Add  ***  10/10/03  **************************************
     C***  Begin Add  ***  11/18/04  **************************************
     C     SCQTTP        ANDNE     'U'
     C***  End   Add  ***  11/18/04  **************************************
     C                   SETON                                        80  90
     C                   ENDIF                                                  SCQTTP<>P/Q/B/M
     C***  END   ADD  ***  11/06/97  *********************************
     C***  BEGIN ADD  ***  06/26/98  *********************************
     C*
     C*  FEIN MUST BE ENTERED IF THE REQUIRED BY ADMIN/DARTS FLAG
     C*
     C                   CLEAR                   EXFIFG
     C*
     C     EXKY          CHAIN     PEX01                              60
     C*
      ***** BEG CHG ***** 07/09/07 ****************************************
     C* N90      SCFEIN    IFEQ *ZEROS
     C****       EXFIFG    ANDEQ'Y'
     C****                 SETON                     83  90
     C****                 ENDIF                           SCFEIN=*ZEROS
     C****
     C* N90      SCSIC     IFEQ *BLANKS
     C****       EXSICR    ANDEQ'Y'
     C****                 SETON                     84  90
     C****                 ENDIF                           SCSIC=*BLANKS
      ***** END CHG ***** 07/09/07 ****************************************
     C***  END   ADD  ***  06/26/98  *********************************
     C***  BEGIN ADD  ***  03/24/00  *********************************
     C*
     C*  NOTIFY USER IF AGENT ASSIGNED TO THE ACCOUNT IS NOT ACTIVE
     C*
     C                   MOVEL     SCEMP#        XXEMP
     C                   MOVE      SCDIV         XXEMP
     C*
     C     XXEMP         LOOKUP    EMP                                    60
     C*
     C  N90*IN60         IFEQ      *OFF
     C*
     C                   CLEAR                   AAEFDT
     C                   CLEAR                   AACNDT
     C                   CLEAR                   AHEFDT
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     AHKY          CHAIN     PAH01                              60
     C*
     C     AHEFDT        IFGT      SYSDT
     C                   Z-ADD     AHEFDT        X7DATE
     C                   ELSE
     C                   Z-ADD     SYSDT         X7DATE
     C                   ENDIF                                                  AHEFDT>SYSDT
     C*
     C  N90AAEFDT        IFGT      X7DATE
     C                   SETON                                        87  90
     C                   ENDIF                                                  AAEFDT>X7DATE
     C*
     C  N90AACNDT        IFNE      *ZERO
     C     AACNDT        ANDLT     X7DATE
     C                   SETON                                        87  90
     C                   ENDIF                                                  AACNDT<>*ZERO
     C*
     C     *IN90         IFEQ      *ON
     C                   Z-ADD     1             X                 3 0
     C     *ZEROS        LOOKUP    EMP(X)                                 60
     C   60              Z-ADD     XXEMP         EMP(X)
     C                   ENDIF                                                  *IN90=*ON
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   ADD  ***  03/24/00  *********************************
     C*** BEG ADD ***** 06/03/04 *************************************
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *INKL=*OFF
     C*** END ADD ***** 06/03/04 *************************************
     C*
     C**
     C***  END   ADD  ***  10/30/97  *********************************
S06  C     SCSEL         WHENEQ    '58'
     C*
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  60
     C*
B02  C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        76  90
     C*
X02  C                   ELSE
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
B03  C     *IN60         IFEQ      *ON
     C     RSJRST        ORNE      'FL'
     C                   SETON                                        76  90
E03  C                   ENDIF                                                  *IN60=*ON/RSJRST<>FL
     C*
E02  C                   ENDIF                                                  *IN60=*ON
     C**
S07  C     SCSEL         WHENEQ    '2'
     C     SCSEL         OREQ      '3'
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
B02  C  N90FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGT     FNEXDT
     C     FNFND         ANDNE     SVFND
     C                   Z-ADD     FNFND         SVFND
     C                   CALL      GRPAPI        GRPPL
     C                   MOVE      '1'           XXCALL
     C                   MOVE      *ON           *IN09
X02  C                   ELSE
     C                   MOVE      @@CALL        XXCALL
E02  C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
E01  C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBSET SELECTIONS
     C*===============================================================
     C     ERRCK2        BEGSR
     C*
      *** BEG ADD ***** 10/10/03 *************************************
     C                   EXSR      OTHFKS
     C*** BEGIN ADD *** 06/01/09 *************************************
     C     *IN90         IFEQ      *OFF
     C     SSFQFL        ANDNE     'F'
     C     SSFQFL        ANDNE     'W'
     C     SSFQFL        ANDNE     ' '
     C                   SETON                                        7690
     C                   ENDIF
     C*** END   ADD *** 06/01/09 *************************************
      *
     C  N90SSQTYP        IFNE      *BLANK
     C***  Begin Chg  ***  05/28/09  *************************************
     C********** SSQTYP    ANDNE'W'
     C********** SSQTYP    ANDNE'Q'
     C********** SSQTYP    ANDNE'L'
     C********** SSQTYP    ANDNE'C'
     C********** SSQTYP    ANDNE'P'
     C********** SSQTYP    ANDNE'D'
     C********** SSQTYP    ANDNE'R'
     C********** SSQTYP    ANDNE'B'
     C********** SSQTYP    ANDNE'M'
     C********** SSQTYP    ANDNE'F'
     C********** SSQTYP    ANDNE'G'
     C*****Begin Add  ***  11/18/04  **************************************
     C********** SSQTYP    ANDNE'U'
     C*****End** Add  ***  11/18/04  **************************************
     C*****Begin Add  ***  10/19/07  **************************************
     C********** SSQTYP    ANDNE'S'
     C*****End** Add  ***  10/19/07  **************************************
     C                   MOVEL     SSQTYP        XXKEY
     C     QSKY          CHAIN     PQS01                              60
     C     *IN60         IFEQ      *ON
     C***  End   Chg  ***  05/28/09  *************************************
     C                   SETON                                        74  90
     C***  Begin Add  ***  05/28/09  *************************************
     C                   ENDIF
     C***  End   Add  ***  05/28/09  *************************************
     C                   ENDIF                                                  SSQTYP<>*BLANK
      *
     C     SSUWTR        IFNE      *BLANK
     C                   MOVEL     SSUWTR        XXSTAF
      *
     C     UPKY          CHAIN     PUP04                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        75  90
     C                   ENDIF
      *
     C                   ENDIF
      *** END ADD ***** 10/10/03 *************************************
     C*
     C                   ENDSR                                                  ERRCK2
     C*===============================================================
     C* GETAGT - SUBROUTINE TO GET AGENT RECORDS
     C*===============================================================
     C*
     C     GETAGT        BEGSR
     C*
     C                   MOVE      *HIVAL        ##FYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##AGT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*** BEGIN ADD *** 10/05/09 **************************************
     C     XXCLT#        IFEQ      '139'
     C                   MOVEL     EMFLXV        ZZAGT#
     C                   MOVEL     EMFLXV        XZAGT#
     C                   MOVEL     EMFLXV        YYAGT#
     C                   ELSE
     C                   MOVEL     XXNUM         XXAGT#            5
     C                   MOVEL     XXAGT#        ZZAGT#
     C                   MOVEL     XXAGT#        XZAGT#
     C                   MOVEL     XXAGT#        YYAGT#
     C                   ENDIF
     C*** END   ADD *** 10/05/09 **************************************
     C*** BEGIN DEL *** 10/05/09 **************************************
     C**********           MOVELXXNUM     ZZAGT#
     C**********           MOVELXXNUM     XZAGT#
     C**********           MOVELXXNUM     YYAGT#
     C*** END   DEL *** 10/05/09 **************************************
     C*
     C                   MOVEL(P)  ##ANAM        XXANAM
     C                   MOVEL(P)  ##ANAM        YYANAM
     C                   MOVEL(P)  ##ANAM        XZANAM
     C                   MOVEL(P)  ##PNAM        YYPNAM
     C*
     C***  BEGIN ADD  ***  12/20/06  *********************************
     C                   MOVEL(P)  ##PNAM        X6PNAM
     C                   MOVEL(P)  ##PEML        X6PEML
     C***  END   ADD  ***  12/20/06  *********************************
     C*
     C                   CLEAR                   PHN
     C                   CLEAR                   AREA
     C                   CLEAR                   PH
     C*
B01  C     ##AREA        IFNE      *ZEROS
     C                   MOVEL     ##AREA        AREA              3
E01  C                   ENDIF                                                  ##AREA<>*ZEROS
     C*
B01  C     ##PHON        IFNE      *ZEROS
     C                   MOVEL     ##PHON        PHN
     C                   MOVEA     '(   )'       PH(1)
     C                   MOVEA     AREA          PH(2)
     C                   MOVEA     PHN1          PH(7)
     C                   MOVEA     '-'           PH(10)
     C                   MOVEA     PHN2          PH(11)
E01  C                   ENDIF                                                  ##PHON<>*ZEROS
     C*
     C                   MOVEA     PH            XXPHON
     C*
     C                   ENDSR                                                  GETAGT
     C*
     C*===============================================================
     C* GETEXT - SUBROUTINE TO GET STAFF CODE EXTENSIONS
     C*===============================================================
     C*
     C     GETEXT        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXEXT#
     C     UPKY          CHAIN     PUP01                              60
     C  N60              Z-ADD     UPEXT#        XXEXT#
     C*
     C                   MOVEL(P)  XXEXT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         ZZEXT#            4
     C*
     C                   ENDSR                                                  GETEXT
     C*
      ***** BEG ADD ******* 06/02/99 **********************************
     C*===============================================================
     C* GETGRP - SUBROUTINE TO GET GROUPS USER IS AUTHORIZED TO
     C*===============================================================
     C*
     C     GETGRP        BEGSR
     C*
     C                   CLEAR                   GRP
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C     ##CO#         CHAIN     PFN01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   CALL      'SA008R'      RSTPLF
     C*
     C     ##RTCD        IFEQ      *OFF
     C                   MOVE      FNFND         GRP(X)
     C                   ADD       1             X
     C                   ENDIF                                                  ##RTCD=*OFF
     C*
     C     ##CO#         READE     PFN01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVEA(P)  GRP(1)        ##GRP1
     C                   MOVEA(P)  GRP(86)       ##GRP2
     C                   MOVEA(P)  GRP(171)      ##GRP3
     C                   MOVEA(P)  GRP(256)      ##GRP4
     C                   MOVEA(P)  GRP(341)      ##GRP5
     C                   MOVEA(P)  GRP(426)      ##GRP6
     C                   MOVEA(P)  GRP(511)      ##GRP7
     C                   MOVEA(P)  GRP(596)      ##GRP8
     C                   MOVEA(P)  GRP(681)      ##GRP9
     C                   MOVEA(P)  GRP(766)      ##GRPA
     C                   MOVEA(P)  GRP(851)      ##GRPB
     C                   MOVEA(P)  GRP(936)      ##GRPC
     C*
     C                   ENDSR                                                  GETGRP
      ***** END ADD ******* 06/02/99 **********************************
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSFND
     C                   CLEAR                   SSNAME
     C                   CLEAR                   SSFDNM
     C                   CLEAR                   SSPAD1
     C                   CLEAR                   SSPHN#
     C                   CLEAR                   SSAGT#
     C*
     C   33              Z-ADD     FNFND         SSFND
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * Check for active user authorization
      *
     C     XX1ST         IFEQ      'Y'
     C                   MOVE      'N'           XX1ST
     C                   CALL      'SA008R'      RSTPL                  99
      *
      * Do NOT check active user authorization to Group/Employer when
      * called as Search from S0009R02
      *
     C     @@CALL        IFEQ      '*'
     C                   MOVE      '2'           @@CALL
     C                   MOVE      'N'           ##XFND
     C                   MOVE      'N'           ##XEMP
     C                   MOVE      *OFF          ##RCDS
     C                   ENDIF                                                  @@CALL
      *
     C                   ENDIF                                                  XX1ST
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C*  GET GROUP INFO (IF CALLED FOR)
     C*
     C     @@RRN2        COMP      *ZERO                              3333
     C*
B01  C     @@RRN2        IFNE      *ZERO
     C     @@RRN2        CHAIN     WMFNP                              60
     C                   CLEAR                   @@RRN2
E01  C                   ENDIF                                                  @@RRN2<>*ZERO
     C*
B01  C     ##XFND        IFEQ      'Y'
B01  C     ##XEMP        OREQ      'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C*
     C     ##RCDS        IFEQ      *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C     GPKEY         CHAIN     WMFNL                              99
     C                   SETON                                        33
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   ENDIF                                                  ##RCDS
     C*
      ***** END ADD ******* 06/02/99 **********************************
E01  C                   ENDIF
     C*
     C                   MOVE      *LOVAL        KKNAME
     C  N33              MOVE      *LOVAL        KKFND
     C   33              Z-ADD     FNFND         KKFND
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C                   MOVE      *LOVAL        KKFDNM
     C                   MOVE      *LOVAL        KKSEMP
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C                   MOVE      *LOVAL        KKFEIN
     C***  END   ADD  ***  08/11/99  *********************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCPST1        KKNAME
     C*
     C                   MOVE      SCPST2        KKFND
     C***  BEGIN CHG  ***  06/02/98  *********************************
     C**********           MOVE SCPSTA    KKEMP#
     C                   MOVE      SCPST9        KKEMP#
     C***  END   CHG  ***  06/02/98  *********************************
     C                   MOVE      SCPSTB        KKDIV
     C*
B01  C     VIEW#         IFEQ      1
     C     VIEW#         OREQ      3
     C     VIEW#         OREQ      4
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C     VIEW#         OREQ      5
     C***  END   ADD  ***  08/11/99  *********************************
     C  N33              MOVE      *LOVAL        KKFND
     C   33              Z-ADD     FNFND         KKFND
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
E01  C                   ENDIF                                                  VIEW#=1/3/4
     C*
     C                   MOVEL     SCPST3        KKFDNM
     C*
     C                   MOVEL     SCPST4        KKSEMP
     C*
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C                   MOVEL     SCPST5        KKFEIN
     C***  END   ADD  ***  08/11/99  *********************************
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCNAME        KKNAME
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C                   MOVEL     SCFDNM        KKFDNM
     C                   MOVEL     SCSEMP        KKSEMP
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C                   MOVE      SCFEIN        KKFEIN
     C***  END   ADD  ***  08/11/99  *********************************
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*** BEG ADD *******  10/10/03 **********************************
     C*===============================================================
     C* KJDO   - SEARCH SUBROUTINE
     C*===============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SSFND'
     C                   MOVE      ##CO#         #1CO#
      *
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           #2CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1FND             3
      *
     C     #1FND         IFNE      *BLANKS
     C                   MOVE      #1FND         SSFND
     C                   SETON                                        72        Position cursor
     C                   ELSE
     C                   SETON                                        71        Position cursor
     C                   ENDIF                                                  #1FND<>*BLANKS
     C*** BEGIN ADD *** 07/08/09 **************************************
      *
     C     HFLD          WHENEQ    'SSQTYP'
     C                   CALL      'W2150R'                             99
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
      *
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PQS02                              99
     C                   MOVEL     QSKEY         SSQTYP
     C                   SETON                                        36         Position cursor
     C                   ELSE
     C                   SETON                                        35         Position cursor
     C                   ENDIF
     C*** END   ADD *** 07/08/09 **************************************
      *
     C     HFLD          WHENEQ    'SSUWTR'
     C                   CALL      'S0005R'      UPRMPL                 99
      *
     C     ##RRN3        IFNE      *ZEROS
     C     ##RRN3        CHAIN     PUP03                              99
     C                   MOVEL     UPSTAF        SSUWTR
     C                   SETON                                        71         Position cursor
     C                   ELSE
     C                   SETON                                        73         Position cursor
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
     C*** END ADD *******  10/10/03 **********************************
     C*===============================================================
     C* LDADDR - SUBROUTINE TO LOAD THE ADDRESS LINES
     C*===============================================================
     C*
     C     LDADDR        BEGSR
     C*
     C                   CLEAR                   DTA2
     C                   CLEAR                   CSZ
     C*
     C     ' '           CHECKR    XXADR1        XXLEN
     C                   Z-ADD     XXLEN         X                 3 0
     C*
     C     ' '           CHECKR    XXADR2        XXLEN             3 0
     C                   Z-ADD     XXLEN         Y                 3 0
     C*
     C     X             ADD       Y             Z                 3 0
     C*
     C                   MOVEA     XXADR1        DTA2
     C*
B01  C     XXADR2        IFNE      *BLANKS
     C*
B02  C     Z             IFGT      44
     C*
     C     22            SUB       Y             Y
     C                   MULT      2             Y
     C     22            ADD       1             Y
     C                   MOVEA     COMMA         DTA2(Y)
     C                   ADD       1             Y
     C                   MOVEA     *BLANK        DTA2(Y)
     C                   ADD       1             Y
     C                   MOVEA     XXADR2        DTA2(Y)
     C*
X02  C                   ELSE
     C*
     C                   ADD       1             X
     C                   MOVEA     COMMA         DTA2(X)
     C                   ADD       2             X
     C                   MOVEA     XXADR2        DTA2(X)
     C*
E02  C                   ENDIF                                                  Z>44
     C*
E01  C                   ENDIF                                                  XXADR2<>*BLANKS
     C*
     C                   MOVEA     XXCTY         CSZ
     C     ' '           CHECKR    XXCTY         XXLEN
     C     XXLEN         ADD       1             X
     C                   MOVEA     COMMA         CSZ(X)
     C                   ADD       1             X
     C                   MOVEA     *BLANK        CSZ(X)
     C                   ADD       1             X
     C                   MOVEA     XXST          CSZ(X)
     C                   ADD       3             X
     C                   MOVEA     XXZIP         CSZ(X)
     C*
     C                   MOVEA     DTA2          XDTA2            48
     C     ' '           CHECKR    XDTA2         XXLEN
     C     XXLEN         ADD       1             X
     C                   MOVEA     COMMA         DTA2(X)
     C                   ADD       2             X
     C                   MOVEA     CSZ           DTA2(X)
     C*
     C                   MOVEL     *BLANKS       XXADR1
     C                   MOVEL     *BLANKS       XXADR2
     C                   MOVEL     *BLANKS       XXCTY
     C**** BEGIN DEL **** 08/21/08 ************************************
     C*************        MOVEL*BLANKS   XXST
     C**** END   DEL **** 08/21/08 ************************************
     C                   MOVEL     *BLANKS       XXZIP
     C*
     C                   ENDSR                                                  LDADDR
      /Free
       //=============================================================
       // Location search (Process opt 10)
       //=============================================================
       Begsr LocationSrch;

       empLocRRN = *zero;
       Chain(n) alky pal01;
       If %found;
          ##call = '2';
          Exsr Opt10;
          empLocRRN = ##rrn;
       Endif;

       Endsr;
      /End-free
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      @@CALL        XXCALL            1
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     VLDOPT        IFNE      'Y'
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C     *IN34         IFEQ      *ON
     C     ##XEMP        IFEQ      'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        ANDEQ     *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C                   Z-ADD     DBRRN2        SCDBRR
     C                   ELSE
     C                   Z-ADD     @@RRN3        SCDBRR
     C                   ENDIF
     C                   ENDIF
     C*
     C***  BEGIN CHG  ***  01/20/98  *********************************
B01  C********** *IN33     IFEQ *ON
     C***  BEGIN DEL  ***  05/10/99  *********************************
     C********** VLDOPT    IFNE 'Y'
     C***  END   DEL  ***  05/10/99  *********************************
     C*
B01  C     SSFND         IFNE      *ZEROS
     C***  BEGIN ADD  ***  08/11/98  *********************************
     C     *IN34         ANDEQ     *OFF
     C***  END   ADD  ***  08/11/98  *********************************
     C***  END   CHG  ***  01/20/98  *********************************
     C   21              Z-ADD     DBRRB         SCDBRR
     C   23              Z-ADD     DBRR3B        SCDBRR
     C   24              Z-ADD     DBRR4B        SCDBRR
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C   25              Z-ADD     DBRR5B        SCDBRR
     C***  END   ADD  ***  08/11/99  *********************************
E01  C                   ENDIF                                                  SSFND<>*ZEROS
     C*
     C***  BEGIN DEL  ***  03/30/99  *********************************
     C*****BEGIN ADD  ***  01/20/98  *********************************
     C**********           ELSE
     C**********
     C**********           MOVE 'N'       VLDOPT
     C**********
     C***  END   DEL  ***  03/30/99  *********************************
     C                   ENDIF                                                  VLDOPT<>Y
     C***  END   ADD  ***  01/20/98  *********************************
     C*
     C                   MOVEL     EMNAME        SCNAME
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVEL     EMFDNM        SCFDNM
     C                   MOVEL     EMSEMP        SCSEMP
     C***  BEGIN ADD  ***  06/26/98  *********************************
     C                   Z-ADD     EMFEIN        SCFEIN
     C                   MOVEL(P)  EMSIC         SCSIC
     C***  END   ADD  ***  06/26/98  *********************************
     C***  Begin Add  ***  10/10/03  **************************************
     C                   MOVEL(P)  EMRQFL        SCRQFL
     C***  End   Add  ***  10/10/03  **************************************
     C***  BEGIN ADD  ***  03/06/06  **************************************
     C                   MOVEL(P)  EMRDFL        SCRDFL
     C***  END   ADD  ***  03/06/06  **************************************
     C*
     C                   EXSR      GETAGT
     C*
     C***  BEGIN ADD  ***  03/24/00  *********************************
     C                   Z-ADD     ##AGT#        SCAGT#
     C***  END   ADD  ***  03/24/00  *********************************
     C*
     C                   MOVEL(P)  EMFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFND
     C*
     C                   MOVEL(P)  EMEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEMP#
     C*
     C                   MOVEL(P)  EMDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXDIV
     C*
     C                   MOVEL(P)  EMSEMP        XXSEMP
     C*
B01  C     XXSEMP        IFEQ      *BLANKS
     C                   MOVEL(P)  NOVALU        XXSEMP
E01  C                   ENDIF                                                  XXSEMP=*BLANKS
     C*
     C                   MOVEL     EMNAME        XXNAME           20
     C                   MOVEL     EMFDNM        XXFDNM           20
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C                   MOVE      EMFEIN        XXFEIN            9
     C***  END   ADD  ***  08/11/99  *********************************
     C*
B01  C     XXFDNM        IFEQ      *BLANKS
     C                   MOVEL     NOVALU        XXFDNM
E01  C                   ENDIF                                                  XXFDNM=*BLANKS
     C*
     C*  LOAD MAILING ADDRESS
     C*
     C                   MOVEL     'Mail:'       SCMAIL
     C*
B01  C     EMMAD1        IFNE      *BLANKS
     C*
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2           22
     C                   MOVEL     EMMCTY        XXCTY            10
     C                   MOVEL     EMMST         XXST              2
     C                   MOVEL     EMMZIP        XXZIP             5
      *
      ***** BEG ADD ***** 03/04/03 ************************************
     C                   MOVE      EMMST         ##RRST
      ***** END ADD ***** 03/04/03 ************************************
     C*
     C                   EXSR      LDADDR
     C*
     C                   MOVEA     DTA2          SCMADR
     C*
X01  C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCMADR
     C*
E01  C                   ENDIF                                                  EMMAD1<>*BLANKS
     C*
     C*  LOAD PHYSICAL ADDRESS
     C*
     C                   MOVEL     'Phys:'       SCPHYS
     C*
B01  C     EMPAD1        IFNE      *BLANKS
     C*
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVEL     EMPCTY        XXCTY
     C                   MOVEL     EMPST         XXST
     C                   MOVEL     EMPZIP        XXZIP
     C*** BEGIN ADD *** 06/01/09 **************************************
     C     EMIUSR        IFEQ      FQIUSR
     C                   MOVEL     'FQ'          XXFQFL
     C                   ELSE
     C                   MOVEL     'WC'          XXFQFL
     C                   ENDIF
     C*** END   ADD *** 06/01/09 **************************************
      *
      ***** BEG ADD ***** 03/04/03 ************************************
     C                   MOVE      EMPST         ##RRST
      ***** END ADD ***** 03/04/03 ************************************
     C*
     C                   EXSR      LDADDR
     C*
     C                   MOVEA     DTA2          SCPADR
     C*
X01  C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCPADR
     C*
E01  C                   ENDIF                                                  EMPAD1<>*BLANKS
     C*
     C*  QUOTE STATUS
     C*
     C     QTKY          SETGT     PQT01
     C     QTKY1         READPE    PQT01                                  60
     C*
B01  C     *IN60         IFEQ      *OFF
     C     QTAGT#        ANDEQ     ##AGT#
     C*
     C                   Z-ADD     1             X
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C     QTTYPE        IFEQ      'T'
     C     QTXFER        ANDNE     *BLANKS
     C                   MOVE      QTXFER        QTTYPE
     C                   ENDIF                                                  QTTYPE=T
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C***  Begin Add  ***  05/28/09  **************************************
     C     *LIKE         DEFINE    QSKEY         XXKEY
     C                   MOVEL     QTTYPE        XXKEY
     C     QSKY          CHAIN     PQS01                              60
     C     *IN60         IFEQ      *OFF
     C     QSUSED        IFNE      *BLANK
     C                   MOVEL     QSUSED        XXKEY
     C     QSKY          CHAIN     PQS01                              60
     C                   ENDIF
     C                   MOVEL(P)  QSDESC        XXQTE
     C                   ELSE
     C***  End   Add  ***  05/28/09  **************************************
     C     QTTYPE        LOOKUP    QTET(X)                                60
     C   60              MOVEL     QTED(X)       XXQTE
     C  N60              MOVEL     *BLANKS       XXQTE
     C***  Begin Add  ***  05/28/09  **************************************
     C                   ENDIF
     C***  End   Add  ***  05/28/09  **************************************
     C***  BEGIN ADD  ***  11/06/97  *********************************
     C                   MOVEL(P)  QTTYPE        SCQTTP
     C***  END   ADD  ***  11/06/97  *********************************
     C*
X01  C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXQTE
     C***  BEGIN ADD  ***  11/06/97  *********************************
     C                   MOVEL     *BLANKS       SCQTTP
     C***  END   ADD  ***  11/06/97  *********************************
     C*
E01  C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  GET STAFF CODE EXT#'S
     C*
     C*     UNDERWRITER
     C*
     C                   MOVEL     *BLANKS       XXUEXT
     C                   MOVEL     EMUWTR        XXSTAF
     C                   EXSR      GETEXT
     C                   MOVE      ZZEXT#        XXUEXT
     C*
     C*     MANAGER
     C*
     C                   MOVEL     *BLANKS       XXMEXT
     C                   MOVEL     EMAMGR        XXSTAF
     C                   EXSR      GETEXT
     C                   MOVE      ZZEXT#        XXMEXT
     C*
     C*     CLERK
     C*
     C                   MOVEL     *BLANKS       XXCEXT
     C                   MOVEL     EMPSRV        XXSTAF
     C                   EXSR      GETEXT
     C                   MOVE      ZZEXT#        XXCEXT
     C*
     C*     REVIEWER
     C*
     C                   MOVEL     *BLANKS       XXREXT
     C                   MOVEL     EMARVW        XXSTAF
     C                   EXSR      GETEXT
     C                   MOVE      ZZEXT#        XXREXT
     C*
     C*  GET MOST RECENT INCEPTION DATE AND CORRESPONDING CX DATE
     C*
B01  C                   DO        5             I1                3 0
     C*
B02  C     EF(I1)        IFNE      *ZERO
     C*
     C                   Z-ADD     EF(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZEFDT            8 0
     C*
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZCXDT            8 0
     C*
X02  C                   ELSE
     C*
     C                   LEAVE
     C*
E02  C                   ENDIF                                                  EF,I1<>*ZERO
     C*
E01  C                   ENDDO                                                  I1->5
     C*
     C                   MOVEL     ZZEFDT        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEFDT
     C*
     C                   MOVEL     ZZCXDT        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCXDT
     C*** BEGIN ADD *** 06/01/09 **************************************
     C     EMIUSR        IFEQ      FQIUSR
     C                   MOVEL     'FQ'          XXFQFL
     C                   ELSE
     C                   MOVEL     'WC'          XXFQFL
     C                   ENDIF
     C*** END   ADD *** 06/01/09 **************************************
     C*
     C*  LOAD DATA INTO SCREEN FIELD
     C*
     C                   CLEAR                   SCDATA
     C                   CLEAR                   DTA1
     C*
B01  C                   SELECT
     C*
S01  C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEA     XXNAME        DTA1
     C                   Z-ADD     22            Z                 3 0
     C**** BEGIN ADD **** 08/21/08 ************************************
     C     EMPST         IFNE      *BLANKS
     C                   MOVEL     EMPST         XXST2             2
     C                   ELSE
     C                   MOVEL     EMMST         XXST2
     C                   END
     C                   MOVEA     XXST2         DTA1(Z)
     C                   Z-ADD     25            Z
     C**** END   ADD **** 08/21/08 ************************************
     C                   MOVEA     FED           DTA1(Z)
     C**** BEGIN CHG **** 08/21/08 ************************************
     C******************** Z-ADD43        Z
     C                   Z-ADD     46            Z
     C**** END   CHG **** 08/21/08 ************************************
     C                   MOVEA     INFO1         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
S02  C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     1
     C                   MOVEA     FED           DTA1
     C                   Z-ADD     22            Z
     C**** BEGIN ADD **** 08/21/01 ************************************
     C     EMPST         IFNE      *BLANKS
     C                   MOVEL     EMPST         XXST2             2
     C                   ELSE
     C                   MOVEL     EMMST         XXST2
     C                   END
     C                   MOVEA     XXST2         DTA1(Z)
     C                   Z-ADD     25            Z
     C**** END   ADD **** 08/21/08 ************************************
     C                   MOVEA     XXNAME        DTA1(Z)
     C**** BEGIN CHG **** 08/21/08 ************************************
     C*******************  Z-ADD43        Z
     C                   Z-ADD     46            Z
     C**** END   CHG **** 08/21/08 ************************************
     C                   MOVEA     INFO1         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
S03  C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     1
     C                   MOVEA     XXFDNM        DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     FED           DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO1         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
S04  C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     1
     C                   MOVEA     XXSEMP        DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     XXNAME        DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO1         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
     C***  BEGIN ADD  ***  08/11/99  *********************************
S04  C     VIEW#         WHENEQ    5
     C     INFO#         ANDEQ     1
     C                   MOVEA     XXFEIN        DTA1
     C                   Z-ADD     11            Z
     C                   MOVEA     EMNAME        DTA1(Z)
     C                   Z-ADD     42            Z
     C                   MOVEA     ' '           DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO1         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C***  END   ADD  ***  08/11/99  *********************************
     C*
S05  C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     2
     C                   MOVEA     XXNAME        DTA1
     C                   Z-ADD     22            Z                 3 0
     C                   MOVEA     FED           DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO2         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
S06  C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     2
     C                   MOVEA     FED           DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     XXNAME        DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO2         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
S07  C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     2
     C                   MOVEA     XXFDNM        DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     FED           DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO2         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
S08  C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     2
     C                   MOVEA     XXSEMP        DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     XXNAME        DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO2         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*
     C***  BEGIN ADD  ***  08/11/99  *********************************
S04  C     VIEW#         WHENEQ    5
     C     INFO#         ANDEQ     2
     C                   MOVEA     XXFEIN        DTA1
     C                   Z-ADD     11            Z
     C                   MOVEA     EMNAME        DTA1(Z)
     C                   Z-ADD     42            Z
     C                   MOVEA     ' '           DTA1(Z)
     C                   Z-ADD     43            Z
     C                   MOVEA     INFO2         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C***  END   ADD  ***  08/11/99  *********************************
     C*
S09  C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     3
     C                   MOVEL     XXNAME        SCDATA
     C                   MOVE      INFO3         SCDATA
     C*
S10  C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     3
     C                   MOVEL     FED           SCDATA
     C                   MOVE      INFO3         SCDATA
     C*
S11  C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     3
     C                   MOVEL     XXFDNM        SCDATA
     C                   MOVE      INFO3         SCDATA
     C*
S12  C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     3
     C                   MOVEL     SEMPNM        SCDATA
     C                   MOVE      INFO3         SCDATA
     C*
     C***  BEGIN ADD  ***  08/11/99  *********************************
S04  C     VIEW#         WHENEQ    5
     C     INFO#         ANDEQ     3
     C                   MOVEA     XXFEIN        DTA1
     C                   Z-ADD     11            Z
     C                   MOVEA     EMNAME        DTA1(Z)
     C                   Z-ADD     24            Z
     C                   MOVEA     ' '           DTA1(Z)
     C                   Z-ADD     25            Z
     C                   MOVEA     INFO3         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C***  END   ADD  ***  08/11/99  *********************************
     C*
S13  C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     4
     C                   MOVEL     XXNAME        SCDATA
     C                   MOVE      INFO4         SCDATA
     C*
S14  C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     4
     C                   MOVEL     FED           SCDATA
     C                   MOVE      INFO4         SCDATA
     C*
S15  C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     4
     C                   MOVEL     XXFDNM        SCDATA
     C                   MOVE      INFO4         SCDATA
     C*
S16  C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     4
     C                   MOVEL     SEMPNM        SCDATA
     C                   MOVE      INFO4         SCDATA
     C*
     C***  BEGIN ADD  ***  08/11/99  *********************************
S04  C     VIEW#         WHENEQ    5
     C     INFO#         ANDEQ     4
     C                   MOVEA     XXFEIN        DTA1
     C                   Z-ADD     11            Z
     C                   MOVEA     EMNAME        DTA1(Z)
     C                   Z-ADD     23            Z
     C                   MOVEA     ' '           DTA1(Z)
     C                   Z-ADD     24            Z
     C                   MOVEA     INFO4         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C***  END   ADD  ***  08/11/99  *********************************
     C*
S17  C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     5
     C                   MOVEL     XXNAME        SCDATA
     C                   MOVE      INFO5         SCDATA
     C*
S18  C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     5
     C                   MOVEL     FED           SCDATA
     C                   MOVE      INFO5         SCDATA
     C*
S19  C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     5
     C                   MOVEL     XXFDNM        SCDATA
     C                   MOVE      INFO5         SCDATA
     C*
S20  C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     5
     C                   MOVEL     SEMPNM        SCDATA
     C                   MOVE      INFO5         SCDATA
     C*
     C***  BEGIN ADD  ***  08/11/99  *********************************
S04  C     VIEW#         WHENEQ    5
     C     INFO#         ANDEQ     5
     C                   MOVEA     XXFEIN        DTA1
     C                   Z-ADD     11            Z
     C                   MOVEA     EMNAME        DTA1(Z)
     C                   Z-ADD     23            Z
     C                   MOVEA     ' '           DTA1(Z)
     C                   Z-ADD     24            Z
     C                   MOVEA     INFO5         DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C***  END   ADD  ***  08/11/99  *********************************
     C***  BEGIN ADD  ***  12/20/06  *********************************
     C* EMPLOYER DBA         PRODUCER NAME          PRODUCER EMAIL
     C*
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     6
     C                   MOVEA     XXNAME        DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     X6PNAM        DTA1(Z)
     C                   Z-ADD     46            Z
     C                   MOVEA     X6PEML        DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*_______________________________________________________________
     C* GRP   EMPLOYER#  DIV  PRODUCER NAME          PRODUCER EMAIL
     C*
     C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     6
     C                   MOVEA     FED           DTA1
     C                   Z-ADD     23            Z
     C                   MOVEA     X6PNAM        DTA1(Z)
     C                   Z-ADD     47            Z
     C                   MOVEA     X6PEML        DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*___________________________________________________________
     C* EMPLOYER FED NAME    PRODUCER NAME          PRODUCER EMAIL
     C*
     C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     6
     C                   MOVEA     XXFDNM        DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     X6PNAM        DTA1(Z)
     C                   Z-ADD     46            Z
     C                   MOVEA     X6PEML        DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*______________________________________________________________
     C* STATE EMPLR #        PRODUCER NAME          PRODUCER EMAIL
     C*
     C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     6
     C                   MOVEA     SEMPNM        DTA1
     C                   Z-ADD     22            Z
     C                   MOVEA     X6PNAM        DTA1(Z)
     C                   Z-ADD     46            Z
     C                   MOVEA     X6PEML        DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*______________________________________________________________
     C* FED ID#   EMPLOYER DBA  PRODUCER NAME          PRODUCER EMAIL
     C*
     C     VIEW#         WHENEQ    5
     C     INFO#         ANDEQ     6
     C                   MOVEA     XXFEIN        DTA1
     C                   Z-ADD     11            Z
     C                   MOVEA     EMNAME        DTA1(Z)
     C                   Z-ADD     24            Z
     C                   MOVEA     ' '           DTA1(Z)
     C                   Z-ADD     25            Z
     C                   MOVEA     X6PNAM        DTA1(Z)
     C                   Z-ADD     48            Z
     C                   MOVEA     ' '           DTA1(Z)
     C                   Z-ADD     49            Z
     C                   MOVEA     X6PEML        DTA1(Z)
     C                   MOVEA     DTA1          SCDATA
     C*_______________________________________________________________
     C***  END   ADD  ***  12/20/06  *********************************
     C*
E01  C                   ENDSL
     C*
     C***  BEGIN ADD  ***  12/20/06  *********************************
     C*
     C     INFO#         IFEQ      6
     C                   MOVE      '*'           ##PLAN
     C                   MOVE      *ZERO         ##DATE
     C                   MOVE      *ZERO         ##NWRT
     C                   MOVE      *ZERO         ##RNRT
     C                   MOVE      *ZERO         ##@RT#
     C                   ENDIF
     C*
     C***  END   ADD  ***  12/20/06  *********************************
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6
     C*===============================================================
     C*
     C* OPTION 6=ADD DIVISION
     C*
     C     OPT6          BEGSR
     C*
     C***** BEGIN ADD ****** 03/23/12 *********************************
     C                   EXSR      CHKRIU
     C     @@RTCD        IFEQ      *OFF
     C***** END ADD   ****** 03/23/12 *********************************
     C                   MOVE      'V'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C***** BEGIN ADD ****** 03/23/12 *********************************
     C                   MOVE      SCDBRR        ##RRN4            9
     C                   CALL      'W0105R'
     C                   PARM      'B'           @@MODE            1
     C                   PARM                    ##RRN4
     C                   PARM      'WMEMP'       @@FNM            10
     C                   ENDIF                                                  ##KC IFEQ
     C***** END ADD   ****** 03/23/12 *********************************
     C*
     C                   ENDSR                                                  OPT6
     C*
     C*===============================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7
     C*===============================================================
     C*
     C* OPTION 7=OWNER/OFFICER INFORMATION
     C*
     C     OPT7          BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C***  Begin Add  ***  10/10/03  **************************************
     C                   MOVE      XXCALL        SVCALL
     C*
     C***  Begin Add  ***  09/27/12  **************************************
     C     EMFQLK        IFNE      'Y'
     C***  End   Add  ***  09/27/12  **************************************
     C     @@QTE         IFEQ      'Q'
     C*** BEG DEL *** 08/17/06  **********************************     ****
     C********** SCRDFL    ANDEQ*BLANKS
     C**********           MOVE '1'       XXCALL           Display Mode
     C**********           ENDIF
     C*** END DEL *** 08/17/06  **********************************     ****
     C*** BEG ADD *** 08/17/06  **********************************     ****
     C     SCRQFL        IFEQ      'Y'
     C     SCRDFL        ANDEQ     *BLANKS
     C                   MOVE      '1'           XXCALL                         Display Mode
     C                   ELSE
     C                   MOVE      ' '           XXCALL                         Update Mode
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 08/17/06  **********************************     ****
     C***  Begin Add  ***  09/27/12  **************************************
     C                   ENDIF
     C***  End   Add  ***  09/27/12  **************************************
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C                   CALL      'W1057R'      ACDPL2                 99
     C*
     C                   ENDSR                                                  OPT7
     C*
     C*===============================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8
     C*===============================================================
     C*
     C* OPTION 8=WORK WITH POLICY YEARS
     C*
     C     OPT8          BEGSR
     C*
     C                   MOVE      XXCALL        ##MODE
     C*
     C                   CALL      'W0051R'      ACDPL1                 99
     C*
     C                   ENDSR                                                  OPT8
     C*
     C*===============================================================
     C* OPT9 - SUBROUTINE FOR OPTION 9
     C*===============================================================
     C*
     C* OPTION 9=AGENT HISTORY
     C*
     C     OPT9          BEGSR
     C*
     C                   CALL      'W0553R'      ACDPL4                 99
     C*
     C                   ENDSR                                                  OPT9
     C*
     C*===============================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10
     C*===============================================================
     C*
     C* OPTION 10=LOCATIONS
     C*
     C     OPT10         BEGSR
     C*
     C                   MOVE      XXCALL        ##MODE
     C*
     C***  Begin Add  ***  09/27/12  **************************************
     C     EMFQLK        IFNE      'Y'
     C***  End   Add  ***  09/27/12  **************************************
     C***  Begin Add  ***  10/10/03  **************************************
     C     @@QTE         IFEQ      'Q'
     C***  BEG   CHG  ***  03/06/06  **************************************
     C     SCRQFL        IFEQ      'Y'
     C     SCRDFL        ANDEQ     *BLANKS
     C                   MOVE      '1'           ##MODE                         Display Mode
     C                   ELSE
     C                   MOVE      ' '           ##MODE                         Update Mode
     C                   ENDIF
     C                   ENDIF
     C***  BEG   CHG  ***  03/06/06  **************************************
     C***  Begin Add  ***  09/27/12  **************************************
     C                   ENDIF
     C***  End   Add  ***  09/27/12  **************************************
     C***  End   Add  ***  10/10/03  **************************************
     C*
      ******** BEGIN CHG ******* 08/07/98 *****************************
     C************         CALL 'W0076R'  ACDPL3       99
     C                   CALL      'W0076R'      ACDPL6                 99
      ******** END   CHG ******* 08/07/98 *****************************
     C*
     C                   ENDSR                                                  OPT10
     C*
     C*===============================================================
     C* OPT11 - SUBROUTINE FOR OPTION 11
     C*===============================================================
     C*
     C* OPTION 11=DEPARTMENTS
     C*
     C     OPT11         BEGSR
     C*
     C                   MOVE      XXCALL        ##MODE
     C*
     C                   CALL      'W1050R'      ACDPL3                 99
     C*
     C                   ENDSR                                                  OPT11
     C*
      *
      *
      *===============================================================
      * OPT12 - Named insureds
      *===============================================================
     C     OPT12         BEGSR
      *
      ***** BEG ADD ******* 01/16/01 **********************************
     C                   Z-ADD     *ZEROS        ##RRN
      *
     C                   CALL      'W0079R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
      ***** BEG CHG ******* 06/09/05 **********************************
     C*********            PARM '1'       ##CALL  1
     C                   PARM      XXCALL        ##CALL            1
      ***** END CHG ******* 06/09/05 **********************************
     C                   PARM      SCDBRR        ##RRN             9 0
      ***** END ADD ******* 01/16/01 **********************************
      *
     C                   ENDSR                                                  OPT12
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * OPT13 - Contacts
      *===============================================================
     C     OPT13         BEGSR
      *
     C                   MOVE      *BLANK        #3CALL                         ADD/CHANGE
      ***** BEG ADD ******* 06/09/05 **********************************
     C                   MOVE      XXCALL        #3CALL
      *
     C     EMFQLK        IFEQ      'Y'
     C                   MOVE      '1'           #3CALL
     C                   ENDIF
      *
      ***** END ADD ******* 06/09/05 **********************************
     C   09              MOVE      '1'           #3CALL                         DISPLAY MODE
     C*
     C***  Begin Add  ***  09/27/12  **************************************
     C     EMFQLK        IFNE      'Y'
     C***  End   Add  ***  09/27/12  **************************************
     C***  Begin Add  ***  10/10/03  **************************************
     C     @@QTE         IFEQ      'Q'
     C***  BEG   CHG  ***  03/06/06  **************************************
     C     SCRQFL        IFEQ      'Y'
     C     SCRDFL        ANDEQ     *BLANKS
     C                   MOVE      '1'           #3CALL                         Display Mode
     C                   ELSE
     C                   MOVE      ' '           #3CALL                         Update Mode
     C                   ENDIF
     C                   ENDIF
     C***  END   CHG  ***  03/06/06  **************************************
     C***  Begin Add  ***  09/27/12  **************************************
     C                   ENDIF
     C***  End   Add  ***  09/27/12  **************************************
     C***  End   Add  ***  10/10/03  **************************************
      *
     C                   CALL      'W0077R'                             99
     C     *INKC         PARM      *BLANK        #3KC              1
     C                   PARM      *BLANK        #3KL              1
     C                   PARM                    #3CALL            1
     C                   PARM      *ZERO         #3RRN             9 0
     C                   PARM      SCDBRR        #3RRN1            9 0
      *
     C                   ENDSR                                                  OPT13
      ***** END ADD ******* 01/31/02 **********************************
      *
      *
     C*
     C*===============================================================
     C* OPT14 - SUBROUTINE FOR OPTION 14
     C*===============================================================
     C*
     C* OPTION 14=DARTS
     C*
     C     OPT14         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C                   MOVE      *BLANKS       QCQTE
     C*
B01  C     @@QTE         IFEQ      'Q'
     C                   MOVE      'Q'           QCQTE
E01  C                   ENDIF                                                  @@QTE=Q
     C*
     C*  CALL COMMENT PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   ENDSR                                                  OPT14
     C*
     C*===============================================================
     C* OPT15 - SUBROUTINE FOR OPTION 15
     C*===============================================================
     C*
     C* OPTION 15=BILLING ADDRESS
     C*
     C     OPT15         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
      ***** BEG ADD ******* 06/09/05 **********************************
      *
     C     EMFQLK        IFEQ      'Y'
     C                   MOVE      'D'           ##MODE
     C                   ENDIF
      *
      ***** END ADD ******* 06/09/05 **********************************
     C*
     C***  Begin Add  ***  09/27/12  **************************************
     C     EMFQLK        IFNE      'Y'
     C***  End   Add  ***  09/27/12  **************************************
     C***  Begin Add  ***  10/10/03  **************************************
     C     @@QTE         IFEQ      'Q'
     C***  End   Add  ***  10/10/03  **************************************
     C     SCRQFL        IFEQ      'Y'
     C     SCRDFL        ANDEQ     *BLANKS
     C                   MOVE      'D'           ##MODE                         Display Mode
     C                   ELSE
     C*** BEG CHG ****** 05/21/07 **********************************************
     C***********          MOVE 'U'       ##MODE           Update Mode
     C  N09              MOVE      'C'           ##MODE                         Update Mode
     C   09              MOVE      'D'           ##MODE
     C*** END CHG ****** 05/21/07 **********************************************
     C                   ENDIF
     C                   ENDIF
     C***  End   Add  ***  10/10/03  **************************************
     C***  Begin Add  ***  09/27/12  **************************************
     C                   ENDIF
     C***  End   Add  ***  09/27/12  **************************************
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C                   CALL      'W0050R15'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT15
     C*
     C***  BEGIN ADD  ***  10/30/97  *********************************
     C*===============================================================
     C* OPT17 - SUBROUTINE FOR OPTION 17
     C*===============================================================
     C*
     C* OPTION 17=TRANSFER QUOTED EMPLOYERS TO LIVE (QUOTE ONLY)
     C*
     C     OPT17         BEGSR
      *
      ***** BEG ADD ***** 03/04/03 ************************************
      * Check State Unemployment Flag in WDRRP/RRURFL to see if required
      * to have Unemployment # before allowing transfer
      ***** BEG ADD ******* 09/30/03 **********************************
      *
     C     EMPST         IFNE      *BLANKS
     C                   MOVE(P)   EMPST         ##RRST
      *
     C                   ELSE
      *
     C                   MOVE(P)   EMMST         ##RRST
     C                   ENDIF
      *
      ***** BEG CHG ***** 07/09/07 ****************************************
      ***** END ADD ******* 09/30/03 **********************************
     C******     RRKY      CHAINPRR01                60
     C******     *IN60     IFEQ *OFF
     C******     RRURFL    ANDEQ'Y'
     C******     EMUE#     ANDEQ*BLANKS
     C******               SETON                     90  36
     C******               ENDIF                           END 60=OFF
      ***** END CHG ***** 07/09/07 ****************************************
      ***** END ADD ***** 03/04/03 ************************************
     C***  Begin Del  ***  06/23/05  **************************************
      ****BEG*ADD *** 03/31/05 ************************************
     C**N90***** *IN60     IFEQ *OFF
     C********** RRNAIC    ANDEQ'Y'
     C********** EMNAIC    ANDEQ*BLANKS
     C**********           SETON                     90  37
     C**********           ENDIF
     C**********
     C********** *IN90     CABEQ*ON       END17
     C**********
      ****END*ADD *** 03/31/05 ************************************
     C***  End   Del  ***  06/23/05  **************************************
     C*
     C***  BEGIN ADD  ***  03/07/98  *********************************
     C*  ONLY DISPLAY MESSAGE IF AUTO ASSIGN FLAG IS 'Y'
     C*
     C     FNAUTE        IFEQ      'Y'
     C***  END   ADD  ***  03/07/98  *********************************
     C***  BEGIN ADD  ***  12/22/97  *********************************
     C*
     C*** BEG ADD ***** 06/03/04 *************************************
     C                   CLEAR                   SCQTE1
     C     SCTFRD        IFEQ      'N'
     C*** END ADD ***** 06/03/04 *************************************
     C*
     C                   CALL      'WA005R'
     C                   PARM      SCEMP#        ZZEMP#
     C                   PARM      SCDIV         ZZDIV
     C                   PARM                    ZZNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL(P)  XXEDN         XXED             15
     C     'Employer'    CAT(P)    XXED:1        SCQTE1
      *
     C*** BEG ADD ***** 06/03/04 *************************************
     C                   ELSE
     C                   MOVEL     ALLDIV        SCQTE1
     C                   ENDIF
     C*** END ADD ***** 06/03/04 *************************************
      *
     C                   CAT       XFER:1        SCQTE1
     C                   MOVEL(P)  SCQTE1        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      50            ##LENG
     C*
     C                   MOVEL(P)  ##DATA        SCQTE1
     C*
     C                   EXFMT     QTEWDW
     C*
     C***  BEGIN ADD  ***  03/07/98  *********************************
     C                   ENDIF                                                  FNAUTE=Y
     C***  END   ADD  ***  03/07/98  *********************************
     C*
     C     *INKL         IFEQ      *OFF
     C***  END   ADD  ***  12/22/97  *********************************
      *** BEG ADD ***** 06/03/04 *************************************
      *
      * If user wants to transfer all divisions, pass 'X' in ##mode;
      * otherwise, a 'T' to transfer just one division.
      *
     C     SCTFRD        IFEQ      'Y'
     C                   MOVE      'X'           ##MODE
     C                   ELSE
      *
      *** END ADD ***** 06/03/04 *************************************
     C     SCTFRD        IFEQ      'N'
     C                   MOVE      'T'           ##MODE
     C                   ENDIF
      *
      *** BEG ADD ***** 06/03/04 *************************************
     C                   ENDIF
      *** END ADD ***** 06/03/04 *************************************
     C*
     C***  BEGIN CHG  ***  03/07/98  *********************************
     C**********           CALL 'W0050R17'QTEPL        99
     C***  Begin Chg  ***  06/23/05  **************************************
     C**********           CALL 'W0050C17'QTEPL1       99
     C                   CALL      'W0050C17'    QTEPL                  99
     C***  End   Chg  ***  06/23/05  **************************************
     C***  END   CHG  ***  03/07/98  *********************************
     C*
     C***  Begin Add  ***  12/12/06  **************************************
     C     ##ERR         IFEQ      'L'
     C                   SETON                                            90
     C                   ENDIF                                                  ##ERR=L
     C***  End   Add  ***  12/12/06  **************************************
     C*
     C***  END   ADD  ***  12/22/97  *********************************
     C                   ENDIF                                                  *INKL=*OFF
     C***  END   ADD  ***  12/22/97  *********************************
     C*
      *** BEG CHG *** 03/31/05 ************************************
     C*****************    ENDSR                           OPT17
     C     END17         ENDSR                                                  OPT17
      *** END CHG *** 03/31/05 ************************************
     C***  END   ADD  ***  10/30/97  *********************************
     C*
     C*===============================================================
     C* OPT21 - SUBROUTINE FOR ADD OPT TO COMPLETE DIARY IN DARTS
     C*===============================================================
     C*
     C* OPTION 21=
     C*
     C     OPT21         BEGSR
     C*** BEG ADD *******  11/14/08  *********************************
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C*** BEG ADD *******  02/25/09  *********************************
B01  C     @@QTE         IFEQ      'Q'
     C                   MOVE      'Q'           QCQTE
E01  C                   ENDIF                                                  @@QTE=Q
     C*** END ADD *******  02/25/09  *********************************
     C*
     C*** CALL WITH FLAG FOR WORK WITH EMPLOYER BASED ON KEY LEN *****
     C*
     C                   CALL      'SC015R2'                            99
     C                   PARM                    QCKEY
     C                   PARM                    QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C*** END ADD *******  11/14/08  *********************************
     C                   ENDSR                                                  OPT21
     C*
     C*** BEG ADD *******  04/11/03  *********************************
     C*===============================================================
     C* OPT26 - SUBROUTINE FOR DIRECT DEBIT INFO
     C*===============================================================
     C     OPT26         BEGSR
     C                   MOVE      *BLANK        ##MODE
     C*** BEG CHG *******  01/13/04  *********************************
     C*******************  CALL 'W1250R02'ACDPL3       99
     C                   CALL      'W1251R'      ACDPL5                 99
     C*** END CHG *******  01/13/04  *********************************
     C                   ENDSR                                                  OPT26
     C*** END ADD *******  04/11/03  *********************************
     C*===============================================================
     C* OPT30 - SUBROUTINE FOR OPTION 30
     C*===============================================================
     C*
     C* OPTION 30=DEPARTMENT LETTERS
     C*
     C     OPT30         BEGSR
     C*
     C*                    MOVE SCEMP#    #@EMP#
     C*                    MOVE SCDIV     #@DIV
     C*
     C*                    CALL 'W4115R'
     C*                    PARM *BLANKS   ##PRDS 40
     C*                    PARM           ##CALL  1        Q=QUOTE SYSTEM
     C*                    PARM           #@EMP#  9
     C*                    PARM           #@DIV   5
     C*                    PARM 'PS'      ##DEPT  2
     C*                    PARM           ##CO#   30
     C*                    PARM SCFND     ##FND   30
     C*                    PARM *ZERO     ##FYR   30
     C*
     C                   ENDSR                                                  OPT30
     C*
      *** BEG ADD ***** 07/18/07 *************************************
     C*===============================================================
     C* OPT36 - SUBROUTINE FOR OPTION 36
     C*===============================================================
     C     OPT36         BEGSR
     C                   CALL      'W0505R'
     C                   PARM      'E'           ##CALL
     C                   PARM      SCDBRR        ##RRN
     C                   ENDSR                                                  OPT36
      *** END ADD ***** 07/18/07 *************************************
     C*===============================================================
     C* OPT40 - SUBROUTINE FOR OPTION 40
     C*===============================================================
     C*
     C* OPTION 40=
     C*
     C     OPT40         BEGSR
     C                   ENDSR                                                  OPT40
     C*** BEG ADD ***** 01/29/98 ***********************************************
      ***** BEG ADD ******* 06/22/04 **********************************
      *
      *
      *
      *===============================================================
      * OPT42 - New Business Tracking
      *===============================================================
     C     OPT42         BEGSR
      *
     C  N09              MOVE      'C'           #1MODE
     C   09              MOVE      'D'           #1MODE
      ***** BEG ADD ******* 06/09/05 **********************************
      *
     C     XXCALL        IFEQ      '1'
     C                   MOVE      'D'           #1MODE
     C                   ENDIF
      *
      ***** END ADD ******* 06/09/05 **********************************
      *
     C                   CALL      'W0050R42'                           99
     C     *INKC         PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM                    #1MODE            1
     C                   PARM                    #1RRN             9 0          WMNBP
     C                   PARM      SCDBRR        #1RRN1            9 0          WMEMP
      *
     C                   ENDSR                                                  OPT42
      *
      ***** END ADD ******* 06/22/04 **********************************
      *
     C*===============================================================
     C* OPT55 - SUBROUTINE FOR OPTION 55
     C*===============================================================
     C*
     C* OPTION 55= MID TERM ENDORSEMENTS
     C*
     C     OPT55         BEGSR
     C*
     C*
     C                   CALL      'W0051R1'     ACDPL5                 99
     C*
     C*** BEG ADD ***** 06/30/98 ***********************************************
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     *BLANKS       SVLDA
     C                   OUT       *DTAARA
     C*** END ADD ***** 06/30/98 ***********************************************
     C*
     C                   ENDSR                                                  OPT55
     C*** END ADD ***** 01/29/98 ***********************************************
      *
     C*===============================================================
     C* OPT58 - SUBROUTINE FOR OPTION 58
     C*===============================================================
     C*
     C* OPTION 58=STATE CERT
     C*
     C     OPT58         BEGSR
     C*
     C                   Z-ADD     ##CO#         XXCO#
     C                   MOVE      'C'           XXTYPE
      *** BEGIN ADD *** 01/05/00 ***********************************************
     C     Q1USER        CHAIN     PUP02                              60
      *** END ADD ***** 01/05/00 ***********************************************
     C*
     C***  BEGIN CHG  ***  12/22/97  *********************************
     C**********           CALL 'WA008R'               99
     C                   CALL      'WA029R'                             99
     C***  END   CHG  ***  12/22/97  *********************************
     C     *INKL         PARM                    ##KL
     C*
     C  NKL              CALL      'QSNDDTAQ'                           99
     C                   PARM      'WEDITQ'      XXQNM            10
     C                   PARM      '*LIBL'       XXLIB            10
     C                   PARM      80            XXFLDL            5 0
     C                   PARM                    XXDATA
     C*
     C                   ENDSR                                                  OPT58
     C*
     C***  BEGIN ADD  ***  06/09/00  *********************************
     C*===============================================================
     C* OPT60 - SUBROUTINE FOR OPTION 60
     C*===============================================================
     C*
     C     OPT60         BEGSR
     C*
     C                   CALL      'W0612R5'                            99
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR                                                  OPT60
     C***  END   ADD  ***  06/09/00  *********************************
     C*
     C*===============================================================
     C* OPT62 - SUBROUTINE FOR OPTION 62
     C*===============================================================
     C*
     C* OPTION 62=CANCELLATION HISTORY
     C*
     C     OPT62         BEGSR
     C*
     C                   CALL      'W0168R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@CALL        ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C*
     C                   ENDSR                                                  OPT62
     C*
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C*===============================================================
     C* OPT70 - SUBROUTINE FOR EFT ACCOUNT INFO
     C*===============================================================
     C     OPT70         BEGSR
     C*
     C                   CALL      'W0028R'                             99
     C                   PARM      @@CALL        ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      *BLANK        ##CLM            13
     C                   PARM      *BLANK        ##FEIN           12
     C                   PARM      *ZERO         ##SEQ#            5 0
     C                   PARM      'E'           ##TYPE            1
     C*
     C                   ENDSR                                                  OPT70
     C*** END   ADD ****** 06/29/09 ************************************
     C*
     C*===============================================================
     C* OPT80 - SUBROUTINE FOR OPTION 80
     C*===============================================================
     C*
     C* OPTION 80=CLAIMS INQUIRY
     C*
     C     OPT80         BEGSR
     C*
     C                   Z-ADD     *ZEROS        ##RRN
     C*
     C                   CALL      'W0070R'                             99
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'Z'           ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      '  '          ##SORT            2
     C*
     C                   ENDSR                                                  OPT80
      *
      *
      ***** BEG ADD ******* 09/06/06 **********************************
      *
      ***** BEG ADD ******* 01/04/10 **********************************
     C*===============================================================
     C* OPT90 - SUBROUTINE FOR OPTION 90
     C*===============================================================
     C*
     C* OPTION 90=CAMPUS CODES
     C*
     C     OPT90         BEGSR
     C*
     C                   MOVE      XXCALL        ##CALL
     C*
     C                   CALL      'W2267R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL
     C                   PARM      SCDBRR        ##RR#             9 0
     C*
     C                   ENDSR                                                  OPT90
      *** END ADD ***** 01/04/10 **************************************
      *** BEG ADD ***** 05/14/09 **************************************
     C*===============================================================
     C* OPT91 - SUBROUTINE FOR OPTION 91 (AUDIT INFORMATION)
     C*===============================================================
     C     OPT91         BEGSR
     C                   CALL      'W0050R91'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C                   ENDSR                                                  OPT91
      *** END ADD ***** 05/14/09 **************************************
      *===============================================================
      * OPT92 - MPN Provider
      *===============================================================
     C     OPT92         BEGSR
      *
     C                   MOVEL     'N'           KKELFL
     C                   MOVEL     *BLANKS       KKELID
     C     MPKY          CHAIN     PMP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     MPRRN         #4RRN
     C                   MOVE      'C'           #4MODE
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         #4RRN
     C                   MOVE      'A'           #4MODE
     C                   ENDIF                                                  MPKY
      *
     C   09              MOVE      'D'           #4MODE
      *
     C                   CALL      'W0050R92'                           99
     C     *INKC         PARM                    #4KC              1
     C                   PARM                    #4KL              1
     C                   PARM                    #4MODE            1
     C                   PARM                    #4RRN             9 0          WMMPP
     C                   PARM      SCDBRR        #4RRN1            9 0          WMEMP
     C                   PARM      Q1PGM         #4PGM            10
     C*
     C                   ENDSR                                                  OPT92
     C*
     C*
      ***** END ADD ******* 09/06/06 **********************************
     C***  Begin Add  ***  11/10/10  **************************************
     C*===============================================================
     C* OPT93 - Subroutine for option 93=HCN Program
     C*===============================================================
     C     OPT93         BEGSR
     C*
     C                   CALL      'W1191R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '1'           ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      'E'           ##MODE            1
      *
     C                   ENDSR                                                  OPT93
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        BEGSR
     C*
      *** BEG ADD ***** 10/10/03 *************************************
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *** END ADD ***** 10/10/03 *************************************
     C*
     C                   ENDSR                                                  OTHFKS
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
      *
     C                   MOVE      @@CALL        SVCALL
     C                   MOVE      *IN09         SVIN09
      *
     C     SCAUTH        IFEQ      'I'
     C     ##RCDS        ANDEQ     *ON
     C                   MOVE      *ON           *IN09
     C                   MOVE      '1'           @@CALL
     C                   ENDIF                                                  SCAUTH=I
     C*
     C                   MOVE      @@CALL        XXCALL
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
B01  C  N09FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGT     FNEXDT
     C*
B02  C     FNFND         IFNE      SVFND2
     C     SCSEL         ANDNE     '14'
     C                   Z-ADD     FNFND         SVFND2
     C                   CALL      GRPAPI        GRPPL
E02  C                   ENDIF                                                  FNFND<>SVFND2
     C*
     C                   MOVE      '1'           XXCALL
     C                   MOVE      *ON           *IN09
X01  C                   ELSE
     C                   MOVE      @@CALL        XXCALL
E01  C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C                   MOVE      SCFND         ##FND
     C                   MOVE      *ZEROS        ##FYR
     C                   MOVE      SCDBRR        ##RRN
      *
     C     EMFQLK        IFEQ      'Y'
     C                   MOVE      @@CALL        S1CALL
     C                   MOVE      XXCALL        S2CALL
     C                   MOVE      SVCALL        S3CALL
     C                   MOVE      '1'           @@CALL
     C                   MOVE      '1'           XXCALL
     C                   ENDIF
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '12'          OPT12
     C     SCSEL         CASEQ     '13'          OPT13
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '15'          OPT15
     C     SCSEL         CASEQ     '17'          OPT17
     C     SCSEL         CASEQ     '21'          OPT21
     C     SCSEL         CASEQ     '26'          OPT26
     C     SCSEL         CASEQ     '30'          OPT30
     C     SCSEL         CASEQ     '36'          OPT36
     C     SCSEL         CASEQ     '40'          OPT40
     C     SCSEL         CASEQ     '42'          OPT42
     C     SCSEL         CASEQ     '55'          OPT55
     C     SCSEL         CASEQ     '58'          OPT58
     C     SCSEL         CASEQ     '60'          OPT60
     C     SCSEL         CASEQ     '62'          OPT62
     C     SCSEL         CASEQ     '70'          OPT70
     C     SCSEL         CASEQ     '80'          OPT80
     C     SCSEL         CASEQ     '90'          OPT90
     C     SCSEL         CASEQ     '91'          OPT91
     C     SCSEL         CASEQ     '92'          OPT92
     C     SCSEL         CASEQ     '93'          OPT93
     C                   ENDCS
     C*
     C     EMFQLK        IFEQ      'Y'
     C                   MOVE      S1CALL        @@CALL
     C                   MOVE      S2CALL        XXCALL
     C                   MOVE      S3CALL        SVCALL
     C                   ENDIF
      *
     C                   MOVE      SVCALL        @@CALL
     C                   MOVE      SVIN09        *IN09
      *
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   CLOSE     WMEML9
     C                   CLOSE     WMEML
     C                   CLOSE     WMEML8
     C                   CLOSE     WMEML7
     C                   CLOSE     WMEML12
      /Free
       Close W0050F;
      /End-free
     C*
B01  C     SNGEMP        IFEQ      *ON
     C                   MOVE      *ON           *INLR
E01  C                   ENDIF                                                  SNGEMP=*ON
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
      *
      *  Save @@CALL after move to work field
     C                   MOVE      *BLANKS       S4CALL            1
     C     @@CALL        IFEQ      'M'
     C                   MOVE      @@CALL        S4CALL
     C                   MOVE      *BLANKS       @@CALL
     C                   ENDIF                                                  @@CALL="M"
     C*
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC            1
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
      *
     C     @@QTE         IFEQ      *BLANK
     C                   MOVE      'W'           @@QTE
     C                   ENDIF                                                  @@QTE=*BLANK
     C*
     C                   CLEAR                   EMP
     C*
     C                   MOVE      *OFF          XXINQ             1            LIST ALL EMPLR'S
     C*
     C     @@CALL        IFEQ      '$'
     C                   MOVE      *ON           XXINQ                          LIST INQ ONLY EMPLR'
     C                   MOVE      'L'           @@CALL
     C                   ENDIF                                                  @@CALL=$
     C*
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     *BLANKS       SVLDA
     C                   OUT       *DTAARA
     C*
     C                   MOVE      *ON           *IN12
     C                   CLEAR                   SVFND
     C                   CLEAR                   SVFND2
     C*
     C*  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
     C*
     C*  Access System Control file 'WC' record
     C*
     C                   MOVEL     'WC'          WCKEY             2
     C*
     C     WCKEY         CHAIN     SWC01                              60
     C*
     C     *IN60         IFEQ      *OFF
      *
      * If Policy Tracking module installed
      *
     C     WCFL26        IFEQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '42'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
      *  Allow option "12=Named insureds", if not QUOTE
      *
      *  Allow option "12=Named insureds" even if QUOTE
     C     WCFL16        IFEQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '12'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  @@QTE=Q
      *
      *  Enable ACH processing if ACH module or CIC Online module is installed.
      *  If WC1806 (EFT Processing Change for V10) is installed, enable
      *  option 70 instead of option 26. WC1806 is installed if WA073R
      *  is installed.
      *
     C     WCFL24        IFEQ      'Y'
     C     WCFL38        OREQ      'Y'
     C                   MOVEL     'WA073R'      OBJNAM           10
     C                   MOVEL     '*PGM'        OBJTYP            7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    OBJNAM           10
     C                   PARM                    OBJTYP            7
     C                   PARM                    OBJERR            1
     C     OBJERR        IFEQ      *OFF
     C                   MOVE      '70'          XXNMBR
     C                   ELSE
     C                   MOVE      '26'          XXNMBR
     C                   ENDIF
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*
     C     WCFL08        IFNE      *BLANK
     C*
     C     WCFL08        IFEQ      'A'
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '80'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   ENDIF                                                  WCFL08=A
     C*
     C     WCFL08        IFEQ      'C'
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '15'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '58'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '62'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   ENDIF                                                  WCFL08=C
     C*
     C                   ENDIF                                                  WCFL08<>*BLANK
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
B01  C     Q1PRMS        IFGT      6
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
E01  C                   ENDIF                                                  Q1PRMS>6
     C*
B01  C     @@CALL        IFEQ      'Z'
     C                   MOVEL     '1'           @@CALL
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '80'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
E01  C                   ENDIF                                                  @@CALL=Z
      *
     C     @@QTE         IFEQ      'Q'
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '03'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '11'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '17'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '36'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '55'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '58'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '60'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '62'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '80'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   ENDIF                                                  @@QTE=Q
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C     @@CALL        COMP      'Y'                                    31    SEARCH W/FYR
     C     @@CALL        COMP      'L'                                    32    SEARCH W/ENT/LOCATIO
     C     @@QTE         COMP      'Q'                                    41    QUOTE ENV
     C*
B01  C     *IN31         IFEQ      *ON
     C     *IN32         OREQ      *ON
     C                   MOVEL     '2'           @@CALL
E01  C                   ENDIF                                                  *IN32=*ON=*IN31
     C*
     C                   Z-ADD     *ZEROS        SVLVL
     C     Q1USER        CHAIN     PUP02                              60
     C                   Z-ADD     UPLVL         SVLVL
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*  GET EMPLOYER INFO (IF CALLED FOR)
     C*
     C                   MOVE      *OFF          SNGEMP            1
     C                   MOVE      *OFF          *IN34
     C*
     C                   CLEAR                   SVRRN3
     C*
B01  C     Q1PRMS        IFGT      7
B02  C     @@RRN3        ANDNE     *ZERO
     C     ##XEMP        OREQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C*
     C     Q1PRMS        IFGT      7
     C                   Z-ADD     @@RRN3        SVRRN3
     C                   ENDIF                                                  Q1PRMS>7
     C*
     C                   MOVE      *ON           SNGEMP
     C                   MOVE      *ON           *IN34
     C                   MOVE      'Y'           FIRST             1
      *
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '11'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '17'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C     ##XEMP        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C                   MOVEL     UPEMP#        KKEMP#
     C     ##XDIV        IFEQ      'Y'
     C                   MOVEL     UPDIV         KKDIV
     C                   Z-ADD     UPDIV         SVDIV
     C                   ENDIF                                                  ##XDIV=Y
     C*
     C                   ENDIF
      *
E01  C                   ENDIF                                                  Q1PRMS>7
     C*
     C     ##RCDS        IFEQ      *ON
     C     ##XFND        ANDEQ     'Y'
     C     *IN33         ANDEQ     *OFF
     C*
     C                   EXSR      GETGRP
     C*
     C*  CALL CL TO CREATE AN OPEN QUERY FILE OVER THE FILES
     C*
     C     OVRDB1        CAT(P)    'WMEML9':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'WMEML':0     XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'WMEML8':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'WMEML7':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'WMEML12'     XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      32            LENGTH           15 5
     C*
     C                   CALL      QRYPGM        QRYPL                  99
     C*
     C                   ENDIF                                                  ##RCDS=*ON
     C*
     C                   OPEN      WMEML9
     C                   OPEN      WMEML
     C                   OPEN      WMEML8
     C                   OPEN      WMEML7
     C                   OPEN      WMEML12
     C     @@QTE         IFNE      'Q'
     C     @@CALL        ANDNE     '2'
     C     Q1USER        CHAIN     PUP02                              60
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      UPFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C     FNPEOF        IFEQ      'S'
     C     FNPEOF        OREQ      'G'
     C                   MOVE      UPFND         KKFND
     C                   MOVE      *ALL'0'       KKEMP#
     C                   MOVE      1             KKEMP#
     C                   MOVE      *ALL'0'       KKDIV
     C     KEY1B         CHAIN     DBRCDF2                            60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
      *
     C     XXCLT#        IFEQ      '114'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '03'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C     XXCLT#        IFEQ      '025'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '90'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C     XXCLT#        IFEQ      '093'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '91'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C     XXCLT#        IFEQ      '139'
     C                   MOVEA     HD139         HD
     C                   ENDIF
     C     CLTID         IFEQ      '139'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '93'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*
     C     S4CALL        IFEQ      'M'
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '12'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  S4CALL="M"
      *
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
B01  C     SNGEMP        IFEQ      *ON
     C*
B02  C     FIRST         IFNE      'Y'
     C                   MOVE      *ON           *IN95
     C                   MOVE      'Y'           FIRST
X02  C                   ELSE
     C                   MOVE      'N'           FIRST
     C     ##XEMP        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     SVRRN3        ANDEQ     *ZEROS
     C     ##XDIV        IFEQ      'Y'
     C     KEY1B         CHAIN     WMEML                              95
     C                   ELSE
     C     KEY1X         READE     WMEML                                  95
     C                   MOVE      'Y'           FIRST
     C                   ENDIF                                                  ##XDIV=Y
     C                   ELSE
     C     @@RRN3        CHAIN     PFRCDF                             95
     C                   ENDIF
E02  C                   ENDIF                                                  FIRST<>Y
     C*
X01  C                   ELSE
     C*
     C     SSFND         IFEQ      *ZEROS
     C   21KEY1E         READE     DBRCDF                                 95
     C   22KEY1E         READE     DBRCDF2                                95
     C   23KEY1E         READE     DBRCDF3                                95
     C   24KEY1E         READE     DBRCDF4                                95
     C   25KEY1E         READE     DBRCDF5                                95
     C                   ELSE
     C   21KEY2E         READE     DBRCDFB                                95
     C   22KEY2E         READE     DBRCDF2                                95
     C   23KEY2E         READE     DBRCDF3B                               95
     C   24KEY2E         READE     DBRCDF4B                               95
     C   25KEY2E         READE     DBRCDF5B                               95
      /Free
       dbRRN  = dbRRb;
       dbRRN2 = dbRRN2;
       dbRRN3 = dbRR3b;
       dbRRN4 = dbRR4b;
       dbRRN5 = dbRR5b;
      /End-free
     C                   ENDIF                                                  SSFND=*ZEROS
     C*
E01  C                   ENDIF                                                  SNGEMP=*ON
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
B01  C     SNGEMP        IFEQ      *ON
     C*
B02  C     FIRST         IFNE      'Y'
     C                   MOVE      *ON           *IN95
     C                   MOVE      'Y'           FIRST
X02  C                   ELSE
     C     ##XEMP        IFEQ      'Y'
     C     ##XDIV        ANDNE     'Y'
     C     KEY1X         READPE    WMEML                                  95
     C                   ELSE
     C                   MOVE      'N'           FIRST
     C     @@RRN         CHAIN     PFRCDF                             95
     C                   ENDIF                                                  ##XEMP=Y
E02  C                   ENDIF                                                  FIRST<>Y
     C*
X01  C                   ELSE
     C*
     C     SSFND         IFEQ      *ZEROS
     C   21KEY1E         READPE    DBRCDF                                 95
     C   22KEY1E         READPE    DBRCDF2                                95
     C   23KEY1E         READPE    DBRCDF3                                95
     C   24KEY1E         READPE    DBRCDF4                                95
     C   25KEY1E         READPE    DBRCDF5                                95
     C                   ELSE
     C   21KEY2E         READPE    DBRCDFB                                95
     C   22KEY2E         READPE    DBRCDF2                                95
     C   23KEY2E         READPE    DBRCDF3B                               95
     C   24KEY2E         READPE    DBRCDF4B                               95
     C   25KEY2E         READPE    DBRCDF5B                               95
      /Free
       dbRRN  = dbRRb;
       dbRRN2 = dbRRN2;
       dbRRN3 = dbRR3b;
       dbRRN4 = dbRR4b;
       dbRRN5 = dbRR5b;
      /End-free
     C                   ENDIF                                                  SSFND=*ZEROS
     C*
E01  C                   ENDIF                                                  SNGEMP=*ON
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C     ##XEMP        IFEQ      'Y'
     C     ##XDIV        ANDNE     'Y'
     C     KEY1B         SETGT     WMEML                              96
     C                   ELSE
     C     SSFND         IFEQ      *ZEROS
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C   23KEY1C         SETGT     DBRCDF3                            96
     C   24KEY1D         SETGT     DBRCDF4                            96
     C   25KEY1F         SETGT     DBRCDF5                            96
     C                   ELSE
     C   21KEY2          SETGT     DBRCDFB                            96
     C   22KEY2B         SETGT     DBRCDF2                            96
     C   23KEY2C         SETGT     DBRCDF3B                           96
     C   24KEY2D         SETGT     DBRCDF4B                           96
     C   25KEY2F         SETGT     DBRCDF5B                           96
     C                   ENDIF                                                  SSFND=*ZEROS
     C                   ENDIF                                                  ##XEMP=Y/##XDIV<>Y
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C     ##XEMP        IFEQ      'Y'
     C     ##XDIV        ANDNE     'Y'
     C     KEY1B         SETLL     WMEML                              96
     C                   ELSE
     C     SSFND         IFEQ      *ZEROS
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C   23KEY1C         SETLL     DBRCDF3                            96
     C   24KEY1D         SETLL     DBRCDF4                            96
     C   25KEY1F         SETLL     DBRCDF5                            96
     C                   ELSE
     C   21KEY2          SETLL     DBRCDFB                            96
     C   22KEY2B         SETLL     DBRCDF2                            96
     C   23KEY2C         SETLL     DBRCDF3B                           96
     C   24KEY2D         SETLL     DBRCDF4B                           96
     C   25KEY2F         SETLL     DBRCDF5B                           96
     C                   ENDIF                                                  SSFND=*ZEROS
     C                   ENDIF                                                  ##XEMP=Y/##XDIV<>Y
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
B01  C                   SELECT
     C*
S01  C     *IN31         WHENEQ    *ON                                          YR SEARCH
     C                   EXSR      OPT8
     C                   Z-ADD     ##RRN         @@RRN2
     C     ##KL          CABEQ     *ON                                    90
     C*
S02  C     *IN32         WHENEQ    *ON                                          ENTITY/LOCATION SRCH
     C*
     C     ALKY          CHAIN     PAL01                              60
     C*
B02  C     *IN60         IFEQ      *OFF
     C                   EXSR      OPT10
     C                   Z-ADD     ##RRN         @@RRN2
     C     ##KL          CABEQ     *ON                                    90
X02  C                   ELSE
     C                   Z-ADD     *ZEROS        @@RRN2
E02  C                   ENDIF                                                  *IN60=*OFF
     C*
E01  C                   ENDSL
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C*** BEG CHG ****** 05/22/07 **********************************************
     C*** U1PDSC is now 100 chars, changed input values accordingly
     C********             PARM 40        STRLEN  30
     C                   PARM      100           STRLEN            3 0
     C*** END CHG ****** 05/22/07 **********************************************
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF EMPLOYERS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
     C*  CHECK EMPLOYERS FUND AGAINST SUBSET FUND
     C*
B01  C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     EMFND
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  EMFND<>SSFND<>*ZEROS
     C*
     C*  CHECK EMPLOYERS DBA AGAINST SEARCH STRING
     C*
B01  C     SSNAME        IFNE      *BLANKS
     C*
     C*** BEG CHG ****** 05/22/07 **********************************************
     C*******    LO:UP     XLATEEMNAME    U1PDSC    P
     C*** Concat EMNAME and EMNAM2 and then move to U1PDSC
     C                   MOVE      *BLANKS       U1PDSC
     C     EMNAME        CAT(P)    EMNAM2        U1PDSC
     C     LO:UP         XLATE(P)  U1PDSC        U1PDSC
     C*** END CHG ****** 05/22/07 **********************************************
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSNAME<>*BLANKS
     C*
     C*  CHECK EMPLOYERS NAME AGAINST SEARCH STRING
     C*
B01  C     SSFDNM        IFNE      *BLANKS
     C*
     C*** BEG CHG ****** 05/22/07 **********************************************
     C*********  LO:UP     XLATEEMFDNM    U1PDSC    P
     C*** Concat EMFDNM and EMFDN2 and then move to U1PDSC
     C                   MOVE      *BLANKS       U1PDSC
     C     EMFDNM        CAT(P)    EMFDN2        U1PDSC
     C     LO:UP         XLATE(P)  U1PDSC        U1PDSC
     C*** END CHG ****** 05/22/07 **********************************************
     C     LO:UP         XLATE(P)  SSFDNM        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSFDNM<>*BLANKS
     C*
     C*  CHECK EMPLOYERS PHYSICAL STREET ADDR 1 AGAINST SEARCH STRING
     C*
B01  C     SSPAD1        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  EMPAD1        U1PDSC
     C     LO:UP         XLATE(P)  SSPAD1        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPAD1<>*BLANKS
     C*
     C*  CHECK EMPLOYERS PHONE # AGAINST SEARCH STRING
     C*
     C***  BEGIN CHG  ***  12/16/97  *********************************
B01  C********** SSPHN#    IFNE *ZEROS
B01  C     SSPHN#        IFNE      *BLANKS
     C***  END   CHG  ***  12/16/97  *********************************
     C*
     C                   MOVEL(P)  EMPHN#        U1PDSC
     C                   MOVEL(P)  SSPHN#        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPHN#<>*BLANKS
     C*
     C*  CHECK EMPLOYERS AGENT AGAINST SUBSET AGENT
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
B01  C     SSAGT#        IFNE      *ZEROS
     C     SSAGT#        ANDNE     ##AGT#
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  EMFND<>SSFND<>*ZEROS
     C*
     C*** BEGIN ADD *** 06/01/09 *************************************
     C*
     C* FQ/WC SUBSET
     C*
     C     SSFQFL        IFNE      *BLANKS
     C     SSFQFL        IFEQ      'F'
     C     EMIUSR        ANDNE     FQIUSR
     C     SSFQFL        OREQ      'W'
     C     EMIUSR        ANDEQ     FQIUSR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/01/09 *************************************
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C                   MOVE      'A'           SCAUTH
      *
     C     ##XFND        IFEQ      'Y'
     C     ##XEMP        OREQ      'Y'
      *
     C     ##RCDS        IFEQ      *ON
     C*
     C                   CALL      'SA008R'      RSTPL1                 99
      *
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C*
     C     ##RTCD        IFEQ      *OFF
     C     ##AUTH        ANDEQ     'I'
     C     XXINQ         ANDEQ     *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*OFF
      *
     C                   ENDIF                                                  ##RCDS=*ON
      *
     C                   ENDIF                                                  ##XEMP=Y=##XFND
      *
      ***** END ADD ******* 06/02/99 **********************************
      *** BEG ADD ***** 10/10/03 **************************************
      *
      *  Subset on mailing address line 1
      *
     C     SSMAD1        IFNE      *BLANK
      *
     C     LO:UP         XLATE(P)  EMMAD1        U1PDSC
     C     LO:UP         XLATE(P)  SSMAD1        U2PDSC
     C                   EXSR      STRING
      *
     C                   ENDIF
      *
      *  Subset on new business/renew flag. This field is displayed in quote
      *  environment only.
      *
     C     SSNRA         IFNE      *BLANK
      *
     C     @@QTE         IFEQ      'Q'                                          IF IN QUOTE
      *
     C     SSNRA         IFEQ      'N'
     C     EMRQFL        ANDNE     ' '
     C     SSNRA         OREQ      'R'
     C     EMRQFL        ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSNRA
      *
     C                   ENDIF                                                  @@QTE = 'W'
      *
     C                   ENDIF                                                  SSNRA
      *
      *  Subset on quote type
      *
     C     SSQTYP        IFNE      *BLANK
      *
     C***  Begin Add  ***  05/28/09  *************************************
     C                   CLEAR                   QTTYPE
     C***  End   Add  ***  05/28/09  *************************************
     C                   CALL      AGTAPI        AGTPL
     C     QTKY          SETGT     PQT01
     C     QTKY1         READPE    PQT01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     SSQTYP        IFNE      QTTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSQTYP
      *
     C                   ELSE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  *IN60
     C*
     C***  Begin Add  ***  05/28/09  *************************************
     C* Check Quote Status File to determine if code is used as another
     C                   MOVEL     QTTYPE        XXKEY
     C                   CLEAR                   QSUSED
     C     QSKY          CHAIN     PQS01                              60
     C     SSQTYP        IFEQ      QSUSED
     C                   MOVE      *ON           MEETSS
     C                   ENDIF
     C***  End   Add  ***  05/28/09  *************************************
      *
     C                   ENDIF                                                  SSQTYP <> *BLANK
      *
      *  Subset on state if primary or incidental coverage
      *
     C     SSJRST        IFNE      *BLANK
      *
      *  Save RRN to restore after this subset function is done. If any views
      *  are added/changed, this if statement must be modified.
      *
     C     SSFND         IFEQ      *ZEROS
     C   21              Z-ADD     DBRRN         SVRRN
     C   22              Z-ADD     DBRRN2        SVRRN
     C   23              Z-ADD     DBRRN3        SVRRN
     C   24              Z-ADD     DBRRN4        SVRRN
     C   25              Z-ADD     DBRRN5        SVRRN
     C                   ELSE
     C   21              Z-ADD     DBRRB         SVRRN
     C   22              Z-ADD     DBRRN2        SVRRN
     C   23              Z-ADD     DBRR3B        SVRRN
     C   24              Z-ADD     DBRR4B        SVRRN
     C   25              Z-ADD     DBRR5B        SVRRN
     C                   ENDIF
      *
      *  Get state reporting division
      *
     C                   Z-ADD     EMFND         K1FND
     C                   Z-ADD     EMEMP#        K1EMP#
     C     ELKY1         SETLL     WMEML
     C     ELKY1         READE     WMEML                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         K1DIV
     C                   LEAVE
     C                   ENDIF                                                  EMSTRP = 'Y'
      *
     C     ELKY1         READE     WMEML                                  60
     C                   ENDDO                                                  *IN60
      *
      *  Get latest policy year of state reporting division
      *
     C     ELKY2         SETGT     PEL01
     C     ELKY2         READPE    PEL01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELFYR         K1FYR
     C                   ENDIF
      *
      *  Read across all divisions that have same year
      *
     C                   MOVE      *OFF          MEETSS
     C     ELKY3         SETLL     PEL02
     C     ELKY3         READE     PEL02                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     SSJRST        IFEQ      RSJRST
     C                   MOVE      *ON           MEETSS
     C                   LEAVE
     C                   ENDIF                                                  SSJRST
      *
     C                   ENDIF                                                  *IN60
      *
     C     ELKY3         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
      *  Restore WMEMP values.
      *
     C     SSFND         IFEQ      *ZEROS
     C   21              Z-ADD     SVRRN         DBRRN
     C   22              Z-ADD     SVRRN         DBRRN2
     C   23              Z-ADD     SVRRN         DBRRN3
     C   24              Z-ADD     SVRRN         DBRRN4
     C   25              Z-ADD     SVRRN         DBRRN5
     C                   ELSE
     C   21              Z-ADD     SVRRN         DBRRB
     C   22              Z-ADD     SVRRN         DBRRN2
     C   23              Z-ADD     SVRRN         DBRR3B
     C   24              Z-ADD     SVRRN         DBRR4B
     C   25              Z-ADD     SVRRN         DBRR5B
     C                   ENDIF
      *
     C     SVRRN         CHAIN     PFRCDF                             60
      *
     C     *IN22         IFEQ      *ON
     C                   Z-ADD     EMFND         K1FND
     C                   Z-ADD     EMEMP#        K1EMP#
     C                   Z-ADD     EMDIV         K1DIV
     C     ELKY2         CHAIN     DBRCDF2                            60
     C                   ENDIF
      *
     C                   ENDIF                                                  SSJRST
      *
      * Subset on underwriter's code
      *
     C     SSUWTR        IFNE      *BLANK
      *
     C     SSUWTR        IFNE      EMUWTR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDIF                                                  SSUWTR
      *** END ADD ***** 10/10/03 **************************************
     C*
     C                   ENDSR                                                  SUBSET
     C*---------------------------------------------------------------
     C* CHECK RECORD IN USE
     C*---------------------------------------------------------------
     C     CHKRIU        BEGSR
     C                   MOVE      *OFF          @@RTCD            1
     C     RIUKY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RIJOBN        JOB6A             6
     C                   MOVEL(P)  RIJOB         SCJOB
     C     SCJOB         CAT       '/':0         SCJOB
     C     SCJOB         CAT       JOB6A:0       SCJOB
     C     SCJOB         CAT       '/':0         SCJOB
     C     SCJOB         CAT       RIUSER:0      SCJOB
     C                   EXFMT     WDWMSG4
     C                   MOVE      *ON           @@RTCD
     C                   ELSE
     C                   TIME                    XSDTTM
     C                   MOVEL     'WMEMP'       RINAM
     C                   MOVE      SCDBRR        RIRRN
     C                   MOVE      ##CO#         RICO#
     C                   MOVE      SCFND         RIFND
     C                   MOVE      SCEMP#        RIEMP#
     C                   MOVE      Q1JOBN        RIJOBN
     C                   MOVE      Q1JOB         RIJOB
     C                   MOVE      Q1USER        RIUSER
     C                   MOVE      XSTIME        RITIME
     C                   MOVE      XSDATE        RIDATE
     C                   WRITE     PRI01
     C                   ENDIF                                                  *IN60
     C                   ENDSR
     C***** END ADD   ****** 03/23/12 *********************************
     C*_______________________________________________________________
     C*
** QTE
PPENDING
QQUOTED
WWRITTEN
CCANCELLED
LLOST
DDECLINED
RREINSTATED
BBOUND
MBOUND W/MONEY
TWRITTEN
FRENEWAL PENDING                                             10/10/03
GRENEWAL QUOTE                                               10/10/03
USUBMITTED                                                   11/18/04
SCLOSED                                                      08/22/07
 *** Future Use
** Headers - HD
Employer DBA         St Grp Employer#   Div  Quote Status  Agent Agent Name
Grp Employer#   Div  St Employer DBA         Quote Status  Agent Agent Name
Employer Fed Name    Grp Employer#   Div  Quote Status  Agent Agent Name
State Risk ID #      Employer DBA         Quote Status  Agent Agent Name
Fed Id #  Employer DBA                    Quote Status  Agent Agent Name
Employer DBA         Grp Employer#   Div   Incept Date   Cancel Date FQ/WC
Grp Employer#   Div  Employer DBA          Incept Date   Cancel Date FQ/WC
Employer Fed Name    Grp Employer#   Div   Incept Date   Cancel Date FQ/WC
State Emplr #        Employer DBA          Incept Date   Cancel Date FQ/WC
Fed Id #  Employer DBA                     Incept Date   Cancel Date FQ/WC
Employer DBA            Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Grp Employer#   Div     Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Employer Fed Name       Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
State Emplr #           Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Fed Id #  Employer DBA  Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Employer DBA           Agent# & Name                  Producer
Grp Employer#   Div    Agent# & Name                  Producer
Employer Fed Name      Agent# & Name                  Producer
State Emplr #          Agent# & Name                  Producer
Fed Id #  Employer DBA Agent# & Name                  Producer
Employer DBA           Agent# & Name                  Phone
Grp Employer#   Div    Agent# & Name                  Phone
Employer Fed Name      Agent# & Name                  Phone
State Emplr #          Agent# & Name                  Phone
Fed Id #  Employer DBA Agent# & Name                  Phone
Employer DBA         Producer Name           Producer email
Grp   Employer#  Div  Producer Name           Producer email
Employer Fed Name    Producer Name           Producer email
State Emplr #        Producer Name           Producer email
Fed Id#   Employer DBA  Producer Name           Producer email
** Headers 139 - HD139
Employer DBA         St Grp Employer#   Div  Quote Status Triage Agent Name
Grp Employer#   Div  St Employer DBA         Quote Status Triage Agent Name
Employer Fed Name    Grp Employer#   Div  Quote Status Triage Agent Name
State Risk ID #      Employer DBA         Quote Status Triage Agent Name
Fed Id #  Employer DBA                    Quote Status Triage Agent Name
Employer DBA         Grp Employer#   Div   Incept Date   Cancel Date FQ/WC
Grp Employer#   Div  Employer DBA          Incept Date   Cancel Date FQ/WC
Employer Fed Name    Grp Employer#   Div   Incept Date   Cancel Date FQ/WC
State Emplr #        Employer DBA          Incept Date   Cancel Date FQ/WC
Fed Id #  Employer DBA                     Incept Date   Cancel Date FQ/WC
Employer DBA            Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Grp Employer#   Div     Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Employer Fed Name       Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
State Emplr #           Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Fed Id #  Employer DBA  Uwr Ext#     Mgr Ext#     Pol Ext#     Adt Ext#
Employer DBA            Triage Agent Name             Producer
Grp Employer#   Div     Triage Agent Name             Producer
Employer Fed Name       Triage Agent Name             Producer
State Emplr #           Triage Agent Name             Producer
Fed Id #  Employer DBA  Triage Agent Name             Producer
Employer DBA            Triage Agent Name             Phone
Grp Employer#   Div     Triage Agent Name             Phone
Employer Fed Name       Triage Agent Name             Phone
State Emplr #           Triage Agent Name             Phone
Fed Id #  Employer DBA  Triage Agent Name             Phone
Employer DBA         Producer Name           Producer email
Grp   Employer#  Div  Producer Name           Producer email
Employer Fed Name    Producer Name           Producer email
State Emplr #        Producer Name           Producer email
Fed Id#   Employer DBA  Producer Name           Producer email
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   NNN                  08/18/06
1044=Delete                 YNN
1055=Display                YYY
1066=Add Division           YNN
1077=Owner/Officer          YYN
1088=Policy Years           YYY
1099=Agent History          YYN
11010=Locations             YYN
11111=Departments           YYN
11212=Named Insureds        YNN
11313=Contacts              YYN
11414=DARTS                 YYN
11515=Billing Address       YYN
11717=Transfer to Live      NNN                            07/06/06
12121=Diary Items           YYN                            11/14/08
12626=Direct debit info     NNN
13030=Dept Letters          NNN
13636=Return Funds          YYN
14242=New business tracking NNN
15555=Mid Term Endorse      YNN
15858=State Cert            YNN
16060=A/R History           YYN
16262=Cancellation Info     YYN
17070=EFT Account info      NNN                            06/29/09
18080=Claims Inquiry        YYN
19090=Campus codes          NNN                            01/04/10
19191=Audit Info            NNN                            05/14/09
19292=MPN Provider          NNN                            09/06/06
19393=HCN Program           NNN                            11/10/10
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
1                           NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YYY
211F11=Change View          YYY
212F12=Cancel               NYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
