     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H debug(*yes) BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W8700R - TRANSFER EMP QUOTES TO EMP MASTER FILE
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 05/13/25 C:65  Support: 115957 (Sunz)                         *
      *                When a quote transfers in a PEO group and the  *
      *                state reporting division no longer is on the   *
      *                new MCP policy because it transferred to the   *
      *                master PEO, flag first division to be new      *
      *                state reporting division if none exist.        *
      *                                                               *
      * 04/16/25 C:062 Ticket: 118896(129)                            *
      *                -Set the correct posting date to transfer rcd  *
      *                -Remove Quote invoice.
      *                -Clear transaction date.                       *
      *                                                               *
      * 04/15/25 C:062 Ticket: 118799(129)                            *
      *                -Remove Quote invoice.
      *                                                               *
      * 04/07/25 C:65  Support: 115957 (Sunz)                         *
      *                When transferring quote, ignore PEO rules if   *
      *                state rpt div has leasing ID of 1 in PEO grp.  *
      *                On multi-div quote transfer each div according *
      *                to its state's transfer rule.  Update the      *
      *                processing to remove the initial loop over     *
      *                transfer file and instead request next avail   *
      *                employer # after its decided the quote may     *
      *                be transferred in main loop.                   *
      *                                                               *
      * 10/03/24 C:062 Ticket: 117690(129)                            *
      *                Add active element to ThirdPartyUpdate json.   *
      *                                                               *
      * 08/14/24 C:291 Project: WC2974                                *
      *                Recompile over WDMPP File changes.             *
      *                                                               *
      * 06/24/24 C:333 Project: 146WKR                                *
      *                Recompile                                      *
      *                                                               *
      * 05/23/24 C:300 Support: 115634 (SUNZ)                         *
      *                Update WMFNP Maintenance fields while updating *
      *                Next Available Employer Number                 *
      *                                                               *
      * 05/10/24 C:079 Ticket: 115133-133                             *
      *                Update WVEMST                                  *
      *                                                               *
      * 03/13/24 C:062 Project: 146WKQ                                *
      *                Recompile                                      *
      *                                                               *
      * 04/19/24 C:062 Project: WC2663(115236(129))                   *
      *                Don't transfer Invoice if not paid.            *
      *                                                               *
      * 03/04/24 C:079 Project: WC2951                                *
      *                Recompile over WDELP changes                   *
      *                                                               *
      * 01/25/24 C:344 PROJECT: 133WTD                                *
      *               -Update GetStaticValues proc to pass in the     *
      *                group# of the payment being processed.         *
      *               -Retrieve WMEXP record and verify if the group  *
      *                is flagged to separate 3rd party batches by    *
      *                group.                                         *
      *                                                               *
      * 11/09/23 C:65  PROJECT: WC2810                                *
      *                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
      *                FIELD IN WDMPP.                                *
      *                                                               *
      * 03/21/23 C:62  Support: 107661 (126)                          *
      *                Expand the emp contact counter.                *
      *                                                               *
      * 01/06/23 C:65  Support: 105837 (Sunz)                         *
      *                Clear the renew to quote flag when policy      *
      *                transfers to live.                             *
      *                                                               *
      * 03/14/22 C:65  Support: 101377 (WC2278)                       *
      *                Contacts are only in live now, only have to    *
      *                update employer # on transfer.  Transfer the   *
      *                contact criteria file as well.                 *
      *                                                               *
      * 02/28/22 C:79  Support: Phw#95821                             *
      *                Fix Quote to Live Waiver Sequence numbering    *
      *                                                               *
      * 12/14/21 C:65  PROJECT: Phw#99842 (LUBA - Correct 95289)      *
      *                Instead of removing the first/last name key    *
      *                fields on checking for copying officer to live,*
      *                which creates duplicates of all officers,      *
      *                add location/entity ID to chain key list.      *
      *                                                               *
      * 05/21/21 C:062 Project WC2663                                 *
      *                Add invoice cloud processing                   *
      *                                                               *
      * 05/17/21 C:318 Support 95289                                  *
      *                Allow Duplicate name of Owner/officer          *
      *                                                               *
      * 05/11/21 C:136 Project: WC1986                                *
      *                Add EFT (WMEFP) to Quote Transfer              *
      *                Adjust 3rd Party for Quote Transfer            *
      *                                                               *
      * 11/18/20 C:180 Project WC2757                                 *
      *                Recompiled only.                               *
      *                                                               *
      * 07/30/20 C:65  PROJECT: Phw#90082 (Sunz)                      *
      *                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
      *                FIELD IN WDMPP.                                *
      *                                                               *
      * 05/15/20 C:062 Project: 102WIB                                *
      *                Transfer employer history records (WMEMPH)     *
      *                                                               *
      * 11/18/19 C:318 Support: 83888                                 *
      *               -Update Department Master file with live        *
      *                employer #.                                    *
      *                                                               *
      * 07/22/19 C:65  Project: SY407                                 *
      *               -Update third party payment file with live      *
      *                employer #.                                    *
      *                                                               *
      * 06/14/19 C:136 Support: 79276 (133WQP)                        *
      *                Add WXFMP1 to files that roll with waivers     *
      *                to quote.                                      *
      *                                                               *
      * 02/13/19 C:33  Project: SY406                                 *
      *               -Update application files with live employer #  *
      *                                                               *
      * 11/13/18 C:65  Project: 114WKA/Phw#74637 (Coverisk)           *
      *                Rename the fields for deleting the quote wxelp *
      *                records as they were overwriting all the data  *
      *                prior to writing to wdelp in live.             *
      *                                                               *
      * 02/21/18 C:136 Project: WC2588                                *
      *                Populate quote transfer date                   *
      *                                                               *
      * 02/08/18 C:33  Support: 64192                                 *
      *               -Account for same employer number in quote &    *
      *                live.                                          *
      *                                                               *
      * 08/10/17 C:062 Project 150WAP                                 *
      *                Add processing for C type PEOs                 *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Clear Before and After Premium                 *
    H*                                                               *
    H* 03/17/17 C:136 Project: 148WAC                                *
    H*                Add blank parm to call of W0051C31             *
    H*                Division Add midterm endorsement only affects  *
     H*                Master PEO
    H*                Add scriptura processing for this              *
    H*                                                               *
    H* 05/25/16 C:136 Support: 41657                                 *
    H*                Allow program to gracefully exit if WAQOP      *
    H*                record doesn't exist.                          *
    H*                                                               *
     H* 06/09/15 C:33  35188                                          *
     H*                Make sure copied renewals write quote tracking *
    H*                                                               *
     H* 05/22/15 C:279 Project: 114WKA                                *
     H*                Modified to delete record from quote env file  *
     H*                WXELLQ.                                        *
     H*                                                               *
     H* 02/27/14 C:242 Project: 114WGU                                *
     H*                Transfer Quote Visit Tracking to Policy Tracking
     H*                                                               *
     H* 07/11/14 C:279 Project: 111WBL                                *
     H*                Transfer Dividend Information to the Live Env  *
     H*                Then Delete it.                                *
     H*                                                               *
     H* 12/05/13 C:180 Support WA146 82907                            *
     H*                New renewal quote statuses.                    *
     H*                                                               *
     H* 07/23/13 C:65  PROJECT: Supp# 82487 (Stoneriver)              *
     H*                If the call to WA200R does not write a record  *
     H*                then write a blank record to prevent a carrier *
     H*                policy # from later being assigned through     *
     H*                editing the policy.                            *
     H*
     H* 07/19/13 C:136 Project: WC2193
     H*                Write WXELP (Unbilled Premium) on transfer
     H*                Clear estimated premium fields in WDELP
     H*
     H* 05/31/13 C:33  Redmine: 2644
     H*                Do not transfer policy if marked as renewal and there
    H*                 is a lapse in coverage.
    H*
     H* 04/01/13 C:282 Project: 147WAK                                *
     H*                 To enhance quote transfer process to process  *
     H*                 a single employer.                            *
     H*                                                               *
     H* 09/06/12 C:180 support 80655
     H*                - Implement WMCLP, WDCYP
     H*
     H* 03/04/11 C:33  Project: WC1851   Priss #111                   *
     H*               -Implement WDEFP.                               *
    H*                                                               *
     H* 02/28/11 C:220 Project: WC1932                                *
     H*                - Compiled only                                *
     H*                                                               *
     H* 01/20/11 C:246 Support: 75109 CLT- 46                         *
     H*                Fix the issue with Mass Billing renewals not   *
    H*                beening processed.                             *
    H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP FOR POLICY PAGE, ITEM 3C.       *
     H*                                                               *
     H* 12/29/10 C:136 Project: 139WPL Priss 66                       *
     H*                Correct logic when transferring HCN credit     *
     H*                to live for 'S' type PEO employers             *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Include HCN Program data.                      *
    H*                                                               *
     H* 06/21/10 C:255 Project: 114WVY                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELDS IN WDELP FOR SPLIT DEVITAIONS.          *
    H*                                                               *
     H* 05/11/10 C:61  Project: WC1473   PRISS 20                     *
     H*                Account for new Employer Payroll Class         *
    H*                Extension field.                               *
    H*                                                               *
     H* 03/12/10 C:62  Project: 25WQK                                 *
     H*                Delete WMSHP                                   *
    H*                                                               *
     H* 03/05/10 C:33  Project: WC1838   25WQW Priss #9               *
     H*                Implement WMLDP.                               *
    H*                                                               *
     H* 02/16/10 C:189 PROJECT: 132WMV(WC1872)                        *
     H*                Employer Copy in Quote                         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/04/10 C:190 PROJECT: 25WRE                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 01/22/10 C:246 Project: 139WNQ                                *
     H*                Adding default Rate for third Tretiary Agent   *
    H*                                                               *
     H* 09/28/09 C:33  Project: WC1788                                *
     H*                Update Quote Tracking if agent changed after   *
    H*                 renewal to quote.                             *
    H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
    H* 05/05/09 C:189 Support: WA135 68246                           *
    H*                WMARPP being hit on quote employer instead of  *
    H*                Live employer number causing WA200R not being  *
    H*                called and Carrier number not assigning        *
    H*                                                               *
    H* 03/03/09 C:191 Support: WA132 63663                           *
    H*                When moving contacts to LIVE check for existen *
    H*                ce of duplicate primary contact designation.   *
    H*                                                               *
    H* 01/20/09 C:136 Support: WA139 65954                           *
    H*                Do not transfer Mass Billing record to LIVE    *
    H*                if any division on the policy has been         *
    H*                previously billed in LIVE                      *
    H*                                                               *
    H* 11/05/08 C:190 PROJECT: WC1700                                *
    H*                Changed the condition for call of  W0051C31.   *
    H*                                                               *
    H* 10/22/08 C:65  PROJECT: 114WGH                                *
    H*                Revise the excluded states copy done to ensure *
    H*                records are copied properly.                   *
    H*                                                               *
    H* 05/23/08 C:65  PROJECT: 114WGH                                *
    H*                Ensure any excluded states at group level are  *
    H*                copied in if they were not on policy in quote. *
    H*                                                               *
    H* 01/04/08 C:136 SUPPORT: WA139 61673                           *
    H*                CORRECT VARIOUS ISSUES WITH RENEWAL TO QUOTE   *
    H*                - DO NOT CONTINUE LOOPING THROUGH WMEML        *
    H*                  AFTER ST RPT DIV IS FOUND                    *
    H*                - POPULATE XPEMP# FOR RENEWAL TO QUOTE         *
    H*                  (THIS CAUSED THE RENEWAL                     *
    H*                  TO QUOTE POLICIES TO BE TRANSFERRED TO       *
    H*                  LIVE WITH NO EMP# OR DIV#)                   *
    H*                                                               *
    H* 12/26/07 C:136 Support: WA045 61616                           *
    H*                Don't transfer 'Renewal to Quote' records      *
    H*                back to live.                                  *
    H*                                                               *
    H* 12/07/07 C:136 SUPPORT: WA114 61338                           *
    H*                SCHEDULED RATING FORM NOT ROLLING              *
    H*                                                               *
    H* 10/31/07 C:136 Support: WA139 60856                           *
    H*                Fix PEO processing from 47509. It changed      *
    H*                a key value after processing WMEMP which       *
    H*                prevents all other files from rolling to live. *
    H*                                                               *
    H* 10/03/07 C:33  Project: 45WFO  Back to Base                   *
     H* 06/18/99 C:61  PROJECT: 45WCE                                 *
     H*                Transfer data in Member Fee Exception File     *
     H*                WMMEP as necessary.                            *
    H*                                                               *
    H* 06/20/07 C:136 Support: WA099 58783                           *
    H*                Fix logic for transferring owner/officer info  *
    H*                                                               *
     H* 04/05/07 C:61  support wa139 57890                            *
    H*                Change so DARTS remarks for PEO groups are     *
    H*                transferred to LIVE.                           *
    H*                                                               *
     H* 03/22/07 C:180 Seq#: WA139/56478                              *
     H*                DOES NOT APPLY V7R1.                           *
    H*                                                               *
    H* 03/13/07 C:33  Seq#: WA045/54881                              *
    H*                Correct usage of WDXS*.                        *
    H*                                                               *
    H* 12/12/06 C:33  Project: WC1299                                *
    H*                Do not re-default the agent rate unless the    *
    H*                 request has been made to do so.               *
    H*                                                               *
    H* 12/06/06 C:61  Project: WC1444                                *
    H*                Call WA200R to create Carrier Policy #.        *
    H*                                                               *
    H* 10/30/06 C:33  Seq#: WA129/55606                              *
    H*                Do not xfer Employers without a Live Emp#.     *
    H*                                                               *
    H* 03/23/06 C:167 Support: WA099/47509                           *
    H*                Print the appropriate LIVE number on the log   *
    H*                 for PEO's.                                    *
    H*                                                               *
    H* 03/22/06 C:168 Support: WA114/51971                           *
    H*                Added copy for file WMRFL from quote to Live   *
    H*                                                               *
    H* 03/15/06 C:168 Project: WC1416                                *
    H*                Cleared the flag on Employer Master (EMRDFL)   *
    H*                before record gets transfer to LIVE            *
    H*                                                               *
    H* 01/31/06 C:33  Seq#: WA139/51142                              *
    H*                Include more files during transfer.            *
    H*                                                               *
    H* 11/23/05 C:33  Project: WC1281    WA135/49992                 *
    H*                Release WMFNP record on chain.                 *
    H*                                                               *
    H* 10/14/05 C:65  PROJECT: 114WGH                                *
    H*                COPY EXCLUDED STATES FOR TRANSFERRED POLICY.   *
    H*                                                               *
    H* 07/12/05 C:61  PROJECT WC1330                                 *
    H*                If PEO group, call process to create Mid-Term  *
    H*                "Add a Division" endorsements.                 *
    H*                                                               *
    H* 06/17/05 C:09  SUPPORT 46940                                  *
    H*                Assign billing start date on quote transfer    *
    H*                correctly for PEO's (re-do)                    *
    H*                                                               *
    H* 06/06/05 C:33  Seq#: WA135/47056                              *
    H*                Verification of correct employer #.            *
    H*                                                               *
    H* 05/23/05 C:53  SUPPORT 46940                                  *
    H*                Assign billing start date on quote transfer    *
    H*                correctly for PEO's.                           *
    H*                                                               *
    H* 05/10/05 C:61  SUPPORT 46984                                  *
    H*                If PEO group, then:                            *
    H*                   If division 0 then 'State reporting' = Y    *
    H*                   else 'State Reporting' = N.                 *
    H*                                                               *
    H* 04/20/05 C:53  PROJECT: WC1279                                *
    H*                PEO PROCESSING FOR S TYPE                      *
    H*                                                               *
    H* 04/18/05 C:09  PROJECT: WC1285/135WCY                         *
    H*                Transfer Additional Named Insureds from QUOTE  *
    H*                to LIVE.                                       *
    H*                                                               *
    H* 04/15/05 C:132 PROJECT 135WBZ                                 *
    H*                Re-compiled.  Added field WMEMP/EMLINK.        *
    H*                                                               *
    H* 04/12/05 C:61  PROJECT WC1283 135WBW                          *
    H*                Transfer Employee WMEEP from QUOTE to LIVE.    *
    H*                                                               *
    H* 04/07/05 C:61  PROJECT WC1281                                 *
    H*                Generate new certificate holder record in LIVE *
    H*                if PEO group.                                  *
    H*                                                               *
    H* 12/14/04 C:65  PROJECT: 114WGH/SUPP                           *
    H*                TRANSFER WDXSP RECORDS TO LIVE.                *
    H*                                                               *
    H* 03/25/04 C:61  PROJECT 114WHL                                 *
    H*                - Update New Business Tracking WMNBP with new  *
    H*                  employer number.                             *
    H*                - Update Renewal Tracking WDRTP with new       *
    H*                  employer number.                             *
    H*                                                               *
    H* 01/13/04 C:136 PROJECT: WC1116                                *
    H*                Transfer direct debit information to live.     *
    H*                                                               *
    H* 01/09/04 C:136 PROJECT: WC1115                                *
    H*                Transfer vendor history to live.               *
    H*                                                               *
    H* 10/10/03 C:33  PROJECT: WC1017                                *
    H*                Don't change QOTEMP if not zero                *
    H*                                                               *
    H* 09/16/02 C:02  SUPPORT IM081 34137                            *
    H*                ADDED WAQHP TO LOG TRANSFER REQUESTS           *
    H*                                                               *
    H* 09/12/02 C:02  SUPPORT IM081 34137                            *
    H*                FIX THIS PROGRAM TO ACCOUNT FOR OPTICAL AND    *
    H*                IMAGE FILES, AS WELL AS MULTI OBJECT DARTS FILES
    H*                                                               *
    H* 07/18/02 C:61  SUPPORT WA109 33442                            *
    H*                UPDATE all DARTS remarks for x-fer employer.   *
    H*                                                               *
    H* 01/31/02 C:61  PROJECT: WC1070                                *
    H*                Transfer Employer Contact Records.             *
    H*                                                               *
    H* 10/15/01 C:90  PROJECT: WC1054                                *
    H*                Assign a carrier policy number to each employer*
    H*                being transferred if the flag is set.          *
    H*                                                               *
    H* 08/14/01 C:133 PROJECT: SEQ#:WC109/26942                      *
    H*                Print route, head, cnt pages even if no trans  *
    H*                fers exist for execution.                      *
    H*                                                               *
    H* 05/21/01 C:90  PROJECT: SEQ#:WC109/27522                      *
    H*                Read LIVE file based on new key CO#/FND/EMP#/  *
    H*                DIV/LSN/FSN for Owner/Officer file (WMOOP).    *
    H*                                                               *
    H* 05/14/01 C:90 SEQ#: WA101/27267                               *
    H*               Add processing of WMMUP AND WMWVP for transfer. *
    H*                                                               *
    H* 09/15/00 C:65  PROJECT: SUPPORT#18745                         *
    H*                CHAIN TO AGENT HISTORY WITH THE CORRECT EMPLR  *
    H*                NUMBER FOR BILLING REQUEST FILE.               *
    H*                                                               *
    H* 06/28/99 C:90  PROJECT: WC901                                 *
    H*                DO NOT TRANSFER A/R TO THE LIVE ENV, DELETE IT.*
    H*                                                               *
    H* 04/28/99 C:90  PROJECT: WC577                                 *
    H*                Move ' ' to ELFAF to show "Not Billed"         *
    H*                                                               *
    H* 03/17/99 C:87  PROJECT: WC878 & SUPP #17050                   *
    H*                Additional Entity/Loc Detail files were not    *
    H*                being transferred into live.  Also chgd key    *
    H*                for Add Ent/Loc file to process correctly.     *
    H*                                                               *
    H* 04/29/98 C:33 SEQ#: WA091/11367                               *
    H*               DO NOT USE DEFAULT RATE # IF IT IS ZERO.        *
    H*                                                               *
    H* 03/10/98 C:33 PROJECT WC577                                   *
    H*               IF AUTO ASSIGN EMP# IS NOT 'Y', DO NOT ASSIGN   *
    H*               NEW EMP#, USE # ASSIGNED BY USER.               *
    H*                                                               *
    H* 02/25/98 C:33 PROJECT WC577                                   *
    H*               BE SURE PROPER EMPLR #S ARE BEING ASSIGNED      *
    H*                                                               *
    H* 02/18/98 C:33 PROJECT WC577                                   *
    H*              -RESTORE PRIMARY AGENT TO XXAGT# B-4 READING AGT *
    H*               QUOTE STATUS                                    *
    H*              -BE SURE NAME WILL PRINT EVEN IF MC IS NOT       *
    H*               INSTALLED.                                      *
    H*                                                               *
    H* 01/21/98 C:33 PROJECT 78WBQ                                   *
    H*               REIMPLEMENT 78WAO (INCREMENT TRANSFERS BY 10)   *
    H*               USING SYSTEM CONTROL VALUE                      *
    H*                                                               *
    H* 12/16/97 C:33 PROJECT WC577                                   *
    H*               USE AGENT DEFAULT RATE UNLESS EXPIRED           *
    H*                                                               *
    H* 11/20/97 C:33 PROJECT WC577                                   *
    H*              -ADD BILLING ADDRESS AND ENT/LOC FILES           *
    H*              -GET NEXT AVAILABLE EMP# FROM WMFNP IF NEEDED    *
    H*                                                               *
    H* 11/04/97 C:33 PROJECT WC577                                   *
    H*              -WRITE RECORD TO MC EMP MAST WHEN WRITING TO WC  *
    H*              -V3 ENHANCEMENTS                                 *
    H*                                                               *
    H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
    H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
    H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
    H*                                                               *
    H* 10/31/97 C:33 PROJECT CM023                                   *
    H*               ADDED BUSINESS FLAG AND RESET FLAGS FOR BILLING.*
    H*                                                               *
    H* 01/09/91 KWK  UPDATE AGENT QUOTE FILE (WAQTP) WITH NEW        *
    H* PROJECT AGH   EMPLOYER NUMBER AND CHANGE TYPE FROM            *
    H*               QUOTE ('Q') TO ACTIVE SYSTEM ('W')              *
    H*                                                               *
    H* 03/13/91 JJJ  CHAIN TO WAQTP WITH 'Q' STATUS                  *
    H*                                                               *
    H* 05/01/92 TMN  PROJECT WC138                                   *
    H*            1. TRANSFER THE FIRST ACTIVE AGENT ONLY IN AGENT   *
    H*               HISTORY FILE.  USE AGT# TO READ WAQTP SO THAT   *
    H*               THE CORRECT (1ST ACTIVE AGT'S) RECORD IS UPDATED*
    H*            2. LOAD SYSTEM DATE TO QTWKDT WHEN UPDATING QUOTE. *
    H*                                                               *
    H* 05/05/92 JJJ  MOVE 'W' TO QTSTAT                              *
    H*                                                               *
    H* 10/23/92 KWK  PROJECT WC138                                   *
    H*               CHANGE CODE ADDED ON 05/01/92 TO TRANSFER ALL   *
    H*               ACTIVE AGENTS IN THE AGENT HISTORY FILE, NOT    *
    H*               JUST THE FIRST ACTIVE AGENT.                    *
    H*                                                               *
    H* 07/25/94 C:11 PROJECT WC321                                   *
    H*               TRANSFER DARTS REMARKS TO THE LIVE SIDE. CHANGE *
    H*               DARTS KEY THAT HAS QUOTE EMP# TO BE LIVE EMP#.  *
    H*                                                               *
    H* 01/30/95 C:11 PROJECT WC213                                   *
    H*               RECOMPILE PGM OVER CHANGED FILE: WDELP.         *
    H*                                                               *
    H* 08/11/97 C:62 PROJECT WC577 SEQ# 8409                         *
    H*               MADE CHANGE SO THAT CMUSER DOESN'T CHANGE UPON  *
    H*               QUOTE TRANSFER.                                 *
    H*                                                               *
    H*****************************************************************
    F*
    F*  THE DAILY BATCH EXECUTES THE FOLLOWING BEFORE CALLING THIS
    F*  PROGRAM:
    F*           ADDLIBLE WOBJQBAS; ADDLIBLE WFILQBAS
    F*           OVRDBF WMEML WMEML.WFIL2
    F*           OVRDBF WDELL WDELL.WFIL2
    F*           OVRDBF WMEPL WMEPL.WFIL2
    F*           OVRDBF WMEAL WMEAL.WFIL2
    F*           OVRDBF WMAHL WMAHL.WFIL2
    F*           OVRDBF WASRL WASRL.WFIL2
    F*           OVRDBF WABLL WABLL.WFIL2
    F*
     *
     * New Business Tracking
     Fwmnbl     uf   e           k disk
     *
     * Renewal Tracking
     Fwdrtl     uf   e           k disk
     Fsmorl     if   e           k disk
     Fwmfnl     uf   e           k disk
     Fsmcol     if   e           k disk
     Fwaqol     uf   e           k disk
     Fwmemlq    uf   e           k disk
     Fwdellq    uf   e           k disk
     Fwxell     uf a e           k disk    rename(pel01:pxel00) usropn
     fwxellq    uf   e           k disk    rename(pel01:pxel0q)
     f                                     prefix(x_)
     Fwmmelq    uf   e           k disk
     *
     * Employee
     Fwmeelq    uf   e           k disk
     F*** Begin Add *** 05/15/20 *************************************
     Fwmemphlq  uf   e           k disk    rename(pemh01:pemh0q)
     F*** End   Add *** 05/15/20 *************************************
     Fwmeplqe   uf   e           k disk
     Fwmealq    uf   e           k disk
     Fwmahlq    uf   e           k disk
     Fwasrlq    uf   e           k disk
     Fwabllq    uf   e           k disk
     Fwmoolq    uf   e           k disk
     Fwmallq    uf   e           k disk
     Fwma3lq    uf   e           k disk
     Fwmanlq    uf   e           k disk
     Fwmanl     uf a e           k disk
     F                                     rename(pan01:pan02)
     Fwmeml     uf a e           k disk
     F                                     rename(pem01:pem02)
     Fwdell     uf a e           k disk
     F                                     rename(pel01:pel02)
     Fwmmel1    uf a e           k disk
     F                                     rename(pme01:pme02)
     *
     * Employee
     F*** Begin Add *** 05/15/20 *************************************
     Fwmemph    o    e             disk    rename(pemh01:pemh0p)
     F*** End   Add *** 05/15/20 *************************************
     Fwmeel     uf a e           k disk
     F                                     rename(pee01:pee02)
     Fwmeple    uf a e           k disk
     F                                     rename(pep01:pep02)
     Fwmahl     uf a e           k disk
     F                                     rename(pah01:pah02)
     Fwasrl2    uf a e           k disk
     F                                     rename(psr01:psr02)
     Fwabll     uf a e           k disk
     F                                     rename(pbl01:pbl02)
     Fwmool9    uf a e           k disk
     F                                     rename(poo01:poo02)
     *
     *  Employer Contact
     F*** begin chg ***  03/14/22  *****************************
     F*****wmeclq    uf   e           k disk
     F*****wmecl     uf a e           k disk
     F*****                                rename(pec01:pec02)
     F*****wmecl1    if   e           k disk
     F*****                                rename(pec01:pec03)
     Fwmecl     uf a e           k disk    rename(pec01:pec00)
     Fwmeclcrt  uf a e           k disk    rename(pec01:pec0c)
     F*** end chg *****  03/14/22  *****************************
     *
     *
     *  Vendor History
     Fwdvhlq    uf   e           k disk
     Fwdvhl     uf a e           k disk
     F                                     rename(pvh01:pvh02)
     *
     *  Direct Debit History
     Fwmdblq    uf   e           k disk
     Fwmdbl3    uf a e           k disk
     F                                     rename(pdb01:pdb02)
     Fwmall     uf a e           k disk
     F                                     rename(pal01:pal02)
     Fwma3l     uf a e           k disk
     F                                     rename(pa301:pa302)
     Fwdldlq    uf   e           k disk
     Fwdldl     uf a e           k disk
     F                                     rename(pld01:pld02)
     Fwaqtl7    uf a e           k disk
     Fwmaal1    if   e           k disk
     Fssylf     if   e           k disk
     fwmdvlq1   uf   e           k disk
     fwmdvl1    uf a e           k disk    rename(pdv01:pdv02)
     Fwmmulq    uf   e           k disk
     Fwmmul     uf a e           k disk
     F                                     rename(pmu01:pmu02)
     Fwmwvl1q   uf   e           k disk
     Fwmwvl1    uf a e           k disk
     F                                     rename(pwv01:pwv02)
     Fwmrpl     if a e           k disk
     Fwmvil5    uf a e           k disk    rename(pvi01:pvi05)
     Fwaqhp     o    e             disk
     Fwdxsl     if a e           k disk
     Fwdxslq    uf   e           k disk    rename(pxs01:pxs01q)
     Fwmldl1    if a e           k disk    rename(pld01:pld03)
     Fwmldl1q   uf   e           k disk    rename(pld01:pld01q)
     Fwdefl1    if a e           k disk
     Fwdefl1q   uf   e           k disk    rename(pef01:pef01q)
     Fwmshl     if a e           k disk
     Fwmshl1    uf   e           k disk    rename(psh01:psh01q)
     Fwdhcl     if a e           k disk    rename(phc01:phc00)
     Fwdhclq    uf   e           k disk    rename(phc01:phc00q)
     *
     *  Certificate Holder
     Fwmcel6    if a e           k disk
     *
     Fwaoflq    uf   e           k disk
     F                                     rename(pof01:pof00q)
     Fwaofl     if a e           k disk
     Fwddslq    uf   e           k disk
     F                                     rename(pds01:pds00q)
     Fwdillq    uf   e           k disk
     F                                     rename(pil01:pil00q)
     Fwdmplq    uf   e           k disk
     F                                     rename(pmp01:pmp00q)
     Fwmrflq    uf   e           k disk
     Fwmrfl     uf a e           k disk
     F                                     rename(rfp01:rfp02)
     fWDEHLT2   if   e           k disk
     fWZEXL     uf   e           k disk
     Fwmcll     if a e           k disk
     Fwmcllq    uf   e           k disk    rename(pcl01:pcl01q)
     Fwdcyl1    if a e           k disk
     Fwdcyl1q   uf   e           k disk    rename(pcy01:pcy01q)
     Fwxfmp1    if a e           k disk    rename(pfm01:pfm01)
     Fwxfml11   uf   e           k disk    rename(pfm01:pfm01q)
     Fwmp3l     uf   e           k disk    rename(pp301:pp300)
      *** beg chg ***** 05/11/21 **************************
     F***wmefl6    uf   e           k disk    rename(pef01:pef06) Prefix(E2:2)
     Fwmefp     uf   e             disk    rename(pef01:pef0p) Prefix(p)
     Fwmefl7    uf   e           k disk    rename(pef01:pef07) Prefix(l7)
      *** end chg ***** 05/11/21 **************************
     Fwmdpl     uf   e           k disk
      *** Beg Add ***** 01/25/24 *** 133WTD ********************************
     Fwmexl     if   e           k disk    rename(pex01:pex00)
      *** End Add ***** 01/25/24 *** 133WTD ********************************
     Fqsysprt   o    f  210        printer oflind(*inoa)
     F                                     usropn
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
    D*   DATA STRUCTURE FOR DARTS RECORD
     D                 ds
     D qmkey                         40
     D  qmco#                         3  0 overlay(qmkey)
     D  qmfnd                         3  0 overlay(qmkey:4)
     D  qmemp#                        9  0 overlay(qmkey:7)
     D  qmdiv                         5  0 overlay(qmkey:16)
     D psds          esds                  extname(szq1p)

     D/copy sPrDate
     D/copy wPrAgent
     D/copy wPrEMplr
     D/copy wPrGroup
     D*** Begin Add *** 05/21/21 *******************************************
     D/copy sPrModule
     D/copy sPrSql
     D/copy wPrAR
     D/copy wPrPmtSvc
     D*** End   Add *** 05/21/21 *******************************************
     D xxCxDt#         s                   like(ELPFDT)
      *
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
      *** beg add ***** 05/11/21 **************************
       Dcl-Ds dsWMP3L LikeRec(pp300);
       Dcl-S chrChkType Char(10);
       Dcl-S chrCo# Char(3);
       Dcl-S chrDftSRD Char(1); // Y if SRD is providing active EFT for NSRD
       Dcl-S chrEfType Char(1) Inz('E');
       Dcl-S chrEfKey  Char(30);
       Dcl-S chrEfDt Char(7);
       Dcl-S chrEnv Char(1);
       Dcl-S chrEobType Char(3);
       Dcl-S chrFnd Char(3);
       Dcl-S chrFyr Char(3);
       Dcl-S chrPre Char(1);
       Dcl-S chrRRN Char(9);
       Dcl-S hasQuoteRecord Ind;
       Dcl-S intLivRRN Packed(9);
       Dcl-S intQteRRN Packed(9);
     d/include wPrBank
      *** end add ***** 05/11/21 **************************
     D/copy SPrRtvBrnd
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##leng          s              3  0
     D #1co#           s              3
     D #1div           s              5
     D #1emp#          s              9
     D #1endt          s              8
     D #1fnd           s              3
     D @fky            s             40
     D @mod            s              1
     D @que            s             10
     D @rtc            s              1
     D @tky            s             40
     D exname          s             25
     d isRenewal       s              1n
     D k#co#           s                   like(elco#)
     D k#div           s                   like(eldiv)
     D k#emp#          s                   like(elemp#)
     D k#fnd           s                   like(elfnd)
     D k#fyr           s                   like(elfyr)
     D kkco#           s                   like(ecco#)
     D kkdiv           s                   like(ecdiv)
     D kkdiv0          s                   like(eldiv)
     D kkemp#          s                   like(ecemp#)
     D kkfnd           s                   like(ecfnd)
     D kkfsn           s                   like(ecfsn)
     D kklsn           s                   like(eclsn)
     D kkmid           s                   like(ecmid)
     D kkpcnt          s                   like(ecpcnt)
     D kkposn          s                   like(ecposn)
     D k1div           s                   like(cediv)
     D k1emp#          s                   like(ceemp#)
     D lekey           s             40
     D line            s            210
     D qekey           s             40
     D qqarea          s              3
     D qqco#           s              3
     D qqdate          s              8
     D qqdba           s             40
     D qqdiv           s              5
     D qqemp#          s              9
     D qqfbif          s              1
     D qqfein          s              9
     D qqfl01          s              1
     D qqfnd#          s              3
     D qqmad1          s             40
     D qqmad2          s             40
     D qqmco           s             10
     D qqmco#          s             20
     D qqmcty          s             25
     D qqmpfl          s              1
     D qqmst           s              2
     D qqmzip          s              9
     D qqname          s             40
     D qqnob           s             40
     D qqpad1          s             40
     D qqpad2          s             40
     D qqpcty          s             25
     D qqphn#          s              7
     D qqpst           s              2
     D qqpzip          s              9
     D qqrgn#          s              3
     D qqrtcd          s              1
     D qqstrp          s              1
     D qqsub#          s              5
     D qqvip           s              1
     D qqwcif          s              1
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D svbsdt          s                   like(elbsdt)
     D svemp#          s                   like(qoemp#)
     D svfnd           s                   like(qofnd)
     D xcname          s             60
     D xddcnt          s              1  0
     D*** Begin Chg *** 03/21/23 *******************************************
     D*xeccnt***       s              3  0
     D xeccnt          s              4  0
     D*** End   Chg *** 03/21/23 *******************************************
     D xofsn           s             15
     D xolsn           s             30
     D xpdiv           s              5  0
     D xxtemp          s                   like(elemp#)
     D xxtdiv          s                   like(eldiv)
     D xpemp#          s              9  0
     D xvdcnt          s              1  0
     D xxacnt          s              2  0
     D xxagt#          s                   like(ahagt#)
     D xxbcnt          s              1  0
     D xxcer#          s                   like(cecer#)
     D xxclas          s                   like(epclas)
     D xxco#           s                   like(qoco#)
     D xxDate          s               d   datfmt(*MDY/)
     D xxdcnt          s              2  0
     D x7Dt            s                   like(qodt)
     D xxecnt          s              1  0
     D xxefdt          s                   like(epefdt)
     D xxelid          s                   like(alelid)
     D xxemp#          s                   like(qoemp#)
     D masterEmp#      s                   Like(QOemp#) Inz(1)
     D statePeoType    s              1
     D xxfcnt          s              2  0
     D xxform          s                   like(muform)
     D xxfyr           s                   like(elfyr)
     D xxkey           s                   like(wckey)
     D xxlcnt          s              5  0
     D xxmanc          s                   like(*in60)
     D xxnaen          s             10  0
     D xxname          s                   like(anname)
     D xxncnt          s              5  0
     D xxhcnt          s              5  0
     D xxocnt          s              2  0
     D xxpcnt          s              5  0
     D xxprt           s              1
     D xxrcnt          s              5  0
     D xxrpmy          s                   like(srrpmy)
     D xxscnt          s              2  0
     D xxseq#          s                   like(anseq#)
     D xxspec          s                   like(epspec)
     D xxext           s                   like(epext)
     D xxss#           s                   like(eess#)
     D xxstat          s              1
     D xxstdv          s                   like(emdiv)
     D xxstem          s                   like(ememp#)
     D xxtime          s                   like(eatime)
     D xxtrdt          s                   like(eatrdt)
     D xxucnt          s              2  0
     D xxvcnt          s              2  0
     D countWMLDP      s              2  0
     D countWDEFP      s              2  0
     D countWMSHP      s              2  0
     D exists          s               n
     D countWDHCP      s              2  0
     D countWDXSP      s              2  0
     D saveFyr         s                   like(xxFyr)
     D countWMCLP      s              2  0
     D countWDCYP      s              2  0
     D xxwav#          s                   like(wvwav#)
     D nbrnflg         s              1
     D xxxcnt          s              5  0
     D xx3cnt          s              2  0
     D x8Date          s              8  0
     D x1emp#          s              9  0
     D x1fyr           s              3  0
     D x2emp#          s              9  0
     D sysDt7          s              7  0
     D sysDt8          s              8  0
     D sysDt           s               d
     D sysTime6        s              6  0
     D saveQTNBDT      s                   like(QTNBDT)
     D currentAgt#     s                   like(AHAGT#)
     D newAgt#         s                   like(AHAGT#)
     D isProcess       s              1n
     d @@fnd           s              3
     d @@emp#          s              9
     d intFnd          s              3  0
     d intEmp          s              9  0
     d isImdTrnsfr     s              1
     D isNoCurrAgt     s              1n
     d*** Begin Add *** 05/21/21 **************************************
     D chrJson         s          32000a   varying inz
     d strSqlStmt      s           1000a
     d decBatch#       s              9  0
     d decSysDate      s              7  0
     d decsysTime      s              6  0
     d timeStamp       s               z   Inz
     d rrnWDEAP_HLD    s              9  0
     d strLiveLib      s             10a   Varying Inz
     d strSql          s           5000a   Varying Inz
     d hasStaticValues...
     d                 s               n
     d intArCnt        s              9  0
     d intPayArCount   s              9  0
     d isOptedIn       s               n
     d isWrToArBatch   s               n
     d savQOEMP#       s              9  0
     d*** End   Add *** 05/21/21 **************************************
      //*** Beg Add 05/10/24 *****************************************
       dcl-s intStRptDiv Like(WVdiv);
      //*** End Add 05/10/24 *****************************************
     d*** Begin Chg *** 04/16/25 ***************************************
     d decInvoice#     s             10s 0
     d*** End   Chg *** 04/16/25 ***************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     d unBilPrmInst    s               n
     d  inObj          s             10
     d  inType         s             10
     d  rtnResult      s              1                                         *off = obj exists
     d ChkObjInLibl    pr                  extpgm('SRCHKOBJ')
     d  inObj                        10
     d  inType                       10
     d  rtnResult                     1                                         *off = obj exists
     d*** Begin Add *** 04/19/24 *******************************************
     D DeleteInvoice   pr                  ExtPgm('DELETEINV')
     D*** Begin Chg *** 04/16/25 *******************************************
     D*inCo#                          3a
     D*inFnd                          3a
     D*inEmp#                         9a
     D*inDiv                          5a
     D*inInv#                        10a
     D inCo#                          3a   Const
     D inFnd                          3a   Const
     D inEmp#                         9a   Const
     D inDiv                          5a   Const
     D inInv#                        10a   Const
     D*** End   Chg *** 04/16/25 *******************************************

     D count           s              5  0
     d*** End   Add *** 04/19/24 *******************************************
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqdate
     c                   parm                    @@fnd
     c                   parm                    @@emp#
     *
     *==============================================================
     *  KLIST
     *==============================================================
     *
     *   Employer Contact
     *
     C*** begin del ***  03/14/22  **********************************
     C*****ecky          klist
     C*****              kfld                    kkco#
     C*****              kfld                    kkfnd
     C*****              kfld                    kkemp#
     C*****              kfld                    kkdiv
     C*****              kfld                    kklsn
     C*****              kfld                    kkfsn
     C*****              kfld                    kkmid
     C*****              kfld                    kkposn
     C*****              kfld                    kkpcnt
     C*****
     C*****ecky1         klist
     C*****              kfld                    kkco#
     C*****              kfld                    kkfnd
     C*****              kfld                    kkemp#
     C*****              kfld                    kkdiv
     C*****              kfld                    kkpcnt
     C*** end del *****  03/14/22  **********************************
    C*
    C*   KEY LIST FOR THE COMPANY FILE
    C*
     C     coky          klist
     C                   kfld                    xxco#
    C*
    C*   KEY LIST FOR THE GROUP
    C*
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
    C*
    C*   KEY LIST FOR THE OVERRIDE FILE
    C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt
    C*
    C*   QUOTE FILE
    C*
     C     qoky          klist
     C                   kfld                    xxco#
      *
     C     keyWAQOL      klist
     C                   kfld                    xxco#
     C                   kfld                    intFnd
     C                   kfld                    intEmp
    C*
    C*   EMPLOYER MASTER FILE - ACTIVE SYSTEM
    C*
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
    C*
     C     emky1         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
      *
     C     ehkyt2        klist
     C                   kfld                    blco#
     C                   kfld                    blfnd
     C                   kfld                    blfyr
     C                   kfld                    kkemp#
    C*
    C*   GLOBAL KEY USED FOR EMP DETAIL, EMP PAYROLL, AGENT HISTORY,
    C*   A/R, BILLING SCHEDULE, & SELF RPT FILES IN THE QUOTE SYSTEM
    C*
     C     qtky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qoemp#
     C                   kfld                    qodiv
    C*
     C     qtky1         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     *
     *  New Business Tracking
     *
     C     nbky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qoemp#
     *
     *  Renewal Tracking
     *
     C     rtky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qoemp#
     *
     *  Employee Tracking
     *
     C     eeky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     C                   kfld                    xxss#
    C*
    C*   EMPLOYER MASTER FILE - ACTIVE SYSTEM
    C*
     C     emky2         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qotemp
     C                   kfld                    qodiv
    C*
     C     emky3         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    k#emp#
     C                   kfld                    k#div
    C*
     C     emkys         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
    C*
    C*   OWNER/OFFICER FILE - ACTIVE SYSTEM
    C*
     C     ooky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qoemp#
     C                   kfld                    qodiv
    C*
     C     ooky1         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C*** Begin Del *** 05/17/21 *******************************************
     C**********         kfld                    xolsn
     C**********         kfld                    xofsn
     C*** End Del *** 05/17/21 *******************************************
     C*** begin add ***  12/14/21  ***************************************
     C                   kfld                    xolsn
     C                   kfld                    xofsn
     C*** end add *****  12/14/21  ***************************************
    C*
    C*   EMPLOYER DETAIL FILE - ACTIVE & QUOTE SYSTEM
    C*
     C     elky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     *
     C     elky4         klist
     C                   kfld                    k#co#
     C                   kfld                    k#fnd
     C                   kfld                    k1emp#
     C                   kfld                    k1div
     C                   kfld                    k#fyr
     *
     C     elky5         klist
     C                   kfld                    k#co#
     C                   kfld                    k#fnd
     C                   kfld                    k#emp#
     C                   kfld                    k#div
     C                   kfld                    k#fyr
    C*
     C     elkys         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxstem
     C                   kfld                    xxstdv
     C                   kfld                    xxfyr
      *
      *   MEMBER FEE EXCEPTION - ACTIVE & QUOTE SYSTEM
      *
     C     MEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QOFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    QODIV
     C                   KFLD                    XXFYR
      *
     *  Certificate Holder
     *
     C     ceky          klist
     C                   kfld                    k#co#
     C                   kfld                    k#fnd
     C                   kfld                    k#fyr
     C                   kfld                    k1emp#
     C                   kfld                    k1div
    C*
    C*   EMPLOYER PAYROLL FILE - ACTIVE & QUOTE SYSTEM
    C*
     C     epky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     C                   kfld                    xxefdt
     C                   kfld                    xxclas
     C                   kfld                    xxspec
     C                   kfld                    xxext
    C*
    C*   ACCOUNTS RECEIVABLE FILE - ACTIVE & QUOTE SYSTEM
    C*
     C     eaky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     C                   kfld                    xxtrdt
     C                   kfld                    xxtime
    C*
    C*   AGENT HISTORY FILE - ACTIVE & QUOTE SYSTEM
    C*
     C     ahky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     C                   kfld                    xxefdt
     C                   kfld                    xxagt#
    C*
     C     ahky2         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qotemp
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
    C*
    C*   SELF RPT PAYROLL FILE - ACTIVE SYSTEM
    C*
     C     srky2         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxfyr
     C                   kfld                    qotemp
     C                   kfld                    qodiv
     C                   kfld                    xxrpmy
     C                   kfld                    xxclas
     C                   kfld                    xxefdt
    C*
    C*   SELF RPT PAYROLL FILE - QUOTE SYSTEM
    C*
     C     srky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qoemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     C                   kfld                    xxrpmy
     C                   kfld                    xxclas
     C                   kfld                    xxefdt
    C*
    C*   BILLING SCHEDULE FILE - ACTIVE SYSTEM
    C*
     C     blky2         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxfyr
     C                   kfld                    qotemp
     C                   kfld                    qodiv
    C*
    C*   BILLING SCHEDULE FILE - QUOTE SYSTEM
    C*
     C     blky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qoemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
    C*
    C*  AGENT QUOTE FILE
    C*
     C     qtky2         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    qoemp#
     C                   kfld                    qodiv
     C                   kfld                    xxstat
     C                   kfld                    xxagt#
    C*
    C*   ADDITIONAL ENTITY/LOCATION FILE
     C     alky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxelid
    C*
    C*   ADDITIONAL ENTITY/LOCATION DETAIL FILE
     C     ldky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxelid
     C                   kfld                    xxfyr
    C*
    C*   COMMENT DETAIL & SYSTEM WIDE COMMENTS FILE
    C*
     C     drky1         klist
     C                   kfld                    qekey
    C*
    C*   AGENT ASSINGMENT FILE
    C*
     C     aaky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxagt#
    C*
    C*   SYSTEM CONTROL FILE
    C*
     C     syky          klist
     C                   kfld                    xxkey
    C*
    C*   EMPLOYER ENDORSEMENT FILE
    C*
     C     muky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     C                   kfld                    xxform
    C*
    C*   WAIVER INFORMATION FILE
    C*
     C     wvky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
     C                   kfld                    xxwav#
    C*
    C*   CARRIER STATE/REPORTING FILE
    C*
     C     rpky          klist
     C                   kfld                    xxco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
    C*
    C*   RATE SET CODE FILE
    C*
     C     rsky          klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd
    C*
     C     dbky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxefdt
     *
     *   EXCLUDED STATES FILE
     *
     C     xsky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    QOEMP#
     C                   kfld                    xxfyr
    C*
     C     xsky1         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    QOTEMP
     C                   kfld                    xxfyr
     C                   kfld                    xsst
     C     xsky2         klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    x1emp#
     C                   kfld                    x1fyr
    C*
     C     ldKy3         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOEMP#
     C                   kfld                    QODIV
    C*
     C     ldKy4         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOTEMP
     C                   kfld                    QODIV
     C                   kfld                    xxfyr
    C*
     C     efKy1         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOEMP#
     C                   kfld                    QODIV
    C*
     C     efKy2         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOTEMP
     C                   kfld                    QODIV
     C                   kfld                    xxfyr
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
    C*
     C     shKyQ         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOEMP#
    C*
     C     shKy          klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOTEMP
     C                   kfld                    xxfyr
     C     CampQ         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOEMP#
     C                   kfld                    QODIV
    C*
     C     Camp          klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOTEMP
     C                   kfld                    QODIV
     C                   kfld                    CLCMPS

     C     Campky        klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOTEMP
     C                   kfld                    QODIV
     C                   kfld                    CYFYR
     C                   kfld                    CYCMPS
    C*
     C     hcKy3         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    QOEMP#
     C                   kfld                    QODIV
    C*
     C     hcKy4         klist
     C                   kfld                    xxco#
     C                   kfld                    QOFND
     C                   kfld                    xxtemp
     C                   kfld                    xxtdiv
     C                   kfld                    xxfyr
    C*
     C     anky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxname
     C                   kfld                    xxseq#

     C     rfky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr
    C*
     C     EXKY          KLIST
     C                   KFLD                    QOCO#
     C                   KFLD                    QOFND
     C                   KFLD                    QOEMP#
     C                   KFLD                    QODIV
      *
     C     EMKY#         KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXOFND
     C                   KFLD                    EXOEMP
     C                   KFLD                    EXODIV

      * Dividend Information File - Active & Quote System
     C     dvky          klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
     C                   kfld                    qodiv
     C                   kfld                    xxfyr

      * Visit Tracking File - Active & Quote System
     C     vstKey        klist
     C                   kfld                    xxco#
     C                   kfld                    qofnd
     C                   kfld                    xxemp#
    C*_______________________________________________________________
    C*
      /Free
       Exec Sql Set Option Commit = *None;
       If %parms >= 3;
          If @@Fnd <> ' ' And @@Emp# <> ' ';
             intFnd = %Int(@@Fnd);
             intEmp = %Int(@@Emp#);
             isImdTrnsfr = 'Y';
          Else;
             isImdTrnsfr = ' ';
          EndIf;
       EndIf;
      /End-Free

    C*  WCMC Installed determination
    C*
     C                   call      'SA001R'
     C                   parm                    qqwcif
     C                   parm                    qqfbif
     C                   parm                    qqfl01
    C*
     C                   if        qqfbif = 'Y'
     C                             and qqfl01 = 'Y'
     C                   move      *on           xxmanc
     C                   else
     C                   move      *off          xxmanc
     C                   endif
    C*
     C                   eval      xxnaen = *zeros
    C*
    C*  GET SYSTEM CONTROL VALUES
    C*
     C                   movel     'WC'          xxkey
     C     syky          chain     swc01                              60
     C                   move      wcal01        xxnaen
    C*
     C                   if        xxnaen = *zero
     C                   eval      xxnaen = 1
     C                   endif
    C*
     C                   move      qqco#         xxco#
    C*
     C                   move      qqdate        x8Date
     C                   eval      x7Dt   = DtCYMD(x8Date)
     C                   eval      xxDate = CvtToDate(x8Date)
     C                   eval      sysDt  = SystemDate
     C                   eval      sysDt7 = DateTo7(sysDt)
     C                   eval      sysDt8 = DateTo8(sysDt)
    C*
     C                   time                    dsdttm
     C                   move      *all'_'       line
     C*
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
      /free
       // Find out if unbilled premium is installed
       inObj = 'WXELP';
       inType = '*FILE';
       Callp ChkObjInLibl (inObj:inType:rtnResult);
       If rtnResult = *off;
          unBilPrmInst = *on;
       Endif;
      /end-free
    C*_______________________________________________________________
    C*
    C*    READ THE QUOTE FILE & ASSIGN NEW EMPLOYER NUMBERS
    C*
     c                   If        isImdTrnsfr = 'Y'
     c     keyWAQOL      chain     pqo01                              60
     c                   Dow       QOitfr <> 'Y' and not(*in60)
     c     keyWAQOL      reade     pqo01                                  60
     c                   enddo
     C                   Else
     C     qoky          chain     pqo01                              60
     C                   EndIf
    C*
     C                   if        *in60 = *on
     C                   open      qsysprt
     C                   move      '0'           xxprt
     C     orky          chain     por01                              71
     C                   if        *in71 = *off
     C                   except    route
     C                   movel(p)  ortitl        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
     C                   movel(p)  ##data        ortitl
     C                   endif
     C                   except    head
     C                   except    norecs
     C                   endif
    C*
     C*** begin chg ***  04/07/25  ************************************************
     C******             dow       *in60 = *off
    C*
     C******             if        *in11 = *off
     C******             exsr      sr1st
     C******             endif
    C*
    C*   CHECK FOR GROUP BREAK  - GET NEXT AVAIL EMP# IN GROUP
    C*
     C******             if        svfnd <> qofnd
     C******             exsr      grpbrk
     C******             endif
      /free
       // Get the type of Peo if Combined peo
       //*****If FNpeoF = 'C';
       //*****   Setll qtKy pel01;
       //*****   Reade qtKy pel01;
       //*****   statePeoType = GetStatePeoFlag(ELfnd:ELrtCd:ELpfDt:ELco#);
       //*****EndIf;
      /end-free
    C*
    C*   CHECK FOR EMPLOYER BREAK - DIFF DIVISIONS UNDER THE SAME EMP#
    C*                              MUST BE ASSIGNED SAME EMP#
    C*
     C******             if        svemp# <> qoemp#
    C*
     C******             if        fnaute = 'Y'
     C******                       and qotemp = *zero
    C*
     C******             if        svfnd = qofnd
    C*
     C*****fnky          chain     pfn01                              60
    C*
     C******             if        *in60 = *off
     C******             eval      xxemp# = fnnaen
     C******             eval      fnnaen = fnnaen + xxnaen
      *** Beg Add ***** 05/23/24 ***********************
      /free
       //***** FNpgm  = Q1pgm;
       //***** FNuser = Q1user;
       //***** FNtime = Timeto6(SystemTime());
       //***** FNdt   = Dateto7(SystemDate());
      /end-free
      *** End Add ***** 05/23/24 ***********************
     C******             update    pfn01
     C******             endif
    C*
     C******             endif
    C*
     C******             endif
    C*
     C******             eval      svemp# = qoemp#
    C*
     C******             endif
    C*
    C*  LOAD NEW EMPLOYER # INTO WAQOP & UPDATE TRANSFER FILE
    C*
     C******             if        fnaute = 'Y'
     C******                       and qotemp = *zero
     C******             eval      qotemp = xxemp#
     C******             endif
    C*
     C******             update    pqo01
    C*
    C*  READ NEXT TRANSFER EMPLOYER
    C*
     C******             If        isImdTrnsfr = 'Y'
     C*****keyWAQOL      reade     pqo01                                  60
     c******             Dow       QOitfr <> 'Y' and not(*in60)
     c*****keyWAQOL      reade     pqo01                                  60
     c******             enddo
     C******             Else
     C*****qoky          reade     pqo01                                  60
     C******             EndIf
    C*
     C******             enddo
    C*_______________________________________________________________
    C*
    C*    READ THE QUOTE FILE, CHECK TO SEE IF EMPLOYER ALREADY
    C*    EXISTS IN ACTIVE FILE, IF IT DOES NOT EXIST WRITE IT,
    C*    DELETE QUOTE TRANSFER RECORD & DELETE QUOTE DETAIL RECORD
    C*
     C******             eval      svfnd = *zero
    C*
     C******             If        isImdTrnsfr = 'Y'
     C*****keyWAQOL      chain     pqo01                              60
     c*****              Dow       QOitfr <> 'Y' and not(*in60)
     c*****keyWAQOL      reade     pqo01                                  60
     c******             enddo
     C******             Else
     C*****qoky          chain     pqo01                              60
     C******             EndIf
     C*
     C                   If        *in60 = *off
     C                   Exsr      sr1st
     C                   Endif
     C*** end chg *****  04/07/25  ************************************************
     C*** begin add ***  05/13/25  ************************************************
     C                   Clear                   svStfnd           3 0
     C                   Clear                   svStemp#          9 0
     C*** end add *****  05/13/25  ************************************************
    C*
     C                   dow       *in60 = *off

      /Free
       isProcess = *on;
      /End-free

     C*** begin del ***  04/07/25  ************************************************
     C******             if        QOTEMP = *zero
     C******             goto      NextXFer
      /Free
       //******   isProcess = *off;
      /End-free
     C******             endif
     C*** end del *****  04/07/25  ************************************************
    C*
    C*   CHECK FOR GROUP BREAK  - PRINT NEW PAGE
    C*
     C                   if        svfnd <> qofnd
    C*
     C     fnky          chain(n)  pfn01                              60
    C*
     C                   if        *in60 = *on
     C                   eval      svfnd = 0
     C                   clear                   fnname
     C                   eval      fnfnd = 0
     C                   else
     C                   eval      svfnd = fnfnd
     C                   endif
    C*
     C                   if        *in60 = *off
     C                   if        fnanam <> *blank
     C                   movel(p)  fnanam        ##data
     C                   else
     C                   movel(p)  coname        ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      60            ##leng
    C*
     C                   movel     ##data        xcname
     C                   endif
    C*
     C                   except    head
    C*
     C                   endif
     C*** begin add ***  04/07/25  ************************************************
     C* Get the type of Peo if Combined peo
        If FNpeoF = 'C';
           Setll qtKy pel01;
           Reade qtKy pel01;
           statePeoType = GetStatePeoFlag(ELfnd:ELrtCd:ELpfDt:ELco#);
        EndIf;
     C                   If        QOTEMP = *zero and fnaute <> 'Y'
     C                   Goto      NextXFer
     C                   Eval      isProcess = *off
     C                   Endif
     C*** end add *****  04/07/25  ************************************************
     C*
     C                   Clear                   EMrqFl
     C     emky2         chain(n)  pem01                              62
     C                   Eval      isRenewal = EMrqFl = 'Y'
     C*** begin add ***  04/07/25  ************************************************
     C                   Move      EMLSID        xxLSID            1
     C*** end add *****  04/07/25  ************************************************
    C*
    C*   WRITE EMPLOYER RECORD, IF NOT ALREADY EXISTING
    C*
     C                   eval      xxecnt = 0
     C                   eval      xxstem = *zeros
     C                   eval      xxstdv = *zeros
      /free
       If FNpeoF = 'S' Or (FNpeoF = 'C' And statePeoType = 'S');
          Chain (xxCo#:QOfnd:masterEmp#) pem02;
          If %Found(wmeml);
             DoU %Eof(wmeml);
                If EMstRp = 'Y';
                   xxStEm = EMemp#;
                   xxStDv = EMdiv;
                   Leave;
                EndIf;
                ReadE (xxCo#:QOfnd:masterEmp#) pem02;
             EndDo;
          EndIf;
       EndIf;
      /end-free
    C*
     C     emky2         chain     pem02                              62
    C*
    C
     c                   move      *zeros        xxcxdt#
    C                   eval      xxCxDt# = GetCxDate(xxco#
    C                                               :qofnd
    C                                               :qoemp#
    C                                               :qodiv)
     C                   If        not isRenewal
     C                   Clear                   xxCxDt#
     C                   Endif
     C                   IF        xxCxDt# <> 0
     C                   goto      NextXFer
      /Free
          isProcess = *off;
      /End-free
     C                   Endif
    C

      /Free
       Setll qtKy pel01;
       Dou %eof;
          Reade qtKy pel01;
          If not %eof and ELnwRw = 'R' and
           CoverageLapse(QOco#:QOfnd:QOemp#:QOdiv:ELfyr:'L'
           :CvtToDate(ELpfDt));
             isProcess = *off;
          Endif;
       Enddo;

       If isProcess;
      /End-free

     C*** begin add ***  04/07/25  ************************************************
     C* Assign the employer # now that ready to transfer if one is needed and
     C* group is set up to auto-assign using old GrpBrk subroutine.
     C                   Select
     C                   When      QOTEmp = 0 and FNAutE = 'Y' and
     C                             (svEmp# <> QOEmp# or saveFnd <> QOFnd)
     C                   Exsr      GrpBrk
     C                   When      QOTEmp = 0 and FNAutE = 'Y' and
     C                             svEmp# = QOEmp# and saveFnd = QOFnd
     C                   Eval      QOTEmp = saveEmp#
     C                   Endsl
     C*** end add *****  04/07/25  ************************************************
     C                   movel(p)  emname        exname
    C*
     C     qtky          chain     pem01                              60
    C*
     C                   if        *in60 = *off
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      ememp# = qotemp
     C                   movel     q1pgm         empgm
     C                   if        (FNpeoF = 'S') Or
     C*** begin chg ***  04/07/25  ************************************************
     C******                       (FNpeoF = 'C' And statePeoType = 'S')
     C                             (FNpeoF = 'C' And statePeoType = 'S'
     C                              And xxLSID <> '1')
     C*** end chg *****  04/07/25  ************************************************
     C                   move      emco#         qqco#
     C                   move      emfnd         qqfnd#
     C                   clear                   qqemp#
     C                   clear                   qqdiv
     C                   call      'WA056R'
     C                   parm                    qqco#
     C                   parm                    qqfnd#
     C                   parm                    qqemp#
     C                   parm                    qqdiv
    C*
     C                   move      qqemp#        xpemp#
     C                   move      qqdiv         xpdiv
     C                   eval      ememp# = xpemp#
     C                   eval      emdiv = xpdiv
    C*
     C                   eval      xxtemp = xpemp#
     C                   eval      xxtdiv = xpdiv
     c                   else
     C                   eval      xxtemp = qotemp
     C                   eval      xxtdiv = qodiv
     C                   eval      xpemp# = qotemp
     C                   eval      xpdiv  = qodiv
    C*
     C                   endif
     *
     C                   if        fnpeof <> ' '
     *
     C*** begin chg ***  05/13/25  ************************************************
     C******             if        emdiv = 0
     C******             move      'Y'           emstrp
     C******             else
     C******             move      'N'           emstrp
     C******             endif
     C                   Select
     C                   When      EMEmp# = 1 and EMDiv = 0
     C                   move      'Y'           emstrp
     C                   When      EMEmp# = 1
     C                   move      'N'           emstrp
     C                   Other
     C                   If        svStfnd <> EMFnd or svStemp# <> EMEmp#
     C                   Move      'Y'           emstrp
     C                   Z-add     EMFnd         svStfnd
     C                   Z-add     EMEmp#        svStemp#
     C                   Else
     C                   Move      'N'           emstrp
     C                   Endif                                                  if svStfnd <>
     C                   Endsl
     C*** end chg *****  05/13/25  ************************************************
     *
     C                   endif
     *
     C                   clear                   emrdfl
      /Free
       EMqtfr = sysDt7;
      /End-Free
     C                   write     pem02
     C                   eval      xxecnt = xxecnt + 1
     C                   movel(p)  emname        exname
    C*
    C*    Call API to update Managed Care - FMCMP
    C*
     C                   if        xxmanc = *on
     C                   exsr      mcapi
     C                   endif
    C*
     C                   else
     C                   eval      xpemp# = qotemp
     C                   eval      xpdiv  = qodiv
     C                   eval      xxtemp = xpemp#
     C                   eval      xxtdiv = xpdiv
    C*
     C                   endif
    C*
     C                   delete    pem01
    C*
     C                   endif
    C*_______________________________________________________________
    C*
    C*   PRINT DETAIL LINE FOR EMPLOYER
    C*
     C                   if        *inoa = *on
     C                   except    head
     C                   endif
     C                   except    det
     C                   write     pqh01
    C*
     C                   exsr      darts1
    C*_______________________________________________________________
    C*
    C*   WRITE EMPLOYER DETAIL RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxdcnt = *zero
    C*
     C     qtky          chain     pel01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   movel     *blanks       nbrnflg
     C                   movel     elnwrw        nbrnflg
     C                   eval      xxfyr = elfyr
     C                   eval      xxemp# = qotemp
    C*
     C                   eval      svbsdt = *zeros
     C     elkys         chain     pel02                              62
     C                   if        *in62 = *off
     C                   eval      svbsdt = elbsdt
     C                   endif
    C*
     C     elky          chain     pel02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     elky          chain     pel01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C*** begin chg ***  04/07/25  ************************************************
     C******                       (FNpeoF = 'C' And statePeoType = 'S')
     C                             (FNpeoF = 'C' And statePeoType = 'S'
     C                              And xxLSID <> '1')
     C*** end chg *****  04/07/25  ************************************************
     C                   eval      elemp# = xpemp#
     C                   eval      eldiv = xpdiv
     C                   if        svbsdt <> *zeros
     C                   eval      elbsdt = svbsdt
     C                   endif
     C                   else
     C                   eval      elemp# = qotemp
     C                   endif

      /free
       // Write the Unbilled Premium Record if installed
       If unBilPrmInst = *on;
          Open WXELL;
          Write pxel00;
          Exsr DelWXELL;
          Close WXELL;
          Clear ELpror;
          Clear ELprtd;
          Clear ELprtf;
          Clear ELmapr;
          Clear ELesbj;
          Clear ELstpr;
          Clear ELempm;
          Clear ELenmp;
          Clear ELanpr;
          Clear ELtxpr;
          Clear ELtxdu;
          Clear ELedc1;
          Clear ELedc2;
          Clear ELedv2;
          Clear ELedc3;
          Clear ELexp1;
          Clear ELexp2;
          Clear ELexp3;
          Clear ELein1;
          Clear ELein2;
          Clear ELedct;
          Clear ELedcm;
          Clear ELedcs;
          Clear ELedcr;
          Clear ELedc;
          Clear EL1apr;
          Clear EL2apr;
          Clear EL3apr;
          Clear EL4apr;
          Clear EL5apr;
          Clear EL6apr;
          Clear EL7apr;
          Clear EL1fpr;
          Clear EL2fpr;
          Clear EL3fpr;
          Clear EL4fpr;
          Clear EL5fpr;
          Clear EL6fpr;
          Clear EL7fpr;
          Clear EL1aam;
          Clear EL2aam;
          Clear EL3aam;
          Clear EL4aam;
          Clear EL5aam;
          Clear EL6aam;
          Clear EL7aam;
          Clear ELewav;
          Clear ELeter;
          Clear ELedtr;
          Clear ELrpe1;
          Clear ELrpe2;
          Clear ELrpe3;
          Clear ELemna;
          Clear ELedd2;
          Clear ELd1e2;
          Clear ELd2e2;
          Clear ELehcn;
          Clear ELearc;
          Clear ELdisp;
          Clear ELefrs;
          Clear ELdev2BP;
          Clear ELdev2AP;
       Endif;
      /end-free

     C*** begin add ***  01/06/23  **************************************
     C                   move      ' '           elrqfl
     C*** end add *****  01/06/23  **************************************
     C                   move      ' '           elfaf
     C                   movel     q1pgm         elpgm
      /Free
       ELqtfr = sysDt7;
       ELfQte = ' ';
      /End-Free
     C                   write     pel02
     C                   eval      xxdcnt = xxdcnt + 1
       //*** Begin Add *** 05/21/21 **************************************
       If QOemp# <> savQOemp#;
          savQOemp# = QOemp#;
          isOptedIn = ChkOptInStatus();
          Exsr Update3rdPartySetup;
       Endif;

       If isOptedIn = *On;
          Exsr PostNewBusPayment;
       Endif;
       //*** End   Add *** 05/21/21 **************************************
    C*
    C* WRITE CARRIER INFORMATION RECORD(S), IF NOT ALREADY EXISTING
    C* AND THE ASSIGN FLAG IS 'Y'
    C*
    C*
     C     rpky          chain(n)  prp01                              60
    C*
     C                   if        *in60 = *on
     *
     C                   call      'WA200R'
     C                   parm                    elco#
     C                   parm                    elfnd
     C                   parm                    elemp#
     C                   parm                    eldiv
     C                   parm                    elfyr
     C                   parm      q1pgm         ##pgm            10
     *
     C     Rpky          Chain(n)  Prp01                              60
     C                   If        *in60 = *on

     C                   Clear                   prp01

     C                   Z-add     elco#         rpco#
     C                   Z-add     elfnd         rpfnd
     C                   Z-add     elemp#        rpemp#
     C                   Z-add     eldiv         rpdiv
     C                   Z-add     elfyr         rpfyr

     c                   Eval      rpiusr = q1user
     c                   Eval      rpuser = q1user
     c                   Eval      rpipgm = q1pgm
     c                   Eval      rppgm = q1pgm

     C                   Eval      Rpdt   = sysDt7
     C                   Eval      Rpidt  = sysDt7
     C                   Eval      Rptime = TimeTo6(SystemTime)
     C                   Eval      Rpitm  = TimeTo6(SystemTime)

     C                   Write     Prp01

     C                   Endif                                                  If *IN60 = *on
     *
     C                   endif
     *
     *  Write Certificate Holder record if PEO employer
     *
     C                   exsr      certrd
    C*
     C                   endif
    C*
     C                   delete    pel01
    C*
     C     qtky          reade     pel01                                  60
    C*
     C                   enddo
     *
     *  Update New Business Tracking record with new employer #
     *
     C     nbky          chain     pnb01                              60
     *
     C                   if        *in60 = *off
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      nbemp# = xpemp#
     C                   else
     C                   eval      nbemp# = qotemp
     C                   endif
     *
     C                   move      q1user        nbuser
     C                   move      q1pgm         nbpgm
     C                   eval      NBTIME = TimeTo6(SystemTime)
     C                   eval      NBDT   = sysDt7
     *
     C                   update    pnb01
     C                   endif
     *
     *  Update Renewal Tracking records with new employer #
     *
     C     rtky          chain     prt01                              60
     *
     C                   dow       *in60 = *off
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      rtemp# = xpemp#
     C                   else
     C                   eval      rtemp# = qotemp
     C                   endif
     *
     C                   move      q1user        rtuser
     C                   move      q1pgm         rtpgm
     C                   eval      RTTIME = TimeTo6(SystemTime)
     C                   eval      RTDT   = sysDt7
     *
     C                   update    prt01
     C     rtky          reade     prt01                                  60
     C                   enddo
     *
     *  Update Employee records with new employer #
     *
     C     qtky          chain     pee01                              60
     *
     C                   dow       *in60 = *off
     C                   eval      xxemp# = qotemp
     C                   eval      xxfyr = eefyr
     C                   move      eess#         xxss#
     *
     C     eeky          chain     pee02                              62
     *
     C                   if        *in62 = *on
     C                   eval      xxemp# = qoemp#
     C     eeky          chain     pee01                              60
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      eeemp# = xpemp#
     C                   eval      eediv = xpdiv
     C                   else
     C                   eval      eeemp# = qotemp
     C                   endif
     *
     C                   movel     q1user        eeuser
     C                   movel     q1pgm         eepgm
     C                   eval      EETIME = TimeTo6(SystemTime)
     C                   eval      EEDT   = sysDt7
     C                   write     pee02
     C                   endif
     *
     C                   delete    pee01
     C     qtky          reade     pee01                                  60
     C                   enddo
     C*** Begin Add *** 05/15/20 *******************************************
     *
     *  Update Employee records with new employer #
     *
     C     qtky          chain     pemh0q                             60
     C                   dow       *in60 = *off
      *
     C                   Eval      EMHemp# = qotemp
     C                   Eval      EMHpgm  = Q1pgm
     C                   Eval      EMHuser = Q1user
     *
     C                   write     pemh0p
     C                   delete    pemh0q
     *
     C     qtky          reade     pemh0q                                 60
     C                   enddo
     C*** End   Add *** 05/15/20 *******************************************
      *_______________________________________________________________
      *
      *   WRITE MEMBER FEE EXCEPTION RECORD(S), IF NOT ALREADY EXISTING
      *
     C     QTKY          CHAIN     PME01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     MEFYR         XXFYR
     C                   Z-ADD     QOTEMP        XXEMP#
     C     MEKY          CHAIN     PME02                              62
      *
     C     *IN62         IFEQ      *ON
     C                   Z-ADD     QOEMP#        XXEMP#
     C     MEKY          CHAIN     PME01                              60
     C                   Z-ADD     QOTEMP        MEEMP#
     C                   MOVEL     Q1PGM         MEPGM
     C                   WRITE     PME02
     C                   ENDIF
      *
     C                   DELETE    PME01
     C     QTKY          READE     PME01                                  60
     C                   ENDDO
      *
      /free
       // Write Dividend Information Record(s), If not already Existing.

       xxdcnt = 0;

       Setll qtky pdv01;
       Reade qtky pdv01;
       Dow not %eof;
          xxFyr  = DVfyr;
          xxEmp# = QOtemp;

          Chain dvky pdv02;
          If not %found;
             xxEmp# = QOemp#;

             Chain dvky pdv01;

             DVemp# = QOtemp;
             DVpgm  = Q1Pgm;
             write pdv02;
             xxdcnt = 1;
          Endif;

          Delete pdv01;
          Reade qtky pdv01;
       Enddo;

       Setll (xxco#:qofnd:qoemp#) pvi05;
       Reade (xxco#:qofnd:qoemp#) pvi05;
       Dow not %eof;
          VIemp# = qotemp;
          VIQte = ' ';
          Update pvi05;

          Reade (xxco#:qofnd:qoemp#) pvi05;
       Enddo;
      /end-free
    C*
    C*   WRITE OWNER/OFFICER DETAIL RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxocnt = *zero
    C*
     C     qtky          chain     poo01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
     C*** begin add ***  12/14/21  ***************************************
     C                   movel(p)  ooelid        xoelid           10
     C*** end add *****  12/14/21  ***************************************
     C                   movel     ooolsn        xolsn
     C                   movel     ooofsn        xofsn
    C*
     C     ooky1         chain     poo02                              62
    C*
     C*** Begin Del *** 05/17/21 *******************************************
     C****               if        *in62 = *on
     C*** End Del *** 05/17/21 *******************************************
     C*** begin add ***  12/14/21  ***************************************
     C                   Dow       *in62 = *off
     C                   If        OOElid = xoElid
     C                   Leave
     C                   Endif
     C     ooky1         Reade     poo02                                  62
     C                   Enddo
     C*
     C                   If        *in62 = *on
     C*** end add *****  12/14/21  ***************************************
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     ooky          chain     poo01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      ooemp# = xpemp#
     C                   eval      oodiv = xpdiv
     C                   else
     C                   eval      ooemp# = qotemp
     C                   endif
     C                   movel     q1pgm         oopgm
     C                   write     poo02
     C                   eval      xxocnt = xxocnt + 1
    C*
     C*** Begin Del *** 05/17/21 *******************************************
     C****               endif
     C*** End Del *** 05/17/21 *******************************************
     C*** begin add ***  12/14/21  ***************************************
     C                   Endif                                                  *in62 = *on
     C*** end add *****  12/14/21  ***************************************
    C*
     C                   delete    poo01
    C*
     C     qtky          reade     poo01                                  60
    C*
     C                   enddo
     *
     *  Write Employer Contact Records, If not already existing
     *
     C                   eval      xeccnt = *zero
     C*** begin chg ***  03/14/22  **********************************
     C*****qtky          chain     pec01                              60
     C     qtky          chain     pec00                              60
     C*** end chg *****  03/14/22  **********************************
     *
     C                   dow       *in60 = *off
     C*** begin del ***  03/14/22  **********************************
     C*****              eval      kkco# = ecco#
     C*****              eval      kkfnd = ecfnd
     C*****              eval      kkemp# = qotemp
     C*****              eval      kkdiv = ecdiv
     C*****              movel(p)  eclsn         kklsn
     C*****              movel(p)  ecfsn         kkfsn
     C*****              move      ecmid         kkmid
     C*****              movel(p)  ecposn        kkposn
     C*****              move      ecpcnt        kkpcnt
     *****
     C*****ecky          chain     pec02                              62
     *****
     C*****              if        *in62 = *on
     C* Contact not found in LIVE file. Check for duplicate contact desg.
     C* check only if contact desg is not blanks
     C*****              if        kkpcnt <> *blanks
     C*****ecky1         chain     pec03                              62
     C*****              endif                                                  kkpcnt <> *blanks
     C*****              if        *in62 = *on
     C* Not found, move contact to LIVE
     C*****              eval      kkemp# = qoemp#
     C*****ecky          chain     pec01                              60
     C*** end del *****  03/14/22  **********************************
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      ecemp# = xpemp#
     C                   eval      ecdiv = xpdiv
     C                   else
     C                   eval      ecemp# = qotemp
     C                   endif
     *
     C                   movel     q1pgm         ecpgm
     C                   eval      ECTIME = TimeTo6(SystemTime)
     C                   eval      ECDT   = sysDt7
     *
     C*** begin chg ***  03/14/22  **********************************
     C*****              write     pec02
     C                   Update    pec00
     C*** end chg *****  03/14/22  **********************************
     C                   eval      xeccnt = xeccnt + 1
     C*** begin chg ***  03/14/22  **********************************
     C*****              endif
     C*****              endif                                                  *in62 = *on
     *
     C*****              delete    pec01
     C*****qtky          reade     pec01                                  60
     C     qtky          Reade     pec00                                  60
     C*** end chg *****  03/14/22  **********************************
     C                   enddo
     C*** begin add ***  03/14/22  **********************************
     C*
     C* Transfer the contact criteria records.
     C*
     C     qtky          chain     pec0c                              60
     *
     C                   dow       *in60 = *off
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      ecemp# = xpemp#
     C                   eval      ecdiv = xpdiv
     C                   else
     C                   eval      ecemp# = qotemp
     C                   endif
     *
     C                   movel     q1pgm         ecpgm
     C                   eval      ECTIME = TimeTo6(SystemTime)
     C                   eval      ECDT   = sysDt7
     *
     C                   Update    pec0c
     C     qtky          Reade     pec0c                                  60
     C                   enddo
     C*** end add *****  03/14/22  **********************************
     *
    C*
    C*  WRITE ENT/LOCATION RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxlcnt = *zero
    C*
     C     qtky          chain     pal01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
    C*
     C                   move      alelid        xxelid
     C     alky          chain     pal02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     alky          chain     pal01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      alemp# = xpemp#
     C                   eval      aldiv = xpdiv
     C                   else
     C                   eval      alemp# = qotemp
     C                   endif
     C                   movel     q1pgm         alpgm
     C                   write     pal02
     C                   eval      xxlcnt = xxlcnt + 1
    C*
     C                   endif
    C*
     C                   delete    pal01
    C*
     C     qtky          reade     pal01                                  60
    C*
     C                   enddo
    C*
    C*  WRITE ADDTNL ENT/LOCATION DTL RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxxcnt = *zero
    C*
     C     qtky          chain     pld01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
     C                   move      ldelid        xxelid
     C                   eval      xxfyr = ldfyr
    C*
     C     ldky          chain     pld02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     ldky          chain     pld01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      ldemp# = xpemp#
     C                   eval      lddiv = xpdiv
     C                   else
     C                   eval      ldemp# = qotemp
     C                   endif
     C                   movel     q1pgm         ldpgm
     C                   write     pld02
     C                   eval      xxxcnt = xxxcnt + 1
    C*
     C                   endif
    C*
     C                   delete    pld01
    C*
     C     qtky          reade     pld01                                  60
    C*
     C                   enddo
    C*
    C*  WRITE ADD'L NAMED INSURED RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxncnt = *zero
    C*
     C     qtky          chain     pan01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
    C*
     C                   move      anname        xxname
     C                   move      anseq#        xxseq#
     C     anky          chain     pan02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     anky          chain     pan01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      anemp# = xpemp#
     C                   eval      andiv = xpdiv
     C                   else
     C                   eval      anemp# = qotemp
     C                   endif
     C                   movel     q1pgm         anpgm
     C                   write     pan02
     C                   eval      xxncnt = xxncnt + 1
    C*
     C                   endif
    C*
     C                   delete    pan01
    C*
     C     qtky          reade     pan01                                  60
    C*
     C                   enddo
    C*
    C*  WRITE SCHEDULE RATING RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxhcnt = *zero
    C*
     C     qtky          chain     rfp01                              60
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
     C     rfky          chain     rfp02                              62
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
     C                   eval      xxfyr  = rffyr
     C     rfky          chain     rfp01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      rfemp# = xpemp#
     C                   eval      rfdiv = xpdiv
     C                   else
     C                   eval      rfemp# = qotemp
     C                   endif
     *
     C                   movel     q1pgm         rfpgm
     C                   write     rfp02
     C                   eval      xxhcnt = xxhcnt + 1
     *
     C                   endif
    C*
     C                   delete    rfp01
    C*
     C     qtky          reade     rfp01                                  60
    C*
     C                   enddo
    C*
    C*  WRITE VENDOR HISTORY RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xvdcnt = *zero
    C*
     C     qtky          chain     pvh01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
     C                   eval      xxfyr = vhfyr
    C*
     C     elky          chain     pvh02                              62
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
     C     elky          chain     pvh01                              60
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      vhemp# = xpemp#
     C                   eval      vhdiv = xpdiv
     C                   else
     C                   eval      vhemp# = qotemp
     C                   endif
     C                   movel     q1pgm         vhpgm
     C                   movel     q1user        vhuser
     C                   eval      VHTIME = TimeTo6(SystemTime)
     C                   eval      VHDT   = sysDt7
     C                   write     pvh02
     C                   eval      xvdcnt = xvdcnt + 1
    C*
     C                   endif
    C*
     C                   delete    pvh01
     C     qtky          reade     pvh01                                  60
    C*
     C                   enddo
    C*
    C*  WRITE VENDOR HISTORY RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xddcnt = *zero
    C*
     C     qtky          chain     pdb01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
     C                   eval      xxefdt = dbefdt
    C*
     C     dbky          chain     pdb02                              62
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
     C     qtky          chain     pdb01                              60
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      dbemp# = xpemp#
     C                   eval      dbdiv = xpdiv
     C                   else
     C                   eval      dbemp# = qotemp
     C                   endif
     C                   movel     q1pgm         dbpgm
     C                   movel     q1user        dbuser
     C                   eval      DBTIME = TimeTo6(SystemTime)
     C                   eval      DBDT   = sysDt7
     C                   write     pdb02
     C                   eval      xddcnt = xddcnt + 1
    C*
     C                   endif
    C*
     C                   delete    pdb01
     C     qtky          reade     pdb01                                  60
    C*
     C                   enddo
    C*
    C*  WRITE BILLING ADDRESS RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xx3cnt = *zero
    C*
     C     qtky          chain     pa301                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
    C*
     C     qtky          chain     pa302                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     qtky          chain     pa301                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      a3emp# = xpemp#
     C                   eval      a3div = xpdiv
     C                   else
     C                   eval      a3emp# = qotemp
     C                   endif
     C                   movel     q1pgm         a3pgm
     C                   write     pa302
     C                   eval      xx3cnt = xx3cnt + 1
    C*
     C                   endif
    C*
     C                   delete    pa301
    C*
     C     qtky          reade     pa301                                  60
    C*
     C                   enddo
    C*
    C* WRITE EMPLOYER ENDORSEMENT RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxucnt = *zero
    C*
     C     qtky          chain     pmu01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   movel     muform        xxform
     C                   eval      xxfyr = mufyr
     C                   eval      xxemp# = qotemp
    C*
     C     muky          chain     pmu02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     muky          chain     pmu01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      muemp# = xpemp#
     C                   eval      mudiv = xpdiv
     C                   else
     C                   eval      muemp# = qotemp
     C                   endif
     C                   movel     q1pgm         mupgm
     C                   write     pmu02
     C                   eval      xxucnt = xxucnt + 1
    C*
     C                   endif
    C*
     C                   delete    pmu01
    C*
     C     qtky          reade     pmu01                                  60
    C*
     C                   enddo
    C*
    C*   WRITE WAIVER INFORMATION RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxvcnt = *zero
    C*
     C     qtky          chain     pwv01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxwav# = wvwav#
     C                   eval      xxfyr = wvfyr
     C                   eval      xxemp# = qotemp
    C*
     C     wvky          chain     pwv02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     wvky          chain     pwv01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      wvemp# = xpemp#
     C                   eval      wvdiv = xpdiv
     C                   else
     C                   eval      wvemp# = qotemp
     C                   endif
     C                   movel     q1pgm         wvpgm
       //*** Beg Chg *** 02/28/22 *********************************************
     C***                write     pwv02
     C***                eval      xxvcnt = xxvcnt + 1
       xxVcnt = xxVcnt + 1;
       WVwav# = xxVcnt;
       //*** Beg Add *** 05/10/24 ******************************************
       intStRptDiv = GetStRptDv(WVco#:WVfnd:WVemp#);
       WVemst = GetRatingState(WVco#:WVfnd:WVemp#:intstRptDiv:
         WVfyr);
       //*** End Add *** 05/10/24 ******************************************
       Write pwv02;
       //*** End Chg *** 02/28/22 *********************************************
    C*
     C                   endif
    C*
     C                   delete    pwv01
    C*
     C     qtky          reade     pwv01                                  60
    C*
     C                   enddo

       // Transfer endorsement detail records
      /Free
       Setll qtKy pfm01q;
       Dou %Eof;
          Reade qtKy pfm01q;
          If %Eof;
             Leave;
          Endif;
          If FNpeoF = 'S' Or (FNpeoF = 'C' And xpEmp# = 1);
             F3emp# = xpEmp#;
             F3div = xpDiv;
          Else;
             F3emp# = QOtemp;
          Endif;
          F3pgm = Q1pgm;
          Write pfm01;
          Delete pfm01q;
       Enddo;
       //*** beg chg ***** 05/11/21 **********************
       // Transfer endorsement detail records
       //Setll qtKy pp300;
       //Dou %Eof;
       //   Reade qtKy pp300;
       //   If %Eof;
       //      Leave;
       //   Endif;
       // If FNpeoF = 'S' Or (FNpeoF = 'C' And xpEmp# = 1);
       //    P3emp# = xpEmp#;
       //    P3div = xpDiv;
       // Else;
       //    P3emp# = QOtemp;
       // Endif;
       // P3pgm = Q1pgm;
       // Update pp300;
       //Enddo;
      */End-Free
       // Transfer 3rd Party Payment setup.
       // Archive and delete live setup.
       // Replace live setup with quote setup.
       hasQuoteRecord = *Off;
       If EMrqfl = 'Y';
          Setgt qtky pp300;
          Dou %Eof;
             Readpe qtKy pp300 dsWMP3L;
             If %Eof;
                Leave;
             Endif;
             If P3EnvFlg = 'Q';
                hasQuoteRecord = *On;
                Iter;
             Endif;
             If hasQuoteRecord = *On And P3EnvFlg = ' ';
                ArchiveWMP3P(dsWMP3L:'QUOTEXFR');
                Delete pp300;
             Endif;
          Enddo;
       Endif;
       Setgt qtky pp300;
       Dou %Eof;
          Readpe qtKy pp300;
          If %Eof;
             Leave;
          Endif;
          If P3envFlg = 'Q';
             If FNpeoF = 'S' Or (FNpeoF = 'C' And xpEmp# = 1);
                P3emp# = xpEmp#;
                P3div = xpDiv;
             Else;
                P3emp# = QOtemp;
             Endif;
             P3EnvFlg = *Blank;
             P3pgm = Q1pgm;
             Update pp300;
          Endif;
       Enddo;
       // Transfer EFT Setup Records
       hasQuoteRecord = *Off;
       chrCo# = %EditC(ELco#:'X');
       chrFnd = %EditC(ELfnd:'X');
       chrFyr = %EditC(ELfyr:'X');
       chrEFKey = %EditC(QOemp#:'X') + %EditC(QOdiv:'X');
       chrEfdt = %EditC(sysDt7:'X');
       // Look for EFT record in Quote
       chrEnv = 'Q';
       If GetEftStatus(chrCo#:chrEfType:chrFnd:
        chrEfKey:chrEfDt:chrPre:chrRRN:chrEobType:
        chrChkType:chrFyr:chrEnv:chrDftSRD) = *On;
          If chrDftSRD <> 'Y'; // SRD not providing EFT for NSRD
             intQteRRN = %Int(chrRRN);
             Chain(n) intQteRRN pef0p;
             If %Found And pEFEnvFlg = 'Q';
                hasQuoteRecord = *On;
             Endif;
          Endif;
       Endif;
       // Look for EFT records in live if renewal quote.
       // Expire it as of the quote record's effective date
       // if the starting policy years are equal and the record
       // is not expired before the quote record's effective date.
       chrEnv = *Blank;
       If EMrqfl = 'Y' And hasQuoteRecord = *On;
          Setgt (chrEfType:ELco#:ELfnd:chrEfKey:sysDt7) pef07;
          Dou %Eof(WMEFL7);
             Readpe (chrEfType:ELco#:ELfnd:chrEfKey) pef07;
             If %Eof;
                Leave;
             EndIf;
             If pEFsFyr = l7EFsFyr
              And l7EFenvFlg = *Blank
              And l7EFexDt = 0 Or l7EFexDt > pEFefDt;
                 l7EFexDT = pEFefDt;
                 l7EFpgm = Q1pgm;
                 Update pef07;
             EndIf;
          Enddo;
       Endif;
       If hasQuoteRecord = *On;
          Setgt (chrEfType:ELco#:ELfnd:chrEfKey) pef07;
          Dou %Eof(WMEFL7);
             Readpe (chrEfType:ELco#:ELfnd:chrEfKey) pef07;
             If %Eof;
                Leave;
             EndIf;
             If l7EFenvFlg = 'Q';
                If FNpeoF = 'S' Or (FNpeoF = 'C' And xpEmp# = 1);
                   l7EFemp# = xpEmp#;
                   l7EFdiv = xpDiv;
                Else;
                   l7EFemp# = QOtemp;
                Endif;
                l7EFenvFlg = *Blank;
                l7EFkey = %EditC(l7EFemp#:'X') + %EditC(l7EFdiv:'X');
                l7EFpgm = Q1pgm;
                Update pef07;
             EndIf;
          Enddo;
       Endif;
       //*** end chg ***** 05/11/21 **********************
       // Update  Department Master File.
      /Free
       Setll qtKy pdp01;
       Dou %Eof;
          Reade qtKy pdp01;
          If %Eof;
             Leave;
          Endif;
          If FNpeoF = 'S' Or (FNpeoF = 'C' And xpEmp# = 1);
             DPEMP# = xpEmp#;
             DPDIV = xpDiv;
          Else;
             DPEMP# = QOtemp;
          Endif;
          DPUSER =Q1user;
          DPPGM = Q1pgm;
          DPDT  = sysDt7;
          DPTIME= TimeTo6(SystemTime);
          Update pdp01;
       Enddo;
      /End-Free

    C*
    C*
    C*   WRITE EXCLUDED STATES RECORD(S)
    C*
     C                   eval      x1emp# = 0
     C                   eval      x1fyr = 0
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      x2emp# = xpemp#
     C                   else
     C                   eval      x2emp# = qotemp
     C                   endif
    C*
     C                   clear                   countWDXSP
    C*
     C     xsky          chain     pxs01q                             60
     C                   dow       *in60 = *off
    C*
     C     xsky1         chain     pxs01                              60
     C                   if        *in60 = *on
    C*
     C                   eval      XSEMP# = QOTEMP
     C                   movel     q1user        xsuser
     C                   movel     q1pgm         xspgm
     C                   eval      XSTIME = TimeTo6(SystemTime)
     C                   eval      XSDT   = sysDt7
     C                   write     pxs01
     C                   eval      countWDXSP = countWDXSP + 1
     C                   endif
    C*
     C                   delete    pxs01q
     C*
     C     xsky          reade     pxs01q                                 60
     C                   enddo
     C*
     C* Ensure states at group level are copied over if necessary.
     C*
     C                   Z-ADD     0             X1EMP#
     C                   Z-ADD     0             X1FYR
     C*
     C     XSKY2         CHAIN     PXS01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     XSKY1         CHAIN     PXS01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     qotemp        XSEMP#
     C                   Z-ADD     xxfyr         XSFYR
     C                   MOVEL     Q1USER        XSIUSR
     C                   MOVEL     Q1USER        XSUSER
     C                   MOVEL     Q1PGM         XSIPGM
     C                   MOVEL     Q1PGM         XSPGM
     C                   eval      XSIDT   = sysDt7
     C                   eval      XSDT   = sysDt7
     C                   eval      XSITIM = TimeTo6(SystemTime)
     C                   eval      XSTIME = TimeTo6(SystemTime)
     C                   WRITE     PXS01
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XSKY2         READE     PXS01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* Large Deductible
     C                   clear                   countWMLDP
    C*
     C     ldKy3         chain     pld01q                             60
     C                   dow       *in60 = *off
    C*
     C                   eval      xxFyr = LDFYR
     C     ldKy4         chain     pld03                              60
     C                   if        *in60 = *on
    C*
     C                   eval      LDEMP# = QOTEMP
     C                   eval      LDUSER = q1user
     C                   eval      LDPGM  = q1pgm
     C                   eval      LDTIME = TimeTo6(SystemTime)
     C                   eval      LDDT   = sysDt7
     C                   write     pld03
     C                   eval      countWMLDP = countWMLDP + 1
     C                   endif
    C*
     C                   delete    pld01q
     C*
     C     ldKy3         reade     pld01q                                 60
     C                   enddo
     C*
     C* Fee information
     C                   clear                   countWDEFP
    C*
     C     efKy1         chain     pef01q                             60
     C                   dow       *in60 = *off
    C*
     C                   eval      xxFyr = EFFYR
     C     efKy2         chain     pef01                              60
     C                   if        *in60 = *on
    C*
     C                   eval      EFEMP# = QOTEMP
     C                   eval      EFUSER = q1user
     C                   eval      EFPGM  = q1pgm
     C                   eval      EFTIME = TimeTo6(SystemTime)
     C                   eval      EFDT   = sysDt7
     C                   write     pef01
     C                   eval      countWDEFP = countWDEFP + 1
     C                   endif
    C*
     C                   delete    pef01q
     C*
     C     efKy1         reade     pef01q                                 60
     C                   enddo
     C*
     C* Submission History
     C                   clear                   countWMSHP
    C*
     C     shKyQ         chain     psh01q                             60
     C                   dow       *in60 = *off
     C                   if        shdiv=qodiv
    C*
     C                   eval      xxFyr = shFYR
     C                   eval      exists=*off
     C     shky          chain(n)  psh01                              60
     C                   dow       *in60 = *off
     C                   if        shdiv=qodiv
     C                   eval      exists=*on
     C                   leave
     C                   endif
     C     shky          reade(n)  psh01                                  60
     C                   enddo
    C*
     C                   if        not exists
     C                   eval      SHEMP# = QOTEMP
     C                   eval      SHUSER = q1user
     C                   eval      SHPGM  = q1pgm
     C                   eval      SHTIME = TimeTo6(SystemTime)
     C                   eval      SHDT   = sysDt7
     C                   write     psh01
     C                   eval      countWMSHP = countWMSHP + 1
     C                   endif
    C*
     C                   delete    psh01q
     C                   else
     C                   update    psh01q
     C                   endif
     C*
     C     shKyQ         reade     psh01q                                 60
     C                   enddo
     C*
     C* Campus codes
     C                   clear                   countWMCLP
    C*
     C     CampQ         setll     wmcllq

     c                   if        %equal(wmcllq)
     C     CampQ         reade     wmcllq
    C*
     c                   dow        not %eof(wmcllq)
     C     Camp          chain(n)  wmcll
     C                   if        not %found(wmcll)
     C                   eval      CLEMP# = QOTEMP
     C                   eval      CLUSER = q1user
     C                   eval      CLPGM  = q1pgm
     C                   eval      CLTIME = TimeTo6(SystemTime)
     C                   eval      CLDT   = sysDt7
     C                   write     pcl01
     C                   delete    pcl01q
     C                   eval      countWMCLP = countWMCLP + 1
     C                   endif
    C*
     C     CampQ         reade     wmcllq
     C                   enddo

     C                   endif

     C* Campus detail
     C                   clear                   countWDCYP
    
     C     CampQ         setll     wdcyl1q

     c                   if        %equal(wdcyl1q)
     C     CampQ         reade     wdcyl1q
    C*
     c                   dow        not %eof(wdcyl1q)
     C     Campky        chain(n)  wdcyl1
     C                   if        not %found(wdcyl1)
     C                   eval      CYEMP# = QOTEMP
     C                   eval      CYUSER = q1user
     C                   eval      CYPGM  = q1pgm
     C                   eval      CYTIME = TimeTo6(SystemTime)
     C                   eval      CYDT   = sysDt7
     C                   write     pcy01
     C                   delete    pcy01q
     C                   eval      countWDCYP = countWDCYP + 1
     C                   endif
    C*
     C     CampQ         reade     wdcyl1q
     C                   enddo

     C                   endif
     C* HCN Program
     C                   clear                   countWDHCP
    C*
     C     hcKy3         chain     phc00q                             60
     C                   dow       *in60 = *off
    C*
     C                   eval      xxFyr = HCFYR
     C     hcKy4         chain     phc00                              60
     C                   if        *in60 = *on
    C*
     C                   eval      HCEMP# = xxtemp
     C                   eval      HCDIV  = xxtdiv
     C                   eval      HCUSER = q1user
     C                   eval      HCPGM  = q1pgm
     C                   eval      HCTIME = TimeTo6(SystemTime)
     C                   eval      HCDT   = sysDt7
     C                   write     phc00
     C                   eval      countWDHCP = countWDHCP + 1
     C                   endif
    C*
     C                   delete    phc00q
     C*
     C     hcKy3         reade     phc00q                                 60
     C                   enddo
      /Free
       sysTime6 = TimeTo6(SystemTime);
       // Application Information
       Exec sql
        update WIAPPANS
         set APemp# = :xxTEmp, APuser = :q1User, APpgm = :q1pgm,
          APdt = :sysDt7, APtime = :sysTime6
         where APco# = :xxCo# and APfnd = :QOfnd and APemp# = :QOemp#;
       Exec sql
        update WIEMP
         set EMemp# = :xxTEmp, EMuser = :q1User, EMpgm = :q1pgm,
          EMdt = :sysDt7, EMtime = :sysTime6
         where EMco# = :xxCo# and EMfnd = :QOfnd and EMemp# = :QOemp#;
       Exec sql
        update WIGSP
         set GSemp# = :xxTEmp, GSuser = :q1User, GSpgm = :q1pgm,
          GSdt = :sysDt7, GStime = :sysTime6
         where GSco# = :xxCo# and GSfnd = :QOfnd and GSemp# = :QOemp#;
       Exec sql
        update WIGSPDRV
         set GSemp# = :xxTEmp, GSuser = :q1User, GSpgm = :q1pgm,
          GSdt = :sysDt7, GStime = :sysTime6
         where GSco# = :xxCo# and GSfnd = :QOfnd and GSemp# = :QOemp#;
      /End-free
    C*_______________________________________________________________
    C*
    C*   WRITE EMPLOYER PAYROLL RECORD(S), IF NOT ALREADY EXISTING
    C*        - ACTIVE PAYROLLS ONLY
    C*
     C                   eval      xxpcnt = *zero
    C*
     C     qtky          chain     pep01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   if        epstat <> 'I'
    C*
     C                   eval      xxfyr = epfyr
     C                   eval      xxefdt = epefdt
     C                   eval      xxclas = epclas
     C                   move      epspec        xxspec
     C                   eval      xxext = epext
     C                   eval      xxemp# = qotemp
    C*
     C     epky          chain     pep02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     epky          chain     pep01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      epemp# = xpemp#
     C                   eval      epdiv = xpdiv
     C                   else
     C                   eval      epemp# = qotemp
     C                   endif
     C                   movel     q1pgm         eppgm
     C                   write     pep02
     C                   eval      xxpcnt = xxpcnt + 1
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   delete    pep01
    C*
     C     qtky          reade     pep01                                  60
    C*
     C                   enddo
    C*_______________________________________________________________
    C*
    C*   WRITE SELF REPORTING RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxscnt = *zero
    C*
     C     qtky          chain     psr01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxfyr = srfyr
     C                   eval      xxrpmy = srrpmy
     C                   eval      xxclas = srclas
     C                   eval      xxefdt = srefdt
    C*
     C     srky2         chain     psr02                              62
    C*
     C                   if        *in62 = *on
    C*
     C     srky          chain     psr01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      sremp# = xpemp#
     C                   eval      srdiv = xpdiv
     C                   else
     C                   eval      sremp# = qotemp
     C                   endif
     C                   movel     q1pgm         srpgm
     C                   write     psr02
     C                   eval      xxscnt = xxscnt + 1
    C*
     C                   endif
    C*
     C                   delete    psr01
    C*
     C     qtky          reade     psr01                                  60
    C*
     C                   enddo
    C*_______________________________________________________________
    C*
    C*   WRITE ACCOUNTS RECEIVABLE RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxrcnt = *zero
    C*
     C     qtky          chain     pea01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   delete    pea01
    C*
     C     qtky          reade     pea01                                  60
    C*
     C                   enddo
    C*_______________________________________________________________
    C*
    C*   WRITE AGENT HISTORY RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxacnt = *zero
    C*
     C     qtky          chain     pah01                              60
    C*
     C                   dow       *in60 = *off
    C*
    C*  TRANSFER ALL ACTIVE AGENTS FOR AN EMPLOYER INTO THE LIVE SYSTEM
    C*
     C                   if        ahstat <> 'I'
    C*
     C                   eval      xxfyr = ahfyr
     C                   eval      xxefdt = ahefdt
     C                   eval      xxagt# = ahagt#
     C                   eval      xxemp# = qotemp
    C*
     C     ahky          chain     pah02                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     ahky          chain     pah01                              60
    C*
     C     aaky          chain     paa01                              60
    C*
     C  n60              if        aaexdt = *zero
     C                             or aaexdt > x7Dt
     C                   if        aadrt# <> *zero
     C                              and QORAFL = 'Y'
     C                   eval      ahrt# = aadrt#
     C                   endif
     C                   endif
    C*
     C                   eval      xxagt# = ahsagt
    C*
     C     aaky          chain     paa01                              60
    C*
     C  n60              if        aaexdt = *zero
     C                             or aaexdt > x7Dt
     C                   if        aadrt# <> *zero
     C                              and QORAFL = 'Y'
     C                   eval      ahsrt# = aadrt#
     C                   endif
     C                   endif
      *
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      ahemp# = xpemp#
     C                   eval      ahdiv = xpdiv
     C                   else
     C                   eval      ahemp# = qotemp
     C                   endif
     C                   movel     q1pgm         ahpgm
      /Free
       isNoCurrAgt = GetEmpAgt(xxCo#:QOfnd:QOtEmp:QOdiv) = *zero;
      /End-free
     C                   eval      currentAgt# =
     C                               GetEmpAgt(xxCo#:QOFND:QOEMP#:QODIV)
     C                   write     pah02
     C                   eval      newAgt# =
     C                               GetEmpAgt(xxCo#:QOFND:QOEMP#:QODIV)
     C                   if        currentAgt# = *zero
     C                   eval      currentAgt# = newAgt#
     C                   endif
     C                   eval      xxacnt = xxacnt + 1
    C*
     C                   endif
    C*
    C*   UPDATE AGENT QUOTE RECORD FROM QUOTE STATUS ('Q') TO ACTIVE
    C*   STATUS ('W'). ALSO, UPDATE AGENT QUOTE RECORD WITH EMPLOYER
    C*   NUMBER FROM ACTIVE SYSTEM
    C*
     C                   if        newAgt# <> currentAgt#
     C                   eval      xxagt# = currentAgt#
     C                   eval      xxStat = 'W'
     C     qtky2         chain     pqt01                              60
     C                   if        QTTYPE <> 'X'
     C                              and QTTYPE <> 'T'
     C                   eval      QTXFER = QTTYPE
     C                   eval      QTTYPE = 'X'
     C                   eval      QTWKDT = sysDt7
     C                   eval      saveQTNBDT = QTNBDT
     C                   exsr      LdInitMntQT
     C                   write     pqt01
     C                   endif
     C                   endif
    C*
     C                   eval      xxagt# = ahagt#
     C                   move      'Q'           xxstat
    C*
     C     qtky2         chain     pqt01                              60
    C*
     C                   if        *in60 = *off

     C                   if        QTTYPE <> 'F' and
     C                             QTTYPE <> 'G'
     C                               and QTTYPE <> 'p' and QTTYPE <> 'q'
     C                               or newAgt# <> currentAgt#
     C                               or isNoCurrAgt

     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      qtemp# = xpemp#
     C                   eval      qtdiv = xpdiv
     C                   else
     C                   eval      qtemp# = qotemp
     C                   endif
    C*
     C                   select
     C                   when      currentAgt# <> newAgt#
     C                   eval      QTTYPE = 'T'
     C                   eval      QTNBDT = saveQTNBDT
     C                   other
     C                   eval      QTTYPE = 'W'
     C                   eval      QTNBDT = sysDt7
     C                   endsl
    C*
     C                   eval      QTWKDT = sysDt7
     C                   eval      QTSTAT = 'W'
    C*
     C                   exsr      LdMntQT
    C*
     C                   update    pqt01
    
     C                   else
     C                   delete    pqt01
     C                   endif
    
     C                   endif
    C*
     C                   endif
    C*
     C                   delete    pah01
    C*
     C     qtky          reade     pah01                                  60
    C*
     C                   enddo
    C*_______________________________________________________________
    C*
    C*   WRITE BILLING SCHEDULE RECORD(S), IF NOT ALREADY EXISTING
    C*
     C                   eval      xxbcnt = *zero
    C*
     C     qtky          chain     pbl01                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxfyr = blfyr
    C*
     C     blky2         chain     pbl02                              62
    C*
     C                   if        *in62 = *on
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      kkemp# = xpemp#
     C                   else
     C                   eval      kkemp# = qotemp
     C                   endif
     c     ehkyt2        chain     peh01
     c                   if        %found
     c                   move      *off          *in62
     c                   endif
     c                   endif
     *
     C                   if        *in62 = *on
    C*
     C     blky          chain     pbl01                              60
    C*
     C                   if        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      blemp# = xpemp#
     C                   eval      bldiv = xpdiv
     C                   else
     C                   eval      blemp# = qotemp
     C                   endif
     C                   movel(p)  q1pgm         blpgm
     C                   move      'Y'           blflg1
     C                   move      'Y'           blflg3
      * The flag is set to 'R' to process the Mass Renewal Billing
     C                   movel     nbrnflg       blrflg
     C                   movel     *blanks       nbrnflg
     C                   eval      blagt# = *zero
    C*
     C     ahky2         setgt     pah02
     C     ahky2         readpe(n) pah02                                  60
    C*
     C                   dow       *in60 = *off
     C                             and ahstat = 'I'
     C                   readpe(n) pah01                                  60
     C                   enddo
    C*
     C                   if        *in60 = *off
     C                   movel     ahagt#        blagt#
     C                   endif
    C*
     C     elky          chain(n)  pel02                              60
    C*
     C                   move      elpfdt        blpfdt
    C*
     C                   write     pbl02
    C*
     C                   eval      xxbcnt = xxbcnt + 1
    C*
     C                   endif
    C*
     C                   delete    pbl01
    C*
     C     qtky          reade     pbl01                                  60
    C*
     C                   enddo
    C*_______________________________________________________________
    C*
    C*   Write Office Auxiliary File record(s), if not already existing
    C*
     C                   eval      xxfcnt = *zero
    C*
     C     qtky          chain     pof00q                             60
    C*
     C                   dow       *in60 = *off
    C*
     C                   eval      xxemp# = qotemp
    C*
     C     qtky1         chain     pof01                              62
    C*
     C                   if        *in62 = *on
    C*
     C                   eval      xxemp# = qoemp#
    C*
     C     qtky1         chain     pof00q                             60
    C*
     C                   select
     C                   When      (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      ofemp# = xpemp#
     C                   eval      ofdiv = xpdiv
     C                   other
     C                   eval      ofemp# = qotemp
     C                   endsl
    C*
     C                   movel     q1pgm         ofpgm
    C*
     C                   write     pof01
    C*
     C                   eval      xxfcnt = xxfcnt + 1
    C*
     C                   endif
    C*
     C                   delete    pof00q
    C*
     C     qtky          reade     pof00q                                 60
    C*
     C                   enddo
    C*_______________________________________________________________
    C*
    C*   Delete Employer Discount Detail File information from QUOTE
    C*
     C                   dou       *in60 = *on
     C     qtky          delete    pds00q                             60
     C                   enddo
    C*
    C*   Delete Employer Increased Limits File information from QUOTE
    C*
     C                   dou       *in60 = *on
     C     qtky          delete    pil00q                             60
     C                   enddo
    C*
    C*   Delete Employer Premium File information from QUOTE
    C*
     C                   dou       *in60 = *on
     C     qtky          delete    pmp00q                             60
     C                   enddo
    C*_______________________________________________________________
    C*
    C*   PRINT TOTAL COUNTS
    C*
     C                   if        *inoa = *on
     C                   except    head
     C                   except    det
     C                   write     pqh01
     C                   endif
    C*
     C                   except    cnt
     *
     *  PEO Group Endorsements
     *
     C                   If        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   move      qoco#         #1co#
     C                   move      qofnd         #1fnd
     C                   move      xpemp#        #1emp#
     C                   move      xpdiv         #1div
     *
     C                   eval      #1EnDt = %editc(DateTo8(SystemDate):'X')
     *
     C                   call      'W0051C31'
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1emp#
     C                   parm                    #1div
     C                   parm                    #1endt
     C                   parm                    #locid           10
     *
     C                   endif
    C*
    C*  DELETE/UPDATE CROSS REFERENCE RECORD
    C*
     C     exky          Chain     pex01                              60
     C                   If        %found
    C*
     C     emky#         Chain     pem02                              60
     C                   If        *IN60 = *off
     C                   eval      exemp# = qotemp
     C                   update    pex01
     C                   else
     C                   delete    pex01
     C                   endif
     C                   endif
    C*
    C*  DELETE TRANSFER RECORD
    C*
     C                   delete    pqo01
      /Free
       Endif;
      /End-free
    C*
    C*  READ NEXT TRANSFER RECORD
    C*
     C     NextXFer      tag
     c                   If        isImdTrnsfr = 'Y'
     c     keyWAQOL      reade     pqo01                                  60
     c                   Dow       QOitfr <> 'Y' and not(*in60)
     c     keyWAQOL      reade     pqo01                                  60
     c                   enddo
     c                   Else
     C     qoky          reade     pqo01                                  60
     C                   EndIf
    C*
     C                   enddo
    C*
     C                   move      *on           *inlr
     *====================================================================
     *  Create PEO Certificate record
     *====================================================================
     C     certrd        begsr
     *
     C     fnky          chain(n)  pfn01                              60
    C*
     C                   eval      k#emp# = elemp#
     C                   eval      k#div = eldiv
    C*
     C                   if        *in60 = *off
     C     emky3         chain     pem02                              60
     C                   endif
     *
     *  If PEO group
     *
     C                   if        fnpeof <> *blanks
     C                             and *in60 = *off
     *
     *  If employer detail record exits for PEO employer
     *
     C                   eval      k#co# = elco#
     C                   eval      k#fnd = elfnd
     C                   eval      k#emp# = elemp#
     C                   eval      k#div = eldiv
     C                   eval      k#fyr = elfyr
     C                   eval      k1emp# = 1
     C                   eval      k1div = 0
     C     elky4         chain(n)  pel02                              60
     *
     C                   if        *in60 = *off
     C     elky5         chain(n)  pel02                              60
     *
     *  Get next Cert # for PEO employer (1st employer in group)
     *
     C     ceky          setgt     pce01
     C     ceky          readpe(n) pce01                                  60
     *
     C                   if        *in60 = *off
     C                   eval      xxcer# = 1 + cecer#
     C                   else
     C                   eval      xxcer# = 1
     C                   endif
     *
     *  Write new Cert Holder record for PEO employer
     *
     C                   clear                   pce01
     C                   eval      ceco# = elco#
     C                   eval      cefnd = elfnd
     C                   eval      cefyr = elfyr
     C                   eval      ceemp# = 1
     C                   eval      cediv = 0
     C                   eval      cecer# = xxcer#
     C                   movel(p)  emname        cename
     *
     C                   if        emmad1 <> *blanks
     C                   movel(p)  emmad1        ceadr1
     C                   movel(p)  emmad2        ceadr2
     C                   movel(p)  emmcty        cecty
     C                   movel(p)  emmst         cest
     C                   eval      cezip = emmzip
     *
     C                   else
     *
     C                   movel(p)  empad1        ceadr1
     C                   movel(p)  empad2        ceadr2
     C                   movel(p)  empcty        cecty
     C                   movel(p)  empst         cest
     C                   eval      cezip = empzip
     C                   endif
     *
     C                   move      'Y'           cerlfl
     C                   move      'A'           cestat
     *
     C                   movel     q1user        ceiusr
     C                   movel     q1user        ceuser
     C                   movel     q1pgm         ceipgm
     C                   movel     q1pgm         cepgm
     C                   eval      CETIME = TimeTo6(SystemTime)
     C                   eval      CEDT   = sysDt7
     C                   eval      CEITM  = CETIME
     C                   eval      CEIDT  = CEDT
     *
     C                   write     pce01
     C                   endif
     *
     C                   endif
     *
     C                   endsr
    C*
      /free
       //==================================================================
       // DelWXELL - Delete record from quote env
       //==================================================================
       Begsr DelWXELL;

       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pxel0q;
       If %found(WXELLQ);
          Delete WXELLQ;
       Endif;

       Endsr;
      /end-free
     C*====================================================================
     C* Load the initial maintenance fields for WAQTP
     C*====================================================================
     C     LdInitMntQT   begsr
     C*
     C                   exsr      LdMntQT
     C*
     C                   eval      QTIUSR = QTUSER
     C                   eval      QTIPGM = QTPGM
     C                   eval      QTITM  = QTTIME
     C                   eval      QTIDT  = QTDT
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Load the maintenance fields for WAQTP
     C*====================================================================
     C     LdMntQT       begsr
     C*
     C                   eval      QTUSER = q1user
     C                   eval      QTPGM  = q1pgm
     C                   eval      QTTIME = TimeTo6(SystemTime)
     C                   eval      QTDT   = sysDt7
     C*
     C                   endsr
    C*
    C*===============================================================
    C*  SR1ST SUBROUTINE - FIRST TIME INITIALIZATION
    C*===============================================================
    C*
     C     sr1st         begsr
    C*
    C*   CHAIN TO THE COMPANY FILE
    C*
     C     coky          chain     pco01                              60
    C*
    C*   GET THE DISTRIBUTION INFORMATION
    C*
     C                   open      qsysprt
    C*
     C                   move      '0'           xxprt
    C*
     C     orky          chain     por01                              60
    C*
     C                   if        *in60 = *off
    C*   PRINT THE DISTRIBUTION INFORMATION
     C                   except    route
     C                   move      *on           *inoa
     *
     C                   movel(p)  ortitl        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
     C                   movel(p)  ##data        ortitl
     *
     C                   endif
    C*
     C                   eval      svfnd = *zero
     C                   eval      svemp# = *zero
     C*** begin add ***  04/07/25  *******************************************
     C                   Clear                   saveFnd           3 0
     C*** end add *****  04/07/25  *******************************************
    C*
     C                   move      *on           *in11
    C*
     C                   endsr
    C*
    C*===============================================================
    C*  GRPBRK SUBROUTINE - GROUP BREAK: GET GROUP NAME PRINT NEW
    C*===============================================================
    C*
     C     grpbrk        begsr
    C*
    C*   CHAIN TO THE GROUP FILE
    C*
     C                   clear                   fnaute
    C*
     C     fnky          chain(n)  pfn01                              60
    C*
     C                   if        *in60 = *on
     C                   eval      svfnd = *zero
     C                   else
     C                   eval      svfnd = fnfnd
     C                   endif
    C*
    C*  GET NEXT AVAILABLE EMPLOYER NUMBER IN THE GROUP
    C*
     C                   if        fnaute = 'Y'
    C*
     C     fnky          chain     pfn01                              60
    C*
     C                   if        *in60 = *off
     C                   eval      xxemp# = fnnaen
     C                   if        qotemp = *zero
     C                   eval      fnnaen = fnnaen + xxnaen
     C                   endif
      *** Beg Add ***** 05/23/24 ***********************
      /free
        FNpgm  = Q1pgm;
        FNuser = Q1user;
        FNtime = Timeto6(SystemTime());
        FNdt   = Dateto7(SystemDate());
      /end-free
      *** End Add ***** 05/23/24 ***********************
     C                   update    pfn01
     C                   else
     C                   eval      xxemp# = xxnaen
     C                   endif
    C*
     C                   endif
    C*
     C                   eval      svemp# = qoemp#
     C*** begin add ***  04/07/25  *******************************************
     C                   Eval      QOTEmp = xxEmp#
     C                   Eval      saveFnd = svfnd
     C                   Move      xxEmp#        saveEmp#          9 0
     C*** end add *****  04/07/25  *******************************************
    C*
     C                   endsr
    C*
    C*===============================================================
    C*  DARTS1 SUBROUTINE -  DARTS UPDATE FOR EMPLOYER MASTER
    C*===============================================================
    C*
     C     darts1        begsr
    C*
    C* ESTABLISH KEY  FOR QUOTE SIDE
    C*
     C                   clear                   qekey
     C                   clear                   qmkey
     C                   eval      qmco# = xxco#
     C                   eval      qmfnd = qofnd
     C                   eval      qmemp# = qoemp#
     C                   eval      qmdiv = qodiv
     C                   move      'Q'           qmkey
    C*
     C                   movel     qmkey         qekey
    C*
    C* ESTABLISH KEY  FOR LIVE SIDE
    C*
     C                   clear                   lekey
     C                   clear                   qmkey
     C                   eval      qmco# = xxco#
     C                   eval      qmfnd = qofnd
     C                   If        (FNpeoF = 'S') Or
     C                             (FNpeoF = 'C' And XPemp# = 1)
     C                   eval      qmemp# = xpemp#
     C                   eval      qmdiv = xpdiv
     C                   else
     C                   eval      qmemp# = qotemp
     C                   eval      qmdiv = qodiv
     C                   endif
    C*
     C                   movel     qmkey         lekey
    C*
    C* UPDATE DARTS' COMMENTS
    C*
     C                   call      'SC900C02'
     C                   parm      qekey         @fky
     C                   parm      lekey         @tky
     C                   parm      'QPRINT  '    @que
     C                   parm      *blank        @mod
     C                   parm      *blank        @rtc
    C*
     C                   endsr
    C*
    C*===============================================================
    C* MCAPI - DATA INTERFACE TO MANAGED CARE - FMEMP
    C*===============================================================
    C*
     C     mcapi         begsr
    C*
     C                   move      emco#         qqco#
     C                   move      emfnd         qqfnd#
     C                   move      ememp#        qqemp#
     C                   move      emdiv         qqdiv
    C*
     C                   move      emfein        qqfein
     C                   move      emmzip        qqmzip
     C                   move      empzip        qqpzip
     C                   move      emarea        qqarea
     C                   move      emphn#        qqphn#
     C                   move      emrgn#        qqrgn#
    C*
     C                   call      'FA010R'
    C*
     C                   parm      *blank        qqrtcd
    C*
     C                   parm                    qqco#
     C                   parm      *blank        qqsub#
     C                   parm                    qqfnd#
     C                   parm                    qqemp#
     C                   parm                    qqdiv
    C*
     C                   parm      emname        qqname
     C                   parm      emmad1        qqmad1
     C                   parm      emmad2        qqmad2
     C                   parm      emmcty        qqmcty
     C                   parm      emmst         qqmst
     C                   parm                    qqmzip
     C                   parm      empad1        qqpad1
     C                   parm      empad2        qqpad2
     C                   parm      empcty        qqpcty
     C                   parm      empst         qqpst
     C                   parm                    qqpzip
     C                   parm                    qqarea
     C                   parm                    qqphn#
     C                   parm                    qqrgn#
     C                   parm                    qqfein
     C                   parm      emvip         qqvip
     C                   parm      emmpfl        qqmpfl
     C                   parm      emstrp        qqstrp
     C                   parm      *blank        qqdba
     C                   parm      emnaob        qqnob
     C                   parm      *blank        qqmco
     C                   parm      *blank        qqmco#
    C*
     C                   endsr
       //*** Beg Add ***** 05/21/21 ***********************
       //---------------------------------------
       Begsr Update3rdPartySetup;
       //---------------------------------------
        timeStamp  = %TimeStamp();
        // Update 3rd Party Policy Setup
        Exec Sql Update wipsp
          Set PSemp# = :ELemp#, PSqteXfer = :timeStamp,
          PS3ptyUpd = :timeStamp
          Where PSco# = :ELco# And PSfnd = :ELfnd And PSemp# = :xxEmp#;
        // Update 3rd Party Policy Setup History
        Exec Sql Update wipsphist
          Set PSemp# = :ELemp#
          Where PSco# = :ELco# And PSfnd = :ELfnd And PSemp# = :xxEmp#;
        // Call CCF 3rd Party Updates if the employer and/or the division
        // change.
        If ELemp# <> qoEmp# Or ELdiv <> qoDiv;

           // Has this been sent to Invoice Cloud
           Exec Sql Select count(*) Into :intPayArCount
                    From WDEAP_HLD Where EAco#=:ELco# And
                    EAfnd=:ELfnd And EAemp#=:xxEmp# And
                    EAipgm='WPIPMTSVC';

           // This went to Invoice Cloud so send CCF update
           If intPayArCount > 0;
              chrJson = '{"informationOld":{' +
                         '"company":"'  + %EditC(ELco#:'X')  + '",'  +
                         '"group":"'    + %EditC(ELfnd:'X')  + '",'  +
                         '"employer":"' + %EditC(xxEmp#:'X') + '"},' +
                         '"informationNew":{' +
                         '"company":"'  + %EditC(ELco#:'X')  + '",'  +
                         '"group":"'    + %EditC(ELfnd:'X')  + '",'  +
                         //*** Begin Chg *** 10/03/24 **********************
                         //*** '"employer":"' + %EditC(ELemp#:'X') + '"}}';
                         '"employer":"' + %EditC(ELemp#:'X') + '",'  +
                         '"active":"1"}}';
                         //*** End   Chg *** 10/03/24 **********************
              ThirdPartyUpdates(chrJson);
              //*** Begin Add *** 04/19/24 *********************************
              // Remove unpaid invoices from IVC
              RemoveUnpaidInvoices(ELco#:ELfnd:xxEmp#:ELdiv);
              //*** End   Add *** 04/19/24 *********************************
           EndIf;
        EndIf;
       EndSr;
       //---------------------------------------
       Begsr PostNewBusPayment;
       //---------------------------------------
        intArCnt = 0;
        Exec Sql
         Select Count(*)
          Into :intArCnt
          From WDEAP_HLD
          Where EAco# = :QOco#
           And EAfnd = :QOfnd
           And EAemp# = :QOemp#
           And EAdiv = :QOdiv
           And EAeatp = 'IP';
        If intArCnt = 0;
           LeaveSr;
        Endif;
        // Get date/time, Pay A/R Flag, Batch#
        //*** Beg Chg *** 01/25/24 *** 133WTD ************************
        //***GetStaticValues();
        GetStaticValues(QOco#:QOfnd);
        //*** End Chg *** 01/25/24 *** 133WTD ************************
       // Update WDEAP_HLD
        Exec Sql
         Update WDEAP_HLD
         Set EAemp# = :ELemp#,
          EAdiv = :ELdiv,
          EAeatp = '  ',
          EAachf = 'T',
          EAbat# = :decBatch#,
          //*** Begin Add *** 04/16/25 *************************************
          EAtrdt = 0,
          //*** End   Add *** 04/16/25 *************************************
          EAdt = :decSysDate,
          EAtime = :decSysTime,
          EAuser = :Q1user,
          EApgm  = :Q1pgm,
          EAidt  = :decSysDate,
          EAiusr = :Q1user,
          EAipgm = :Q1pgm
         Where EAco# = :ELco#
          And EAfnd = :ELfnd
          And EAemp# = :QOemp#
          And EAdiv = :QOdiv
          And EAfyr = :ELfyr
          And EAeatp = 'IP';
       // Transfer new business payment out of WDEAP_HLD to A/R Batch or Live A/R
       //*** Begin Chg *** 04/16/25 ****************************************
       //***  strSqlStmt = 'Select rrn(WDEAP_HLD) From WDEAP_HLD Where ' +
       //***   'EAco#='  + %EditC(ELco#:'X')  + ' And ' +
       //***   'EAfnd='  + %EditC(ELfnd:'X')  + ' And ' +
       //***   'EAemp#=' + %EditC(ELemp#:'X') + ' And ' +
       //***   'EAdiv='  + %EditC(ELdiv:'X')  + ' And ' +
       //***   'EAfyr='  + %EditC(ELfyr:'X');
        strSqlStmt = 'Select rrn(WDEAP_HLD), EAinv# From WDEAP_HLD Where ' +
         'EAco#='  + %EditC(ELco#:'X')  + ' And ' +
         'EAfnd='  + %EditC(ELfnd:'X')  + ' And ' +
         'EAemp#=' + %EditC(ELemp#:'X') + ' And ' +
         'EAdiv='  + %EditC(ELdiv:'X')  + ' And ' +
         'EAfyr='  + %EditC(ELfyr:'X');
       //*** End   Chg *** 04/16/25 ****************************************
        Exec Sql Prepare sqlWDEAP_HLD From :strSqlStmt;
        Exec SQL Declare cursorWDEAP_HLD Cursor For sqlWDEAP_HLD;
        Exec SQL Open cursorWDEAP_HLD;
        //*** Begin Chg *** 04/16/25 ***************************************
        //*** Exec SQL Fetch Next From cursorWDEAP_HLD Into :rrnWDEAP_HLD;
        Exec SQL Fetch Next From cursorWDEAP_HLD
                            Into :rrnWDEAP_HLD, :decInvoice#;
        //*** End   Chg *** 04/16/25 ***************************************
        If Not(IsValidFetch(sqlStt));
           LeaveSr;
        Endif;
        If isWrToArBatch = *On;
          // Load WDEAP_HLD to WDEAP
           strSql = 'Insert Into ' +
            %Trim(strLiveLib) + '/WDEAP ' +
            'Select * From WDEAP_HLD ' +
            'Where rrn(WDEAP_HLD) = ' +
            %Editc(rrnWDEAP_HLD:'3');
           Exec Sql Execute Immediate :strSql;
          // Remove A/R from WDEAP_HLD
           Exec Sql
            Delete from WDEAP_HLD
            Where rrn(WDEAP_HLD) = :rrnWDEAP_HLD;
          //*** Begin Add *** 04/16/25 *************************************
          //
          // Remove Quote Invoice from Invoice Cloud, it is being replaced
          // by the transferred live invoice.
          DeleteInvoice(%EditC(ELco#:'X'):%EditC(ELfnd:'X'):
              %EditC(QOemp#:'X'):%EditC(QOdiv:'X'):%EditC(decInvoice#:'X'));
          //*** End   Add *** 04/16/25 *************************************
          // Update Batch Amount
           strSql = 'Update ' +
            %Trim(strLiveLib) + '/WDEAP ' +
            'Set EAbata = (Select Sum(EAamt) From ' +
            %Trim(strLiveLib) + '/WDEAP ' +
            'Where EAbat# = ' + %Editc(decBatch#:'3') + ') ' +
            'Where EAbat# = ' + %Editc(decBatch#:'3');
           Exec Sql Execute Immediate :strSql;
        Else;
          // Transfer Batch to Live
           TransferARToLive('WDEAP_HLD':rrnWDEAP_HLD);
        EndIf;
        Exec SQL Close cursorWDEAP_HLD;
       Endsr;
       //*** End Add ***** 05/21/21 ***********************
    C*
     Oqsysprt   e            route          1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          e            route          1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          e            route          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          e            route          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          e            route          1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          e            route          1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          e            route          1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          e            route          1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          e            route          1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          e            route          1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          e            route          1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          e            route          3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          e            route          1
     O                                           15 'ROUTE TO:'
     O                       ordis1              60
     O          e            route          1
     O                       ordis2              60
     O          e            route          1
     O                       ordis3              60
     O          e            route          2
     O                       ordis4              60
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
     O          e            route          2
     O                                           19 'PROGRAM NAME:'
     O                       orpnam              30
     O                                           36 'FILE:'
     O                       orfil#              38
     O                                           46 'COPIES:'
     O                       orcopy        z     50
     O                                           56 'HOLD:'
     O                       orhold              61
     O          e            route          2
     O                                           19 'OUTPUT QUEUE:'
     O                       oroutq              30
     O                                           37 'FORMS:'
     O                       orform              48
     O                                           56 'SAVE:'
     O                       orsave              61
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O          e            head           1 06
     O                       dstime              08 '  :  :  '
     O                       xcname             129
     O                       xxDate             198
     O          e            head           2
     O                       q1pgm               10
     O                       ortitl             129
     O                                          192 'PAGE:'
     O                       page          z    198
     O          e            head           2
     O                                           06 'GROUP:'
     O                       fnfnd         z     10
     O                       fnname              71
     O          e            head           1
     O                                           13 'ASSIGNED'
     O                                           57 'QUOTED'
     O                                           66 'EMPL'
     O                                           77 'EMPL'
     O                                           88 'EMPL'
     O                                           97 'EMPL'
     O                                          105 'EMPL'
     O                                          113 'EMPL'
     O                                          123 'A/R'
     O                                          135 'AGENT'
     O                                          142 'BILL'
     O                                          148 'S/R'
     O                                          160 'ADDL'
     O                                          169 'EMPL'
     O                                          176 'VDR'
     O                                          182 'ACH'
     O                                          191 'EMPL'
     O                                          201 ' SCHD '
     O                                          208 'EXCL'
     *
     O          e            head        0  0
     O                       line               210
     *
     O          e            head           2
     O                                              ' EMPLOYER'
     O                                         +  3 'DIV'
     O                                         +  1 'EMPLOYER NAME'
     O                                           54 'EMPLOYER'
     O                                         +  3 'DIV'
     O                                         +  1 'MASTER'
     O                                         +  1 'OWNER/OFFICER'
     O                                         +  1 'ENT/LOC'
     O                                         +  1 'BILL ADDR'
     O                                         +  1 'DETAIL'
     O                                         +  1 'PAYROLLS'
     O                                         +  1 'TRANSACTIONS'
     O                                         +  1 'HISTORY'
     O                                         +  1 'SCHED'
     O                                         +  1 'PAYROLL'
     O                                         +  1 '  ENT/LOC  '
     O                                         +  2 'CONTACT'
     O                                         +  2 'HIST'
     O                                         +  2 'INF'
     O                                         +  2 'NAMED INS'
     O                                         +  2 'RATING'
     O                                          208 ' ST '
     O          e            det            0
     O                       xxtemp        z     09
     O                       xxtdiv        z     15
     O                       exname              41
     O                       qoemp#        z     54
     O                       qodiv         z     60
     O          e            cnt            1
     O                       xxecnt        z     65
     O                       xxocnt        z     76
     O                       xxlcnt        z     88
     O                       xx3cnt        z     96
     O                       xxdcnt        z    104
     O                       xxpcnt        2    114
     O                       xxrcnt        2    124
     O                       xxacnt        z    133
     O                       xxbcnt        z    140
     O                       xxscnt        z    147
     O                       xxxcnt        z    156
     O                       xeccnt        z    169
     O                       xvdcnt        z    178
     O                       xddcnt        z    184
     O                       xxncnt        z    192
     O                       xxhcnt        z    198
     O                       countWDXSP    z    208
     O          e            norecs      2  2
     O                                           24 'NO RECORDS MEET SELECTIO'
     O                                           34 'N CRITERIA'
       //*** beg add ***** 05/21/21 ***********************
       //-------------------------------------
       Dcl-Proc ChkOptInStatus;
       //-------------------------------------
        Dcl-Pi *N Ind;
        End-Pi;
        Dcl-S flgOptIn Char(1);
        Dcl-S indOptIn Ind;
        Exec Sql Select PSoptFlag Into :flgOptIn From WIPSP
         Where PSco#=:ELco# And PSfnd=:ELfnd And PSemp#=:QOemp#;
        If flgOptIn = 'I';
           indOptIn = *On;
        Endif;
        Return indOptIn;
       End-Proc;
       //-------------------------------------
       Dcl-Proc GetStaticValues;
       //-------------------------------------
       //*** Beg Add *** 01/25/24 *** 133WTD *******************************
       Dcl-Pi *N;
        inCo# Like(EAco#);
        inFnd Like(EAfnd);
       End-Pi;
       //*** End Add *** 01/25/24 *** 133WTD *******************************
        Dcl-S flgPayAR Char(1);
        //*** Beg Add *** 01/25/24 *** 133WTD ******************************
        Dcl-S flg3pyGrp Char(1);
        Chain (inCo#:inFnd) pex00;
        Clear flg3pyGrp;
        If %found(WMEXL);
           flg3pyGrp = EXiCpsGrp;
        Endif;
        //*** End Add *** 01/25/24 *** 133WTD ******************************
       // Execute routine just one time
        If hasStaticValues = *On;
           Return;
        EndIf;
        hasStaticValues = *On;
       // Get the system date and time
        decSysDate = DateTo7(systemDate());
        decSysTime = TimeTo6(systemTime());
       // Get the IC Pay AR Flag
        Exec Sql Select EXicPayAR Into :flgPayAR From WMEXP
         Where EXco#=:ELco# And EXfnd=:ELfnd;
        If flgPayAR <> 'Y';
           isWrToArBatch = *On;
        EndIf;
       // Get live library name
        strLiveLib = RtvQuoteLib('*LIV');
       // Get batch # if writing to A/R Batch
        If isWrToArBatch = *On;
           // Is there a non-approved 3rd party batch in WDEAP
           //  created today?
           strSql = 'Select EAbat# ' +
            'From ' + %Trim(strLivelib) + '/WDEAP ' +
            'Where EAachF = ''T'' And EAtfAp <> ''Y'' And '  +
            //*** Beg Chg *** 01/25/24 *** 133WTD **************************
            ///***'EAidt = ' + %Editc(decSysDate:'3') + ' Limit 1';
            'EAidt = ' + %Editc(decSysDate:'3');
            //*** End Chg *** 01/25/24 *** 133WTD **************************
            //*** Beg Add *** 01/25/24 *** 133WTD **************************
            Select;
            When flg3pyGrp = 'Y';
               strSql += ' And EAfnd = ' + %Editc(EXfnd:'X');
            Other;
               strSql += ' And EAfnd In (Select EXfnd ' +
               'From ' + %Trim(strLivelib) + '/WMEXP ' +
               'Where EXicpsgrp <> ''Y'')';
            Endsl;
            strSql += ' Limit 1';
            //*** End Add *** 01/25/24 *** 133WTD **************************

           Exec SQL Prepare e1 From :strSQL;
           Exec SQL Declare e1Cursor Cursor For e1;
           Exec SQL Open e1Cursor;
           Exec SQL Fetch Next From e1Cursor
            Into :decBatch#;
           Exec SQL Close e1Cursor;

          // Get next available batch# if not
           If decBatch# = 0;
              strSql = 'Select Max(EAbat#) + 1 From ' +
               %Trim(strLivelib) + '/WDEAP';
              Exec SQL Prepare e2 From :strSQL;
              Exec SQL Declare e2Cursor Cursor For e2;
              Exec SQL Open e2Cursor;
              Exec SQL Fetch Next From e2Cursor
               Into :decBatch#;
              Exec SQL Close e2Cursor;
           Endif;
          // In case WDEAP has no records, the next batch # = 1
           If decBatch# = 0;
              decBatch# = 1;
           EndIf;
        Endif;
       End-Proc;
       //*** end add ***** 05/21/21 ***********************
       //*** Begin Add *** 04/19/24 *********************************
       //
       // Remove unpaid invoices from IVC
       Dcl-Proc RemoveUnpaidInvoices;
       Dcl-Pi *N;
          inCo#  Like(EAco#);
          inFnd  Like(EAfnd);
          inEmp# Like(EAemp#);
          inDiv  Like(EAdiv);
       End-Pi;

       Dcl-S strQuoteLib Char(10);
       Dcl-S decInvoice# Zoned(10:0);
       Dcl-S chrCo#      Char(3);
       Dcl-S chrFnd      Char(3);
       Dcl-S chrEmp#     Char(9);
       Dcl-S chrDiv      Char(5);
       Dcl-S chrInv#     Char(10);
       Dcl-S strSql2     Like(strSql) Inz;
       Dcl-S strSql3     Like(strSql) Inz;

       // Get live library name
       strQuoteLib = RtvQuoteLib('*QTE');

       // Find unpaid invoices.
       chrCo#  = %EditC(inCo#:'X');
       chrFnd  = %EditC(inFnd:'X');
       chrEmp# = %EditC(inEmp#:'X');
       chrDiv  = %EditC(inDiv:'X');
       strSql  = 'Select EAinv#  ' +
                 'From ' + %Trim(strQuotelib) + '/WDEAP_HLD ' +
                 'Where EAco#=' + chrCo# + ' And EAfnd=' + chrFnd +
                 ' And EAemp#=' + chrEmp# + ' And EAdiv=' + chrDiv;

       // Define global variable to hold A/R Count
       Exec Sql Drop Variable AR_Count;
       Exec Sql Create Or Replace Variable AR_Count Decimal(5,0);

       Exec SQL close batchDelCursor;
       Exec SQL Prepare batchDel From :strSQL;
       Exec SQL Declare batchDelCursor Cursor For batchDel;
       Exec SQL Open batchDelCursor;

       Dou Not IsValidFetch(sqlStt);
          Exec SQL Fetch Next From batchDelCursor Into :decInvoice#;
          If isValidFetch(sqlStt);

             // Check to see if there is any A/R which means it is paid
             chrInv# = %EditC(decInvoice#:'X');

             // Set AR_Count
             strSql3 = 'Set AR_count = ' +
                       '(Select count(*) ' +
                       'From ' + %Trim(strQuotelib) + '/WDEAP_HLD ' +
                       'Where EAco#=' + chrCo# + ' And EAfnd=' + chrFnd +
                       ' And EAemp#=' + chrEmp# + ' And EAdiv=' + chrDiv +
                       ' And EAinv#=' + chrInv# + ' And EAeaTp = ''IP'')';
             Exec Sql Execute Immediate :strSql3;

             // Load global variable into work field
             exec sql Set :count = AR_Count;

             // Drop global variable

             // Skip if there the invoice was paid
             If count <> 0;
                iter;
             EndIf;

             // Remove invoice from IVC
             DeleteInvoice(chrCo#:chrFnd:chrEmp#:chrDiv:chrInv#);

             // Remove invoice from batch hold file
             strSql2 = 'Delete ' +
                       'From ' + %Trim(strQuotelib) + '/WDEAP_HLD ' +
                       'Where EAco#=' + chrCo# + ' And EAfnd=' + chrFnd +
                       ' And EAemp#=' + chrEmp# + ' And EAdiv=' + chrDiv +
                       ' And EAinv#=' + chrInv#;
             Exec Sql Execute Immediate :strSql2;

          EndIf;
       EndDo;

       Exec Sql Drop Variable AR_Count;
       Exec SQL close batchDelCursor;

       End-Proc;
       //*** End   Add *** 04/19/24 *********************************


